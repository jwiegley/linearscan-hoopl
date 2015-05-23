{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Assembly where

import           Compiler.Hoopl as Hoopl hiding ((<*>))
import           Control.Applicative
import           Data.Foldable
import qualified Data.List
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Traversable
import           Lens.Family hiding (Constant)
import           LinearScan
import           LinearScan.Hoopl
import           LinearScan.Hoopl.DSL

default (Int)

-- | The basic instructions that have nothing to do with control flow.
data Instruction reg
    = Add reg reg reg
    | Nop
    deriving (Eq, Functor, Foldable, Traversable)

instance Show r => Show (Instruction r) where
    show (Add x1 x2 x3) = "add " ++ show x1 ++ " " ++ show x2 ++ " " ++ show x3
    show Nop = "nop"

-- | Tests used for branching (correspond to branching instructions)
data Test = Zero                -- ^ beq
          | NonZero             -- ^ bne
          | Positive            -- ^ bgt
          | Negative            -- ^ blt
          deriving (Eq, Show)

data CConv = InlineC
           | CConvC { ccArgs     :: [PhysReg]
                    , ccResults  :: [PhysReg]
                    , ccIsBrack  :: Bool
                    }
    deriving (Eq, Show)

type Src a      = a -- ^ Type synonym for indicating source operands
type Dst a      = a -- ^ Type synonym for indicating destination operands
type Success a  = a -- ^ Type synonym for indicating success or true branch
type Failure a  = a -- ^ Type synonym for indicating failure or false branch

data Node v e x where
    Label :: Label -> Node v C O

    Alloc     :: Maybe (Src v) -> Dst v -> Node v O O
    Reclaim   :: Src v -> Node v O O
    Instr     :: Instruction v -> Node v O O
    Call      :: CConv -> Int -> Node v O O
    LoadConst :: Int -> Dst v -> Node v O O
    Move      :: Src v -> Dst v -> Node v O O
    Copy      :: Src v -> Dst v -> Node v O O
    Save      :: Src v -> Dst Int -> Node v O O
    Restore   :: Src Int -> Dst v -> Node v O O
    Trace     :: String -> Node v O O

    Jump        :: Label -> Node v O C
    Branch      :: Test -> v -> Success Label -> Failure Label -> Node v O C
    ReturnInstr :: [PhysReg] -> Instruction v -> Node v O C

deriving instance Eq v => Eq (Node v e x)

instance Show v => Show (Node v e x) where
    show (Label l)         = show l ++ ":"
    show (Alloc x1 x2)     = "\t@alloc " ++
                             (case x1 of Just v -> " " ++ show v ; _ -> " _")
                             ++ " " ++ show x2
    show (Reclaim v)       = "\t@reclaim " ++ show v
    show (Instr i)         = "\t" ++ show i
    show (Call c l)        = "\t@call " ++ show c ++ " " ++ show l
    show (LoadConst _c v)  = "\t@lc " ++ show v -- ++ " " ++ show c
    show (Move x1 x2)      = "\t@mvrr " ++ show x1 ++ " " ++ show x2
    show (Copy x1 x2)      = "\t@cprr " ++ show x1 ++ " " ++ show x2
    show (Save src dst)    = "\t@save " ++ show src ++ " " ++ show dst
    show (Restore src dst) = "\t@restore " ++ show src ++ " " ++ show dst
    show (Trace str)       = "\tTRACE " ++ " " ++ show str
    show (Jump l)          = "\t@jmp " ++ show l
    show (Branch c v t f)  = "\t@b" ++ show c ++ " " ++ show v
                          ++ " " ++ show t ++ "; @jmp " ++ show f
    show (ReturnInstr regs i) = "\t@return " ++ show regs ++ " " ++ show i

instance NonLocal (Node v) where
  entryLabel (Label l) = l

  successors (Jump l)          = [l]
  successors (Branch _ _ t f)  = [t, f]
  successors (ReturnInstr _ _) = []

instance HooplNode (Node v) where
    mkBranchNode = Jump
    mkLabelNode  = Label

variables :: Applicative f => LensLike f (Node v1 e x) (Node v2 e x) v1 v2
variables f = go
  where
    go (Alloc msrc dst)           = Alloc <$> traverse f msrc <*> f dst
    go (Reclaim src)              = Reclaim <$> f src
    go (Instr i)                  = Instr <$> traverse f i
    go (LoadConst c dst)          = LoadConst c <$> f dst
    go (Move src dst)             = Move <$> f src <*> f dst
    go (Copy src dst)             = Copy <$> f src <*> f dst
    go (Save src x)               = Save <$> f src <*> pure x
    go (Restore x src)            = Restore x <$> f src
    go (Trace str)                = pure $ Trace str
    go (Branch x1 cond x2 x3)     = Branch x1 <$> f cond <*> pure x2 <*> pure x3
    go (Call cc i)                = pure $ Call cc i
    go (ReturnInstr liveInRegs i) = ReturnInstr liveInRegs <$> traverse f i
    go (Label x)                  = pure $ Label x
    go (Jump x)                   = pure $ Jump x

add :: v -> v -> v -> BodyNode (Node v)
add x0 x1 x2 = bodyNode $ Instr (Add x0 x1 x2)

nop :: BodyNode (Node v)
nop = bodyNode $ Instr Nop

move :: v -> v -> BodyNode (Node v)
move x0 x1 = bodyNode $ Move x0 x1

call :: Int -> BodyNode (Node v)
call dst = bodyNode $ Call InlineC dst

lc :: v -> BodyNode (Node v)
lc x0 = bodyNode $ LoadConst 0 x0

save :: v -> Dst PhysReg -> BodyNode (Node v)
save r dst = bodyNode $ Save r dst

restore :: Src PhysReg -> v -> BodyNode (Node v)
restore src r = bodyNode $ Restore src r

trace :: String -> BodyNode (Node v)
trace str = bodyNode $ Trace str

branch :: Test -> v -> String -> String -> EndNode (Node v)
branch tst v good bad =
    endNode $ Branch tst v <$> getLabel good <*> getLabel bad

return_ :: EndNode (Node v)
return_ = endNode $ return $ ReturnInstr [] Nop

data Assign a b = Assign a b

instance Show a => Show (Assign a PhysReg) where
    show (Assign v (-1)) = "<<v" ++ show v ++ ">>"
    -- show (Assign v r)    = "r" ++ show r ++ "|v" ++ show v
    show (Assign _ r)    = show r

data IRVar = PhysicalIV PhysReg | VirtualIV Int deriving Eq

instance Show IRVar where
    show (PhysicalIV r) = "r" ++ show r
    show (VirtualIV n)  = "v" ++ show n

instance NodeAlloc Node IRVar (Assign VarId PhysReg) where
    fromVar (PhysicalIV r) = Left r
    fromVar (VirtualIV n)  = Right n

    fromReg (Assign _ r) = r

    isCall (Call {}) = True
    isCall _ = False

    isBranch (Jump {})   = True
    isBranch (Branch {}) = True
    isBranch _ = False

    retargetBranch (Jump _) _ lab = Jump lab
    retargetBranch (Branch b v x y) old lab
        | x == old  = Branch b v lab y
        | otherwise = Branch b v x lab
    retargetBranch x _ _ = error $ "Cannot retarget " ++ show x

    mkLabelOp = Label
    mkJumpOp  = Jump

    getReferences = go
      where
        go :: Node IRVar e x -> [VarInfo]
        go (Label _)         = mempty
        go (Instr i)         = fromInstr i
        go (Jump _)          = mempty
        go (Call _ _)        = mempty
        go (Move src dst)    = mkv Input src <> mkv Output dst
        go (LoadConst _ v)   = mkv Output v
        go (Branch _ v _ _)  = mkv Input v
        go (Trace _)         = mempty
        go (ReturnInstr _ i) = fromInstr i
        go n = error $ "getReferences: unhandled node: " ++ show n

        fromInstr :: Instruction IRVar -> [VarInfo]
        fromInstr Nop = mempty
        fromInstr (Add s1 s2 d1) =
            mkv Input s1 <> mkv Input s2 <> mkv Output d1

        mkv :: VarKind -> IRVar -> [VarInfo]
        mkv k (PhysicalIV n) = [vinfo k (Left n)]
        mkv k (VirtualIV n)  = [vinfo k (Right n)]

        vinfo k en = VarInfo
            { varId       = en
            , varKind     = k
            , regRequired = True
            }

    setRegisters m g = do
        for_ m $ \(v, r) -> setAssignment r v
        return $ over variables go g
      where
        go :: IRVar -> Assign VarId PhysReg
        go (PhysicalIV r) = Assign (-1) r
        go (VirtualIV n)  = Assign n (fromMaybe (-1) (Data.List.lookup n m))

    mkMoveOps src dst = do
        vid <- getAssignment src
        return [Move (Assign vid src) (Assign vid dst)]
    mkSwapOps src dst =
        liftA2 (++) (mkRestoreOps Nothing dst)
                    (mkSaveOps src Nothing)

    mkSaveOps src dst = do
        off <- getStackSlot dst
        vid <- getAssignment src
        return [Save (Assign vid src) off]
    mkRestoreOps src dst = do
        off <- getStackSlot src
        vid <- getAssignment dst
        return [Restore off (Assign vid dst)]

    op1ToString = show

var :: Int -> IRVar
var = VirtualIV

v0  = var 0
v1  = var 1
v2  = var 2
v3  = var 3
v4  = var 4
v5  = var 5
v6  = var 6
v7  = var 7
v8  = var 8
v9  = var 9
v10 = var 10
v11 = var 11
v12 = var 12
v13 = var 13
v14 = var 14
v15 = var 15
v16 = var 16
v17 = var 17
v18 = var 18
v19 = var 19
v20 = var 20
v21 = var 21
v22 = var 22
v23 = var 23
v24 = var 24
v25 = var 25
v26 = var 26
v27 = var 27
v28 = var 28
v29 = var 29
v30 = var 30
v31 = var 31
v32 = var 32
v33 = var 33
v34 = var 34
v35 = var 35
v36 = var 36
v37 = var 37
v38 = var 38
v39 = var 39
v40 = var 40

reg :: PhysReg -> IRVar -> Assign VarId PhysReg
reg _ (PhysicalIV _) = error "Don't use reg to reference a literal register"
reg r (VirtualIV v) = Assign v r

r0  = reg 0
r1  = reg 1
r2  = reg 2
r3  = reg 3
r4  = reg 4
r5  = reg 5
r6  = reg 6
r7  = reg 7
r8  = reg 8
r9  = reg 9
r10 = reg 10
r11 = reg 11
r12 = reg 12
r13 = reg 13
r14 = reg 14
r15 = reg 15
r16 = reg 16
r17 = reg 17
r18 = reg 18
r19 = reg 19
r20 = reg 20
r21 = reg 21
r22 = reg 22
r23 = reg 23
r24 = reg 24
r25 = reg 25
r26 = reg 26
r27 = reg 27
r28 = reg 28
r29 = reg 29
r30 = reg 30
r31 = reg 31
r32 = reg 32
