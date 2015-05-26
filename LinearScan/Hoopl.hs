{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LinearScan.Hoopl where

import           Compiler.Hoopl as Hoopl hiding ((<*>))
import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State (evalStateT, modify)
import           Data.Foldable
import qualified Data.Map as M
import           Data.Monoid
import           Debug.Trace
import           LinearScan
import           LinearScan.Hoopl.DSL
import           Unsafe.Coerce

class HooplNode (n v) => NodeAlloc n v r | n -> v, n -> r where
    fromVar :: v -> Either PhysReg VarId
    fromReg :: r -> PhysReg

    isCall   :: n v O O -> Bool
    isBranch :: n v O C -> Bool

    retargetBranch :: n v O C -> Label -> Label -> n v O C

    mkLabelOp :: Label -> n v C O
    mkJumpOp  :: Label -> n v O C

    getReferences :: n v e x -> [VarInfo]
    setRegisters  :: [(Int, PhysReg)] -> n v e x -> Env (n r e x)

    mkMoveOps    :: PhysReg -> PhysReg -> Env [n r O O]
    mkSwapOps    :: PhysReg -> PhysReg -> Env [n r O O]
    mkSaveOps    :: PhysReg -> Maybe VarId -> Env [n r O O]
    mkRestoreOps :: Maybe VarId -> PhysReg -> Env [n r O O]

    op1ToString  :: n v e x -> String

data NodeV n = NodeCO { getNodeCO :: n C O }
             | NodeOO { getNodeOO :: n O O }
             | NodeOC { getNodeOC :: n O C }

blockInfo :: (NodeAlloc n v r, NonLocal (n v))
          => (Label -> Env Int)
          -> BlockInfo Env (Block (n v) C C) (Block (n r) C C)
                           (NodeV (n v)) (NodeV (n r))
blockInfo getBlockId = BlockInfo
    { blockId = getBlockId . entryLabel

    , blockSuccessors = Prelude.mapM getBlockId . successors

    , splitCriticalEdge = \(BlockCC b m e)
                           (BlockCC next _ _) -> do
        let lab = entryLabel next
        lab' <- lift freshLabel
        modify $ \st ->
            st { envLabels   = M.insert (show lab ++ "'") lab' (envLabels st)
               , envBlockIds = let m = envBlockIds st in
                               M.insert lab' (M.size m + 1) m
               }
        let e' = retargetBranch e lab lab'
        return (BlockCC b m e',
                BlockCC (mkLabelOp lab') BNil (mkJumpOp lab))

    , blockOps = \(BlockCC a b z) ->
        ([NodeCO a], Prelude.map NodeOO (blockToList b), [NodeOC z])

    , setBlockOps = \_ [a] b [z] ->
        BlockCC
            (getNodeCO a)
            (blockFromList (Prelude.map getNodeOO b))
            (getNodeOC z)
    }

opInfo :: NodeAlloc n v r => OpInfo Env (NodeV (n v)) (NodeV (n r))
opInfo = OpInfo
    { opKind = \node -> case node of
           NodeOO n | isCall n  -> IsCall
                    | otherwise -> IsNormal
           NodeOC n | isBranch n -> IsBranch
                    | otherwise  -> IsNormal
           _ -> IsNormal

    , opRefs = \node -> case node of
           NodeCO n -> getReferences n
           NodeOO n -> getReferences n
           NodeOC n -> getReferences n

    , moveOp    = \x y -> fmap NodeOO <$> mkMoveOps x y
    , swapOp    = \x y -> fmap NodeOO <$> mkSwapOps x y
    , saveOp    = \x y -> fmap NodeOO <$> mkSaveOps x y
    , restoreOp = \x y -> fmap NodeOO <$> mkRestoreOps x y

    , applyAllocs = \node m ->
        case node of
           NodeCO n -> setRegisters m n >>= \alloc -> return [NodeCO alloc]
           NodeOO n -> setRegisters m n >>= \alloc -> return [NodeOO alloc]
           NodeOC n -> setRegisters m n >>= \alloc -> return [NodeOC alloc]

    , showOp1 = \node -> case node of
           NodeCO n -> op1ToString n
           NodeOO n -> op1ToString n
           NodeOC n -> op1ToString n
    }

newtype SimpleUniqueMonad' a = SUM' { unSUM' :: [Int] -> (a, [Int]) }

runSimpleUniqueMonad' :: Int -> SimpleUniqueMonad a -> a
runSimpleUniqueMonad' start m = fst (unSUM' (unsafeCoerce m) [start..])

allocateHoopl :: (NonLocal (n v), NonLocal (n r), NodeAlloc n v r)
              => Int             -- ^ Number of machine registers
              -> Int             -- ^ Offset of the spill stack
              -> Int             -- ^ Size of spilled register in bytes
              -> Label           -- ^ Label of graph entry block
              -> Graph (n v) C C -- ^ Program graph
              -> Either [String] (Graph (n r) C C)
allocateHoopl regs offset slotSize entry graph =
    newGraph <$> runSimpleUniqueMonad' (1 + length blocks) go
  where
    newGraph xs = GMany NothingO (newBody xs) NothingO
      where
        newBody = Data.Foldable.foldl' (flip addBlock) emptyBody

    blocks = postorder_dfs_from body entry
      where
        GMany NothingO body NothingO = graph

    go = evalStateT alloc (newEnvState offset slotSize)
      where
        alloc = allocate regs (blockInfo getBlockId) opInfo blocks
          where
            getBlockId :: Hoopl.Label -> Env Int
            getBlockId = return . unsafeCoerce
