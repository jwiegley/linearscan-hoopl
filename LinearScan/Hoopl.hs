{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LinearScan.Hoopl
    ( NodeAlloc(..)
    , allocateHoopl
    ) where

import           Compiler.Hoopl as Hoopl hiding ((<*>))
import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.Functor.Identity
import qualified Data.IntMap as IM
import qualified Data.Map as M
import           Data.Monoid
import           Debug.Trace
import           LinearScan
import           LinearScan.Hoopl.DSL
import           Unsafe.Coerce

class HooplNode nv => NodeAlloc nv nr | nv -> nr, nr -> nv where
    isCall   :: nv O O -> Bool
    -- ^ Return @True@ if the operation node represents a call to another
    -- procedure.

    isBranch :: nv O C -> Bool
    -- ^ Return @True@ if the operation node is a branch at the end of a basic
    -- block. Often, the only other possibility is a return instruction.

    retargetBranch :: nv O C -> Label -> Label -> nv O C
    -- ^ Given a branching node and a destination label, retarget the branch
    -- so it goes to the second label in place of the first.

    mkLabelOp :: Label -> nv C O
    -- ^ Construct a label operation.

    mkJumpOp  :: Label -> nv O C
    -- ^ Construct a jump operation to the given label.

    getReferences :: nv e x -> [VarInfo]
    -- ^ Given a node, return its list of 'LinearScan.VarInfo' references.

    setRegisters  :: [((VarId, VarKind), PhysReg)] -> nv e x -> Env (nr e x)
    -- ^ Given a set of register allocations and an operation node, apply
    -- those allocations within the provided 'Env' environment and produce a
    -- result node with the allocations applied.

    mkMoveOps    :: PhysReg -> VarId -> PhysReg -> Env [nr O O]
    -- ^ Construct operation(s) to move a variable's value from one register
    -- to another.

    mkSaveOps    :: PhysReg -> VarId -> Env [nr O O]
    -- ^ Construct operation(s) that spill a variable's value from a register
    -- to the spill stack.

    mkRestoreOps :: VarId -> PhysReg -> Env [nr O O]
    -- ^ Construct operation(s) that load a variable's value into a register
    -- from the spill stack.

    op1ToString  :: nv e x -> String
    -- ^ Render the given operation node as a 'String'.

data NodeV n = NodeCO { getNodeCO :: n C O }
             | NodeOO { getNodeOO :: n O O }
             | NodeOC { getNodeOC :: n O C }

blockInfo :: (NodeAlloc nv nr, NonLocal nv, NonLocal nr)
          => BlockInfo Env (Block nv C C) (Block nr C C)
                           (NodeV nv) (NodeV nr)
blockInfo = BlockInfo
    { blockId = unsafeCoerce . entryLabel

    , blockSuccessors = unsafeCoerce . successors

    , splitCriticalEdge = \(BlockCC b m e)
                           (BlockCC next _ _) -> do
        let lab = entryLabel next
        (next:supply, stack) <- get
        put (supply, stack)
        let lab' = unsafeCoerce next
        return (BlockCC b m (retargetBranch e lab lab'),
                BlockCC (mkLabelOp lab') BNil (mkJumpOp lab))

    , blockOps = \(BlockCC a b z) ->
        ([NodeCO a], Prelude.map NodeOO (blockToList b), [NodeOC z])

    , setBlockOps = \_ as b zs ->
        case as of
            [a] -> case zs of
                [z] ->
                    BlockCC (getNodeCO a)
                            (blockFromList (Prelude.map getNodeOO b))
                            (getNodeOC z)
                [] -> error "End node missing!"
                _ -> error "Too many end nodes"
            [] -> error "Beginning node missing!"
            _ -> error "Too many beginning nodes"
    }

opInfo :: NodeAlloc nv nr => OpInfo Env (NodeV nv) (NodeV nr)
opInfo = OpInfo
    { opKind = \node -> case node of
           NodeOO n | isCall n   -> IsCall
                    | otherwise  -> IsNormal
           NodeOC n | isBranch n -> IsBranch
                    | otherwise  -> IsNormal
           _ -> IsNormal

    , opRefs = \node -> case node of
           NodeCO n -> getReferences n
           NodeOO n -> getReferences n
           NodeOC n -> getReferences n

    , moveOp    = \x xv y    -> fmap NodeOO <$> mkMoveOps x xv y
    , saveOp    = \x xv      -> fmap NodeOO <$> mkSaveOps x xv
    , restoreOp = \yv y      -> fmap NodeOO <$> mkRestoreOps yv y

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

allocateHoopl
    :: (NodeAlloc nv nr, NonLocal nv, NonLocal nr)
    => Int          -- ^ Number of machine registers available
    -> Int          -- ^ Offset of the spill stack in bytes
    -> Int          -- ^ Size of a spilled register in bytes
    -> UseVerifier  -- ^ Whether to use the runtime allocation verifier
    -> Label        -- ^ Entry label of the program graph
    -> Graph nv C C -- ^ Hoopl program graph
    -> (String, Either [String] (Graph nr C C))
                   -- ^ Status dump and allocated blocks, or error w/ context
allocateHoopl regs offset slotSize useVerifier entry graph =
    fmap newGraph <$> runIdentity (go (1 + IM.size (unsafeCoerce body)))
  where
    newGraph xs = GMany NothingO (newBody xs) NothingO
      where
        newBody = Data.Foldable.foldl' (flip addBlock) emptyBody

    GMany NothingO body NothingO = graph

    go n = evalStateT alloc ([n..], newSpillStack offset slotSize)
      where
        alloc = allocate regs blockInfo opInfo useVerifier $
                    postorder_dfs_from body entry
