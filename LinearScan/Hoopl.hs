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
import           Control.Monad.Trans.Tardis
import           Data.Foldable
import           Data.Functor.Identity
import qualified Data.Map as M
import           Data.Monoid
import           Debug.Trace
import           LinearScan
import           LinearScan.Hoopl.DSL
import           Unsafe.Coerce

class HooplNode nv => NodeAlloc nv nr | nv -> nr, nr -> nv where
    isCall   :: nv O O -> Bool
    isBranch :: nv O C -> Bool

    retargetBranch :: nv O C -> Label -> Label -> nv O C

    mkLabelOp :: Label -> nv C O
    mkJumpOp  :: Label -> nv O C

    getReferences :: nv e x -> [VarInfo]
    setRegisters  :: [(Int, PhysReg)] -> nv e x -> Env (nr e x)

    mkMoveOps    :: PhysReg -> PhysReg -> Env [nr O O]
    mkSwapOps    :: PhysReg -> PhysReg -> Env [nr O O]
    mkSaveOps    :: PhysReg -> Maybe VarId -> Env [nr O O]
    mkRestoreOps :: Maybe VarId -> PhysReg -> Env [nr O O]

    op1ToString  :: nv e x -> String

data NodeV n = NodeCO { getNodeCO :: n C O }
             | NodeOO { getNodeOO :: n O O }
             | NodeOC { getNodeOC :: n O C }

blockInfo :: (NodeAlloc nv nr, NonLocal nv, NonLocal nr)
          => (Label -> Env Int)
          -> BlockInfo Env (Block nv C C) (Block nr C C)
                           (NodeV nv) (NodeV nr)
blockInfo getBlockId = BlockInfo
    { blockId = getBlockId . entryLabel

    , blockSuccessors = Prelude.mapM getBlockId . successors

    , splitCriticalEdge = \(BlockCC b m e)
                           (BlockCC next _ _) -> do
        let lab = entryLabel next
        (next:supply, stack) <- getPast
        sendFuture (supply, stack)
        let lab' = unsafeCoerce next
        return (BlockCC b m (retargetBranch e lab lab'),
                BlockCC (mkLabelOp lab') BNil (mkJumpOp lab))

    , blockOps = \(BlockCC a b z) ->
        ([NodeCO a], Prelude.map NodeOO (blockToList b), [NodeOC z])

    , setBlockOps = \_ [a] b [z] ->
        BlockCC
            (getNodeCO a)
            (blockFromList (Prelude.map getNodeOO b))
            (getNodeOC z)
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

allocateHoopl :: (NodeAlloc nv nr, NonLocal nv, NonLocal nr)
              => Int             -- ^ Number of machine registers
              -> Int             -- ^ Offset of the spill stack
              -> Int             -- ^ Size of spilled register in bytes
              -> Label           -- ^ Label of graph entry block
              -> Graph nv C C -- ^ Program graph
              -> Either [String] (Graph nr C C)
allocateHoopl regs offset slotSize entry graph =
    newGraph <$> runIdentity (go (1 + length blocks))
  where
    newGraph xs = GMany NothingO (newBody xs) NothingO
      where
        newBody = Data.Foldable.foldl' (flip addBlock) emptyBody

    blocks = postorder_dfs_from body entry
      where
        GMany NothingO body NothingO = graph

    go n = evalTardisT alloc (mempty, ([n..], newSpillStack offset slotSize))
      where
        alloc = allocate regs (blockInfo getBlockId) opInfo blocks
          where
            getBlockId :: Hoopl.Label -> Env Int
            getBlockId = return . unsafeCoerce
