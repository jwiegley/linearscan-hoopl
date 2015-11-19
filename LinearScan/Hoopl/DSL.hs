{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module LinearScan.Hoopl.DSL
    ( -- * Compiling Assembly programs
      compile
      -- * Programs
    , ProgramF(..)
    , Program
      -- * Assembly nodes
    , Asm
    , Nodes
    , nodesToList
    , BodyNode
    , bodyNode
    , EndNode
    , endNode
      -- * Labels
    , Labels
    , getLabel
      -- * Spill stack
    , SpillStack(..)
    , newSpillStack
    , getStackSlot
    , Env
      -- * operations
    , LinearScan.Hoopl.DSL.label
    , jump
    ) where

import           Compiler.Hoopl as Hoopl hiding ((<*>))
import           Control.Applicative
import           Control.Arrow (first)
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Free
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Free as TF
import           Control.Monad.Trans.Free hiding (FreeF(..), Free)
import           Control.Monad.Trans.State
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           LinearScan
import           Unsafe.Coerce
import           Test.QuickCheck

data SpillStack = SpillStack
    { stackPtr      :: Int
      -- ^ Offset to the beginning of the spill stack. This can have whatever
      -- meaning the user of this library desires; it is not used directly by
      -- the allocation code.

    , stackSlotSize :: Int
      -- ^ The size of a stack slot in bytes. This should be the same or
      -- larger than the size of a register.

    , stackSlots    :: M.Map (Maybe VarId) Int
      -- ^ A mapping of variables to their stack slot offsets. The special
      -- variable 'Nothing' is used for temporary storage, for example when
      -- swapping registers through the stack.
    }
    deriving (Eq, Show)

type Env = State ([Int], SpillStack)

-- | Create a new 'SpillStack', given an offset and slot size.
newSpillStack :: Int -> Int -> SpillStack
newSpillStack offset slotSize = SpillStack
    { stackPtr      = offset
    , stackSlotSize = slotSize
    , stackSlots    = mempty
    }

-- | Given a variable identifier, determine its spill stack offset. The value
-- 'Nothing' refers to the temporary stack slot.
getStackSlot :: Maybe VarId -> Env Int
getStackSlot vid = do
    (supply, stack) <- get
    case M.lookup vid (stackSlots stack) of
        Just off -> return off
        Nothing -> do
            let off = stackPtr stack
            put (supply, stack
                { stackPtr   = off + stackSlotSize stack
                , stackSlots = M.insert vid off (stackSlots stack)
                })
            return off

-- | Labels is the type of a mapping from label names to Hoopl labels.
type Labels = M.Map String Label

-- | The 'Asm' static monad allows for the creation labels by name, and
-- referencing them later.
type Asm = StateT Labels SimpleUniqueMonad

getLabel :: String -> Asm Label
getLabel str = do
    l <- gets (M.lookup str)
    case l of
        Just lbl -> return lbl
        Nothing -> do
            lbl <- lift freshLabel
            modify (M.insert str lbl)
            return lbl

-- | A series of 'Nodes' is a set of assembly instructions that ends with some
-- kind of closing operation, such as a jump, branch or return. The Free monad
-- is used as a convenient way to describe a list that must result in a
-- closing operation at the end.
type Nodes n a = Free ((,) (n O O)) a

-- | 'nodesToList' renders a set of nodes as a list of operations followed by
-- a final value 'a'.
nodesToList :: Nodes n a -> (a, [n O O])
nodesToList (Pure a) = (a, [])
nodesToList (Free (n, xs)) = (n :) <$> nodesToList xs

-- | A 'BodyNode' represents an instruction within a program.
type BodyNode n = Nodes n ()

-- | Construct a 'BodyNode' from a Hoopl graph node.
bodyNode :: n O O -> BodyNode n
bodyNode n = Free (n, Pure ())

-- | An 'EndNode' represents a program with a final instruction. This
-- instruction is generated from an 'Asm' environment, so that it may refer to
-- and create labels for other blocks.
type EndNode n = Nodes n (Asm (n O C))

-- | Construct an 'EndNode' from a Hoopl final node generated from an 'Asm'
-- environment.
endNode :: Asm (n O C) -> EndNode n
endNode = return

-- | A 'ProgramF' abstracts a generic basic block: a series of 'Nodes',
-- associated with a label, that ends in a final node.
data ProgramF n = FreeBlock
    { labelEntry :: Label
    , labelBody  :: EndNode n
    }

-- | A 'Program' abstracts a sequence of basic blocks generated from an 'Asm'
-- environment.
type Program n = FreeT ((,) (ProgramF n)) Asm ()

-- | Create and associate a label with an series of instructions, creating a
-- 'Program' for that block.
label :: String -> EndNode n -> Program n
label str body = do
    lbl <- lift $ getLabel str
    liftF (FreeBlock lbl body, ())

-- | Create a final jump instruction to the given label.
jump :: HooplNode n => String -> EndNode n
jump dest = endNode $ mkBranchNode <$> getLabel dest

-- | When a program is compiled, the result is a closed Hoopl Graph, and the
-- label corresponding to the requested entry label name. This is done within
-- a 'SimpleUniqueMonad' so that unique labels may be created.
compile :: (NonLocal n, HooplNode n)
        => String    -- ^ Entry label name
        -> Program n -- ^ The assembly language program
        -> SimpleUniqueMonad (Graph n C C, Label)
                    -- ^ Returns the Hoopl 'Graph' and its entry 'Label'
compile name prog
    = flip evalStateT (mempty :: Labels)
    $ do body  <- go prog
         entry <- gets (M.lookup name)
         case entry of
             Nothing  -> error $ "Missing label: " ++ name
             Just lbl -> return (bodyGraph body, lbl)
  where
    go m = do
        p <- runFreeT m
        case p of
            TF.Pure () -> return emptyBody
            TF.Free (blk, xs) -> addBlock <$> comp blk <*> go xs

    comp (FreeBlock lbl body) = do
        let (close, blocks) = nodesToList body
        BlockCC (mkLabelNode lbl) (blockFromList blocks) <$> close
