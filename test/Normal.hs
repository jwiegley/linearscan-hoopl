{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Normal where

import Assembly
import Compiler.Hoopl as Hoopl hiding ((<*>))
import Control.DeepSeq

instance (NFData a, NFData b) => NFData (Assign a b) where
    rnf (Assign a b) = a `deepseq` b `deepseq` ()

instance NFData IRVar where
    rnf (PhysicalIV r) = r `deepseq` ()
    rnf (VirtualIV r) = r `deepseq` ()

instance NFData v => NFData (Instruction v) where
    rnf (Add x y z)  = x `deepseq` y `deepseq` z `deepseq` ()
    rnf (Offp x y z) = x `deepseq` y `deepseq` z `deepseq` ()
    rnf (Offlpi x)   = x `deepseq` ()
    rnf Nop          = ()

instance NFData CConv where
    rnf InlineC = ()
    rnf (CConvC args results _) = args `deepseq` results `deepseq` ()

instance NFData Label where rnf _ = ()

instance NFData Test where rnf _ = ()

instance NFData v => NFData (Node v e x) where
    rnf (Alloc msrc dst)           = msrc `deepseq` dst `deepseq` ()
    rnf (Reclaim src)              = src `deepseq` ()
    rnf (Instr i)                  = i `deepseq` ()
    rnf (LoadConst c dst)          = c `deepseq` dst `deepseq` ()
    rnf (Move src dst)             = src `deepseq` dst `deepseq` ()
    rnf (Copy src dst)             = src `deepseq` dst `deepseq` ()
    rnf (Save src x)               = src `deepseq` x `deepseq` ()
    rnf (Restore x src)            = x `deepseq` src `deepseq` ()
    rnf (Trace str)                = str `deepseq` ()
    rnf (Branch x1 cond x2 x3)     = x1 `deepseq` cond `deepseq`
                                     x2 `deepseq` x3 `deepseq` ()
    rnf (Call cc i)                = cc `deepseq` i `deepseq` ()
    rnf (ReturnInstr liveInRegs i) = liveInRegs `deepseq` i `deepseq` ()
    rnf (Label x)                  = x `deepseq` ()
    rnf (Jump x)                   = x `deepseq` ()

instance NFData v => NFData (Block (Node v) e x) where
    rnf (BlockCO i b)   = i `deepseq` b `deepseq` ()
    rnf (BlockCC i b o) = i `deepseq` b `deepseq` o `deepseq` ()
    rnf (BlockOC b o)   = b `deepseq` o `deepseq` ()
    rnf BNil            = ()
    rnf (BMiddle n)     = n `deepseq` ()
    rnf (BCat n1 n2)    = n1 `deepseq` n2 `deepseq` ()
    rnf (BSnoc b n)     = b `deepseq` n `deepseq` ()
    rnf (BCons n b)     = n `deepseq` b `deepseq` ()
