{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Generated (generatedTests) where

import           Assembly
import           Compiler.Hoopl hiding ((<*>))
import           Control.Applicative
import           Control.Exception (evaluate)
import           Control.Monad
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import           Data.List (intercalate, isInfixOf)
import           LinearScan (UseVerifier(VerifyDisabled))
import           LinearScan.Hoopl
import           Test.FuzzCheck as F hiding (branch)
import           Test.Hspec
import           Test.QuickCheck hiding (label)
import           Unsafe.Coerce

instance Arbitrary IRVar where
    arbitrary = sized $ \n -> frequency
        [ (1, PhysicalIV <$> choose (0,31))
        , (50, VirtualIV <$> choose (0,n))
        ]

instance Arbitrary (Instruction IRVar) where
    arbitrary = frequency
        [ (3, Add <$> arbitrary <*> arbitrary <*> arbitrary)
        , (1, Offp <$> arbitrary <*> arbitrary <*> arbitrary)
        , (1, Offlpi <$> arbitrary)
        , (1, pure Nop)
        ]

instance Arbitrary (Node IRVar O C) where
    arbitrary = sized $ \n -> frequency
        [ (1, Jump <$> unsafeCoerce <$> (choose (0,n) :: Gen Int))
        , (1, Branch <$> pure Zero
                     <*> arbitrary
                     <*> (unsafeCoerce <$> (choose (0,n) :: Gen Int))
                     <*> (unsafeCoerce <$> (choose (0,n) :: Gen Int)))
        , (1, ReturnInstr [] <$> arbitrary)
        ]

instance Arbitrary (Node IRVar O O) where
    arbitrary = sized $ \n -> frequency
        [ (1, Alloc <$> arbitrary <*> arbitrary)
        -- , (1, Reclaim <$> arbitrary)
        , (40, Instr <$> arbitrary)
        , (20, Call InlineC <$> choose (0,n))
        , (30, LoadConst <$> arbitrary <*> arbitrary)
        , (50, Move <$> arbitrary <*> arbitrary)
        , (25, Copy <$> arbitrary <*> arbitrary)
        -- , (0, Save <$> arbitrary <*> arbitrary)
        -- , (0, Restore <$> arbitrary <*> arbitrary)
        -- , (0, pure $ Trace "tracer")
        ]

-- instance Arbitrary (Block (Node IRVar) C C) where
--     arbitrary = BlockCC (Label (unsafeCoerce (0 :: Int)))
--                     <$> arbitrary <*> arbitrary

-- instance Arbitrary (Block (Node IRVar) C O) where
--     arbitrary = BlockCO (Label (unsafeCoerce (0 :: Int))) <$> arbitrary

instance Arbitrary (Block (Node IRVar) O C) where
    arbitrary = BlockOC <$> arbitrary <*> arbitrary

instance Arbitrary (Block (Node IRVar) O O) where
    arbitrary = frequency
        [ (2, pure BNil)
        , (1, BMiddle <$> arbitrary)
        , (1, BCat <$> arbitrary <*> arbitrary)
        , (2, BSnoc <$> arbitrary <*> arbitrary)
        , (3, BCons <$> arbitrary <*> arbitrary)
        ]

instance Arbitrary (Graph' Block (Node IRVar) C C) where
    arbitrary = sized $ \n ->
        GMany NothingO
            <$> (unsafeCoerce
                 <$> IM.fromList
                 <$> (forM [1..n] $ \(i :: Int) ->
                       (,) <$> pure i
                           <*> (BlockCC (Label (unsafeCoerce i))
                                    <$> arbitrary <*> arbitrary
                                :: Gen (Block (Node IRVar) C C))))
            <*> pure NothingO

instance Show (Graph' Block (Node IRVar) C C) where
    show = showGraph show

generatedTests :: SpecWith ()
generatedTests =
  it "Handles generated tests" $
    (\a -> fuzzCheck' a 100 (return ())) $ do
      (graph :: Graph (Node IRVar) C C) <- "create graph" ?> pure <$> rand
      let GMany _ body _ = graph
      let entry = unsafeCoerce
                    (head (IS.elems (unsafeCoerce
                                     (externalEntryLabels body))))
      case allocateHoopl 32 0 8 VerifyDisabled entry graph of
          Left (dump, err)
              | any ("Cannot insert interval onto unhandled list"
                     `isInfixOf`) err ->
                  True `shouldBe` True
              | otherwise ->
                  error $ "Allocation failed: " ++ intercalate "\n" err ++ "\n"
                      ++ dump
          Right graph' -> do
              let g = showGraph show graph'
              _ <- evaluate (length g)
              return ()
              -- putStrLn $ "---- Compiled  ----\n" ++ g
