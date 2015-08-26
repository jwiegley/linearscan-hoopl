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
import           LinearScan
import           LinearScan.Hoopl
import           Test.Hspec
import           Test.QuickCheck hiding (label)
import           Test.QuickCheck.Test (isSuccess)
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
        [ (1, Jump <$> unsafeCoerce <$> (choose (1,n) :: Gen Int))
        , (1, Branch <$> pure Zero
                     <*> arbitrary
                     <*> (unsafeCoerce <$> (choose (1,n) :: Gen Int))
                     <*> (unsafeCoerce <$> (choose (1,n) :: Gen Int)))
        , (1, pure $ ReturnInstr [] Nop)
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

instance Show (Graph' Block (Node (Assign VarId PhysReg)) C C) where
    show = showGraph show

generatedTests :: SpecWith ()
generatedTests = it "Handles generated tests" $ do
  res <- quickCheckWithResult stdArgs { maxSuccess = 1000000000 } $
      forAll arbitrary testGraph
  isSuccess res `shouldBe` True

testGraph :: Graph (Node IRVar) C C -> Expectation
testGraph graph@(GMany _ body _) =
    case IS.elems (unsafeCoerce (externalEntryLabels body)) of
        [] -> True `shouldBe` True
        (entry:_) -> case allocateHoopl 32 0 8 VerifyEnabled
                                       (unsafeCoerce entry) graph of
            (dump, Left errs)
                | any ("Cannot insert onto unhandled:" `isInfixOf`) errs ->
                    True `shouldBe` True
                | otherwise ->
                    error $ "Allocation failed: "
                         ++ intercalate "\n" errs ++ "\n" ++ dump
            (dump, Right graph') -> do
                -- Make sure everything about the graph is computed
                _ <- evaluate $ length (show graph') + length dump
                return ()
