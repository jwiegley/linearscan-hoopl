{-# LANGUAGE GADTs #-}

module AsmTest where

import           Assembly
import           Compiler.Hoopl as Hoopl hiding ((<*>))
import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.Trans.State (evalStateT)
import           Data.Foldable
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           LinearScan
import           LinearScan.Hoopl
import           LinearScan.Hoopl.DSL
import           Normal ()
import           Test.Hspec

asmTest :: Int -> Program (Node IRVar) -> Program (Node PhysReg) -> Expectation
asmTest regs program expected = do
    let (eres, result) = runSimpleUniqueMonad $ do
            (prog, entry) <- compile "entry" program
            (res, _)      <- compile "entry" expected
            liftA2 (,) (runTest prog entry) (pure res)
    case eres of
        Left err -> error $ "Allocation failed: " ++ err
        Right blks -> do
            let graph' = newGraph $!! blks
            catch (showGraph show graph' `shouldBe`
                   showGraph show result) $ \e -> do
                putStrLn $ "---- Expecting ----\n" ++ showGraph show result
                putStrLn $ "---- Compiled  ----\n" ++ showGraph show graph'
                putStrLn "-------------------"
                throwIO (e :: SomeException)
  where
    runTest prog entry =
        go $ M.fromList
           $ zip (Prelude.map entryLabel blocks) [(0 :: Int)..]
      where
        GMany NothingO body NothingO = prog
        blocks = postorder_dfs_from body entry

        alloc blockIds = allocate regs (blockInfo getBlockId) opInfo $!! blocks
          where
            getBlockId :: Hoopl.Label -> Int
            getBlockId lbl =
                fromMaybe (error $ "Unable to find block at label " ++ show lbl)
                (M.lookup lbl blockIds)

        go blockIds =
            evalStateT (alloc blockIds) (mempty :: Labels, newSpillStack 0 8)

    newBody = Data.Foldable.foldl' (flip addBlock) emptyBody
    newGraph xs = GMany NothingO (newBody xs) NothingO
