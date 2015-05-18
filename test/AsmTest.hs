{-# LANGUAGE GADTs #-}

module AsmTest where

import           Assembly
import           Compiler.Hoopl as Hoopl hiding ((<*>))
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.Trans.State (evalStateT, gets)
import           Data.Foldable
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           LinearScan
import           LinearScan.Hoopl
import           LinearScan.Hoopl.DSL
import           Normal ()
import           Test.Hspec

asmTestLiteral :: Int -> Program (Node IRVar) -> String -> Expectation
asmTestLiteral regs program expected = do
    let eres = runSimpleUniqueMonad $ do
            x <- compile "entry" program
            uncurry runTest x
    case eres of
        Left err -> error $ "Allocation failed: " ++ err
        Right blks -> do
            let graph' = newGraph $!! blks
            let g = showGraph show graph'
            catch (g `shouldBe` expected) $ \e -> do
                let input = runSimpleUniqueMonad $ compile "entry" program
                putStrLn $ "---- Parsed ----\n" ++ showGraph show (fst input)
                putStrLn $ "---- Expecting ----\n" ++ expected
                putStrLn $ "---- Compiled  ----\n" ++ g
                putStrLn "-------------------"
                throwIO (e :: SomeException)
  where
    runTest prog entry =
        go $ M.fromList $ zip (Prelude.map entryLabel blocks) [(0 :: Int)..]
      where
        GMany NothingO body NothingO = prog
        blocks = postorder_dfs_from body entry

        alloc = allocate regs (blockInfo getBlockId) opInfo $!! blocks
          where
            getBlockId :: Hoopl.Label -> Env Int
            getBlockId lbl = do
                bids <- gets envBlockIds
                return $ fromMaybe
                    (error $ "Unable to find block at label " ++ show lbl)
                    (M.lookup lbl bids)

        go blockIds = evalStateT alloc (newEnvState { envBlockIds = blockIds })

    newBody = Data.Foldable.foldl' (flip addBlock) emptyBody
    newGraph xs = GMany NothingO (newBody xs) NothingO

asmTest :: Int
        -> Program (Node IRVar)
        -> Program (Node (Assign VarId PhysReg))
        -> Expectation
asmTest regs program expected
    = asmTestLiteral regs program
    $ showGraph show
    $ fst
    $ runSimpleUniqueMonad
    $ compile "entry" expected
