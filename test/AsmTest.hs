{-# LANGUAGE GADTs #-}

module AsmTest where

import Assembly
import Compiler.Hoopl as Hoopl hiding ((<*>))
import Control.Exception
import Data.List (intercalate)
import LinearScan
import LinearScan.Hoopl
import LinearScan.Hoopl.DSL
import Normal ()
import Test.Hspec

asmTestLiteral :: UseVerifier -> Int -> Program (Node IRVar) -> Maybe String
               -> Expectation
asmTestLiteral verif regs program mexpected = do
    let (graph, entry) = runSimpleUniqueMonad $ compile "entry" program
    case allocateHoopl regs 0 8 verif entry graph of
        Left (dump, err) ->
            error $ "Allocation failed: " ++ intercalate "\n" err ++ "\n"
                ++ dump
        Right graph' -> case mexpected of
            Nothing -> return ()
            Just expected ->
                let g = showGraph show graph' in
                catch (g `shouldBe` expected) $ \e -> do
                    putStrLn $ "---- Parsed ----\n" ++ showGraph show graph
                    putStrLn $ "---- Expecting ----\n" ++ expected
                    putStrLn $ "---- Compiled  ----\n" ++ g
                    putStrLn "-------------------"
                    throwIO (e :: SomeException)

asmTest :: Int
        -> Program (Node IRVar)
        -> Program (Node (Assign VarId PhysReg))
        -> Expectation
asmTest regs program
    = asmTestLiteral VerifyEnabledStrict regs program
    . Just
    . showGraph show
    . fst
    . runSimpleUniqueMonad
    . compile "entry"

asmTest_ :: Int -> Program (Node IRVar) -> Expectation
asmTest_ regs program = asmTestLiteral VerifyEnabledStrict regs program Nothing
