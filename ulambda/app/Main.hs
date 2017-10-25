module Main where

import ULambda.Types
import ULambda.Evaluator
import ULambda.Parser

import Text.Parsec
import System.Environment
import Control.Monad.IO.Class


main = do 
  args <- getArgs
  case args of
    [filename] -> do
      prg <- readFile filename 
      {-putStrLn $ "File Content \n" ++ (show $ ( unwords . words ) prg) ++ "\n"-}
      ast <- fmap (parseProgram . unwords .  words ) (readFile filename)
      putStr "AST ==> \n" 
      putStrLn (show ast)
      case ast of
        ParseError  err -> putStrLn $ show err
        exp -> putStrLn $ "\nEvaluation result  ==> \n" ++ (show . run) exp
    _ -> getProgName >>= \n ->  putStrLn $ "Usage: " ++ n  ++ "<sourceFile>"

