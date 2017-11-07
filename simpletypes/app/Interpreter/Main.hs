module Main where

import SimpleTypes.Syntax
import SimpleTypes.Evaluator
import SimpleTypes.Parser.Parser

import Text.Parsec
import System.Environment
import Control.Monad.IO.Class

main = do 
  args <- getArgs
  case args of
    [filename] -> do
      prg <- readFile filename 
      ast <- fmap (parseProgram)  (readFile filename)
      putStr "AST ==> \n" 
      putStrLn (show ast)
      case ast of
        Left err -> putStrLn $ show err
        Right exp -> do  
          putStrLn $ "\nEvaluation result  ==>" 
          putStrLn $ either show show (run exp)
    _ -> getProgName >>= \n ->  putStrLn $ "Usage: " ++ n  ++ "<sourceFile>"

