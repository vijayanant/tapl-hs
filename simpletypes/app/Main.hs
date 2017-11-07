module Main where

import SimpleTypes.Syntax
import SimpleTypes.Evaluator
import SimpleTypes.Parser

import Text.Parsec
import System.Environment
import Control.Monad.IO.Class

main = do 
  args <- getArgs
  case args of
    [filename] -> do
      prg <- readFile filename 
      ast <- fmap (parseProgram . unwords .  words ) (readFile filename)
      putStr "AST ==> \n" 
      putStrLn (show ast)
      case ast of
        ParseError err -> putStrLn $ show err
        exp -> do  
          putStrLn $ "\nEvaluation result  ==>" 
          putStrLn $ either show show (run exp)
    _ -> getProgName >>= \n ->  putStrLn $ "Usage: " ++ n  ++ "<sourceFile>"

