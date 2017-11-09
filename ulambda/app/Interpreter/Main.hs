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
      case prg of
        "" ->  putStrLn "Empty file..."
        str -> do
          ast <- fmap (parseProgram . unwords .  words ) (readFile filename)
          case ast of
            ParseError  err -> putStrLn $ show err
            exp -> putStrLn $ (show . run) exp
    _ -> getProgName >>= \n ->  putStrLn $ "Usage: " ++ n ++ " <sourceFile>"

