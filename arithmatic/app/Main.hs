module Main where

import Arith.Evaluator
import Arith.Parser
import Text.Parsec
import System.Environment

main = do 
  args <- getArgs
  case args of
    [filename] -> do
      parseTree <- fmap (parse parseTerm "Arith") (readFile filename)
      putStr "Parse tree ==> \n" 
      putStrLn (show parseTree)
      case parseTree of
        Right exp -> putStrLn $ "\nEvaluation result  ==> \n" ++ (show.eval) exp
        Left  err -> putStrLn $ show err
    _ -> getProgName >>= \n ->  putStrLn $ "Usage: " ++ n  ++ "<sourceFile>"

