module Main where

import ULambda.Types
import ULambda.Evaluator
import ULambda.Parser

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO ()
process line = do
  let res = parseProgram line
  case res of
    ParseError err -> print err
    ex -> print (run ex)

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just "" -> loop
      Just input -> (liftIO $ process input) >> loop
