module Main where

import SimpleTypes.Syntax
import SimpleTypes.Evaluator
import SimpleTypes.Parser.Parser

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO ()
process line = do
  let res = parseProgram line
  case res of
    Left err -> print err
    Right ex -> mapM_ print (run ex)

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop
