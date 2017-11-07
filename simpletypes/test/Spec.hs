module Main where

import Test.Tasty (defaultMain, testGroup)

import SimpleTypes.Tests 

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ SimpleTypes.Tests.tests
    ]
