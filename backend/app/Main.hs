module Main (main) where

import Ffi

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print =<< startBoard
