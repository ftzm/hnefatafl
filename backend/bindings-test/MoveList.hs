module MoveList where

import Hnefatafl.Bindings
import Test.Tasty
import Test.Tasty.HUnit

-- HUnit test case
unit_listCompare :: IO ()
unit_listCompare = [1 :: Int, 2, 3] `compare` [1, 2] @?= GT

-- HUnit test case with additional info
unit_listInfo :: IO String
unit_listInfo = return "This test provides info"

unit_basicMoveListString :: IO ()
unit_basicMoveListString = do
  let expected = ""
  actual <-
    moveListToBase64
      [ Move 1 2
      , Move 5 77
      , Move 5 87
      , Move 110 109
      , Move 6 77
      , Move 8 87
      , Move 81 109
      , Move 81 109
      ]
  assertEqual "" expected actual
