module Hnefatafl.Api.SpecTest where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Hnefatafl.Api.AsyncApi (asyncApiSpec)
import Hnefatafl.Api.OpenApi (openApiSpec)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

test_specGeneration :: TestTree
test_specGeneration =
  testGroup
    "API spec generation"
    [ testCase "OpenAPI spec generates without error" $
        assertBool "spec should be non-empty" $ LBS.length (encode openApiSpec) > 0
    , testCase "AsyncAPI spec generates without error" $
        assertBool "spec should be non-empty" $ LBS.length (encode asyncApiSpec) > 0
    ]
