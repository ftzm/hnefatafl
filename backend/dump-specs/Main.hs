module Main (main) where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as LBS
import Hnefatafl.Api.OpenApi (openApiSpec)

main :: IO ()
main = do
  LBS.writeFile "openapi.json" (encodePretty openApiSpec)
  putStrLn "Wrote openapi.json"
