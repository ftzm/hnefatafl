{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hnefatafl.Api.OpenApi (
  openApiSpec,
) where

import Data.OpenApi (
  NamedSchema (..),
  OpenApiType (..),
  ToParamSchema (..),
  ToSchema (..),
 )
import Data.OpenApi qualified as OpenApi
import Hnefatafl.Api.Routes (DocumentedAPI, SearchTrustedInput)
import Hnefatafl.Bindings (EngineGameStatus, SearchTrustedResult)
import Hnefatafl.Core.Data (ExternBoard, Layer)
import Hnefatafl.Core.Data qualified as Core
import Hnefatafl.Search (SearchTimeout)
import Servant.OpenApi (toOpenApi)

-- Layer has custom ToJSON (space-separated Word64 string) — cannot use Generic
instance ToSchema Layer where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "Layer") $
        mempty
          { OpenApi._schemaType = Just OpenApiString
          , OpenApi._schemaDescription = Just "Two space-separated Word64 values (lower upper)"
          }

-- GameId is used in Capture parameters, needs ToParamSchema
instance ToParamSchema Core.GameId where
  toParamSchema _ = mempty{OpenApi._schemaType = Just OpenApiString}

-- FFI types that can't be modified at the definition site
deriving anyclass instance ToSchema ExternBoard
deriving anyclass instance ToSchema Core.Move
deriving anyclass instance ToSchema EngineGameStatus
deriving anyclass instance ToSchema SearchTrustedResult
deriving anyclass instance ToSchema SearchTimeout
deriving anyclass instance ToSchema SearchTrustedInput

openApiSpec :: OpenApi.OpenApi
openApiSpec = toOpenApi (Proxy @DocumentedAPI)
