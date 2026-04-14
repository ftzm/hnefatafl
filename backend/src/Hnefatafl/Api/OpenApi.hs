{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hnefatafl.Api.OpenApi (
  openApiSpec,
) where

import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.OpenApi (
  NamedSchema (..),
  OpenApiType (..),
  ToParamSchema (..),
  ToSchema (..),
  declareSchemaRef,
 )
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Declare (runDeclare)
import Hnefatafl.Api.Routes (DocumentedAPI, SearchTrustedInput)
import Hnefatafl.Api.Types.WS (AuthMessage, PendingActionPayload, WsError, WsErrorCode)
import Hnefatafl.Api.Types.WS.AI (AIClientMessage, AIServerMessage)
import Hnefatafl.Api.Types.WS.Online (OnlineClientMessage, OnlineServerMessage)
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

-- | Collect all referenced schemas for a given type
collectSchemas :: forall a. ToSchema a => Proxy a -> InsOrd.InsOrdHashMap Text OpenApi.Schema
collectSchemas proxy =
  let (defs, _) = runDeclare (declareSchemaRef proxy) mempty
   in InsOrd.mapKeys toText defs

openApiSpec :: OpenApi.OpenApi
openApiSpec =
  let baseSpec = toOpenApi (Proxy @DocumentedAPI)
      -- Collect schemas from all WS message types
      wsSchemas =
        collectSchemas (Proxy @AIServerMessage)
          <> collectSchemas (Proxy @AIClientMessage)
          <> collectSchemas (Proxy @OnlineServerMessage)
          <> collectSchemas (Proxy @OnlineClientMessage)
          <> collectSchemas (Proxy @WsError)
          <> collectSchemas (Proxy @WsErrorCode)
          <> collectSchemas (Proxy @AuthMessage)
          <> collectSchemas (Proxy @PendingActionPayload)
      -- Merge WS schemas into the existing component schemas
      components' = baseSpec._openApiComponents
      existingSchemas = components'._componentsSchemas
      mergedSchemas = existingSchemas <> wsSchemas
   in baseSpec
        { OpenApi._openApiComponents =
            components'
              { OpenApi._componentsSchemas = mergedSchemas
              }
        }
