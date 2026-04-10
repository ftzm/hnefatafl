module Hnefatafl.Api.AsyncApi (
  asyncApiSpec,
) where

import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.OpenApi (
  Referenced,
  Schema,
  ToSchema,
  declareSchemaRef,
 )
import Data.OpenApi.Declare (runDeclare)
import Hnefatafl.Api.OpenApi ()
-- orphan ToSchema instances
import Hnefatafl.Api.Types.WS (AuthMessage, WsError)
import Hnefatafl.Api.Types.WS.AI (AIClientMessage, AIServerMessage)
import Hnefatafl.Api.Types.WS.Online (OnlineClientMessage, OnlineServerMessage)

-- | Extract the schema ref and all referenced definitions for a type.
schemaWithDefs ::
  forall a. ToSchema a => Proxy a -> (Referenced Schema, [(Text, Schema)])
schemaWithDefs proxy =
  let (defs, ref) = runDeclare (declareSchemaRef proxy) mempty
   in (ref, map (first toText) $ InsOrd.toList defs)

-- | Convert OpenAPI discriminator format to AsyncAPI format.
-- OpenAPI: {"discriminator": {"propertyName": "type", "mapping": {}}}
-- AsyncAPI: {"discriminator": "type"}
convertDiscriminator :: Value -> Value
convertDiscriminator (Object obj) =
  let obj' = KM.map convertDiscriminator obj
   in case KM.lookup "discriminator" obj' of
        Just (Object disc) -> case KM.lookup "propertyName" disc of
          Just (String propName) -> Object $ KM.insert "discriminator" (String propName) obj'
          _ -> Object $ KM.delete "discriminator" obj'
        _ -> Object obj'
convertDiscriminator (Array arr) = Array $ fmap convertDiscriminator arr
convertDiscriminator v = v

-- | Convert a schema to AsyncAPI-compatible JSON
schemaToAsyncApi :: Schema -> Value
schemaToAsyncApi = convertDiscriminator . toJSON

-- | The AsyncAPI 3.0 specification for WebSocket message channels.
asyncApiSpec :: Value
asyncApiSpec =
  object
    [ "asyncapi" .= ("3.0.0" :: Text)
    , "info"
        .= object
          [ "title" .= ("Hnefatafl WebSocket API" :: Text)
          , "version" .= ("0.0.16" :: Text)
          , "description"
              .= ("WebSocket message schemas for AI and Online game modes" :: Text)
          ]
    , "channels"
        .= object
          [ "ai-ws"
              .= object
                [ "address" .= ("/ai/ws" :: Text)
                , "description" .= ("AI game WebSocket channel" :: Text)
                , "messages"
                    .= object
                      [ "serverMessage" .= ref "#/components/messages/AIServerMessage"
                      , "errorMessage" .= ref "#/components/messages/WsError"
                      , "clientMessage" .= ref "#/components/messages/AIClientMessage"
                      , "authMessage" .= ref "#/components/messages/AuthMessage"
                      ]
                ]
          , "online-ws"
              .= object
                [ "address" .= ("/online/ws" :: Text)
                , "description" .= ("Online game WebSocket channel" :: Text)
                , "messages"
                    .= object
                      [ "serverMessage" .= ref "#/components/messages/OnlineServerMessage"
                      , "errorMessage" .= ref "#/components/messages/WsError"
                      , "clientMessage" .= ref "#/components/messages/OnlineClientMessage"
                      , "authMessage" .= ref "#/components/messages/AuthMessage"
                      ]
                ]
          ]
    , "operations"
        .= object
          [ "sendAiClient"
              .= object
                [ "action" .= ("send" :: Text)
                , "channel" .= ref "#/channels/ai-ws"
                , "messages"
                    .= [ ref "#/channels/ai-ws/messages/clientMessage"
                       , ref "#/channels/ai-ws/messages/authMessage"
                       ]
                ]
          , "receiveAiServer"
              .= object
                [ "action" .= ("receive" :: Text)
                , "channel" .= ref "#/channels/ai-ws"
                , "messages"
                    .= [ ref "#/channels/ai-ws/messages/serverMessage"
                       , ref "#/channels/ai-ws/messages/errorMessage"
                       ]
                ]
          , "sendOnlineClient"
              .= object
                [ "action" .= ("send" :: Text)
                , "channel" .= ref "#/channels/online-ws"
                , "messages"
                    .= [ ref "#/channels/online-ws/messages/clientMessage"
                       , ref "#/channels/online-ws/messages/authMessage"
                       ]
                ]
          , "receiveOnlineServer"
              .= object
                [ "action" .= ("receive" :: Text)
                , "channel" .= ref "#/channels/online-ws"
                , "messages"
                    .= [ ref "#/channels/online-ws/messages/serverMessage"
                       , ref "#/channels/online-ws/messages/errorMessage"
                       ]
                ]
          ]
    , "components"
        .= object
          [ "messages"
              .= object
                [ "AIServerMessage" .= messageWith (Proxy @AIServerMessage)
                , "AIClientMessage" .= messageWith (Proxy @AIClientMessage)
                , "OnlineServerMessage" .= messageWith (Proxy @OnlineServerMessage)
                , "OnlineClientMessage" .= messageWith (Proxy @OnlineClientMessage)
                , "WsError" .= messageWith (Proxy @WsError)
                , "AuthMessage" .= messageWith (Proxy @AuthMessage)
                ]
          , "schemas" .= allSchemas
          ]
    ]
 where
  ref :: Text -> Value
  ref path = object ["$ref" .= path]

  messageWith :: forall a. ToSchema a => Proxy a -> Value
  messageWith proxy =
    let (schemaRef, _) = schemaWithDefs proxy
     in object ["payload" .= convertDiscriminator (toJSON schemaRef)]

  -- Collect all referenced schemas from all message types
  allSchemas :: Value
  allSchemas =
    let collect :: forall a. ToSchema a => Proxy a -> [(Text, Schema)]
        collect = snd . schemaWithDefs
        all' =
          collect (Proxy @AIServerMessage)
            <> collect (Proxy @AIClientMessage)
            <> collect (Proxy @OnlineServerMessage)
            <> collect (Proxy @OnlineClientMessage)
            <> collect (Proxy @WsError)
            <> collect (Proxy @AuthMessage)
        schemasMap = fromList @(Map Text Value) $ map (second schemaToAsyncApi) all'
     in toJSON schemasMap
