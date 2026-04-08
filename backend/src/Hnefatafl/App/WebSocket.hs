module Hnefatafl.App.WebSocket (
  safeSend,
  encodeAuthMsg,
  decodeAuthToken,
) where

import Data.Aeson (
  decode,
  encode,
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.Aeson.Types (Parser, parseMaybe)
import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.MVar qualified as MVar
import Effectful.Exception (catch)
import Hnefatafl.Effect.WebSocket (WebSocket, sendData)
import Network.WebSockets (Connection, ConnectionException)

-- | Send data on a connection held in an MVar. The MVar ensures only one
-- thread writes to the connection at a time. Silently catches connection
-- exceptions.
safeSend ::
  (Concurrent :> es, WebSocket :> es) =>
  MVar Connection -> LByteString -> Eff es ()
safeSend connVar msg =
  MVar.withMVar connVar $ \conn ->
    sendData conn msg
      `catch` \(_ :: ConnectionException) -> pure ()

encodeAuthMsg :: Text -> LByteString
encodeAuthMsg token = encode $ object ["type" .= ("auth" :: Text), "token" .= token]

decodeAuthToken :: LByteString -> Maybe Text
decodeAuthToken msg = do
  obj <- decode msg
  flip parseMaybe obj $ withObject "auth" $ \o -> do
    typ <- o .: "type" :: Parser Text
    guard (typ == "auth")
    o .: "token"
