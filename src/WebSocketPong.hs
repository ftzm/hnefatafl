module WebSocketPong where

-- Liberal borrowing from https://github.com/channable/icepeak/pull/113/files#diff-4fd96219394d66c3ab58b71c194da62af625e990ae6709717b3a8a0e65f1657f

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Exception (AsyncException, handle, throwIO)
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.WebSockets qualified as WS
import System.Timeout (timeout)

pongHandler :: IORef NominalDiffTime -> IO ()
--pongHandler lastPongRef = getPOSIXTime >>= atomicWriteIORef lastPongRef
pongHandler lastPongRef = do
  putStrLn "got pong"
  currentTime <- getPOSIXTime
  atomicWriteIORef lastPongRef currentTime

{- |
An action passed to 'withInterruptiblePingThread' that is used together
with 'pongHandler' to terminate a WebSocket connection if the client stops
sending timely pongs. This returns @True@ if the connection has timed out and
should be terminated.
-}
pingHandler ::
  -- | last pong ref
  IORef NominalDiffTime ->
  -- | ping interval in seconds
  Integer ->
  -- | pong timeout in seconds
  Integer ->
  IO Bool
pingHandler lastPongRef pingIntervalSeconds pongTimeoutSeconds = do
  putStrLn "pingHandler"
  now <- getPOSIXTime
  let pingInterval = fromInteger pingIntervalSeconds
      pongTimeout = fromInteger pongTimeoutSeconds
      lastPongDeadline = now - pingInterval - pongTimeout

  lastPong <- readIORef lastPongRef
  return $! lastPong < lastPongDeadline

{- |
Similar to 'WS.withPingThread', except that it uses a combination of
'pingHandler' and the 'pongHandler' set in the websocket connection's pong
handler to detect that the thread client stopped sending pongs. If that
happens the @app@ action will be canceled immediately.

The @pingAction@ is exected on every ping, and it should return @True@ if the
client has timed out and the connection should be terminated.
-}
withInterruptiblePingThread :: WS.Connection -> Int -> IO Bool -> IO () -> IO ()
withInterruptiblePingThread conn pingInterval pingAction
  | pingInterval <= 0 = id
  | otherwise = race_ (interruptiblePingThread conn pingInterval pingAction)

{- |
This is based on 'WS.pingThread', with the following differences:
  * Instead of running an `IO ()` action after a ping is sent, this uses an
    `IO Bool` action. If that action returns true then the connection is to
    be considered timed out and this function will return.
  * The check for `pingInterval` being 0 or less has been moved to
    `withInterruptiblePingThread` to avoid spawning threads when unnecessary.
    Still calling this function with a zero or negative ping interval will
    not break anything, although it will cause it to spam pings.
-}
interruptiblePingThread :: WS.Connection -> Int -> IO Bool -> IO ()
interruptiblePingThread conn pingInterval pingAction = ignore `handle` go 1
 where
  pingIntervalUs :: Int
  pingIntervalUs = pingInterval * 1000 * 1000

  go :: Int -> IO ()
  go i = do
    threadDelay pingIntervalUs
    -- If the send buffer is full (e.g. because we pushed a lot of updates to
    -- a client that's timed out) then this send will block indefinitely.
    -- Adding the timeout here prevents this from happening, and it also
    -- interacts nicely with the @pingAction@.
    result <- timeout pingIntervalUs $ putStrLn "sending ping" >> WS.sendPing conn (T.pack $ show i)
    case result of
      Nothing -> putStrLn "ping timed out"
      _ -> putStrLn "ping sent successfully"
    -- The difference with the original 'pingThread' is that this action now
    -- returns a boolean, and we'll terminate this thread when that action
    -- returns true
    putStrLn "do ping action"
    hasTimedOut <- pingAction
    unless hasTimedOut $ go (i + 1)
    putStrLn "pong timed out"

  -- The rest of this function is exactly the same as the 'pingThread' in
  -- @websockets-0.12.7.3@
  ignore e = case fromException e of
    Just async -> throwIO (async :: AsyncException)
    Nothing -> print e >> throwIO e
