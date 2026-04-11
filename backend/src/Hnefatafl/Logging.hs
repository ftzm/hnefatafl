module Hnefatafl.Logging (
  -- * Allocation pairs for ResourceT 'allocate'
  mkJsonLogEnv,
  mkNoLogEnv,
  closeLogEnv,

  -- * CPS bracket variants for non-ResourceT callers
  withJsonLogEnv,
  withNoLogEnv,
) where

import Control.Exception (bracket)
import Katip (
  ColorStrategy (ColorIfTerminal),
  LogEnv,
  Namespace (..),
  Severity,
  Verbosity (V2),
  closeScribes,
  defaultScribeSettings,
  initLogEnv,
  jsonFormat,
  mkHandleScribeWithFormatter,
  permitItem,
  registerScribe,
 )

-- | Build a Katip 'LogEnv' that emits JSON log lines to stdout at the
-- given minimum severity. Must be paired with 'closeLogEnv' on exit.
mkJsonLogEnv :: Text -> Severity -> IO LogEnv
mkJsonLogEnv appName minSeverity = do
  scribe <-
    mkHandleScribeWithFormatter
      jsonFormat
      ColorIfTerminal
      stdout
      (permitItem minSeverity)
      V2
  env <- initLogEnv (Namespace [appName]) "production"
  registerScribe "stdout" scribe defaultScribeSettings env

-- | Build a 'LogEnv' with no scribes — all log messages are silently
-- dropped. Must be paired with 'closeLogEnv' on exit (no-op in practice,
-- but keeps the API uniform with 'mkJsonLogEnv').
mkNoLogEnv :: Text -> IO LogEnv
mkNoLogEnv appName = initLogEnv (Namespace [appName]) "production"

-- | Release a 'LogEnv', flushing and closing any registered scribes.
closeLogEnv :: LogEnv -> IO ()
closeLogEnv = void . closeScribes

withJsonLogEnv :: Text -> Severity -> (LogEnv -> IO a) -> IO a
withJsonLogEnv appName minSeverity =
  bracket (mkJsonLogEnv appName minSeverity) closeLogEnv

withNoLogEnv :: Text -> (LogEnv -> IO a) -> IO a
withNoLogEnv appName = bracket (mkNoLogEnv appName) closeLogEnv
