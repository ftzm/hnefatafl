module Hnefatafl.Interpreter.Log.JSON (
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

withJsonLogEnv :: Text -> Severity -> (LogEnv -> IO a) -> IO a
withJsonLogEnv appName minSeverity action = do
  scribe <-
    mkHandleScribeWithFormatter
      jsonFormat
      ColorIfTerminal
      stdout
      (permitItem minSeverity)
      V2
  let mkEnv =
        registerScribe "stdout" scribe defaultScribeSettings
          =<< initLogEnv (Namespace [appName]) "production"
  bracket mkEnv closeScribes action

-- | A LogEnv with no scribes — all log messages are silently dropped.
withNoLogEnv :: Text -> (LogEnv -> IO a) -> IO a
withNoLogEnv appName action = do
  env <- initLogEnv (Namespace [appName]) "production"
  bracket (pure env) closeScribes action
