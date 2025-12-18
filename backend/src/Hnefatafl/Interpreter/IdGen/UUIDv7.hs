{-# LANGUAGE GADTs #-}

module Hnefatafl.Interpreter.IdGen.UUIDv7 (
  runIdGenUUIDv7,
) where

import Data.UUID.Types qualified as UUID
import Effectful
import Effectful.Dispatch.Dynamic
import Heptapod (generate)
import Hnefatafl.Effect.IdGen

-- | Run IdGen effect using UUIDv7 via heptapod
runIdGenUUIDv7 :: IOE :> es => Eff (IdGen : es) a -> Eff es a
runIdGenUUIDv7 = interpret $ \_ -> \case
  GenerateId -> coerce . UUID.toText <$> liftIO generate
