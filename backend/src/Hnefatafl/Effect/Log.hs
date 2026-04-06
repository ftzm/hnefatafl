{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Hnefatafl.Effect.Log (
  Log,
  runLog,
) where

import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (
  SideEffects (..),
  StaticRep,
  evalStaticRep,
  getStaticRep,
  localStaticRep,
 )
import Katip (
  Katip (..),
  KatipContext (..),
  LogContexts,
  LogEnv,
  Namespace,
 )

data Log :: Effect

type instance DispatchOf Log = 'Static 'WithSideEffects

data instance StaticRep Log = LogRep
  { logEnv :: LogEnv
  , logCtx :: LogContexts
  , logNs :: Namespace
  }

instance (IOE :> es, Log :> es) => Katip (Eff es) where
  getLogEnv = (.logEnv) <$> getStaticRep @Log
  localLogEnv f m = localStaticRep @Log (\r -> r{logEnv = f r.logEnv}) m

instance (IOE :> es, Log :> es) => KatipContext (Eff es) where
  getKatipContext = (.logCtx) <$> getStaticRep @Log
  localKatipContext f m = localStaticRep @Log (\r -> r{logCtx = f r.logCtx}) m
  getKatipNamespace = (.logNs) <$> getStaticRep @Log
  localKatipNamespace f m = localStaticRep @Log (\r -> r{logNs = f r.logNs}) m

runLog :: (IOE :> es) => LogEnv -> Eff (Log : es) a -> Eff es a
runLog env = evalStaticRep (LogRep env mempty mempty)
