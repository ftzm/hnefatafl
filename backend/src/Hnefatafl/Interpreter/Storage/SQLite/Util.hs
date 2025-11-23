module Hnefatafl.Interpreter.Storage.SQLite.Util (
  execute',
  query',
  selectSingle,
  selectMaybe,
  (<<<$>>>),
) where

import Database.SQLite.Simple

-- | Utility functions for database operations
execute' :: ToRow p => Query -> p -> Connection -> IO ()
execute' q params conn = execute conn q params

query' :: (ToRow p, FromRow r) => Query -> p -> Connection -> IO [r]
query' q p c = query c q p

selectSingle :: (ToRow p, FromRow r) => Query -> p -> Connection -> IO r
selectSingle q p conn = limit =<< query' q p conn
 where
  limit :: [r] -> IO r
  limit [] = fail "1 element expected; found 0"
  limit [x] = pure x
  limit (_ : _) = fail "1 element expected; found >1"

selectMaybe :: (ToRow p, FromRow r) => Query -> p -> Connection -> IO (Maybe r)
selectMaybe q p conn = limit =<< query' q p conn
 where
  limit :: [r] -> IO (Maybe r)
  limit [] = pure Nothing
  limit [x] = pure $ Just x
  limit (_ : _) = fail "at most 1 element expected; found >1"

-- | Triple-nested applicative syntax for convenience
infixl 4 <<<$>>>

(<<<$>>>) ::
  (Functor f1, Functor f2, Functor f3) =>
  (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
(<<<$>>>) = fmap . fmap . fmap