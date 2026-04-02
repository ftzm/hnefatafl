{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Hnefatafl.Util (uncurry3, pattern Snoc) where

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

pattern Snoc :: [a] -> a -> NonEmpty a
pattern Snoc xs x <- ((\ne -> (init ne, last ne)) -> (xs, x))
{-# COMPLETE Snoc #-}
