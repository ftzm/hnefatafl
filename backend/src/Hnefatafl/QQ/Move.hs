{-# LANGUAGE TemplateHaskell #-}

module Hnefatafl.QQ.Move (
  -- moves,
  -- movesNE,
) where

-- import Language.Haskell.TH
-- import Language.Haskell.TH.Quote
-- import Hnefatafl.Serialization (parseMoveList)
-- import Hnefatafl.Core.Data (Move(..))

-- | Quasiquoter for move notation that produces a list of moves [Move]
--
-- Example usage:
--   [moves|k1j2 j3i4|]  -- produces [Move 0 12, Move 24 35]
--
-- The notation follows the existing format:
--   - Files are labeled 'k', 'j', 'i', ..., 'a' (from left to right)
--   - Ranks are labeled 1-11 (from bottom to top)
--   - Moves are written as originDestination (e.g., "k1j2")
--   - Multiple moves are separated by spaces

-- TODO: FFI and Template Haskell don't play nicely together
-- Commenting out for now to fix build issues

{-
moves :: QuasiQuoter
moves = QuasiQuoter
  { quoteExp = parseMovesToList
  , quotePat = error "moves quasiquoter cannot be used in patterns"
  , quoteType = error "moves quasiquoter cannot be used in types"
  , quoteDec = error "moves quasiquoter cannot be used in declarations"
  }

-- | Quasiquoter for move notation that produces a non-empty list of moves (NonEmpty Move)
--
-- Example usage:
--   [movesNE|k1j2 j3i4|]  -- produces NonEmpty (Move 0 12 :| [Move 24 35])
--
-- Same notation as 'moves' but requires at least one move and produces NonEmpty Move
movesNE :: QuasiQuoter
movesNE = QuasiQuoter
  { quoteExp = parseMovesToNonEmpty
  , quotePat = error "movesNE quasiquoter cannot be used in patterns"
  , quoteType = error "movesNE quasiquoter cannot be used in types"
  , quoteDec = error "movesNE quasiquoter cannot be used in declarations"
  }

parseMovesToList :: String -> ExpQ
parseMovesToList input = case parseMoveList (toText input) of
  Left err ->
    fail $ "Invalid move notation: " <> toString err <> " in: " <> input
  Right nonEmptyMoves ->
    listE $ map moveToExpQ (toList nonEmptyMoves)

parseMovesToNonEmpty :: String -> ExpQ
parseMovesToNonEmpty input = case parseMoveList (toText input) of
  Left err ->
    fail $ "Invalid move notation: " <> toString err <> " in: " <> input
  Right nonEmptyMoves ->
    let moveExps = map moveToExpQ (toList nonEmptyMoves)
    in case moveExps of
      [] -> fail "movesNE quasiquoter requires at least one move"
      (x:xs) -> [| $(x) :| $(listE xs) |]

-- | Convert a Move to a Template Haskell expression in the Q monad
moveToExpQ :: Move -> ExpQ
moveToExpQ (Move orig dest) =
  [| Move $(litE (integerL (fromIntegral orig))) $(litE (integerL (fromIntegral dest))) |]
-}