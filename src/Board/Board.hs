{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Board.Board (
  Board (..),
  readBoard,
  readWord128ByChar,
  showBoard,
  showWord128Board,
  b,
  bl,
) where

import Data.Data (Data)
import Data.List.Split (chunksOf, splitOn)
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as M
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Base qualified as U
import Data.WideWord.Word128 (Word128 (..))
import GHC.Bits (Bits (..))
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (liftData)

--------------------------------------------------------------------------------
-- Board

data Board = Board
  { whitePawns :: {-# UNPACK #-} !Word128
  , -- , revWhitePawns :: !Word128
    king :: {-# UNPACK #-} !Word128
  , -- , revKing :: !Word128
    blackPawns :: {-# UNPACK #-} !Word128
    -- , revBlackPawns :: !Word128
  }
  deriving (Data, Generic, NFData)

--------------------------------------------------------------------------------
-- Unbox instance for word128

instance U.IsoUnbox Word128 (Word64, Word64) where
  toURepr (Word128 x y) = (x, y)
  fromURepr (x, y) = Word128 x y
  {-# INLINE toURepr #-}
  {-# INLINE fromURepr #-}

newtype instance U.MVector s Word128 = MV_Word128 (U.MVector s (Word64, Word64))
newtype instance U.Vector Word128 = V_Word128 (U.Vector (Word64, Word64))
deriving via (Word128 `U.As` (Word64, Word64)) instance M.MVector U.MVector Word128
deriving via (Word128 `U.As` (Word64, Word64)) instance G.Vector  U.Vector  Word128
instance U.Unbox Word128

--------------------------------------------------------------------------------

instance U.IsoUnbox Board (Word128, Word128, Word128) where
 toURepr (Board x y z) = (x, y, z)
 fromURepr (x, y, z) = Board x y z
 {-# INLINE toURepr #-}
 {-# INLINE fromURepr #-}

newtype instance U.MVector s Board = MV_Board (U.MVector s (Word128, Word128, Word128))
newtype instance U.Vector Board = V_Board (U.Vector (Word128, Word128, Word128))
deriving via (Board `U.As` (Word128, Word128, Word128)) instance M.MVector U.MVector Board
deriving via (Board `U.As` (Word128, Word128, Word128)) instance G.Vector  U.Vector  Board
instance U.Unbox Board

--------------------------------------------------------------------------------
-- Read

readWord128ByChar :: Char -> String -> Word128
readWord128ByChar c =
  foldl' (\acc (i, x) -> if x == c then setBit acc i else acc) 0
    . zip [0 ..]
    . concat
    . reverse
    . filter (not . null)
    . splitOn "\n"
    . filter (' ' /=)

readBoard :: String -> Board
readBoard s =
  Board
    { whitePawns = readWord128ByChar 'O' s
    , -- , revWhitePawns = 0
      king = readWord128ByChar '#' s
    , -- , revKing = 0
      blackPawns = readWord128ByChar 'X' s
      -- , revBlackPawns = 0
    }

b :: QuasiQuoter
b =
  QuasiQuoter
    { quoteExp = liftData . readBoard
    , quotePat = error "undefined"
    , quoteType = error "undefined"
    , quoteDec = error "undefined"
    }

bl :: QuasiQuoter
bl =
  QuasiQuoter
    { quoteExp = liftData . readWord128ByChar 'X'
    , quotePat = error "undefined"
    , quoteType = error "undefined"
    , quoteDec = error "undefined"
    }

--------------------------------------------------------------------------------
-- Show

showWord128Board :: Word128 -> [Char]
showWord128Board board =
  intercalate "\n" $
    reverse $
      chunksOf 33 $
        concatMap (bool " . " " X " . testBit board) [0 .. 120]

showBoard :: Board -> [Char]
showBoard Board{..} =
  intercalate "\n" $
    reverse $
      chunksOf 33 $
        concatMap (\i -> ' ' : getChar i : [' ']) [0 .. 120]
 where
  getChar i =
    if
        | testBit whitePawns i -> 'O'
        | testBit king i -> '#'
        | testBit blackPawns i -> 'X'
        | otherwise -> '.'
