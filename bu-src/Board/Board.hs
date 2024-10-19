{-# LANGUAGE DeriveAnyClass #-}
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
  Move (..),
  Moves (..),
  PieceType (..),
  Team (..),
  opp,
  testBoardBit,
  allVariations,
  serializeBoard,
) where

import Control.Applicative (liftA3)
import Data.Data (Data)
import Data.List.Split (chunksOf, splitOn)
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as M
import Data.Vector.Unboxed.Base qualified as U
import Data.WideWord.Word128 (Word128 (..))
import Foreign.Storable (Storable (..))
import Foreign.Storable.Record as Store
import GHC.Bits (Bits (..))
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (liftData)

import Board.Geometry (rotateIndex180, rotateIndex270, rotateIndex90)
import Data.Aeson
import Foreign.Marshal.Array
import Foreign.Storable
import GHC.Show (Show (..))

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
  deriving stock (Data, Generic, Eq)
  deriving anyclass (NFData)

instance Show Board where
  show = ('\n' :) . showBoard

instance ToJSON Board where
  toJSON = String . toText . ('\n' :) . showBoard

instance FromJSON Board where
  parseJSON = withText "Board" (pure . readBoard . toString)

-- instance Eq Board where
--   a == b = showBoard a == showBoard b

store :: Store.Dictionary Board
store =
  Store.run $
    liftA3
      Board
      (Store.element whitePawns)
      (Store.element king)
      (Store.element blackPawns)

instance Storable Board where
  sizeOf = Store.sizeOf store
  alignment = Store.alignment store
  peek = Store.peek store
  poke = Store.poke store

--------------------------------------------------------------------------------
-- Teams

data Team = White | Black
  deriving (Show, Generic, Eq)
  deriving anyclass (NFData)

opp :: Team -> Team
opp White = Black
opp Black = White

data PieceType = WhiteType | KingType | BlackType
  deriving (Show, Generic, Eq)
  deriving anyclass (NFData, ToJSON, FromJSON)

--------------------------------------------------------------------------------

testBoardBit :: Word128 -> Int8 -> Bool
testBoardBit w i = testBit w $ fromIntegral i

--------------------------------------------------------------------------------
-- Unbox instance for word128

instance U.IsoUnbox Word128 (Word64, Word64) where
  toURepr (Word128 x y) = (x, y)
  fromURepr (x, y) = Word128 x y
  {-# INLINE toURepr #-}
  {-# INLINE fromURepr #-}

newtype instance U.MVector s Word128 = MV_Word128 (U.MVector s (Word64, Word64))
newtype instance U.Vector Word128 = V_Word128 (U.Vector (Word64, Word64))
deriving via
  (Word128 `U.As` (Word64, Word64))
  instance
    M.MVector U.MVector Word128
deriving via
  (Word128 `U.As` (Word64, Word64))
  instance
    G.Vector U.Vector Word128
instance U.Unbox Word128

--------------------------------------------------------------------------------
-- Unbox instance for word128

instance U.IsoUnbox Board (Word128, Word128, Word128) where
  toURepr (Board x y z) = (x, y, z)
  fromURepr (x, y, z) = Board x y z
  {-# INLINE toURepr #-}
  {-# INLINE fromURepr #-}

newtype instance U.MVector s Board = MV_Board (U.MVector s (Word128, Word128, Word128))
newtype instance U.Vector Board = V_Board (U.Vector (Word128, Word128, Word128))
deriving via
  (Board `U.As` (Word128, Word128, Word128))
  instance
    M.MVector U.MVector Board
deriving via
  (Board `U.As` (Word128, Word128, Word128))
  instance
    G.Vector U.Vector Board
instance U.Unbox Board

--------------------------------------------------------------------------------
-- Read

readWord128ByChar :: Char -> String -> Word128
readWord128ByChar c =
  foldl' (\acc (i, x) -> if x == c then setBit acc i else acc) 0
    . zip [0 ..]
    . concat
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
    chunksOf 33 $
      concatMap (bool " . " " X " . testBit board) [0 .. 120]

showBoard :: Board -> [Char]
showBoard Board{..} =
  intercalate "\n" $
    chunksOf 33 $
      concatMap (\i -> ' ' : getChar i : [' ']) [0 .. 120]
 where
  getChar i =
    if
        | testBit whitePawns i -> 'O'
        | testBit king i -> '#'
        | testBit blackPawns i -> 'X'
        | otherwise -> '.'

--------------------------------------------------------------------------------
-- Serialize

serializeBoard :: Board -> [Char]
serializeBoard Board{..} =
  map getChar [0 .. 120]
 where
  getChar i =
    if
        | testBit whitePawns i -> 'O'
        | testBit king i -> '#'
        | testBit blackPawns i -> 'X'
        | otherwise -> '.'

--------------------------------------------------------------------------------
-- Move

data Move = Move {orig :: Int8, dest :: Int8}
  deriving (Show)

instance Storable Move where
  alignment _ = 1
  sizeOf _ = 2

  peek p = Move <$> (`peekByteOff` 0) p <*> (`peekByteOff` 1) p

  poke = error "undefined"

--------------------------------------------------------------------------------
-- FFI

newtype Moves = Moves {unMoves :: [Move]}
  deriving (Show)

instance Storable Moves where
  alignment _ = 8
  sizeOf _ = 16

  peek p = do
    len <- (`peekByteOff` 0) p
    msPtr <- (`peekByteOff` 8) p
    ms <- peekArray len msPtr
    pure $ Moves ms

  poke = error "undefined"

--------------------------------------------------------------------------------
-- Rotate board

rotateLayer :: (Int8 -> Int8) -> Word128 -> Word128
rotateLayer f input =
  foldl'
    ( \acc i ->
        if testBit input (fromIntegral i)
          then setBit acc (fromIntegral $ f i)
          else acc
    )
    0
    [0 .. 120]

rotateLayer90 :: Word128 -> Word128
rotateLayer90 = rotateLayer rotateIndex90

rotateLayer180 :: Word128 -> Word128
rotateLayer180 = rotateLayer rotateIndex180

rotateLayer270 :: Word128 -> Word128
rotateLayer270 = rotateLayer rotateIndex270

rotateBoard :: (Word128 -> Word128) -> Board -> Board
rotateBoard f board =
  Board
    { whitePawns = f board.whitePawns
    , king = f board.king
    , blackPawns = f board.blackPawns
    }

rotateBoard90 :: Board -> Board
rotateBoard90 = rotateBoard rotateLayer90

rotateBoard180 :: Board -> Board
rotateBoard180 = rotateBoard rotateLayer180

rotateBoard270 :: Board -> Board
rotateBoard270 = rotateBoard rotateLayer270

allVariations :: Board -> [Board]
allVariations board =
  [ board
  , rotateBoard90 board
  , rotateBoard180 board
  , rotateBoard270 board
  ]
