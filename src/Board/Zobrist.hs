{-# LANGUAGE MultiWayIf #-}

module Board.Zobrist (
  MultiZobrist (..),
  boardToMultiZobrist,
  updateMultiZobrist,
  selectZobrist,
) where

import Board.Geometry (
  flipIndexHorizontal,
  flipIndexSouthEast,
  flipIndexSouthWest,
  flipIndexVertical,
  rotateIndex180,
  rotateIndex270,
  rotateIndex90,
 )
import Data.Bits (Bits (..))
import Data.Vector.Unboxed qualified as V
import System.Random (Random (random), mkStdGen)

import Board.Board (Board (..), PieceType (..))
import Data.Foldable (maximum)

--------------------------------------------------------------------------------

data MultiZobrist = MultiZobrist
  { rotated0 :: Word64
  , rotated90 :: Word64
  , rotated180 :: Word64
  , rotated270 :: Word64
  , flippedHorizontal :: Word64
  , flippedVertical :: Word64
  , flippedSouthEast :: Word64
  , flippedSouthWest :: Word64
  }

--------------------------------------------------------------------------------

transformMasks :: V.Vector Word64 -> (Int -> Int) -> V.Vector Word64
transformMasks ms f = V.map ((ms V.!) . f) $ V.fromList [0 .. 120]

--------------------------------------------------------------------------------

whitePawnMasks :: V.Vector Word64
whitePawnMasks = V.unfoldrN 121 (Just . random) $ mkStdGen 125

whitePawnMasks90 :: V.Vector Word64
whitePawnMasks90 = transformMasks whitePawnMasks rotateIndex90

whitePawnMasks180 :: V.Vector Word64
whitePawnMasks180 = transformMasks whitePawnMasks rotateIndex180

whitePawnMasks270 :: V.Vector Word64
whitePawnMasks270 = transformMasks whitePawnMasks rotateIndex270

whitePawnMasksFlippedHorizontal :: V.Vector Word64
whitePawnMasksFlippedHorizontal = transformMasks whitePawnMasks flipIndexHorizontal

whitePawnMasksFlippedVertical :: V.Vector Word64
whitePawnMasksFlippedVertical = transformMasks whitePawnMasks flipIndexVertical

whitePawnMasksFlippedSouthEast :: V.Vector Word64
whitePawnMasksFlippedSouthEast = transformMasks whitePawnMasks flipIndexSouthEast

whitePawnMasksFlippedSouthWest :: V.Vector Word64
whitePawnMasksFlippedSouthWest = transformMasks whitePawnMasks flipIndexSouthWest

kingMasks :: V.Vector Word64
kingMasks = V.unfoldrN 121 (Just . random) $ mkStdGen 7234

kingMasks90 :: V.Vector Word64
kingMasks90 = transformMasks kingMasks rotateIndex90

kingMasks180 :: V.Vector Word64
kingMasks180 = transformMasks kingMasks rotateIndex180

kingMasks270 :: V.Vector Word64
kingMasks270 = transformMasks kingMasks rotateIndex270

kingMasksFlippedHorizontal :: V.Vector Word64
kingMasksFlippedHorizontal = transformMasks kingMasks flipIndexHorizontal

kingMasksFlippedVertical :: V.Vector Word64
kingMasksFlippedVertical = transformMasks kingMasks flipIndexVertical

kingMasksFlippedSouthEast :: V.Vector Word64
kingMasksFlippedSouthEast = transformMasks kingMasks flipIndexSouthEast

kingMasksFlippedSouthWest :: V.Vector Word64
kingMasksFlippedSouthWest = transformMasks kingMasks flipIndexSouthWest

blackPawnMasks :: V.Vector Word64
blackPawnMasks = V.unfoldrN 121 (Just . random) $ mkStdGen 938457

blackPawnMasks90 :: V.Vector Word64
blackPawnMasks90 = transformMasks blackPawnMasks rotateIndex90

blackPawnMasks180 :: V.Vector Word64
blackPawnMasks180 = transformMasks blackPawnMasks rotateIndex180

blackPawnMasks270 :: V.Vector Word64
blackPawnMasks270 = transformMasks blackPawnMasks rotateIndex270

blackPawnMasksFlippedHorizontal :: V.Vector Word64
blackPawnMasksFlippedHorizontal = transformMasks blackPawnMasks flipIndexHorizontal

blackPawnMasksFlippedVertical :: V.Vector Word64
blackPawnMasksFlippedVertical = transformMasks blackPawnMasks flipIndexVertical

blackPawnMasksFlippedSouthEast :: V.Vector Word64
blackPawnMasksFlippedSouthEast = transformMasks blackPawnMasks flipIndexSouthEast

blackPawnMasksFlippedSouthWest :: V.Vector Word64
blackPawnMasksFlippedSouthWest = transformMasks blackPawnMasks flipIndexSouthWest

blackTurnMask :: Word64
blackTurnMask = fst $ random $ mkStdGen 374

--------------------------------------------------------------------------------

-- generate initial zobrist hash for board
boardToZobrist ::
  -- | white pawn masks
  V.Vector Word64 ->
  -- | king masks
  V.Vector Word64 ->
  -- | black pawn masks
  V.Vector Word64 ->
  Board ->
  Bool ->
  Word64
boardToZobrist wpms kms bpms b isBlackTurn =
  foldl'
    (\hash i -> maybe hash (xor hash) (selectMask i))
    (if isBlackTurn then blackTurnMask else 0)
    [0 .. 120]
 where
  selectMask i =
    if
        | testBit b.whitePawns i -> Just $ wpms V.! i
        | testBit b.king i -> Just $ kms V.! i
        | testBit b.blackPawns i -> Just $ bpms V.! i
        | otherwise -> Nothing

boardToZobrist0 :: Board -> Bool -> Word64
boardToZobrist0 =
  boardToZobrist whitePawnMasks kingMasks blackPawnMasks

boardToZobrist90 :: Board -> Bool -> Word64
boardToZobrist90 =
  boardToZobrist whitePawnMasks90 kingMasks90 blackPawnMasks90

boardToZobrist180 :: Board -> Bool -> Word64
boardToZobrist180 =
  boardToZobrist whitePawnMasks180 kingMasks180 blackPawnMasks180

boardToZobrist270 :: Board -> Bool -> Word64
boardToZobrist270 =
  boardToZobrist whitePawnMasks270 kingMasks270 blackPawnMasks270

boardToZobristFlippedHorizontal :: Board -> Bool -> Word64
boardToZobristFlippedHorizontal =
  boardToZobrist
    whitePawnMasksFlippedHorizontal
    kingMasksFlippedHorizontal
    blackPawnMasksFlippedHorizontal

boardToZobristFlippedVertical :: Board -> Bool -> Word64
boardToZobristFlippedVertical =
  boardToZobrist
    whitePawnMasksFlippedVertical
    kingMasksFlippedVertical
    blackPawnMasksFlippedVertical

boardToZobristFlippedSouthEast :: Board -> Bool -> Word64
boardToZobristFlippedSouthEast =
  boardToZobrist
    whitePawnMasksFlippedSouthEast
    kingMasksFlippedSouthEast
    blackPawnMasksFlippedSouthEast

boardToZobristFlippedSouthWest :: Board -> Bool -> Word64
boardToZobristFlippedSouthWest =
  boardToZobrist
    whitePawnMasksFlippedSouthWest
    kingMasksFlippedSouthWest
    blackPawnMasksFlippedSouthWest

boardToMultiZobrist :: Board -> Bool -> MultiZobrist
boardToMultiZobrist board isBlackTurn =
  MultiZobrist
    { rotated0 = boardToZobrist0 board isBlackTurn
    , rotated90 = boardToZobrist90 board isBlackTurn
    , rotated180 = boardToZobrist180 board isBlackTurn
    , rotated270 = boardToZobrist270 board isBlackTurn
    , flippedHorizontal = boardToZobristFlippedHorizontal board isBlackTurn
    , flippedVertical = boardToZobristFlippedVertical board isBlackTurn
    , flippedSouthEast = boardToZobristFlippedSouthEast board isBlackTurn
    , flippedSouthWest = boardToZobristFlippedSouthWest board isBlackTurn
    }

--------------------------------------------------------------------------------

updateMultiZobrist :: [(PieceType, Int8)] -> MultiZobrist -> MultiZobrist
updateMultiZobrist pieces zobrist = foldl' (\acc (t, i) -> updateForPiece t i acc) turnToggled pieces
 where
  turnToggled =
    MultiZobrist
      { rotated0 = xor zobrist.rotated0 blackTurnMask
      , rotated90 = xor zobrist.rotated90 blackTurnMask
      , rotated180 = xor zobrist.rotated180 blackTurnMask
      , rotated270 = xor zobrist.rotated270 blackTurnMask
      , flippedHorizontal = xor zobrist.flippedHorizontal blackTurnMask
      , flippedVertical = xor zobrist.flippedVertical blackTurnMask
      , flippedSouthEast = xor zobrist.flippedSouthEast blackTurnMask
      , flippedSouthWest = xor zobrist.flippedSouthWest blackTurnMask
      }
  updateForPiece :: PieceType -> Int8 -> MultiZobrist -> MultiZobrist
  updateForPiece t i z =
    case t of
      WhiteType ->
        MultiZobrist
          { rotated0 = xor z.rotated0 (whitePawnMasks V.! fromIntegral i)
          , rotated90 = xor z.rotated90 (whitePawnMasks90 V.! fromIntegral i)
          , rotated180 = xor z.rotated180 (whitePawnMasks180 V.! fromIntegral i)
          , rotated270 = xor z.rotated270 (whitePawnMasks270 V.! fromIntegral i)
          , flippedHorizontal = xor zobrist.flippedHorizontal (whitePawnMasksFlippedHorizontal V.! fromIntegral i)
          , flippedVertical = xor zobrist.flippedVertical (whitePawnMasksFlippedVertical V.! fromIntegral i)
          , flippedSouthEast = xor zobrist.flippedSouthEast (whitePawnMasksFlippedSouthEast V.! fromIntegral i)
          , flippedSouthWest = xor zobrist.flippedSouthWest (whitePawnMasksFlippedSouthWest V.! fromIntegral i)
          }
      KingType ->
        MultiZobrist
          { rotated0 = xor z.rotated0 (kingMasks V.! fromIntegral i)
          , rotated90 = xor z.rotated90 (kingMasks90 V.! fromIntegral i)
          , rotated180 = xor z.rotated180 (kingMasks180 V.! fromIntegral i)
          , rotated270 = xor z.rotated270 (kingMasks270 V.! fromIntegral i)
          , flippedHorizontal = xor zobrist.flippedHorizontal (kingMasksFlippedHorizontal V.! fromIntegral i)
          , flippedVertical = xor zobrist.flippedVertical (kingMasksFlippedVertical V.! fromIntegral i)
          , flippedSouthEast = xor zobrist.flippedSouthEast (kingMasksFlippedSouthEast V.! fromIntegral i)
          , flippedSouthWest = xor zobrist.flippedSouthWest (kingMasksFlippedSouthWest V.! fromIntegral i)
          }
      BlackType ->
        MultiZobrist
          { rotated0 = xor z.rotated0 (blackPawnMasks V.! fromIntegral i)
          , rotated90 = xor z.rotated90 (blackPawnMasks90 V.! fromIntegral i)
          , rotated180 = xor z.rotated180 (blackPawnMasks180 V.! fromIntegral i)
          , rotated270 = xor z.rotated270 (blackPawnMasks270 V.! fromIntegral i)
          , flippedHorizontal = xor zobrist.flippedHorizontal (blackPawnMasksFlippedHorizontal V.! fromIntegral i)
          , flippedVertical = xor zobrist.flippedVertical (blackPawnMasksFlippedVertical V.! fromIntegral i)
          , flippedSouthEast = xor zobrist.flippedSouthEast (blackPawnMasksFlippedSouthEast V.! fromIntegral i)
          , flippedSouthWest = xor zobrist.flippedSouthWest (blackPawnMasksFlippedSouthWest V.! fromIntegral i)
          }

selectZobrist :: MultiZobrist -> Word64
selectZobrist z =
  maximum
    [ z.rotated0
    , z.rotated90
    , z.rotated180
    , z.rotated270
    , z.flippedHorizontal
    , z.flippedVertical
    , z.flippedSouthEast
    , z.flippedSouthWest
    ]
