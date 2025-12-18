module Hnefatafl.Serialization (
  moveToNotation,
  movesToNotation,
  parseMove,
  parseMoveList,
) where

import Data.Attoparsec.Text
import Data.Char (isAlphaNum)
import Data.Either.Combinators (mapLeft)
import Data.List (elemIndex, (!!))
import Data.Maybe (fromJust)
import Data.Text (singleton)
import Hnefatafl.Core.Data

newtype File = File {unFile :: Int}
  deriving (Show)

newtype Rank = Rank {unRank :: Int}
  deriving (Show)

data Position = Position File Rank
  deriving (Show)

data MovePositions = MovePositions Position Position
  deriving (Show)

--------------------------------------------------------------------------------
-- Output

positionToNotation :: Position -> Text
positionToNotation (Position (File file) (Rank rank)) =
  let fileSymbols :: [Text] = map singleton ['k', 'j' ..]
   in (fileSymbols !! file) <> show (rank + 1)

positionToIndex :: Position -> Int
positionToIndex (Position (File file) (Rank rank)) = rank * 11 + file

indexToPosition :: Word8 -> Position
indexToPosition index =
  let
    rank = Rank $ fromIntegral $ index `div` 11
    file = File $ fromIntegral $ index `mod` 11
   in
    Position file rank

indexToNotation :: Word8 -> Text
indexToNotation = positionToNotation . indexToPosition

moveToNotation :: Move -> Text
moveToNotation Move{orig, dest} = indexToNotation orig <> indexToNotation dest

movesToNotation :: [Move] -> Text
movesToNotation = mconcat . intersperse " " . map moveToNotation

--------------------------------------------------------------------------------
-- Parse notation

fileParser :: Parser File
fileParser = do
  c <- satisfy (inClass chars)
  let index = fromJust $ elemIndex c chars
  pure $ File index
 where
  chars = ['k', 'j' .. 'a']

rankParser :: Parser Rank
rankParser = do
  rawInt <- decimal @Int
  guard $ rawInt < 12 && rawInt > 0
  pure $ Rank $ rawInt - 1

positionParser :: Parser Position
positionParser = Position <$> fileParser <*> rankParser

captureParser :: Parser ()
captureParser = do
  _ <- char 'x'
  _ <- Data.Attoparsec.Text.takeWhile isAlphaNum
  pure ()

moveParser :: Parser Move
moveParser = do
  orig <- fromIntegral . positionToIndex <$> positionParser
  _ <- optional $ char '-'
  dest <- fromIntegral . positionToIndex <$> positionParser
  _ <- optional captureParser
  pure $ Move orig dest

sepByNonEmpty ::
  forall (f :: Type -> Type) a s. Alternative f => f a -> f s -> f (NonEmpty a)
sepByNonEmpty p s = fromList <$> sepBy1 p s

moveListParser :: Parser (NonEmpty Move)
moveListParser = skipSpace *> sepByNonEmpty moveParser skipSpace <* skipSpace

parseMove :: Text -> Either Text Move
parseMove = mapLeft toText . parseOnly moveParser

parseMoveList :: Text -> Either Text (NonEmpty Move)
parseMoveList = mapLeft toText . parseOnly moveListParser
