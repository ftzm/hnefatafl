module Hnefatafl.Serialization (
  indexToNotation,
  parseMove,
  parseMoveList,
) where

import Data.Attoparsec.ByteString.Char8
import Data.Char (isAlphaNum)
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
positionToIndex (Position (File file) (Rank rank)) = file + rank

indexToPosition :: Word8 -> Position
indexToPosition index =
  let
    file = File $ fromIntegral $ index `div` 11
    rank = Rank $ fromIntegral $ index `mod` 11
   in
    Position file rank

indexToNotation :: Word8 -> Text
indexToNotation = positionToNotation . indexToPosition

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
  _ <- Data.Attoparsec.ByteString.Char8.takeWhile isAlphaNum
  pure ()

moveParser :: Parser Move
moveParser = do
  orig <- fromIntegral . positionToIndex <$> positionParser
  _ <- optional $ char '-'
  dest <- fromIntegral . positionToIndex <$> positionParser
  _ <- optional captureParser
  pure $ Move orig dest

moveListParser :: Parser [Move]
moveListParser = skipSpace *> sepBy1 moveParser skipSpace <* skipSpace

parseMove :: ByteString -> Maybe Move
parseMove = rightToMaybe . parseOnly moveParser

parseMoveList :: ByteString -> Maybe [Move]
parseMoveList = rightToMaybe . parseOnly moveListParser
