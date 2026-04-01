{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Hnefatafl.Import (
  GameImport (..),
  importGame,
  importGameFromFile,
  getOrCreateHumanPlayer,
) where

import Chronos (
  Time,
  datetimeToTime,
  decode_lenient,
  encodeIso8601,
  timeToDatetime,
 )
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  eitherDecodeStrict,
  object,
  withObject,
  (.:),
  (.:?),
  (.=),
 )
import Data.Either.Combinators (mapLeft)
import Effectful (Eff, (:>))
import Effectful.Console.ByteString
import Effectful.Error.Static (Error, tryError)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO.ByteString
import Hnefatafl.Bindings (
  EngineGameStatus,
  applyMoveSequence,
  nextGameState,
  toGameStatus,
 )
import Hnefatafl.Core.Data (
  Game (..),
  GameId (..),
  GameMode (..),
  GameMove (..),
  GameStatus (..),
  HumanPlayer (..),
  Move,
  MoveResult (..),
  Participant (..),
  PlayerColor (..),
  PlayerId (..),
 )
import Hnefatafl.Effect.Clock
import Hnefatafl.Effect.IdGen (IdGen, generateId)
import Hnefatafl.Effect.Storage (
  Storage,
  StorageTx,
  humanPlayerFromName,
  insertGame,
  insertHumanPlayer,
  insertMoves,
  runTransaction,
 )
import Hnefatafl.Serialization (movesToNotation, parseMoveList)
import Prelude hiding (putStrLn, readFile)

data GameImport = GameImport
  { gameName :: Maybe Text
  , blackPlayerName :: Text
  , whitePlayerName :: Text
  , startTime :: Maybe Time
  , endTime :: Maybe Time
  , gameStatus :: Maybe Text
  , moves :: NonEmpty Move
  }
  deriving (Show, Eq, Generic)

-- | Parse ISO8601 datetime string to Chronos Time
parseTimeFromString :: Text -> Maybe Time
parseTimeFromString t = datetimeToTime <$> decode_lenient t

instance FromJSON GameImport where
  parseJSON = withObject "GameImport" $ \o -> do
    gameName <- o .:? "gameName"
    blackPlayerName <- o .: "blackPlayerName"
    whitePlayerName <- o .: "whitePlayerName"
    startTimeText <- o .:? "startTime"
    startTime <- case startTimeText of
      Nothing -> pure Nothing
      Just t -> case parseTimeFromString t of
        Just time -> pure (Just time)
        Nothing -> fail $ "Invalid startTime format: " <> toString t
    endTimeText <- o .:? "endTime"
    endTime <- case endTimeText of
      Nothing -> pure Nothing
      Just t -> case parseTimeFromString t of
        Just time -> pure (Just time)
        Nothing -> fail $ "Invalid endTime format: " <> toString t
    gameStatus <- o .:? "gameStatus"
    movesText <- o .: "moves"
    moves <- case parseMoveList movesText of
      Left err -> fail $ toString err
      Right parsedMoves -> pure parsedMoves
    pure $
      GameImport
        gameName
        blackPlayerName
        whitePlayerName
        startTime
        endTime
        gameStatus
        moves

instance ToJSON GameImport where
  toJSON GameImport{..} =
    object
      [ "gameName" .= gameName
      , "blackPlayerName" .= blackPlayerName
      , "whitePlayerName" .= whitePlayerName
      , "startTime" .= fmap (encodeIso8601 . timeToDatetime) startTime
      , "endTime" .= fmap (encodeIso8601 . timeToDatetime) endTime
      , "gameStatus" .= gameStatus
      , "moves" .= movesToNotation (toList moves)
      ]

-- | Parse text game status into GameStatus type
parseGameStatus :: Text -> Maybe GameStatus
parseGameStatus = \case
  "ongoing" -> Just Ongoing
  "black_won_king_captured" -> Just BlackWonKingCaptured
  "black_won_white_surrounded" -> Just BlackWonWhiteSurrounded
  "black_won_no_white_moves" -> Just BlackWonNoWhiteMoves
  "black_won_resignation" -> Just BlackWonResignation
  "black_won_timeout" -> Just BlackWonTimeout
  "white_won_king_escaped" -> Just WhiteWonKingEscaped
  "white_won_exit_fort" -> Just WhiteWonExitFort
  "white_won_no_black_moves" -> Just WhiteWonNoBlackMoves
  "white_won_resignation" -> Just WhiteWonResignation
  "white_won_timeout" -> Just WhiteWonTimeout
  "draw" -> Just Draw
  "abandoned" -> Just Abandoned
  _ -> Nothing

-- | Look up a human player by name, creating one if it doesn't exist (within a transaction)
getOrCreateHumanPlayerTx ::
  PlayerId -> Text -> StorageTx HumanPlayer
getOrCreateHumanPlayerTx freshId playerName =
  humanPlayerFromName playerName >>= \case
    Just player -> pure player
    Nothing -> do
      let newPlayer = HumanPlayer freshId playerName Nothing
      insertHumanPlayer newPlayer
      pure newPlayer

-- | Look up a human player by name, creating one if it doesn't exist
getOrCreateHumanPlayer ::
  (Storage :> es, IdGen :> es) => Text -> Eff es HumanPlayer
getOrCreateHumanPlayer playerName = do
  freshId <- generateId
  runTransaction $ getOrCreateHumanPlayerTx freshId playerName

importGame ::
  (Clock :> es, Storage :> es, IdGen :> es, Error String :> es) =>
  GameImport -> Eff es (Either Text ())
importGame input = do
  result <- tryError @String $ do
    case validStatus of
      Left err -> pure $ Left err
      Right engineStatus -> do
        gameId <- generateId @GameId
        currentTime <- now
        blackFreshId <- generateId
        whiteFreshId <- generateId
        let startTime = fromMaybe currentTime input.startTime
        -- The engine status can be relied on when the game ends in normal
        -- victory condition, but in the event of a timeout or resignation
        -- when the game is technically ongoing we'll need to defer to the
        -- explicitly defined status.
        let status = case toGameStatus engineStatus of
              Ongoing -> fromMaybe Ongoing (input.gameStatus >>= parseGameStatus)
              other -> other

        let (moveResults, _finalStatus) = applyMoveSequence input.moves
            gameMoves = map (moveResultToGameMove startTime) (toList moveResults)

        runTransaction $ do
          blackPlayer <- getOrCreateHumanPlayerTx blackFreshId input.blackPlayerName
          whitePlayer <- getOrCreateHumanPlayerTx whiteFreshId input.whitePlayerName
          let game =
                Game
                  { gameId = gameId
                  , name = input.gameName
                  , mode =
                      Online
                        (Just (RegisteredPlayer whitePlayer.playerId))
                        (Just (RegisteredPlayer blackPlayer.playerId))
                  , startTime = startTime
                  , endTime = input.endTime
                  , gameStatus = status
                  , createdAt = currentTime
                  }
          insertGame game
          insertMoves gameId gameMoves
        pure $ Right ()
  case result of
    Left (_, err) -> pure $ Left $ "Import failed: " <> toText err
    Right eitherResult -> pure eitherResult
 where
  validStatus :: Either Text EngineGameStatus = mapLeft show $ nextGameState input.moves True
  moveResultToGameMove :: Time -> MoveResult -> GameMove
  moveResultToGameMove time (MoveResult move board captures wasBlackTurn _) =
    GameMove
      { playerColor = if wasBlackTurn then Black else White
      , move = move
      , boardStateAfter = board
      , captures = captures
      , timestamp = time
      }

importGameFromFile ::
  ( FileSystem :> es
  , Clock :> es
  , Storage :> es
  , IdGen :> es
  , Error String :> es
  , Console :> es
  ) =>
  Text -> Eff es (Either Text ())
importGameFromFile filename = do
  fileContent <- readFile $ toString filename
  case eitherDecodeStrict fileContent of
    Left err -> pure $ Left $ "Failed to parse JSON: " <> toText err
    Right gameImports -> do
      results <- forM (zip [1 :: Int ..] gameImports) \(i, g) -> do
        let gameNumber :: ByteString = "Processing game: " <> show i
        putStrLn gameNumber
        importGame g
      case partitionEithers results of
        ([], _) -> pure $ Right ()
        (errors, _) ->
          pure $
            Left $
              "Failed to import some games: " <> mconcat (intersperse ", " errors)
