{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Strict #-}

module Game.AI where

import AI.Assessment (blackVictory, whiteVictory)
import AI.NegamaxABZ (nextBoardNABZ)
import Board.Board (Board, PieceType (..), Team (..))
import Board.Constant (startBoard)
import Board.Move (
  applyMoveBlack,
  applyMoveKing,
  applyMoveWhitePawn,
  blackMoves',
  kingMoves',
  nextMoveBoardsBlack',
  nextMoveBoardsWhite',
  whiteMoves'',
 )
import Command (Command (..), IndexedMove (..), Move (..))
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.Chan.Unagi (InChan, OutChan, newChan, readChan, writeChan, writeList2Chan)
import Data.Aeson.Text (encodeToLazyText)
import Data.List qualified as L
import Data.Map qualified as M
import Event (BoardUpdate (..), Event (..), Resolution (..))
import Html.Board (renderBoard)
import Lucid (Attributes, Html, renderText)
import Lucid.Html5 hiding (for_)

--------------------------------------------------------------------------------
-- Game state

data GameState = GameState
  { board :: Board
  , isBlackTurn :: Bool
  , moves :: Map Int [IndexedMove]
  , humanIsBlack :: Bool
  }

initialGameState :: Bool -> GameState
initialGameState = GameState startBoard True (getBlackMoves startBoard)

--------------------------------------------------------------------------------
-- Moves

buildMoveMap :: PieceType -> [(Int8, Int8)] -> Map Int [IndexedMove]
buildMoveMap p =
  foldl'
    ( \acc (orig, dest) ->
        M.alter
          ( maybe
              (Just [IndexedMove p $ fromIntegral dest])
              (Just . (IndexedMove p (fromIntegral dest) :))
          )
          (fromIntegral orig)
          acc
    )
    mempty

getBlackMoves :: Board -> Map Int [IndexedMove]
getBlackMoves = buildMoveMap BlackType . blackMoves'

getWhiteMoves :: Board -> Map Int [IndexedMove]
getWhiteMoves board =
  buildMoveMap KingType (kingMoves' board)
    <> buildMoveMap WhiteType (whiteMoves'' board)

--------------------------------------------------------------------------------
-- Move Selection Utils

toSeeMoves :: Int -> [Attributes]
toSeeMoves i =
  [onclick_ $ "'socket.send(" <> toStrict (encodeToLazyText (SeeMoves i)) <> ")'"]

toSeeMovesMap :: Map Int [IndexedMove] -> Map Int Command
toSeeMovesMap = M.mapWithKey (\k _ -> SeeMoves k)

toMovesMap :: Int -> [IndexedMove] -> Map Int Command
toMovesMap orig ims = M.fromList $ map (\im -> (im.dest, MakeMove $ Move im.pieceType orig im.dest)) ims

seeMovesMapBoard :: GameState -> Text
seeMovesMapBoard gs = toStrict $ renderText $ renderBoard gs.board (M.keysSet gs.moves) mempty

movesMapBoard :: PieceType -> Set Int -> [Int] -> GameState -> Text
movesMapBoard pt commandSquares contentSquares gs =
  toStrict $ renderText $ renderBoard gs.board commandSquares pieceMap
 where
  pieceMap :: Map Int (Html ())
  pieceMap = M.fromList $ map (,fadedPiece) contentSquares
  fadedPiece :: Html ()
  fadedPiece = case pt of
    WhiteType -> div_ [class_ "circle whitePiece faded"] " "
    KingType -> div_ [class_ "circle king faded"] " "
    BlackType -> div_ [class_ "circle blackPiece faded"] " "


-- actually I don't really need to send a map of moves over to the javascript side
-- and then send it back; I can just keep the command map server side and send an
-- index over the socket, and look up the command via the index. Since there's
-- only one potentialy on-click action per square I don't need to be specific
-- about what to do, I only need to specify the square and the rest can be inferred.

--------------------------------------------------------------------------------
-- Make Move

updateBoard :: Board -> Move -> Board
updateBoard board (Move{pieceType, orig, dest}) = case pieceType of
  WhiteType -> applyMoveWhitePawn board (fromIntegral orig, fromIntegral dest)
  KingType -> applyMoveKing board (fromIntegral dest)
  BlackType -> applyMoveBlack board (fromIntegral orig, fromIntegral dest)

makeMove :: GameState -> Move -> GameState
makeMove gs m =
  GameState
    { board = newBoard
    , isBlackTurn = not gs.isBlackTurn
    , moves =
        if gs.isBlackTurn
          then getWhiteMoves newBoard
          else getBlackMoves newBoard
    , humanIsBlack = gs.humanIsBlack
    }
 where
  newBoard = updateBoard gs.board m

--------------------------------------------------------------------------------

handleCommand :: GameState -> Command -> Either Text (Maybe GameState, [Event])
handleCommand g c = case c of
  SeeMoves i -> case M.lookup i g.moves of
    Nothing -> Left "Invalid moves request"
    Just ms ->
      let
        movesMap = toMovesMap i ms
        seeMovesMap = toSeeMovesMap g.moves
        commands = movesMap <> M.singleton i UnSeeMoves <> seeMovesMap
       in
        Right
          ( Nothing
          ,
            [ UpdateBoard $
                BoardUpdate commands $
                  movesMapBoard
                    (L.head ms).pieceType
                    (M.keysSet commands)
                    (map (.dest) ms)
                    g
            ]
          )
  UnSeeMoves ->
    Right
      ( Nothing
      ,
        [ UpdateBoard $
            BoardUpdate (toSeeMovesMap g.moves) $
              seeMovesMapBoard g
        ]
      )
  CommitAiMove board ->
    let
      newGameState =
        GameState
          board
          (not g.isBlackTurn)
          ( if g.isBlackTurn
              then getWhiteMoves board
              else getBlackMoves board
          )
          g.humanIsBlack
      resolutionO =
        if
            | whiteVictory board -> Just WhiteVictory
            | blackVictory board -> Just BlackVictory
            | otherwise -> Nothing
      events =
        case resolutionO of
          Nothing ->
            let a = 1
             in [ UpdateBoard $
                    BoardUpdate (toSeeMovesMap newGameState.moves) $
                      seeMovesMapBoard newGameState
                , UpdateStatus $
                    if newGameState.isBlackTurn
                      then "Black to move"
                      else "White to move"
                ]
          Just resolution ->
            [ UpdateBoard $
                BoardUpdate mempty $
                  toStrict $
                    renderText $
                      renderBoard newGameState.board mempty mempty
            , UpdateStatus $ show resolution
            ]
     in
      Right
        ( Just newGameState
        , events
        )
  MakeMove move ->
    let
      postMoveGameState = makeMove g move
      postMoveResolutionO =
        if
            | whiteVictory postMoveGameState.board -> Just WhiteVictory
            | blackVictory postMoveGameState.board -> Just BlackVictory
            | otherwise -> Nothing
      events =
        case postMoveResolutionO of
          Nothing ->
            let a = 1
             in [ UpdateBoard $
                    BoardUpdate mempty $
                      toStrict $
                        renderText $
                          renderBoard postMoveGameState.board mempty mempty
                , UpdateStatus "Waiting for AI..."
                , AwaitingAi
                ]
          Just resolution ->
            [ UpdateBoard $
                BoardUpdate mempty $
                  toStrict $
                    renderText $
                      renderBoard postMoveGameState.board mempty mempty
            , UpdateStatus $ show resolution
            ]
     in
      Right
        ( Just postMoveGameState
        , events
        )
  Concede -> undefined

runLoop ::
  (GameState -> IO ()) ->
  Team ->
  IORef GameState ->
  InChan Command ->
  OutChan Command ->
  InChan Event ->
  IO ()
runLoop saveState team gameStateRef commandInChan commandOutChan eventChan = do
  (moveRequestIn, moveRequestOut) <- newChan @Board

  -- Perform the first move when the player is white
  gs <- readIORef gameStateRef
  when (not gs.humanIsBlack && gs.isBlackTurn) $ do
    let boardUpdate =
          UpdateBoard $
            BoardUpdate mempty $
              toStrict $
                renderText $
                  renderBoard gs.board mempty mempty
    writeChan eventChan boardUpdate
    let aiResult = nextBoardNABZ gs.board team
    let newGameState =
          GameState
            aiResult
            (not gs.isBlackTurn)
            ( if gs.isBlackTurn
                then getWhiteMoves aiResult
                else getBlackMoves aiResult
            )
            gs.humanIsBlack
    let newBoardUpdate =
          UpdateBoard $
            BoardUpdate (toSeeMovesMap newGameState.moves) $
              seeMovesMapBoard newGameState
    let newStatus =
          UpdateStatus $
            if newGameState.isBlackTurn
              then "Black to move"
              else "White to move"
    writeIORef gameStateRef newGameState
    writeList2Chan eventChan [newBoardUpdate, newStatus]

  let
    aiService = forever $ do
      putStrLn "aiService"
      req <- readChan moveRequestOut
      let result = nextBoardNABZ req team
      writeChan commandInChan $ CommitAiMove result

  withAsync aiService $ const $ forever $ do
    putStrLn "runloop"
    gameState <- readIORef gameStateRef
    command <- readChan commandOutChan
    print command
    case handleCommand gameState command of
      Left err -> print err
      Right (newGsO, events) -> do
        writeList2Chan eventChan events
        when
          (AwaitingAi `elem` events)
          (writeChan moveRequestIn $ maybe gameState.board (.board) newGsO)
        for_ newGsO $ \newGs -> do
          writeIORef gameStateRef newGs
          saveState newGs

--------------------------------------------------------------------------------
