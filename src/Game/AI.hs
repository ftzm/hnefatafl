{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Strict #-}

module Game.AI where

import AI.Assessment (blackVictory, whiteVictory)
import Board.Board (Board, PieceType (..))
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
import Data.Aeson.Text (encodeToLazyText)
import Data.List qualified as L
import Data.Map qualified as M
import Event (BoardUpdate (..), Event (..), Resolution (..))
import Html.Board (renderBoard)
import Lucid (Attributes, Html, renderText)
import Lucid.Html5

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
  MakeMove move ->
    let
      postMoveGameState = makeMove g move
      postMoveResolutionO =
        if
            | whiteVictory postMoveGameState.board -> Just WhiteVictory
            | blackVictory postMoveGameState.board -> Just BlackVictory
            | otherwise -> Nothing
      postAIGameState :: GameState = case postMoveResolutionO of
        Nothing ->
          let
            nextBoard =
              if g.humanIsBlack
                then snd $ L.head $ nextMoveBoardsWhite' postMoveGameState.board
                else snd $ L.head $ nextMoveBoardsBlack' postMoveGameState.board
           in
            GameState
              { board = nextBoard
              , isBlackTurn = not postMoveGameState.isBlackTurn
              , moves =
                  if postMoveGameState.isBlackTurn
                    then getWhiteMoves nextBoard
                    else getBlackMoves nextBoard
              , humanIsBlack = postMoveGameState.humanIsBlack
              }
        Just _ -> postMoveGameState
      postAIResolutionO = case postMoveResolutionO of
        Nothing ->
          if
              | whiteVictory postMoveGameState.board -> Just WhiteVictory
              | blackVictory postMoveGameState.board -> Just BlackVictory
              | otherwise -> Nothing
        Just x -> Just x
      events =
        case postAIResolutionO of
          Nothing ->
            let a = 1
             in [ UpdateBoard $
                    BoardUpdate (toSeeMovesMap postAIGameState.moves) $
                      seeMovesMapBoard postAIGameState
                      -- , UpdateStatus $
                      -- if newGameState.isBlackTurn
                      -- then "Black to move"
                      -- else "White to move"
                ]
          Just resolution ->
            [ UpdateBoard $
                BoardUpdate mempty $
                  toStrict $
                    renderText $
                      renderBoard postAIGameState.board mempty mempty
            , UpdateStatus $ show resolution
            ]
     in
      Right
        ( Just postAIGameState
        , events
        )
  Concede -> undefined

--------------------------------------------------------------------------------
