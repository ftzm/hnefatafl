{-# LANGUAGE MultiWayIf #-}

module Html.Board where

import Board.Board (Board (..))
import Board.Constant (corners, startBoard)
import Command (Command)
import Data.Aeson.Text (encodeToLazyText)
import Data.Bits (testBit, (.|.))
import Data.List.Split
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.WideWord (Word128)
import Lucid (Attributes, Html)
import Lucid.Html5

renderBoard :: Board -> Set Int -> Map Int (Html ()) -> Html ()
renderBoard Board{whitePawns, king, blackPawns} commandSet contentMap = do
  -- script_ $ T.intercalate "\n" $ map (uncurry defineCommand) $ M.toList commandMap
  table_ [class_ "board"] $ do
    mconcat $ map (tr_ . mconcat) $ chunksOf 11 $ map square [0 .. 120]
 where
  colorBoard :: Word128
  colorBoard = startBoard.whitePawns .|. startBoard.king .|. startBoard.blackPawns .|. corners
  -- defineCommand :: Int -> Command -> Text
  -- defineCommand i command = "let command" <> show i <> " = " <> toStrict (encodeToLazyText command)
  shouldColor = testBit colorBoard
  square :: Int -> Html ()
  square i =
    td_
      ( [class_ $ if shouldColor i then "dark" else "light"]
          <> ( [ onclick_ $
                "socket.send(JSON.stringify(boardCommands['"
                  <> show i
                  <> "']))"
               | S.member i commandSet
               ]
             )
      )
      $ getPiece i <> M.findWithDefault "" i contentMap
  getPiece :: Int -> Html ()
  getPiece i =
    if
        | testBit whitePawns i -> span_ [class_ "circle whitePiece"] " "
        | testBit king i -> span_ [class_ "circle king"] " "
        | testBit blackPawns i -> div_ [class_ "circle blackPiece"] " "
        | otherwise -> span_ ""
