module Hnefatafl.SelfPlay.UI.Widgets (
  renderBoardGrid,
) where

import Brick (
  Widget (..),
  fill,
  hBox,
  hLimit,
  render,
  txt,
  vBox,
  vLimit,
 )
import Brick.Types (Context (..), Size (..), getContext)
import Data.List.Split (chunksOf)
import Hnefatafl.Board (formatMoveResult)
import Hnefatafl.Core.Data (MoveResult (..))
import Hnefatafl.SelfPlay (
  GameKey,
  mkGameName,
 )

-- Board dimensions
boardWidth :: Int
boardWidth = 40

boardHeight :: Int
boardHeight = 15 -- 1 title line + 14 board lines (13 board + 1 blank from formatMoveResult)

-- | Render items in a grid with evenly distributed spacing
paddedGrid ::
  Int -> -- item width
  Int -> -- item height
  [a] -> -- items to render
  (a -> Widget ()) -> -- render function for each item
  Widget ()
paddedGrid itemWidth itemHeight items renderItem = Widget Fixed Fixed $ do
  ctx <- getContext
  let availWidth = ctx.availWidth
      availHeight = ctx.availHeight

      -- Calculate how many items fit per row
      calculateItemsPerRow :: Int -> Int
      calculateItemsPerRow n =
        let widthForItems = n * itemWidth
            minSpacing = n + 1 -- minimum 1 char spacing
            required = widthForItems + minSpacing
         in if required <= availWidth && n < length items
              then calculateItemsPerRow (n + 1)
              else n - 1

      itemsPerRow = max 1 (calculateItemsPerRow 1)

      -- Calculate horizontal spacing
      totalItemWidth = itemsPerRow * itemWidth
      remainingWidth = availWidth - totalItemWidth
      hSpacing = max 1 (remainingWidth `div` (itemsPerRow + 1))

      -- Calculate vertical layout
      rowsToShow = max 1 (availHeight `div` (itemHeight + 1))
      totalItemHeight = rowsToShow * itemHeight
      remainingHeight = availHeight - totalItemHeight
      vSpacing = max 1 (remainingHeight `div` (rowsToShow + 1))

      displayItems = take (itemsPerRow * rowsToShow) items
      groupedItems = chunksOf itemsPerRow displayItems

      hSpacer = hLimit hSpacing $ fill ' '
      vSpacer = vLimit vSpacing $ fill ' '

      renderRow rowItems =
        hBox $ hSpacer : intersperse hSpacer (map renderItem rowItems) ++ [hSpacer]

  render $
    vBox $
      vSpacer : intersperse vSpacer (map renderRow groupedItems) ++ [vSpacer]

renderBoard :: (GameKey, MoveResult) -> Widget ()
renderBoard (key, moveResult) =
  let name = mkGameName key
   in hLimit boardWidth $
        vLimit boardHeight $
          vBox
            [ txt name
            , txt $ formatMoveResult moveResult
            ]

renderBoardGrid :: [(GameKey, MoveResult)] -> Widget ()
renderBoardGrid games = paddedGrid boardWidth boardHeight games renderBoard
