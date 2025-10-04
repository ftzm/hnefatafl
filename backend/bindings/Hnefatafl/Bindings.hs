{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v1 #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}

module Hnefatafl.Bindings (
  startBoard,
  moveListToBase64,
  moveListFromBase64,
  startBlackMoves,
  getPossibleMoves,
  Move (..),
)
where

import Foreign (
  Ptr,
  Storable (peek),
  alloca,
  allocaBytes,
  free,
  nullPtr,
  peekArray,
  withArray,
  withArrayLen,
 )
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Storable.Generic (GStorable)
import System.IO.Unsafe (unsafePerformIO)

data Layer = Layer {lower :: Int64, upper :: Int64}
  deriving (Show, Read, Generic, GStorable)

data ExternBoard = ExternBoard
  {black :: Layer, white :: Layer, king :: Word8}
  deriving (Show, Read, Generic, GStorable)

data Move = Move
  {orig :: Word8, dest :: Word8}
  deriving (Show, Read, Eq, Generic, GStorable)

data GameStatus
  = Ongoing
  | KingCaptured -- black victory
  | WhiteSurrounded -- black victory
  | NoWhiteMoves -- black victory
  | KingEscaped -- white victory
  | ExitFort -- white victory
  | NoBlackMoves -- white victory
  deriving (Show, Read, Eq)

foreign import ccall unsafe "start_board_extern"
  start_board_extern :: Ptr ExternBoard -> IO ()

startBoard :: IO ExternBoard
startBoard = alloca $ \ptr -> do
  start_board_extern ptr
  peek ptr

foreign import ccall unsafe "base64_encoded_size"
  c_base64_encoded_size :: CInt -> CInt

foreign import ccall unsafe "move_list_to_base64"
  c_move_list_to_base64 :: Ptr Move -> CInt -> CString -> IO ()

moveListToBase64 :: [Move] -> String
moveListToBase64 moves =
  let len = length moves
   in if len == 0
        then ""
        else unsafePerformIO $ withArray moves $ \movesPtr -> do
          let inputSize = len * 2 -- moves are 2 bytes
          let encodedSize = fromIntegral $ c_base64_encoded_size (fromIntegral inputSize)
          allocaBytes encodedSize $ \outputPtr -> do
            c_move_list_to_base64 movesPtr (fromIntegral len) outputPtr
            peekCString outputPtr

foreign import ccall unsafe "move_list_from_base64"
  c_move_list_from_base64 :: CString -> Ptr CInt -> IO (Ptr Move)

moveListFromBase64 :: String -> [Move]
moveListFromBase64 base64Str =
  unsafePerformIO
    $ withCString
      base64Str
    $ \base64Ptr ->
      alloca $ \countPtr -> do
        movesPtr <- c_move_list_from_base64 base64Ptr countPtr
        count <- peek countPtr
        if count == 0
          then return []
          else do
            moves <- peekArray (fromIntegral count) movesPtr
            free movesPtr -- Free C-allocated memory
            return moves
foreign import ccall unsafe "&start_black_moves"
  c_start_black_moves :: Ptr Move

startBlackMoves :: NonEmpty Move
startBlackMoves = fromList $ unsafePerformIO $ peekArray 116 c_start_black_moves

foreign import ccall unsafe "get_possible_moves"
  c_get_possible_moves :: Ptr Move -> CInt -> Ptr CInt -> IO (Ptr Move)

getPossibleMoves :: NonEmpty Move -> [Move]
getPossibleMoves moveHistory = unsafePerformIO $
  withArrayLen (toList moveHistory) $ \historyLen -> \historyPtr ->
    alloca $ \moveCountPtr -> do
      resultPtr <-
        c_get_possible_moves historyPtr (fromIntegral historyLen) moveCountPtr
      if resultPtr == nullPtr
        then return []
        else do
          count <- peek moveCountPtr
          moves <- peekArray (fromIntegral count) resultPtr
          free resultPtr
          return moves
