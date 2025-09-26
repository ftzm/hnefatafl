{-# LANGUAGE DeriveAnyClass #-}
{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v1 #-}
{- ORMOLU_ENABLE -}

module Hnefatafl.Bindings (startBoard, moveListToBase64, moveListFromBase64, Move (..)) where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable.Generic

data Layer = Layer {lower :: Int64, upper :: Int64}
  deriving (Show, Read, Generic, GStorable)

data ExternBoard = ExternBoard
  {black :: Layer, white :: Layer, king :: Word8}
  deriving (Show, Read, Generic, GStorable)

data Move = Move
  {orig :: Word8, dest :: Word8}
  deriving (Show, Read, Eq, Generic, GStorable)

foreign import ccall unsafe "start_board_extern"
  start_board_extern :: Ptr ExternBoard -> IO ()

startBoard :: IO ExternBoard
startBoard = alloca $ \ptr -> do
  start_board_extern ptr
  peek ptr

foreign import ccall unsafe "move_list_to_base64"
  c_move_list_to_base64 :: Ptr Move -> CInt -> CString -> IO ()

foreign import ccall unsafe "move_list_from_base64"
  c_move_list_from_base64 :: CString -> Ptr CInt -> IO (Ptr Move)

foreign import ccall unsafe "base64_encoded_size"
  c_base64_encoded_size :: CInt -> CInt

moveListToBase64 :: [Move] -> IO String
moveListToBase64 moves =
  let len = length moves
   in if len == 0
        then return ""
        else withArray moves $ \movesPtr -> do
          let inputSize = len * 2 -- moves are 2 bytes
          let encodedSize = fromIntegral $ c_base64_encoded_size (fromIntegral inputSize)
          allocaBytes encodedSize $ \outputPtr -> do
            c_move_list_to_base64 movesPtr (fromIntegral len) outputPtr
            peekCString outputPtr

moveListFromBase64 :: String -> IO [Move]
moveListFromBase64 base64Str =
  withCString
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
