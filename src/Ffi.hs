{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v1 #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}

module Ffi (startBoard, boardToCode) where

import Foreign
import Foreign.C (CChar, peekCAStringLen)
import Foreign.Storable.Generic

data Layer = Layer {lower :: Int64, upper :: Int64}
  deriving (Show, Read, Generic, GStorable)

data ExternBoard = ExternBoard
  {black :: Layer, white :: Layer, king :: Word8}
  deriving (Show, Read, Generic, GStorable)

foreign import ccall unsafe "start_board_extern"
  start_board_extern :: Ptr ExternBoard -> IO ()

startBoard :: IO ExternBoard
startBoard = alloca $ \ptr -> do
  start_board_extern ptr
  peek ptr

foreign import ccall unsafe "board_to_code"
  board_to_code :: Ptr CChar -> Ptr ExternBoard -> IO ()

boardToCode :: ExternBoard -> IO String
boardToCode b = allocaBytes 44 $ \str_ptr ->
  alloca $ \brd_ptr -> do
    poke brd_ptr b
    board_to_code str_ptr brd_ptr
    peekCAStringLen (str_ptr, 44)
