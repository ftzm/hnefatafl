{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v1 #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}

-- oioi
module Hnefatafl.Bindings (
  startBoard,
  moveListToBase64,
  moveListFromBase64,
  startBlackMoves,
  nextGameState,
  Move (..),
  GameStatus (..),
)
where

import Foreign (
  Ptr,
  Storable (alignment, peek, poke, sizeOf),
  alloca,
  allocaBytes,
  castPtr,
  free,
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
  deriving (Show, Read, Eq, Enum)

instance Storable GameStatus where
  sizeOf _ = sizeOf (0 :: CInt)
  alignment _ = alignment (0 :: CInt)
  peek ptr = do
    val <- peek (castPtr ptr :: Ptr CInt)
    return $ toEnum (fromIntegral val)
  poke ptr val = poke (castPtr ptr :: Ptr CInt) (fromIntegral $ fromEnum val)

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

foreign import ccall unsafe "next_game_state"
  c_get_possible_moves ::
    Ptr Move -> CInt -> Ptr CInt -> Ptr GameStatus -> IO (Ptr Move)

nextGameState :: NonEmpty Move -> (GameStatus, [Move])
nextGameState moveHistory = unsafePerformIO $
  withArrayLen (toList moveHistory) $ \historyLen -> \historyPtr ->
    alloca $ \moveCountPtr ->
      alloca $ \gameStatusPtr -> do
        movesPtr <-
          c_get_possible_moves
            historyPtr
            (fromIntegral historyLen)
            moveCountPtr
            gameStatusPtr
        status <- peek gameStatusPtr
        moves <-
          if status == Ongoing
            then do
              count <- peek moveCountPtr
              moves <- peekArray (fromIntegral count) movesPtr
              free movesPtr
              return moves
            else return $ [] ++ []
        return (status, moves)
