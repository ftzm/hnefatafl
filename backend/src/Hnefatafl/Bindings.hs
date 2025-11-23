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
  EngineGameStatus (..),
) where

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
import Hnefatafl.Core.Data (
  DomainMapping (..),
  ExternBoard (..),
  Layer (..),
  Move (..),
 )
import System.IO.Unsafe (unsafePerformIO)

-- | Game status from the engine's perspective
data EngineGameStatus
  = EngineOngoing
  | EngineKingCaptured -- black victory
  | EngineWhiteSurrounded -- black victory
  | EngineNoWhiteMoves -- black victory
  | EngineKingEscaped -- white victory
  | EngineExitFort -- white victory
  | EngineNoBlackMoves -- white victory
  deriving (Show, Read, Eq, Enum)

-- Internal storable types for FFI
data StorableLayer = StorableLayer {lower :: Int64, upper :: Int64}
  deriving (Show, Read, Generic, GStorable)

data StorableExternBoard = StorableExternBoard
  {black :: StorableLayer, white :: StorableLayer, king :: Word8}
  deriving (Show, Read, Generic, GStorable)

data StorableMove = StorableMove
  {orig :: Word8, dest :: Word8}
  deriving (Show, Read, Eq, Generic, GStorable)

data StorableGameStatus
  = Ongoing
  | KingCaptured -- black victory
  | WhiteSurrounded -- black victory
  | NoWhiteMoves -- black victory
  | KingEscaped -- white victory
  | ExitFort -- white victory
  | NoBlackMoves -- white victory
  deriving (Show, Read, Eq, Enum)

instance Storable StorableGameStatus where
  sizeOf _ = sizeOf (0 :: CInt)
  alignment _ = alignment (0 :: CInt)
  peek ptr = do
    val <- peek (castPtr ptr :: Ptr CInt)
    return $ toEnum (fromIntegral val)
  poke ptr val = poke (castPtr ptr :: Ptr CInt) (fromIntegral $ fromEnum val)

-- DomainMapping instances
instance DomainMapping StorableLayer Layer where
  toDomain (StorableLayer l u) = Layer l u
  fromDomain (Layer l u) = StorableLayer l u

instance DomainMapping StorableExternBoard ExternBoard where
  toDomain (StorableExternBoard b w k) = ExternBoard (toDomain b) (toDomain w) k
  fromDomain (ExternBoard b w k) = StorableExternBoard (fromDomain b) (fromDomain w) k

instance DomainMapping StorableMove Move where
  toDomain (StorableMove o d) = Move o d
  fromDomain (Move o d) = StorableMove o d

instance DomainMapping StorableGameStatus EngineGameStatus where
  toDomain Hnefatafl.Bindings.Ongoing = EngineOngoing
  toDomain KingCaptured = EngineKingCaptured
  toDomain WhiteSurrounded = EngineWhiteSurrounded
  toDomain NoWhiteMoves = EngineNoWhiteMoves
  toDomain KingEscaped = EngineKingEscaped
  toDomain ExitFort = EngineExitFort
  toDomain NoBlackMoves = EngineNoBlackMoves

  fromDomain EngineOngoing = Hnefatafl.Bindings.Ongoing
  fromDomain EngineKingCaptured = KingCaptured
  fromDomain EngineWhiteSurrounded = WhiteSurrounded
  fromDomain EngineNoWhiteMoves = NoWhiteMoves
  fromDomain EngineKingEscaped = KingEscaped
  fromDomain EngineExitFort = ExitFort
  fromDomain EngineNoBlackMoves = NoBlackMoves

foreign import ccall unsafe "start_board_extern"
  start_board_extern :: Ptr StorableExternBoard -> IO ()

startBoard :: IO ExternBoard
startBoard = alloca $ \ptr -> do
  start_board_extern ptr
  storableBoard <- peek ptr
  return $ toDomain storableBoard

foreign import ccall unsafe "base64_encoded_size"
  c_base64_encoded_size :: CInt -> CInt

foreign import ccall unsafe "move_list_to_base64"
  c_move_list_to_base64 :: Ptr StorableMove -> CInt -> CString -> IO ()

moveListToBase64 :: [Move] -> String
moveListToBase64 moves =
  let len = length moves
   in if len == 0
        then ""
        else unsafePerformIO $ withArray (map fromDomain moves) $ \movesPtr -> do
          let inputSize = len * 2 -- moves are 2 bytes
          let encodedSize = fromIntegral $ c_base64_encoded_size (fromIntegral inputSize)
          allocaBytes encodedSize $ \outputPtr -> do
            c_move_list_to_base64 movesPtr (fromIntegral len) outputPtr
            peekCString outputPtr

foreign import ccall unsafe "move_list_from_base64"
  c_move_list_from_base64 :: CString -> Ptr CInt -> IO (Ptr StorableMove)

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
            storableMoves <- peekArray (fromIntegral count) movesPtr
            free movesPtr -- Free C-allocated memory
            return $ map toDomain storableMoves

foreign import ccall unsafe "&start_black_moves"
  c_start_black_moves :: Ptr StorableMove

startBlackMoves :: NonEmpty Move
startBlackMoves = fromList $ map toDomain $ unsafePerformIO $ peekArray 116 c_start_black_moves

foreign import ccall unsafe "next_game_state"
  c_get_possible_moves ::
    Ptr StorableMove ->
    CInt ->
    Ptr CInt ->
    Ptr StorableGameStatus ->
    IO (Ptr StorableMove)

nextGameState :: NonEmpty Move -> (EngineGameStatus, [Move])
nextGameState moveHistory = unsafePerformIO $
  withArrayLen (map fromDomain $ toList moveHistory) $ \historyLen historyPtr -> alloca $ \moveCountPtr ->
    alloca $ \gameStatusPtr -> do
      movesPtr <-
        c_get_possible_moves
          historyPtr
          (fromIntegral historyLen)
          moveCountPtr
          gameStatusPtr
      storableStatus <- peek gameStatusPtr
      let status = toDomain storableStatus
      moves <-
        if storableStatus == Hnefatafl.Bindings.Ongoing
          then do
            count <- peek moveCountPtr
            storableMoves <- peekArray (fromIntegral count) movesPtr
            free movesPtr
            return $ map toDomain storableMoves
          else return []
      return (status, moves)
