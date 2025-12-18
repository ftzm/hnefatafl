{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v1 #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}

module Hnefatafl.Bindings (
  startBoard,
  moveListToBase64,
  moveListFromBase64,
  startBlackMoves,
  nextGameState,
  nextGameStateWithMoves,
  nextGameStateWithMovesTrusted,
  applyMoveSequence,
  toGameStatus,
  EngineGameStatus (..),
  MoveError (..),
  MoveValidationResult (..),
) where

import Control.Monad.Trans.Cont
import Foreign (
  Ptr,
  Storable (alignment, peek, poke, sizeOf),
  alloca,
  allocaBytes,
  castPtr,
  free,
  peekArray,
  with,
  withArray,
 )
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Storable.Generic (GStorable)
import Hnefatafl.Core.Data (
  DomainMapping (..),
  ExternBoard (..),
  GameStatus (..),
  Layer (..),
  Move (..),
  MoveResult (..),
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

toGameStatus :: EngineGameStatus -> GameStatus
toGameStatus = \case
  EngineOngoing -> Hnefatafl.Core.Data.Ongoing
  EngineKingCaptured -> BlackWonKingCaptured
  EngineWhiteSurrounded -> BlackWonWhiteSurrounded
  EngineNoWhiteMoves -> BlackWonNoWhiteMoves
  EngineKingEscaped -> WhiteWonKingEscaped
  EngineExitFort -> WhiteWonExitFort
  EngineNoBlackMoves -> WhiteWonNoBlackMoves

-- Internal storable types for FFI
data StorableLayer = StorableLayer {lower :: Int64, upper :: Int64}
  deriving (Show, Read, Generic, GStorable)

data StorableExternBoard = StorableExternBoard
  {black :: StorableLayer, white :: StorableLayer, king :: Word8}
  deriving (Show, Read, Generic, GStorable)

data StorableMove = StorableMove
  {orig :: Word8, dest :: Word8}
  deriving (Show, Read, Eq, Generic, GStorable)

data StorableMoveResult = StorableMoveResult
  { move :: StorableMove
  , board :: StorableExternBoard
  , captures :: StorableLayer
  , was_black_turn :: Bool
  }
  deriving (Show, Read, Generic, GStorable)

data StorableGameStatus
  = Ongoing
  | KingCaptured -- black victory
  | WhiteSurrounded -- black victory
  | NoWhiteMoves -- black victory
  | KingEscaped -- white victory
  | ExitFort -- white victory
  | NoBlackMoves -- white victory
  deriving (Show, Read, Eq, Enum)

data MoveError
  = MoveErrorNoError
  | MoveErrorNoPieceAtOrigin
  | MoveErrorWrongPieceForTurn
  | MoveErrorInvalidDestination
  | MoveErrorNotOrthogonal
  | MoveErrorPathBlocked
  | MoveErrorThreefoldRepetition
  deriving (Show, Read, Eq, Enum)

data MoveValidationResult = MoveValidationResult
  { err :: MoveError
  , moveIndex :: CInt
  }
  deriving (Eq, Show, Read, Generic, GStorable)

instance Storable StorableGameStatus where
  sizeOf _ = sizeOf (0 :: CInt)
  alignment _ = alignment (0 :: CInt)
  peek ptr = do
    val <- peek (castPtr ptr :: Ptr CInt)
    return $ toEnum (fromIntegral val)
  poke ptr val = poke (castPtr ptr :: Ptr CInt) (fromIntegral $ fromEnum val)

instance Storable MoveError where
  sizeOf _ = sizeOf (0 :: CInt)
  alignment _ = alignment (0 :: CInt)
  peek ptr = do
    val <- peek (castPtr ptr :: Ptr CInt)
    return $ toEnum (fromIntegral val)
  poke ptr val = poke (castPtr ptr :: Ptr CInt) (fromIntegral $ fromEnum val)

instance ToText MoveError where
  toText = toText @String . show

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

instance DomainMapping StorableMoveResult MoveResult where
  toDomain (StorableMoveResult m b c wbt) = MoveResult (toDomain m) (toDomain b) (toDomain c) wbt
  fromDomain (MoveResult m b c wbt) = StorableMoveResult (fromDomain m) (fromDomain b) (fromDomain c) wbt

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

startBoard :: ExternBoard
{-# NOINLINE startBoard #-}
startBoard = unsafePerformIO $ alloca $ \ptr -> do
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

-- -- Convert move validation error to human readable error message
-- moveValidationErrorMessage :: MoveError -> Int -> Text
-- moveValidationErrorMessage err moveIndex =
--   "Move validation failed: "
--     <> toText err
--     <> " at move "
--     <> toText @String (show moveIndex)

-- Convert Haskell moves to C array and pass to continuation with pointer and length
withStorableMoveArray ::
  NonEmpty Move -> (Ptr StorableMove -> CInt -> ContT r IO a) -> ContT r IO a
withStorableMoveArray moveHistory action = do
  let historyLen = fromIntegral $ length moveHistory
      storableHistory = map (fromDomain @StorableMove) (toList moveHistory)
  historyPtr <- ContT $ withArray storableHistory
  action historyPtr historyLen

foreign import ccall unsafe "next_game_state"
  c_next_game_state ::
    Ptr StorableMove ->
    CInt ->
    Ptr StorableGameStatus ->
    Ptr MoveValidationResult ->
    CBool ->
    IO ()

foreign import ccall unsafe "next_game_state_with_moves"
  c_next_game_state_with_moves ::
    Ptr StorableMove ->
    CInt ->
    Ptr (Ptr StorableMove) ->
    Ptr CInt ->
    Ptr StorableGameStatus ->
    Ptr MoveValidationResult ->
    CBool ->
    IO ()

foreign import ccall unsafe "next_game_state_with_moves_trusted"
  c_next_game_state_with_moves_trusted ::
    Ptr StorableExternBoard ->
    CBool ->
    Ptr StorableMove ->
    CInt ->
    Ptr (Ptr StorableMove) ->
    Ptr CInt ->
    Ptr StorableGameStatus ->
    IO CInt

nextGameState ::
  NonEmpty Move -> Bool -> Either MoveValidationResult EngineGameStatus
nextGameState moveHistory allowRepetition = unsafePerformIO $ evalContT $ do
  withStorableMoveArray moveHistory $ \historyPtr historyLen -> do
    gameStatusPtr <- ContT (alloca @StorableGameStatus)
    validationPtr <- ContT (alloca @MoveValidationResult)
    liftIO $
      c_next_game_state
        historyPtr
        historyLen
        gameStatusPtr
        validationPtr
        (if allowRepetition then 1 else 0)
    result <- liftIO $ peek validationPtr
    case result.err of
      MoveErrorNoError -> do
        storableStatus <- liftIO $ peek gameStatusPtr
        return $ Right $ toDomain storableStatus
      _ -> return $ Left result

nextGameStateWithMoves ::
  NonEmpty Move -> Bool -> Either MoveValidationResult (EngineGameStatus, [Move])
nextGameStateWithMoves moveHistory allowRepetition = unsafePerformIO $ evalContT $ do
  withStorableMoveArray moveHistory $ \historyPtr historyLen -> do
    movesPtrPtr <- ContT alloca
    moveCountPtr <- ContT alloca
    gameStatusPtr <- ContT alloca
    validationPtr <- ContT (alloca @MoveValidationResult)
    liftIO $
      c_next_game_state_with_moves
        historyPtr
        historyLen
        movesPtrPtr
        moveCountPtr
        gameStatusPtr
        validationPtr
        (if allowRepetition then 1 else 0)
    result <- liftIO $ peek validationPtr
    case result.err of
      MoveErrorNoError -> do
        storableStatus <- liftIO $ peek gameStatusPtr
        let status = toDomain storableStatus
        moves <-
          if storableStatus == Hnefatafl.Bindings.Ongoing
            then liftIO $ do
              count <- peek moveCountPtr
              movesPtr <- peek movesPtrPtr
              storableMoves <- peekArray (fromIntegral count) movesPtr
              free movesPtr
              return $ map toDomain storableMoves
            else return []
        return $ Right (status, moves)
      _ -> return $ Left result

nextGameStateWithMovesTrusted ::
  ExternBoard -> Bool -> NonEmpty Move -> (EngineGameStatus, [Move])
nextGameStateWithMovesTrusted trustedBoard isBlackTurn moveHistory = unsafePerformIO $ evalContT $ do
  let
    historyLen :: CInt = fromIntegral $ length moveHistory
    storableHistory = map (fromDomain @StorableMove) (toList moveHistory)
    storableBoard = fromDomain @StorableExternBoard trustedBoard
  boardPtr <- ContT $ with storableBoard
  historyPtr <- ContT $ withArray storableHistory
  movesPtrPtr <- ContT alloca
  moveCountPtr <- ContT alloca
  gameStatusPtr <- ContT alloca
  result <-
    liftIO $
      c_next_game_state_with_moves_trusted
        boardPtr
        (if isBlackTurn then 1 else 0)
        historyPtr
        historyLen
        movesPtrPtr
        moveCountPtr
        gameStatusPtr
  when (result /= 0) $ Prelude.error "next_game_state_with_moves_trusted failed"
  storableStatus <- liftIO $ peek gameStatusPtr
  let status = toDomain storableStatus
  moves <-
    if storableStatus == Hnefatafl.Bindings.Ongoing
      then liftIO $ do
        count <- peek moveCountPtr
        movesPtr <- peek movesPtrPtr
        storableMoves <- peekArray (fromIntegral count) movesPtr
        free movesPtr
        return $ map toDomain storableMoves
      else return []
  return (status, moves)

foreign import ccall unsafe "apply_move_sequence"
  c_apply_move_sequence ::
    Ptr StorableMove ->
    CInt ->
    IO (Ptr StorableMoveResult)

applyMoveSequence :: NonEmpty Move -> NonEmpty MoveResult
applyMoveSequence moves = unsafePerformIO $ evalContT $ do
  let
    moveCount :: CInt = fromIntegral $ length moves
    storableMoves = map (fromDomain @StorableMove) (toList moves)
  movesPtr <- ContT $ withArray storableMoves
  moveResultsPtr <- liftIO $ c_apply_move_sequence movesPtr moveCount
  storableMoveResults <-
    liftIO $ peekArray (fromIntegral moveCount) moveResultsPtr
  liftIO $ free moveResultsPtr
  return $ fromList $ map toDomain storableMoveResults
