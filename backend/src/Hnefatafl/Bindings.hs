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
  searchTrusted,
  TranspositionTable,
  ttCreate,
  ttDestroy,
  ttClear,
  ttNewGeneration,
  EngineGameStatus (..),
  MoveError (..),
  MoveValidationResult (..),
  SearchTrustedResult (..),
) where

import Control.Monad.Trans.Cont
import Data.Aeson (FromJSON, ToJSON)
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
import Foreign.C.Types (CBool (..), CInt (..), CSize (..))
import Foreign.Storable.Generic (GStorable)
import Hnefatafl.Core.Data (
  DomainMapping (..),
  ExternBoard (..),
  Layer (..),
  Move (..),
  MoveResult (..),
  MoveWithCaptures (..),
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
  | EngineDrawOffered
  | EngineWhiteResigned -- black victory
  | EngineBlackResigned -- white victory
  deriving (Show, Read, Eq, Ord, Enum, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Internal storable types for FFI
data StorableLayer = StorableLayer {lower :: Word64, upper :: Word64}
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
  , zobrist_hash :: Word64
  }
  deriving (Show, Read, Generic, GStorable)

data StorableMoveWithCaptures = StorableMoveWithCaptures
  { move :: StorableMove
  , captures :: StorableLayer
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
  | DrawOffered
  | WhiteResigned -- black victory
  | BlackResigned -- white victory
  deriving (Show, Read, Eq, Enum)

data MoveError
  = MoveErrorNoError
  | MoveErrorNoPieceAtOrigin
  | MoveErrorWrongPieceForTurn
  | MoveErrorInvalidDestination
  | MoveErrorNotOrthogonal
  | MoveErrorPathBlocked
  | MoveErrorThreefoldRepetition
  | MoveErrorPositionOutOfBounds
  | MoveErrorDestEqualsOrigin
  deriving (Show, Read, Eq, Enum)

data MoveValidationResult = MoveValidationResult
  { err :: MoveError
  , moveIndex :: CInt
  }
  deriving (Eq, Show, Read, Generic, GStorable)

data SearchTrustedResult = SearchTrustedResult
  { searchMove :: Move
  , updatedBoard :: ExternBoard
  , captures :: Layer
  , updatedZobristHash :: Word64
  , gameStatus :: EngineGameStatus
  }
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  toDomain (StorableMoveResult m b c wbt zh) = MoveResult (toDomain m) (toDomain b) (toDomain c) wbt zh
  fromDomain (MoveResult m b c wbt zh) = StorableMoveResult (fromDomain m) (fromDomain b) (fromDomain c) wbt zh

instance DomainMapping StorableMoveWithCaptures MoveWithCaptures where
  toDomain (StorableMoveWithCaptures m c) = MoveWithCaptures (toDomain m) (toDomain c)
  fromDomain (MoveWithCaptures m c) = StorableMoveWithCaptures (fromDomain m) (fromDomain c)

instance DomainMapping StorableGameStatus EngineGameStatus where
  toDomain Hnefatafl.Bindings.Ongoing = EngineOngoing
  toDomain KingCaptured = EngineKingCaptured
  toDomain WhiteSurrounded = EngineWhiteSurrounded
  toDomain NoWhiteMoves = EngineNoWhiteMoves
  toDomain KingEscaped = EngineKingEscaped
  toDomain ExitFort = EngineExitFort
  toDomain NoBlackMoves = EngineNoBlackMoves
  toDomain DrawOffered = EngineDrawOffered
  toDomain WhiteResigned = EngineWhiteResigned
  toDomain BlackResigned = EngineBlackResigned

  fromDomain EngineOngoing = Hnefatafl.Bindings.Ongoing
  fromDomain EngineKingCaptured = KingCaptured
  fromDomain EngineWhiteSurrounded = WhiteSurrounded
  fromDomain EngineNoWhiteMoves = NoWhiteMoves
  fromDomain EngineKingEscaped = KingEscaped
  fromDomain EngineExitFort = ExitFort
  fromDomain EngineNoBlackMoves = NoBlackMoves
  fromDomain EngineDrawOffered = DrawOffered
  fromDomain EngineWhiteResigned = WhiteResigned
  fromDomain EngineBlackResigned = BlackResigned

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

startBlackMoves :: NonEmpty MoveWithCaptures
startBlackMoves =
  fromList $
    map (\m -> MoveWithCaptures (toDomain m) (Layer 0 0)) $
      unsafePerformIO $
        peekArray 116 c_start_black_moves

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
    Ptr (Ptr StorableMoveWithCaptures) ->
    Ptr CInt ->
    Ptr StorableGameStatus ->
    Ptr MoveValidationResult ->
    CBool ->
    Ptr StorableMoveResult ->
    IO ()

foreign import ccall unsafe "next_game_state_with_moves_trusted"
  c_next_game_state_with_moves_trusted ::
    Ptr StorableExternBoard ->
    CBool ->
    Ptr StorableMove ->
    Ptr Word64 ->
    CInt ->
    Ptr StorableMoveResult ->
    Ptr StorableGameStatus ->
    Ptr (Ptr StorableMoveWithCaptures) ->
    Ptr CInt ->
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
  NonEmpty Move ->
  Bool ->
  Either MoveValidationResult (MoveResult, EngineGameStatus, [MoveWithCaptures])
nextGameStateWithMoves moveHistory allowRepetition = unsafePerformIO $ evalContT $ do
  withStorableMoveArray moveHistory $ \historyPtr historyLen -> do
    movesPtrPtr <- ContT alloca
    moveCountPtr <- ContT alloca
    gameStatusPtr <- ContT alloca
    validationPtr <- ContT (alloca @MoveValidationResult)
    moveResultPtr <- ContT alloca
    liftIO $
      c_next_game_state_with_moves
        historyPtr
        historyLen
        movesPtrPtr
        moveCountPtr
        gameStatusPtr
        validationPtr
        (if allowRepetition then 1 else 0)
        moveResultPtr
    result <- liftIO $ peek validationPtr
    case result.err of
      MoveErrorNoError -> do
        moveResult <- toDomain <$> liftIO (peek moveResultPtr)
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
        return $ Right (moveResult, status, moves)
      _ -> return $ Left result

nextGameStateWithMovesTrusted ::
  ExternBoard ->
  Bool ->
  Move ->
  [Word64] ->
  Either MoveError (MoveResult, EngineGameStatus, [MoveWithCaptures])
nextGameStateWithMovesTrusted trustedBoard isBlackTurn move hashes = unsafePerformIO $ evalContT $ do
  let
    storableBoard = fromDomain @StorableExternBoard trustedBoard
    storableMove = fromDomain @StorableMove move
    hashCount :: CInt = fromIntegral $ length hashes
  boardPtr <- ContT $ with storableBoard
  movePtr <- ContT $ with storableMove
  hashesPtr <- ContT $ withArray hashes
  moveResultPtr <- ContT alloca
  gameStatusPtr <- ContT alloca
  movesPtrPtr <- ContT alloca
  moveCountPtr <- ContT alloca
  result <-
    liftIO $
      c_next_game_state_with_moves_trusted
        boardPtr
        (if isBlackTurn then 1 else 0)
        movePtr
        hashesPtr
        hashCount
        moveResultPtr
        gameStatusPtr
        movesPtrPtr
        moveCountPtr
  if result /= 0
    then return $ Left (toEnum (fromIntegral result))
    else do
      moveResult <- toDomain <$> liftIO (peek moveResultPtr)
      storableStatus <- liftIO $ peek gameStatusPtr
      let status = toDomain storableStatus
      validMoves <-
        if storableStatus == Hnefatafl.Bindings.Ongoing
          then liftIO $ do
            count <- peek moveCountPtr
            movesPtr <- peek movesPtrPtr
            storableMoves <- peekArray (fromIntegral count) movesPtr
            free movesPtr
            return $ map toDomain storableMoves
          else return []
      return $ Right (moveResult, status, validMoves)

foreign import ccall unsafe "apply_move_sequence"
  c_apply_move_sequence ::
    Ptr StorableMove ->
    CInt ->
    Ptr StorableGameStatus ->
    IO (Ptr StorableMoveResult)

applyMoveSequence :: NonEmpty Move -> (NonEmpty MoveResult, EngineGameStatus)
applyMoveSequence moves = unsafePerformIO $ do
  let
    moveCount :: CInt = fromIntegral $ length moves
    storableMoves = map (fromDomain @StorableMove) (toList moves)

  withArray storableMoves $ \movesPtr ->
    alloca $ \statusPtr -> do
      moveResultsPtr <- c_apply_move_sequence movesPtr moveCount statusPtr
      storableMoveResults <- peekArray (fromIntegral moveCount) moveResultsPtr
      finalStatus <- peek statusPtr
      free moveResultsPtr
      return (fromList $ map toDomain storableMoveResults, toDomain finalStatus)

-- Opaque pointer to transposition table
data TranspositionTable

foreign import ccall safe "tt_create"
  c_tt_create :: CSize -> IO (Ptr TranspositionTable)

foreign import ccall safe "tt_destroy"
  c_tt_destroy :: Ptr TranspositionTable -> IO ()

foreign import ccall safe "tt_clear"
  c_tt_clear :: Ptr TranspositionTable -> IO ()

foreign import ccall safe "tt_new_generation"
  c_tt_new_generation :: Ptr TranspositionTable -> IO ()

ttCreate :: Int -> IO (Ptr TranspositionTable)
ttCreate sizeMb = c_tt_create (fromIntegral sizeMb)

ttDestroy :: Ptr TranspositionTable -> IO ()
ttDestroy = c_tt_destroy

ttClear :: Ptr TranspositionTable -> IO ()
ttClear = c_tt_clear

ttNewGeneration :: Ptr TranspositionTable -> IO ()
ttNewGeneration = c_tt_new_generation

foreign import ccall safe "search_trusted"
  c_search_trusted ::
    Ptr StorableExternBoard ->
    CBool ->
    Ptr Word64 ->
    CInt ->
    Ptr CBool ->
    Ptr TranspositionTable ->
    Ptr StorableMove ->
    Ptr StorableExternBoard ->
    Ptr StorableLayer ->
    Ptr Word64 ->
    Ptr StorableGameStatus ->
    CBool ->
    IO ()

searchTrusted ::
  ExternBoard ->
  Bool ->
  [Word64] ->
  Ptr CBool ->
  Ptr TranspositionTable ->
  Bool ->
  IO SearchTrustedResult
searchTrusted trustedBoard isBlackTurn zobristHashes shouldStopPtr ttPtr enableAdminEndings = evalContT $ do
  let
    storableBoard = fromDomain @StorableExternBoard trustedBoard
    hashCount = fromIntegral $ length zobristHashes
  boardPtr <- ContT $ with storableBoard
  hashArrayPtr <- ContT $ withArray zobristHashes
  movePtr <- ContT (alloca @StorableMove)
  outBoardPtr <- ContT (alloca @StorableExternBoard)
  capturesPtr <- ContT (alloca @StorableLayer)
  hashPtr <- ContT (alloca @Word64)
  statusPtr <- ContT (alloca @StorableGameStatus)

  liftIO $
    c_search_trusted
      boardPtr
      (if isBlackTurn then 1 else 0)
      hashArrayPtr
      hashCount
      shouldStopPtr
      ttPtr
      movePtr
      outBoardPtr
      capturesPtr
      hashPtr
      statusPtr
      (if enableAdminEndings then 1 else 0)

  storableMove <- liftIO $ peek movePtr
  storableOutBoard <- liftIO $ peek outBoardPtr
  storableCaptures <- liftIO $ peek capturesPtr
  outHash <- liftIO $ peek hashPtr
  storableStatus <- liftIO $ peek statusPtr
  return $
    SearchTrustedResult
      { searchMove = toDomain storableMove
      , updatedBoard = toDomain storableOutBoard
      , captures = toDomain storableCaptures
      , updatedZobristHash = outHash
      , gameStatus = toDomain storableStatus
      }
