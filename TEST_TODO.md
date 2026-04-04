# Test Coverage TODO

Concrete test descriptions ordered by priority. Each item describes a single
test file or small group of tests to implement.

## Legend

- **P1** — Critical: silent correctness bugs or untested production paths
- **P2** — High: important integration points or complex logic
- **P3** — Medium: useful but lower risk

---

## libhnefatafl (C)

### P1

- [ ] **test/transposition_table.c** — Transposition table unit tests
  - Store and probe entries; verify hit returns correct depth/score/move
  - Probe miss on empty table returns no match
  - Bucket collision: fill all 4 entries in a bucket, verify replacement policy (depth-preferred)
  - Generation increment: verify old-generation entries are replaced before same-generation
  - Generation wraparound at uint8 boundary (255 → 0)
  - Store with different hash but same bucket index; verify no data corruption
  - Probe after overwrite: newer entry replaces older, old data gone

- [ ] **test/game_state.c** — Game state reconstruction tests
  - `board_state_from_move_list()` with a known move sequence; compare resulting board to expected
  - Empty move list returns starting position
  - Single move applied correctly (check piece positions)
  - Full game replay: apply all moves from a recorded game, verify final board matches expected
  - Invalid move in sequence: verify error handling / return code

### P2

- [ ] **test/board.c** — Board mutation unit tests
  - `apply_black_move()`: piece moves from source to destination, source cleared
  - `apply_white_move()`: same for white pawns
  - `apply_king_move()`: king position updated, old square cleared
  - `to_compact()` / `from_compact()` roundtrip: convert board to compact and back, compare equality
  - Rotation consistency: apply move, check all 4 rotated layers are consistent

- [ ] **test/io.c** — I/O and notation tests
  - Algebraic notation roundtrip: position → notation string → position, verify equality
  - Parse all 121 board positions (a1–k11), verify correct index
  - Board-to-string output for starting position matches expected snapshot
  - Edge cases: corner squares (a1, a11, k1, k11), throne (f6)

### P3

- [ ] **test/layer.c** — Bitboard layer operation tests
  - `LAYER_AND`, `LAYER_OR`, `LAYER_XOR` with known inputs, verify output
  - `LAYER_POPCOUNT` on empty, single-bit, full layers
  - Rotation: rotate a single-bit layer 4 times, verify return to original
  - Set/clear/test individual bit positions at boundaries (0, 63, 64, 120)

---

## backend (Haskell)

### P1

- [ ] **test/Hnefatafl/Api/HandlersTest.hs** — API handler tests
  - GET /health returns 200
  - GET /version returns valid version string
  - POST /search-trusted with valid board returns a legal move
  - POST /search-trusted with invalid board returns 400

- [ ] **test/Hnefatafl/Api/Handlers/HotseatTest.hs** — Hotseat endpoint tests
  - POST create game returns game ID and token
  - GET load game returns correct board state
  - POST make move with valid move updates board and switches turn
  - POST make move with invalid move returns error
  - POST undo reverts last move
  - POST resign ends game with correct winner

- [ ] **test/Hnefatafl/Api/Handlers/OnlineTest.hs** — Online endpoint tests
  - POST create game returns game ID and two tokens (black/white)
  - POST join game with valid token succeeds
  - POST make move only accepted from current turn's player
  - POST make move from wrong player returns 403
  - Draw offer / accept / reject flow through endpoints
  - Undo request / accept / reject flow through endpoints

### P2

- [ ] **test/Hnefatafl/SerializationTest.hs** — Move notation tests
  - `moveToNotation` for known moves matches expected strings (e.g. "a2-a5")
  - `notationToMove` roundtrip for all valid notation formats
  - Invalid notation strings (empty, missing dash, out of range) return parse error
  - Edge positions: corners (a1, k11), throne-adjacent squares

- [ ] **test/Hnefatafl/App/HotseatTest.hs** — Hotseat app logic tests
  - Create game initializes correct starting board
  - Make move updates game state and persists
  - Undo after one move returns to start
  - Undo when no moves returns error
  - Resign sets correct winner based on current turn
  - Agree to draw when draw offered ends game as draw

- [ ] **test/Hnefatafl/App/OnlineTest.hs** — Online app logic tests
  - Create game with two players stores both tokens
  - Move by correct player succeeds
  - Move by wrong player fails
  - Draw offer persists as pending action
  - Move after draw offer cancels the offer (already tested at game logic level, verify at app level)
  - Undo with 1 move undoes 1; undo with 2+ undoes 2

- [ ] **test/Hnefatafl/Interpreter/Search/LocalTest.hs** — Local search interpreter test
  - Search from starting position returns a legal move
  - Search result board matches applying the returned move to input board
  - Search with very short timeout still returns a move (best effort)

### P3

- [ ] **test/Hnefatafl/BoardTest.hs** — Board display tests
  - `printBoard` for starting position matches expected multi-line string
  - `printBoardWithMove` highlights source and destination squares
  - `printBoardWithCaptures` marks captured pieces

- [ ] **test/Hnefatafl/Interpreter/Storage/SQLite/ErrorTest.hs** — Storage error paths
  - Insert duplicate game name returns appropriate error
  - Insert move for non-existent game fails gracefully
  - Concurrent game updates don't corrupt state (two moves at once)

- [ ] **test/Hnefatafl/CLITest.hs** — CLI option parsing
  - Default options parse correctly with no arguments
  - --port flag sets port
  - --db flag sets database path
  - Invalid flags produce help message

---

## frontend (SolidJS/TypeScript)

### P1

- [ ] **src/controllers/HotseatController.test.tsx** — Hotseat controller tests
  - Making a move calls service.makeMove with correct arguments
  - After move, turn switches to other player
  - Undo calls service.undo and reverts board state
  - Resign calls service.resign and shows game over
  - Draw offer / accept flow updates game status

- [ ] **src/controllers/AiController.test.tsx** — AI controller tests
  - Human move triggers AI response
  - AI move is applied to board after human move
  - Undo undoes both AI and human moves (2-ply undo)
  - Resign during AI turn handled correctly
  - Loading state shown while AI is thinking

- [ ] **src/controllers/OnlineController.test.tsx** — Online controller tests
  - Receiving opponent move via event updates board
  - Sending move emits correct event
  - Draw offer sends event; receiving draw offer shows prompt
  - Undo request/accept/reject events handled correctly
  - Connection loss shows error state

### P2

- [ ] **src/utils/async-lock.test.ts** — AsyncLock tests
  - Sequential execution: tasks run in FIFO order
  - Concurrent acquire: second task waits for first to complete
  - Released lock allows next task to proceed
  - Error in locked task still releases the lock

- [ ] **src/mocks/mock-hotseat.test.ts** — Mock hotseat service tests
  - makeMove applies move and returns updated state
  - makeMove with illegal move returns error
  - undo reverts to previous state
  - resign returns game-over state
  - Matches HotseatService interface contract

- [ ] **src/mocks/mock-ai-game.test.ts** — Mock AI service tests
  - makeMove triggers AI response move
  - AI move is legal
  - Game ends when king escapes or is captured
  - Matches AiGameService interface contract

- [ ] **src/mocks/mock-online-game.test.ts** — Mock online service tests
  - makeMove emits move event
  - Receiving move event updates state
  - Draw/undo offer events work bidirectionally
  - Matches OnlineGameService interface contract

- [ ] **src/components/Controls.test.tsx** — Game controls tests
  - Prev/Next buttons call correct history navigation functions
  - Undo button visible only when moves exist
  - Resign button triggers confirmation
  - Draw button visible only in appropriate game modes
  - Buttons disabled when game is over

- [ ] **src/components/GameStatus.test.tsx** — Game status display tests
  - Shows "Black's turn" / "White's turn" during play
  - Shows "Black wins" / "White wins" on game over
  - Shows "Draw" on draw
  - Shows correct win reason (king escaped, king captured, etc.)

### P3

- [ ] **src/components/HotseatSetupModal.test.tsx** — Hotseat setup modal tests
  - Renders with default options
  - Start button creates game with selected options
  - Cancel closes modal without creating game

- [ ] **src/components/AiSetupModal.test.tsx** — AI setup modal tests
  - Difficulty selection updates state
  - Side selection (black/white) works
  - Start creates game with correct config

- [ ] **src/components/OnlineSetupModal.test.tsx** — Online setup modal tests
  - Create game generates shareable link/code
  - Join game with code connects to existing game

- [ ] **src/components/GameLayout.test.tsx** — Game layout tests
  - Desktop layout shows board and sidebar
  - Mobile layout shows bottom sheet instead of sidebar
  - Toolbar buttons trigger correct actions
