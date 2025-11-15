-- Chess-like Game Database Schema for SQLite
-- Supports mixed human/ENGINE players with proper referential integrity

-- Base players table (supertype)
CREATE TABLE player (
    id TEXT PRIMARY KEY,
    player_type TEXT NOT NULL CHECK (player_type IN ('human', 'engine')),
    created_at DATETIME NOT NULL
);

-- Human players table (subtype)
CREATE TABLE human_player (
    player_id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    email TEXT,
    player_type TEXT NOT NULL CHECK (player_type = 'human'),
    FOREIGN KEY (player_id, player_type) REFERENCES player(id, player_type) ON DELETE CASCADE
);

-- ENGINE players table (subtype)
CREATE TABLE engine_player (
    player_id TEXT PRIMARY KEY,
    version TEXT NOT NULL,
    player_type TEXT NOT NULL CHECK (player_type = 'engine'),
    FOREIGN KEY (player_id, player_type) REFERENCES player(id, player_type) ON DELETE CASCADE
);

-- Games table
CREATE TABLE game (
    id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    white_player_id TEXT, -- NULL for anonymous players
    black_player_id TEXT, -- NULL for anonymous players
    start_time DATETIME NOT NULL,
    end_time DATETIME,
    game_status TEXT CHECK (game_status IN (
        'ongoing',
        'white_won',
        'white_won_resignation',
        'white_won_timeout',
        'black_won',
        'black_won_resignation',
        'black_won_timeout',
        'draw',
        'abandoned'
    )),
    created_at DATETIME NOT NULL,
    FOREIGN KEY (white_player_id) REFERENCES player(id),
    FOREIGN KEY (black_player_id) REFERENCES player(id),
);

-- Moves table
CREATE TABLE move (
    id TEXT PRIMARY KEY,
    game_id TEXT NOT NULL,
    move_number INTEGER NOT NULL CHECK (move_number >= 0),
    player_color TEXT NOT NULL CHECK (player_color IN ('white', 'black')),
    from_position INTEGER,
    to_position INTEGER,
    black_lower INTEGER,
    black_upper INTEGER,
    white_lower INTEGER,
    white_upper INTEGER,
    king INTEGER CHECK (king >= 0 AND king <= 120),
    timestamp DATETIME NOT NULL,
    FOREIGN KEY (game_id) REFERENCES game(id) ON DELETE CASCADE,
    UNIQUE (game_id, move_number)
);

-- Trigger to enforce sequential move numbers
CREATE TRIGGER enforce_sequential_moves
BEFORE INSERT ON move
WHEN NEW.move_number > 0
BEGIN
    SELECT CASE
        WHEN NOT EXISTS (
            SELECT 1 FROM move
            WHERE game_id = NEW.game_id
            AND move_number = NEW.move_number - 1
        )
        THEN RAISE(ABORT, 'Previous move number must exist')
    END;
END;


-- Participant tokens for remote game access
CREATE TABLE game_participant_token (
    id TEXT PRIMARY KEY,
    game_id TEXT NOT NULL,
    token TEXT NOT NULL UNIQUE,
    role TEXT NOT NULL CHECK (role IN ('white', 'black')),
    created_at DATETIME NOT NULL,
    is_active BOOLEAN NOT NULL,
    FOREIGN KEY (game_id) REFERENCES game(id) ON DELETE CASCADE
);

-- Indexes for performance
CREATE INDEX idx_games_players ON game(white_player_id, black_player_id);
CREATE INDEX idx_moves_game ON move(game_id, move_number);
CREATE INDEX idx_players_type ON player(player_type);
CREATE INDEX idx_game_participant_tokens_game ON game_participant_token(game_id);
CREATE INDEX idx_game_participant_tokens_token ON game_participant_token(participant_token);

-- Example usage queries:

-- Create a human player
-- INSERT INTO player (player_type) VALUES ('human');
-- INSERT INTO human_player (player_id, name, email)
-- VALUES (last_insert_rowid(), 'Alice Smith', 'alice@example.com');

-- Create an ENGINE player
-- INSERT INTO player (player_type) VALUES ('engine');
-- INSERT INTO engine_player (player_id, version)
-- VALUES (last_insert_rowid(), 'DeepChess', '2.1', 'minimax');

-- Query games with player details
-- SELECT
--     g.*,
--     wp.player_type as white_type,
--     COALESCE(wh.name, wa.name) as white_name,
--     bp.player_type as black_type,
--     COALESCE(bh.name, ba.name) as black_name
-- FROM game g
-- JOIN player wp ON g.white_player_id = wp.id
-- JOIN player bp ON g.black_player_id = bp.id
-- LEFT JOIN human_player wh ON wp.id = wh.player_id AND wp.player_type = 'human'
-- LEFT JOIN engine_player wa ON wp.id = wa.player_id AND wp.player_type = 'engine'
-- LEFT JOIN human_player bh ON bp.id = bh.player_id AND bp.player_type = 'human'
-- LEFT JOIN engine_player ba ON bp.id = ba.player_id AND bp.player_type = 'engine';

-- POTENTIAL DATABASE ERRORS FROM INVALID INPUT:

-- PRIMARY KEY VIOLATIONS:
-- - Duplicate player.id: "UNIQUE constraint failed: player.id"
-- - Duplicate game.id: "UNIQUE constraint failed: game.id"
-- - Duplicate move.id: "UNIQUE constraint failed: move.id"
-- - Duplicate game_participant_token.id: "UNIQUE constraint failed: game_participant_token.id"
-- - Duplicate human_player.player_id: "UNIQUE constraint failed: human_player.player_id"
-- - Duplicate engine_player.player_id: "UNIQUE constraint failed: engine_player.player_id"

-- UNIQUE CONSTRAINT VIOLATIONS:
-- - Duplicate token: "UNIQUE constraint failed: game_participant_token.token"
-- - Duplicate move number in same game: "UNIQUE constraint failed: move.game_id, move.move_number"

-- FOREIGN KEY VIOLATIONS:
-- - Invalid white_player_id: "FOREIGN KEY constraint failed" (player doesn't exist)
-- - Invalid black_player_id: "FOREIGN KEY constraint failed" (player doesn't exist)
-- - Invalid game_id in move: "FOREIGN KEY constraint failed" (game doesn't exist)
-- - Invalid game_id in game_participant_token: "FOREIGN KEY constraint failed" (game doesn't exist)
-- - Invalid player_id for human_player: "FOREIGN KEY constraint failed" (player doesn't exist or wrong type)
-- - Invalid player_id for engine_player: "FOREIGN KEY constraint failed" (player doesn't exist or wrong type)
-- - Composite FK violation in human_player: "FOREIGN KEY constraint failed" (player_id exists but player_type mismatch)
-- - Composite FK violation in engine_player: "FOREIGN KEY constraint failed" (player_id exists but player_type mismatch)

-- CHECK CONSTRAINT VIOLATIONS:
-- - Invalid player_type in player: "CHECK constraint failed: player_type IN ('human', 'engine', 'anonymous')"
-- - Invalid player_type in human_player: "CHECK constraint failed: player_type = 'human'"
-- - Invalid player_type in engine_player: "CHECK constraint failed: player_type = 'engine'"
-- - Invalid game_status: "CHECK constraint failed: game_status IN (...)"
-- - Invalid move_number: "CHECK constraint failed: move_number >= 0"
-- - Invalid player_color in move: "CHECK constraint failed: player_color IN ('white', 'black')"
-- - Invalid king position: "CHECK constraint failed: king >= 0 AND king <= 120"
-- - Invalid role in game_participant_token: "CHECK constraint failed: role IN ('white', 'black')"

-- NOT NULL CONSTRAINT VIOLATIONS:
-- - Missing player.id: "NOT NULL constraint failed: player.id"
-- - Missing player.player_type: "NOT NULL constraint failed: player.player_type"
-- - Missing player.created_at: "NOT NULL constraint failed: player.created_at"
-- - Missing human_player.name: "NOT NULL constraint failed: human_player.name"
-- - Missing human_player.player_type: "NOT NULL constraint failed: human_player.player_type"
-- - Missing engine_player.player_type: "NOT NULL constraint failed: engine_player.player_type"
-- - Missing game.id: "NOT NULL constraint failed: game.id"
-- - Missing game.name: "NOT NULL constraint failed: game.name"
-- - Missing game.white_player_id: "NOT NULL constraint failed: game.white_player_id"
-- - Missing game.black_player_id: "NOT NULL constraint failed: game.black_player_id"
-- - Missing game.start_time: "NOT NULL constraint failed: game.start_time"
-- - Missing game.created_at: "NOT NULL constraint failed: game.created_at"
-- - Missing move.id: "NOT NULL constraint failed: move.id"
-- - Missing move.game_id: "NOT NULL constraint failed: move.game_id"
-- - Missing move.move_number: "NOT NULL constraint failed: move.move_number"
-- - Missing move.player_color: "NOT NULL constraint failed: move.player_color"
-- - Missing move.timestamp: "NOT NULL constraint failed: move.timestamp"
-- - Missing game_participant_token.id: "NOT NULL constraint failed: game_participant_token.id"
-- - Missing game_participant_token.game_id: "NOT NULL constraint failed: game_participant_token.game_id"
-- - Missing game_participant_token.token: "NOT NULL constraint failed: game_participant_token.token"
-- - Missing game_participant_token.role: "NOT NULL constraint failed: game_participant_token.role"
-- - Missing game_participant_token.created_at: "NOT NULL constraint failed: game_participant_token.created_at"
-- - Missing game_participant_token.is_active: "NOT NULL constraint failed: game_participant_token.is_active"

-- TRIGGER VIOLATIONS:
-- - Non-sequential move number: "Previous move number must exist" (from enforce_sequential_moves trigger)

-- COMMON APPLICATION-LEVEL VALIDATION ERRORS TO HANDLE:
-- - Same player assigned to both white and black in game
-- - Creating moves for completed games
-- - Creating moves with wrong player_color for the move_number (e.g., white moves on odd numbers)
-- - Invalid ULID format for ID fields
-- - Invalid datetime format for timestamp fields
-- - Attempting to create human_player without corresponding player record
-- - Attempting to create engine_player without corresponding player record
-- - Creating game_participant_token for non-existent games
-- - Token format validation (ensure sufficient entropy/length)

-- COMMON QUERIES FOR APPLICATION FLOW:

-- === PLAYER MANAGEMENT ===

-- Create a human player
-- INSERT INTO player (id, player_type, created_at) VALUES (?, 'human', ?);
-- INSERT INTO human_player (player_id, name, email, player_type) VALUES (?, ?, ?, 'human');

-- Create an engine player
-- INSERT INTO player (id, player_type, created_at) VALUES (?, 'engine', ?);
-- INSERT INTO engine_player (player_id, version, player_type) VALUES (?, ?, 'engine');

-- Get player by ID with details
-- SELECT p.*,
--        h.name as human_name,
--        h.email,
--        e.version as engine_version
-- FROM player p
-- LEFT JOIN human_player h ON p.id = h.player_id
-- LEFT JOIN engine_player e ON p.id = e.player_id
-- WHERE p.id = ?;

-- List all human players
-- SELECT p.id, p.created_at, h.name, h.email
-- FROM player p
-- JOIN human_player h ON p.id = h.player_id
-- ORDER BY h.name;

-- List all engine players
-- SELECT p.id, p.created_at, e.version
-- FROM player p
-- JOIN engine_player e ON p.id = e.player_id
-- ORDER BY e.version;

-- === GAME MANAGEMENT ===

-- Create a new game (registered players)
-- INSERT INTO game (id, name, white_player_id, black_player_id, start_time, game_status, created_at)
-- VALUES (?, ?, ?, ?, ?, 'ongoing', ?);

-- Create a new game (one anonymous player - human vs AI)
-- INSERT INTO game (id, name, white_player_id, black_player_id, start_time, game_status, created_at)
-- VALUES (?, ?, NULL, ?, ?, 'ongoing', ?); -- white is anonymous, black is AI

-- Create a new game (both anonymous players)
-- INSERT INTO game (id, name, white_player_id, black_player_id, start_time, game_status, created_at)
-- VALUES (?, ?, NULL, NULL, ?, 'ongoing', ?);

-- Create participant tokens
-- INSERT INTO game_participant_token (id, game_id, token, role, created_at, is_active)
-- VALUES (?, ?, ?, 'white', ?, true), (?, ?, ?, 'black', ?, true);

-- Get game details with player information (handling anonymous players)
-- SELECT g.*,
--        CASE
--          WHEN g.white_player_id IS NULL THEN 'anonymous'
--          ELSE wp.player_type
--        END as white_type,
--        CASE
--          WHEN g.white_player_id IS NULL THEN 'Anonymous Player'
--          ELSE COALESCE(wh.name, we.version)
--        END as white_name,
--        CASE
--          WHEN g.black_player_id IS NULL THEN 'anonymous'
--          ELSE bp.player_type
--        END as black_type,
--        CASE
--          WHEN g.black_player_id IS NULL THEN 'Anonymous Player'
--          ELSE COALESCE(bh.name, be.version)
--        END as black_name
-- FROM game g
-- LEFT JOIN player wp ON g.white_player_id = wp.id
-- LEFT JOIN player bp ON g.black_player_id = bp.id
-- LEFT JOIN human_player wh ON wp.id = wh.player_id AND wp.player_type = 'human'
-- LEFT JOIN engine_player we ON wp.id = we.player_id AND wp.player_type = 'engine'
-- LEFT JOIN human_player bh ON bp.id = bh.player_id AND bp.player_type = 'human'
-- LEFT JOIN engine_player be ON bp.id = be.player_id AND bp.player_type = 'engine'
-- WHERE g.id = ?;

-- List all games for a registered player
-- SELECT g.*,
--        CASE
--          WHEN g.white_player_id = ? THEN 'white'
--          WHEN g.black_player_id = ? THEN 'black'
--        END as player_color,
--        CASE
--          WHEN g.white_player_id = ? AND g.black_player_id IS NULL THEN 'Anonymous Player'
--          WHEN g.black_player_id = ? AND g.white_player_id IS NULL THEN 'Anonymous Player'
--          WHEN g.white_player_id = ? THEN COALESCE(bh.name, be.version)
--          WHEN g.black_player_id = ? THEN COALESCE(wh.name, we.version)
--        END as opponent_name
-- FROM game g
-- LEFT JOIN player wp ON g.white_player_id = wp.id
-- LEFT JOIN player bp ON g.black_player_id = bp.id
-- LEFT JOIN human_player wh ON wp.id = wh.player_id AND wp.player_type = 'human'
-- LEFT JOIN engine_player we ON wp.id = we.player_id AND wp.player_type = 'engine'
-- LEFT JOIN human_player bh ON bp.id = bh.player_id AND bp.player_type = 'human'
-- LEFT JOIN engine_player be ON bp.id = be.player_id AND bp.player_type = 'engine'
-- WHERE g.white_player_id = ? OR g.black_player_id = ?
-- ORDER BY g.created_at DESC;

-- List ongoing games with player types
-- SELECT g.*,
--        CASE
--          WHEN g.white_player_id IS NULL THEN 'Anonymous'
--          ELSE COALESCE(wh.name, we.version)
--        END as white_name,
--        CASE
--          WHEN g.black_player_id IS NULL THEN 'Anonymous'
--          ELSE COALESCE(bh.name, be.version)
--        END as black_name
-- FROM game g
-- LEFT JOIN player wp ON g.white_player_id = wp.id
-- LEFT JOIN player bp ON g.black_player_id = bp.id
-- LEFT JOIN human_player wh ON wp.id = wh.player_id
-- LEFT JOIN engine_player we ON wp.id = we.player_id
-- LEFT JOIN human_player bh ON bp.id = bh.player_id
-- LEFT JOIN engine_player be ON bp.id = be.player_id
-- WHERE g.game_status = 'ongoing'
-- ORDER BY g.start_time DESC;

-- Update game status (end game)
-- UPDATE game
-- SET game_status = ?, end_time = ?
-- WHERE id = ?;

-- === MOVE MANAGEMENT ===

-- Add a move to a game
-- INSERT INTO move (id, game_id, move_number, player_color, from_position, to_position,
--                  black_lower, black_upper, white_lower, white_upper, king, timestamp)
-- VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);

-- Get all moves for a game (chronological order)
-- SELECT * FROM move
-- WHERE game_id = ?
-- ORDER BY move_number;

-- Get latest move for a game
-- SELECT * FROM move
-- WHERE game_id = ?
-- ORDER BY move_number DESC
-- LIMIT 1;

-- Get move count for a game
-- SELECT COUNT(*) as move_count
-- FROM move
-- WHERE game_id = ?;

-- Get game state at specific move number
-- SELECT * FROM move
-- WHERE game_id = ? AND move_number <= ?
-- ORDER BY move_number;

-- === REMOTE ACCESS / TOKEN MANAGEMENT ===

-- Validate participant token access
-- SELECT gpt.role, gpt.is_active, g.game_status
-- FROM game_participant_token gpt
-- JOIN game g ON gpt.game_id = g.id
-- WHERE gpt.token = ? AND g.id = ? AND gpt.is_active = true;

-- Get game by participant token
-- SELECT g.*, gpt.role as player_role
-- FROM game g
-- JOIN game_participant_token gpt ON g.id = gpt.game_id
-- WHERE gpt.token = ? AND gpt.is_active = true;

-- List all tokens for a game (admin/debug)
-- SELECT * FROM game_participant_token
-- WHERE game_id = ?
-- ORDER BY role;

-- Deactivate participant token
-- UPDATE game_participant_token
-- SET is_active = false
-- WHERE token = ?;

-- === GAME HISTORY AND STATISTICS ===

-- Player statistics (registered players only)
-- SELECT
--   COUNT(*) as total_games,
--   COUNT(CASE WHEN g.game_status LIKE 'white_won%' AND g.white_player_id = ? THEN 1
--              WHEN g.game_status LIKE 'black_won%' AND g.black_player_id = ? THEN 1 END) as wins,
--   COUNT(CASE WHEN g.game_status = 'draw' THEN 1 END) as draws,
--   COUNT(CASE WHEN g.game_status = 'ongoing' THEN 1 END) as ongoing_games
-- FROM game g
-- WHERE g.white_player_id = ? OR g.black_player_id = ?;

-- Recent completed games
-- SELECT g.*,
--        CASE
--          WHEN g.white_player_id IS NULL THEN 'Anonymous'
--          ELSE COALESCE(wh.name, we.version)
--        END as white_name,
--        CASE
--          WHEN g.black_player_id IS NULL THEN 'Anonymous'
--          ELSE COALESCE(bh.name, be.version)
--        END as black_name,
--        COUNT(m.id) as total_moves
-- FROM game g
-- LEFT JOIN player wp ON g.white_player_id = wp.id
-- LEFT JOIN player bp ON g.black_player_id = bp.id
-- LEFT JOIN human_player wh ON wp.id = wh.player_id
-- LEFT JOIN engine_player we ON wp.id = we.player_id
-- LEFT JOIN human_player bh ON bp.id = bh.player_id
-- LEFT JOIN engine_player be ON bp.id = be.player_id
-- LEFT JOIN move m ON g.id = m.game_id
-- WHERE g.game_status != 'ongoing' AND g.end_time IS NOT NULL
-- GROUP BY g.id
-- ORDER BY g.end_time DESC
-- LIMIT 10;

-- === MAINTENANCE QUERIES ===

-- Find games without any moves (potential cleanup)
-- SELECT g.* FROM game g
-- LEFT JOIN move m ON g.id = m.game_id
-- WHERE m.game_id IS NULL
-- AND g.created_at < datetime('now', '-24 hours');

-- Find inactive remote games (no recent moves)
-- SELECT g.*, MAX(m.timestamp) as last_move_time
-- FROM game g
-- JOIN game_participant_token gpt ON g.id = gpt.game_id
-- LEFT JOIN move m ON g.id = m.game_id
-- WHERE g.game_status = 'ongoing'
-- GROUP BY g.id
-- HAVING last_move_time < datetime('now', '-7 days') OR last_move_time IS NULL;

-- Count games by player type combinations
-- SELECT
--   COUNT(CASE WHEN white_player_id IS NOT NULL AND black_player_id IS NOT NULL THEN 1 END) as registered_vs_registered,
--   COUNT(CASE WHEN white_player_id IS NULL AND black_player_id IS NOT NULL THEN 1 END) as anonymous_vs_registered,
--   COUNT(CASE WHEN white_player_id IS NOT NULL AND black_player_id IS NULL THEN 1 END) as registered_vs_anonymous,
--   COUNT(CASE WHEN white_player_id IS NULL AND black_player_id IS NULL THEN 1 END) as anonymous_vs_anonymous
-- FROM game;

-- POTENTIAL TEST CASES:

-- === PLAYER CREATION ===
-- Test creating human player with all fields
-- Test creating human player without email (optional field)
-- Test creating engine player with version
-- Test creating engine player without version (optional field)
-- Test duplicate player ID rejection
-- Test invalid player_type rejection ('invalid_type')
-- Test missing required fields (name for human, player_type for both)
-- Test creating player with invalid ULID format
-- Test creating player with NULL datetime

-- === PLAYER SUBTYPE INTEGRITY ===
-- Test creating human_player without corresponding player record
-- Test creating human_player with engine player_type (should fail)
-- Test creating engine_player with human player_type (should fail)
-- Test deleting player cascades to human_player/engine_player
-- Test creating human_player with non-existent player_id
-- Test player_type mismatch between player and human_player tables

-- === GAME CREATION ===
-- Test creating game with two registered players
-- Test creating game with one anonymous player (NULL white_player_id)
-- Test creating game with both anonymous players (both NULL)
-- Test creating game with invalid white_player_id (non-existent)
-- Test creating game with invalid black_player_id (non-existent)
-- Test creating game with same player as both white and black
-- Test creating game with invalid game_status
-- Test creating game with duplicate game ID
-- Test creating game with invalid ULID format
-- Test creating game with NULL required fields (name, start_time, created_at)

-- === MOVE CREATION ===
-- Test adding first move (move_number = 0)
-- Test adding sequential moves (0, 1, 2...)
-- Test adding non-sequential move (0, 2) should fail trigger
-- Test duplicate move number in same game should fail
-- Test move with invalid player_color
-- Test move with invalid king position (< 0 or > 120)
-- Test move with negative move_number should fail
-- Test move referencing non-existent game
-- Test move with duplicate move ID
-- Test move with NULL required fields

-- === TOKEN MANAGEMENT ===
-- Test creating participant token for existing game
-- Test creating participant token for non-existent game
-- Test creating participant token with duplicate token value
-- Test creating participant token with invalid role
-- Test token validation with valid active token
-- Test token validation with inactive token (is_active = false)
-- Test token validation with non-existent token
-- Test deactivating existing token
-- Test creating multiple tokens for same game/role combination

-- === ANONYMOUS PLAYER SCENARIOS ===
-- Test querying game details with anonymous white player
-- Test querying game details with anonymous black player
-- Test querying game details with both anonymous players
-- Test player statistics query excludes anonymous players
-- Test game listing shows "Anonymous" for NULL player_ids
-- Test move creation works with anonymous player games

-- === COMPLEX QUERIES ===
-- Test JSON aggregation query with game containing no moves
-- Test JSON aggregation query with game containing multiple moves
-- Test JSON aggregation query with anonymous players
-- Test player statistics with mixed win/loss/draw scenarios
-- Test game listing for player with no games
-- Test game listing for player with only anonymous opponent games
-- Test recent completed games query with various end states

-- === EDGE CASES ===
-- Test game with moves but NULL end_time
-- Test game with end_time but status still 'ongoing'
-- Test very long game names (boundary testing)
-- Test games created with future timestamps
-- Test moves with very large position values
-- Test token expiration scenarios
-- Test concurrent token creation (race conditions)
-- Test games with thousands of moves (performance)

-- === DATA INTEGRITY ===
-- Test foreign key constraint enforcement on deletion
-- Test cascade deletion from game to moves
-- Test cascade deletion from game to participant_tokens
-- Test preventing deletion of player referenced by games
-- Test composite foreign key enforcement in player subtypes
-- Test check constraints with boundary values
-- Test unique constraint violations across different scenarios
