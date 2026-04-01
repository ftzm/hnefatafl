-- migrate:up

PRAGMA foreign_keys = OFF;

-- Rebuild game table: drop player columns, add game_type
CREATE TABLE game_new (
    id TEXT PRIMARY KEY,
    name TEXT UNIQUE,
    game_type TEXT NOT NULL CHECK (game_type IN ('hotseat', 'ai', 'online')),
    start_time DATETIME NOT NULL,
    end_time DATETIME,
    game_status TEXT,
    created_at DATETIME NOT NULL
);

INSERT INTO game_new (id, name, game_type, start_time, end_time, game_status, created_at)
SELECT id, name, 'online', start_time, end_time, game_status, created_at FROM game;

DROP TABLE game;
ALTER TABLE game_new RENAME TO game;

CREATE UNIQUE INDEX idx_game_id_type ON game(id, game_type);

-- Variant tables
CREATE TABLE hotseat_game (
    game_id TEXT PRIMARY KEY,
    owner_id TEXT,
    game_type TEXT NOT NULL DEFAULT 'hotseat' CHECK (game_type = 'hotseat'),
    FOREIGN KEY (game_id, game_type) REFERENCES game(id, game_type) ON DELETE CASCADE,
    FOREIGN KEY (owner_id) REFERENCES player(id)
);

CREATE TABLE ai_game (
    game_id TEXT PRIMARY KEY,
    player_id TEXT,
    player_color TEXT NOT NULL CHECK (player_color IN ('white', 'black')),
    engine_id TEXT NOT NULL,
    game_type TEXT NOT NULL DEFAULT 'ai' CHECK (game_type = 'ai'),
    FOREIGN KEY (game_id, game_type) REFERENCES game(id, game_type) ON DELETE CASCADE,
    FOREIGN KEY (player_id) REFERENCES player(id),
    FOREIGN KEY (engine_id) REFERENCES player(id)
);

CREATE TABLE online_game (
    game_id TEXT PRIMARY KEY,
    white_player_id TEXT,
    white_name TEXT,
    black_player_id TEXT,
    black_name TEXT,
    game_type TEXT NOT NULL DEFAULT 'online' CHECK (game_type = 'online'),
    FOREIGN KEY (game_id, game_type) REFERENCES game(id, game_type) ON DELETE CASCADE,
    FOREIGN KEY (white_player_id) REFERENCES player(id),
    FOREIGN KEY (black_player_id) REFERENCES player(id)
);

-- Migrate existing games as online
INSERT INTO online_game (game_id, white_player_id, black_player_id)
SELECT id, NULL, NULL FROM game;

PRAGMA foreign_keys = ON;

-- migrate:down

DROP TABLE IF EXISTS online_game;
DROP TABLE IF EXISTS ai_game;
DROP TABLE IF EXISTS hotseat_game;
DROP INDEX IF EXISTS idx_game_id_type;

PRAGMA foreign_keys = OFF;

CREATE TABLE game_old (
    id TEXT PRIMARY KEY,
    name TEXT UNIQUE,
    white_player_id TEXT,
    black_player_id TEXT,
    start_time DATETIME NOT NULL,
    end_time DATETIME,
    game_status TEXT,
    created_at DATETIME NOT NULL,
    FOREIGN KEY (white_player_id) REFERENCES player(id),
    FOREIGN KEY (black_player_id) REFERENCES player(id)
);

INSERT INTO game_old (id, name, white_player_id, black_player_id, start_time, end_time, game_status, created_at)
SELECT id, name, NULL, NULL, start_time, end_time, game_status, created_at FROM game;

DROP TABLE game;
ALTER TABLE game_old RENAME TO game;

PRAGMA foreign_keys = ON;
