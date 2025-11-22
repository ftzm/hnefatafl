CREATE TABLE IF NOT EXISTS "schema_migrations" (version varchar(128) primary key);
CREATE TABLE player (
    id TEXT PRIMARY KEY,
    player_type TEXT NOT NULL CHECK (player_type IN ('human', 'engine')),
    created_at DATETIME NOT NULL
);
CREATE TABLE human_player (
    player_id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    email TEXT,
    player_type TEXT NOT NULL DEFAULT 'human' CHECK (player_type = 'human'),
    FOREIGN KEY (player_id, player_type) REFERENCES player(id, player_type) ON DELETE CASCADE
);
CREATE TABLE engine_player (
    player_id TEXT PRIMARY KEY,
    version TEXT NOT NULL,
    player_type TEXT NOT NULL DEFAULT 'engine' CHECK (player_type = 'engine'),
    FOREIGN KEY (player_id, player_type) REFERENCES player(id, player_type) ON DELETE CASCADE
);
CREATE TABLE game (
    id TEXT PRIMARY KEY,
    name TEXT UNIQUE,
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
    FOREIGN KEY (black_player_id) REFERENCES player(id)
);
CREATE TABLE move (
    id TEXT PRIMARY KEY,
    game_id TEXT NOT NULL,
    move_number INTEGER NOT NULL CHECK (move_number >= 0),
    player_color TEXT NOT NULL CHECK (player_color IN ('white', 'black')),
    from_position INTEGER NOT NULL,
    to_position INTEGER NOT NULL,
    black_lower INTEGER NOT NULL,
    black_upper INTEGER NOT NULL,
    white_lower INTEGER NOT NULL,
    white_upper INTEGER NOT NULL,
    king INTEGER NOT NULL CHECK (king >= 0 AND king <= 120),
    timestamp DATETIME NOT NULL,
    FOREIGN KEY (game_id) REFERENCES game(id) ON DELETE CASCADE,
    UNIQUE (game_id, move_number)
);
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
CREATE TABLE game_participant_token (
    id TEXT PRIMARY KEY,
    game_id TEXT NOT NULL,
    token TEXT NOT NULL UNIQUE,
    role TEXT NOT NULL CHECK (role IN ('white', 'black')),
    created_at DATETIME NOT NULL,
    is_active BOOLEAN NOT NULL,
    FOREIGN KEY (game_id) REFERENCES game(id) ON DELETE CASCADE
);
CREATE INDEX idx_games_players ON game(white_player_id, black_player_id);
CREATE INDEX idx_moves_game ON move(game_id, move_number);
CREATE INDEX idx_players_type ON player(player_type);
CREATE INDEX idx_game_participant_tokens_game ON game_participant_token(game_id);
CREATE INDEX idx_game_participant_tokens_token ON game_participant_token(token);
CREATE UNIQUE INDEX idx_unique_active_game_role ON game_participant_token(game_id, role) WHERE is_active = true;
-- Dbmate schema migrations
INSERT INTO "schema_migrations" (version) VALUES
  ('20251115123657');
