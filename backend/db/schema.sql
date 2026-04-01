CREATE TABLE IF NOT EXISTS "schema_migrations" (version varchar(128) primary key);
CREATE TABLE player (
    id TEXT PRIMARY KEY,
    player_type TEXT NOT NULL CHECK (player_type IN ('human', 'engine')),
    created_at DATETIME NOT NULL
);
CREATE TABLE human_player (
    player_id TEXT PRIMARY KEY,
    name TEXT NOT NULL UNIQUE,
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
CREATE TABLE move (
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
    captures_lower INTEGER NOT NULL,
    captures_upper INTEGER NOT NULL,
    timestamp DATETIME NOT NULL,
    PRIMARY KEY (game_id, move_number),
    FOREIGN KEY (game_id) REFERENCES game(id) ON DELETE CASCADE
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
CREATE INDEX idx_players_type ON player(player_type);
CREATE INDEX idx_game_participant_tokens_game ON game_participant_token(game_id);
CREATE INDEX idx_game_participant_tokens_token ON game_participant_token(token);
CREATE UNIQUE INDEX idx_unique_active_game_role ON game_participant_token(game_id, role) WHERE is_active = true;
CREATE TABLE pending_game_action (
    game_id TEXT PRIMARY KEY,
    action_type TEXT NOT NULL CHECK (action_type IN ('draw_offer', 'undo_request')),
    offered_by TEXT NOT NULL CHECK (offered_by IN ('white', 'black')),
    created_at DATETIME NOT NULL,
    FOREIGN KEY (game_id) REFERENCES game(id) ON DELETE CASCADE
);
CREATE TABLE IF NOT EXISTS "game" (
    id TEXT PRIMARY KEY,
    name TEXT UNIQUE,
    game_type TEXT NOT NULL CHECK (game_type IN ('hotseat', 'ai', 'online')),
    start_time DATETIME NOT NULL,
    end_time DATETIME,
    game_status TEXT,
    created_at DATETIME NOT NULL
);
CREATE UNIQUE INDEX idx_game_id_type ON game(id, game_type);
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
-- Dbmate schema migrations
INSERT INTO "schema_migrations" (version) VALUES
  ('20251115123657'),
  ('20260330183402'),
  ('20260401080529');
