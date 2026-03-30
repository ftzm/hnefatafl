-- migrate:up

CREATE TABLE pending_game_action (
    game_id TEXT PRIMARY KEY,
    action_type TEXT NOT NULL CHECK (action_type IN ('draw_offer', 'undo_request')),
    offered_by TEXT NOT NULL CHECK (offered_by IN ('white', 'black')),
    created_at DATETIME NOT NULL,
    FOREIGN KEY (game_id) REFERENCES game(id) ON DELETE CASCADE
);

-- migrate:down

DROP TABLE IF EXISTS pending_game_action;
