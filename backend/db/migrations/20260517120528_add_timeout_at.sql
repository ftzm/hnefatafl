-- migrate:up
ALTER TABLE online_game ADD COLUMN timeout_at TEXT;
CREATE INDEX idx_online_game_timeout_at ON online_game(timeout_at)
  WHERE timeout_at IS NOT NULL;

-- migrate:down
DROP INDEX IF EXISTS idx_online_game_timeout_at;
ALTER TABLE online_game DROP COLUMN timeout_at;
