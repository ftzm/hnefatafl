-- migrate:up

ALTER TABLE online_game ADD COLUMN white_remaining_ns INTEGER CHECK (white_remaining_ns >= 0);
ALTER TABLE online_game ADD COLUMN black_remaining_ns INTEGER CHECK (black_remaining_ns >= 0);
ALTER TABLE online_game ADD COLUMN turn_started_at TEXT;

-- migrate:down

ALTER TABLE online_game DROP COLUMN white_remaining_ns;
ALTER TABLE online_game DROP COLUMN black_remaining_ns;
ALTER TABLE online_game DROP COLUMN turn_started_at;
