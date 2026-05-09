-- migrate:up

ALTER TABLE online_game ADD COLUMN initial_seconds INTEGER;
ALTER TABLE online_game ADD COLUMN increment_seconds INTEGER;

-- migrate:down

ALTER TABLE online_game DROP COLUMN initial_seconds;
ALTER TABLE online_game DROP COLUMN increment_seconds;
