-- migrate:up

ALTER TABLE online_game ADD COLUMN initial_seconds INTEGER CHECK (initial_seconds > 0);
ALTER TABLE online_game ADD COLUMN increment_seconds INTEGER CHECK (increment_seconds >= 0);

-- migrate:down

ALTER TABLE online_game DROP COLUMN initial_seconds;
ALTER TABLE online_game DROP COLUMN increment_seconds;
