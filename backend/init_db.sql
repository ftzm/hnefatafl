CREATE TABLE game (
  id UUID NOT NULL UNIQUE,
  board TEXT NOT NULL,
  is_black_turn BOOLEAN NOT NULL,
  PRIMARY KEY (Id)
);

-- table for persisted local games (hotseat)
CREATE TABLE hotseat (
  id UUID NOT NULL UNIQUE,
  PRIMARY KEY (id),
  FOREIGN KEY (id) REFERENCES game(id)
);

-- table for vs games online
CREATE TABLE vs (
  game_id UUID NOT NULL UNIQUE,
  white_id UUID NOT NULL UNIQUE,
  black_id UUID NOT NULL UNIQUE,
  FOREIGN KEY (game_id) REFERENCES game(id),
  PRIMARY KEY (white_id, black_id)
);

-- table for against ai games
CREATE TABLE ai (
  id UUID NOT NULL UNIQUE,
  human_is_black BOOLEAN NOT NULL,
  PRIMARY KEY (id),
  FOREIGN KEY (id) REFERENCES game(id)
);

-- CREATE TABLE game_interface (
--   id UUID NOT NULL,
--   hotseat_id UUID,
--   vs_id UUID,
--   ai_id UUID,
--   FOREIGN KEY (id) REFERENCES game(id),
--   PRIMARY KEY (id),
--   FOREIGN KEY (hotseat_id) REFERENCES hotseat(id),
--   FOREIGN KEY (vs_id) REFERENCES vs(id),
--   FOREIGN KEY (ai_id) REFERENCES ai(id)
-- );
