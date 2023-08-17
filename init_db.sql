CREATE TABLE game (
  id UUID NOT NULL,
  board TEXT NOT NULL,
  is_black_turn BOOLEAN NOT NULL,
  PRIMARY KEY (Id)
);

-- table for persisted local games (hotseat)
CREATE TABLE hotseat (
  id UUID NOT NULL,
  PRIMARY KEY (id),
  FOREIGN KEY (id) REFERENCES game(id)
);

-- table for vs games online
CREATE TABLE vs (
  id UUID NOT NULL,
  black_id UUID NOT NULL,
  white_id UUID NOT NULL,
  PRIMARY KEY (id),
  FOREIGN KEY (id) REFERENCES game(id)
);

-- table for against ai games
CREATE TABLE ai (
  id UUID NOT NULL,
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
