CREATE TABLE game (
  id UUID NOT NULL,
  board TEXT NOT NULL,
  PRIMARY KEY (Id)
);

-- table for persisted local games (hotseat)
CREATE TABLE hotseat (
  id UUID NOT NULL,
  PRIMARY KEY (id)
);

-- table for vs games online
CREATE TABLE vs (
  id UUID NOT NULL,
  PRIMARY KEY (id)
);

-- table for against ai games
CREATE TABLE ai (
  id UUID NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE game_interface (
  id UUID NOT NULL,
  hotseat_id UUID,
  vs_id UUID,
  ai_id UUID,
  FOREIGN KEY (id) REFERENCES game(id),
  PRIMARY KEY (id),
  FOREIGN KEY (hotseat_id) REFERENCES hotseat(id),
  FOREIGN KEY (vs_id) REFERENCES vs(id),
  FOREIGN KEY (ai_id) REFERENCES ai(id)
);
