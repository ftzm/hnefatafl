-- Select 100 different games, with 4 games at each depth (1-25 moves)
-- Deterministic selection - same results every time

WITH player_stats AS (
  SELECT
    p.player_id,
    COUNT(*) as total_games,
    SUM(CASE
      WHEN (g.white_player_id = p.player_id AND g.game_status LIKE 'white_won_%')
           OR (g.black_player_id = p.player_id AND g.game_status LIKE 'black_won_%')
      THEN 1 ELSE 0
    END) as total_wins
  FROM human_player p
  JOIN game g ON (g.white_player_id = p.player_id OR g.black_player_id = p.player_id)
  WHERE g.game_status != 'ongoing'
    AND g.white_player_id IS NOT NULL
    AND g.black_player_id IS NOT NULL
  GROUP BY p.player_id
  HAVING total_games >= 10
),

top_players AS (
  SELECT player_id
  FROM player_stats
  ORDER BY (100.0 * total_wins / total_games) DESC, total_games DESC
  LIMIT 50
),

high_quality_games AS (
  SELECT g.id, g.game_status
  FROM game g
  JOIN top_players wp ON g.white_player_id = wp.player_id
  JOIN top_players bp ON g.black_player_id = bp.player_id
  WHERE g.game_status NOT IN ('ongoing', 'draw', 'abandoned')
),

long_games AS (
  SELECT hqg.id, g.name, g.game_status
  FROM high_quality_games hqg
  JOIN game g ON g.id = hqg.id
  WHERE (SELECT COUNT(*) FROM move m WHERE m.game_id = hqg.id) >= 30
),

-- Deterministically rank all long games
ranked_games AS (
  SELECT
    *,
    ROW_NUMBER() OVER (
      ORDER BY
        ABS((CAST(SUBSTR(id, -8) AS INTEGER) * 2654435761) % 1000000007),
        id
    ) as game_rank
  FROM long_games
  LIMIT 100  -- Take top 100 games
),

-- Assign each game to a depth (1-25)
games_with_depth AS (
  SELECT
    *,
    ((game_rank - 1) / 4) + 1 as target_depth  -- Games 1-4 -> depth 1, Games 5-8 -> depth 2, etc.
  FROM ranked_games
),

-- Convert moves to notation
game_moves AS (
  SELECT
    gwd.id as game_id,
    gwd.name as game_name,
    gwd.game_status,
    gwd.target_depth,
    m.move_number,
    -- Convert to notation using exact Haskell logic
    CASE (m.from_position % 11)
      WHEN 0 THEN 'k' WHEN 1 THEN 'j' WHEN 2 THEN 'i' WHEN 3 THEN 'h' WHEN 4 THEN 'g'
      WHEN 5 THEN 'f' WHEN 6 THEN 'e' WHEN 7 THEN 'd' WHEN 8 THEN 'c' WHEN 9 THEN 'b'
      ELSE 'a'
    END || (m.from_position / 11 + 1) ||
    CASE (m.to_position % 11)
      WHEN 0 THEN 'k' WHEN 1 THEN 'j' WHEN 2 THEN 'i' WHEN 3 THEN 'h' WHEN 4 THEN 'g'
      WHEN 5 THEN 'f' WHEN 6 THEN 'e' WHEN 7 THEN 'd' WHEN 8 THEN 'c' WHEN 9 THEN 'b'
      ELSE 'a'
    END || (m.to_position / 11 + 1) as move_notation
  FROM games_with_depth gwd
  JOIN move m ON m.game_id = gwd.id
  WHERE m.move_number <= gwd.target_depth
)

-- Generate final move sequences
SELECT
  game_id,
  game_name,
  game_status,
  target_depth,
  GROUP_CONCAT(move_notation, ' ') as move_sequence
FROM game_moves
GROUP BY game_id, game_name, game_status, target_depth
ORDER BY target_depth, game_id;