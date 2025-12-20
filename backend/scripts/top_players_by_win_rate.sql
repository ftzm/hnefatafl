-- Find top 50 human players with the highest win percentage who have played over 20 games
WITH human_player_stats AS (
  SELECT
    p.id,
    hp.name,
    COUNT(*) as total_games,
    SUM(CASE
      WHEN (g.white_player_id = p.id AND g.game_status IN (
        'white_won_king_escaped',
        'white_won_exit_fort',
        'white_won_no_black_moves',
        'white_won_resignation',
        'white_won_timeout'
      ))
      OR (g.black_player_id = p.id AND g.game_status IN (
        'black_won_king_captured',
        'black_won_white_surrounded',
        'black_won_no_white_moves',
        'black_won_resignation',
        'black_won_timeout'
      ))
      THEN 1
      ELSE 0
    END) as wins
  FROM player p
  INNER JOIN human_player hp ON p.id = hp.player_id
  INNER JOIN game g ON (g.white_player_id = p.id OR g.black_player_id = p.id)
  WHERE g.game_status IS NOT NULL
    AND g.game_status != 'ongoing'
    AND g.game_status != 'abandoned'  -- Only count completed games
  GROUP BY p.id, hp.name
  HAVING COUNT(*) > 20
)
SELECT
  name,
  total_games,
  wins,
  ROUND((wins * 100.0 / total_games), 2) as win_percentage
FROM human_player_stats
ORDER BY win_percentage DESC, total_games DESC
LIMIT 50;