-- Count of games by status
SELECT
    game_status,
    COUNT(*) as count
FROM game
GROUP BY game_status
ORDER BY count DESC;