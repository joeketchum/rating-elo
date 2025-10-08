-- Migration script to import existing player data
-- Replace the INSERT statements below with your actual player data

-- Example: Insert your existing players
-- (You'll need to modify this with your actual data from the JSON)

INSERT INTO players (id, name, rating, matches_played, plays_am, plays_pm) VALUES
(1, 'Bryce Lapping', 2213, 112, true, true),
(2, 'Zach Gutterman', 2242, 113, true, true),
(3, 'Craig Leduc', 2263, 123, true, false),
(4, 'Erik Zilli', 2163, 113, true, true),
(5, 'Omar Khatib', 2077, 113, true, true),
-- Add all your other players here...
(50, 'Neil Jy Wen', 450, 107, true, true)
ON CONFLICT (name) DO UPDATE SET
  rating = EXCLUDED.rating,
  matches_played = EXCLUDED.matches_played,
  plays_am = EXCLUDED.plays_am,
  plays_pm = EXCLUDED.plays_pm;

-- Reset the ID sequence to continue from the highest ID
SELECT setval('players_id_seq', (SELECT MAX(id) FROM players));