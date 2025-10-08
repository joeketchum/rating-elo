@echo off
echo Updating players in Supabase database...

REM Update players using Supabase REST API
curl -X PATCH "https://rkuftaxaycdgirtjgfhz.supabase.co/rest/v1/players?id=eq.1" ^
  -H "apikey: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InJrdWZ0YXhheWNkZ2lydGpnZmh6Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3MjgzNDE1ODcsImV4cCI6MjA0MzkxNzU4N30.4BOyU-DZozEKdOzcQ4QwmKGZq0TmQEh4OX8q_7wwdRY" ^
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InJrdWZ0YXhheWNkZ2lydGpnZmh6Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3MjgzNDE1ODcsImV4cCI6MjA0MzkxNzU4N30.4BOyU-DZozEKdOzcQ4QwmKGZq0TmQEh4OX8q_7wwdRY" ^
  -H "Content-Type: application/json" ^
  -d "{\"rating\": 1100, \"matches_played\": 0, \"plays_am\": false, \"plays_pm\": true}"

curl -X PATCH "https://rkuftaxaycdgirtjgfhz.supabase.co/rest/v1/players?id=eq.26" ^
  -H "apikey: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InJrdWZ0YXhheWNkZ2lydGpnZmh6Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3MjgzNDE1ODcsImV4cCI6MjA0MzkxNzU4N30.4BOyU-DZozEKdOzcQ4QwmKGZq0TmQEh4OX8q_7wwdRY" ^
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InJrdWZ0YXhheWNkZ2lydGpnZmh6Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3MjgzNDE1ODcsImV4cCI6MjA0MzkxNzU4N30.4BOyU-DZozEKdOzcQ4QwmKGZq0TmQEh4OX8q_7wwdRY" ^
  -H "Content-Type: application/json" ^
  -d "{\"rating\": 1050, \"matches_played\": 0, \"plays_am\": true, \"plays_pm\": false}"

curl -X PATCH "https://rkuftaxaycdgirtjgfhz.supabase.co/rest/v1/players?id=eq.102" ^
  -H "apikey: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InJrdWZ0YXhheWNkZ2lydGpnZmh6Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3MjgzNDE1ODcsImV4cCI6MjA0MzkxNzU4N30.4BOyU-DZozEKdOzcQ4QwmKGZq0TmQEh4OX8q_7wwdRY" ^
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InJrdWZ0YXhheWNkZ2lydGpnZmh6Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3MjgzNDE1ODcsImV4cCI6MjA0MzkxNzU4N30.4BOyU-DZozEKdOzcQ4QwmKGZq0TmQEh4OX8q_7wwdRY" ^
  -H "Content-Type: application/json" ^
  -d "{\"rating\": 1000, \"matches_played\": 0, \"plays_am\": true, \"plays_pm\": true}"

echo First 3 players updated. You can continue with more players...
echo Or better yet, run this in your Supabase SQL Editor:
echo.
echo UPDATE players SET rating = 1100, matches_played = 0, plays_am = false, plays_pm = true WHERE id = 1;
echo UPDATE players SET rating = 1050, matches_played = 0, plays_am = true, plays_pm = false WHERE id = 26;
echo ...and so on for all players