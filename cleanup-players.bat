@echo off
echo Cleaning up Supabase players with invalid IDs...

REM Delete players with hash-based IDs (anything not in range 1-103)
echo Deleting players with ID greater than 103...
curl -X DELETE ^
  "https://rkuftaxaycdgirtjgfhz.supabase.co/rest/v1/players?id=gt.103" ^
  -H "apikey: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InJrdWZ0YXhheWNkZ2lydGpnZmh6Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3MjgzNDE1ODcsImV4cCI6MjA0MzkxNzU4N30.4BOyU-DZozEKdOzcQ4QwmKGZq0TmQEh4OX8q_7wwdRY" ^
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InJrdWZ0YXhheWNkZ2lydGpnZmh6Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3MjgzNDE1ODcsImV4cCI6MjA0MzkxNzU4N30.4BOyU-DZozEKdOzcQ4QwmKGZq0TmQEh4OX8q_7wwdRY" ^
  -H "Content-Type: application/json"

echo.
echo Deleting players with ID less than 1...
curl -X DELETE ^
  "https://rkuftaxaycdgirtjgfhz.supabase.co/rest/v1/players?id=lt.1" ^
  -H "apikey: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InJrdWZ0YXhheWNkZ2lydGpnZmh6Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3MjgzNDE1ODcsImV4cCI6MjA0MzkxNzU4N30.4BOyU-DZozEKdOzcQ4QwmKGZq0TmQEh4OX8q_7wwdRY" ^
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InJrdWZ0YXhheWNkZ2lydGpnZmh6Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3MjgzNDE1ODcsImV4cCI6MjA0MzkxNzU4N30.4BOyU-DZozEKdOzcQ4QwmKGZq0TmQEh4OX8q_7wwdRY" ^
  -H "Content-Type: application/json"

echo.
echo Checking remaining players...
curl -X GET ^
  "https://rkuftaxaycdgirtjgfhz.supabase.co/rest/v1/players?select=id,name&order=id.asc" ^
  -H "apikey: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InJrdWZ0YXhheWNkZ2lydGpnZmh6Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3MjgzNDE1ODcsImV4cCI6MjA0MzkxNzU4N0.4BOyU-DZozEKdOzcQ4QwmKGZq0TmQEh4OX8q_7wwdRY" ^
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InJrdWZ0YXhheWNkZ2lydGpnZmh6Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3MjgzNDE1ODcsImV4cCI6MjA0MzkxNzU4N30.4BOyU-DZozEKdOzcQ4QwmKGZq0TmQEh4OX8q_7wwdRY"

echo.
echo Cleanup complete!