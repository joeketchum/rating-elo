module Config exposing (supabaseConfig)

{-| Configuration for Supabase connection
-}

import Supabase


{-| Supabase configuration
Replace these with your actual Supabase project values:
1. Go to your Supabase project dashboard
2. Click "Settings" > "API"
3. Copy the "Project URL" and "anon public" key
-}
supabaseConfig : Supabase.Config
supabaseConfig =
    { url = "https://qzvqbnaixpevuisqaxky.supabase.co"
    , anonKey = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InF6dnFibmFpeHBldnVpc3FheGt5Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3NTk4NjU2NTMsImV4cCI6MjA3NTQ0MTY1M30.ZSX2dCWhTQQCG2v6aCI7gojUUOXaLrobP_VVQ-HDZjM"
    }


-- INSTRUCTIONS:
-- 1. Replace "your-project-id" with your actual Supabase project ID
-- 2. Replace "your-anon-key-here" with your actual anon/public key from Supabase
-- 
-- To find these values:
-- 1. Go to https://supabase.com/dashboard
-- 2. Select your project
-- 3. Go to Settings > API
-- 4. Copy "Project URL" and "anon public" key