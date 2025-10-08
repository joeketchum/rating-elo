# Deploy Edge Function to Supabase

## Prerequisites
1. Install Supabase CLI: `npm install -g supabase`
2. Login to Supabase: `supabase login`
3. Link your project: `supabase link --project-ref YOUR_PROJECT_ID`

## Deploy the Edge Function
```bash
supabase functions deploy vote --no-verify-jwt
```

## Set Environment Variables (if needed)
The function uses these environment variables which should be automatically available:
- `SUPABASE_URL` - Your Supabase project URL
- `SUPABASE_SERVICE_ROLE_KEY` - Service role key for admin operations

## Test the Edge Function
```bash
curl -X POST 'https://YOUR_PROJECT_ID.supabase.co/functions/v1/vote' \
  -H 'Authorization: Bearer YOUR_ANON_KEY' \
  -H 'Content-Type: application/json' \
  -d '{"a_id": 1, "b_id": 2, "winner": 1}'
```

## Function Features
- ✅ Proper CORS headers for GitHub Pages
- ✅ Elo rating calculation
- ✅ Player updates (rating, matches_played)
- ✅ Match recording in matches table
- ✅ League state updates (votes_until_sync)
- ✅ Error handling and validation
- ✅ Atomic operations (all succeed or fail together)