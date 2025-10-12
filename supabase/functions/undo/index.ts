import { serve } from "https://deno.land/std@0.168.0/http/server.ts"
import { createClient } from 'https://esm.sh/@supabase/supabase-js@2'

const corsHeaders = {
  'Access-Control-Allow-Origin': '*',
  'Access-Control-Allow-Headers': 'authorization, Authorization, x-client-info, apikey, content-type',
  'Access-Control-Allow-Methods': 'POST, OPTIONS',
}

serve(async (req: Request) => {
  console.log('Undo function called with method:', req.method)
  
  if (req.method === 'OPTIONS') {
    return new Response('ok', { headers: corsHeaders })
  }

  try {
    // No body required; we undo the most recent match globally
    
    const supabaseUrl = Deno.env.get('SUPABASE_URL')
    const supabaseServiceKey = Deno.env.get('SUPABASE_SERVICE_ROLE_KEY')
    
    if (!supabaseUrl || !supabaseServiceKey) {
      throw new Error('Missing environment variables: SUPABASE_URL or SUPABASE_SERVICE_ROLE_KEY')
    }

    const supabaseClient = createClient(supabaseUrl, supabaseServiceKey)

    console.log('Supabase client created successfully')

    // Find the most recent match overall
    const { data: matches, error: matchErr } = await supabaseClient
      .from('matches')
      .select('*')
      .order('played_at', { ascending: false })
      .limit(1)

    console.log('Match query result:', { matches, matchErr })

    if (matchErr) {
      throw new Error(`Failed to fetch last match: ${matchErr.message}`)
    }

    if (!matches || matches.length === 0) {
      console.log('No matches found to undo')
      return new Response(JSON.stringify({ success: false, reason: 'No match found' }), { headers: { ...corsHeaders, 'Content-Type': 'application/json' }, status: 404 })
    }

    const m = matches[0]
    console.log('Found match to undo:', m)

    // Fetch current players to know matches_played
    console.log('Fetching players:', [m.player_a_id, m.player_b_id])
    const { data: players, error: playersErr } = await supabaseClient
      .from('players')
      .select('id, name, rating, matches_played, plays_am, plays_pm, is_deleted')
      .in('id', [m.player_a_id, m.player_b_id])

    console.log('Player query result:', { players, playersErr })

    if (playersErr) {
      throw new Error(`Failed to fetch players: ${playersErr.message}`)
    }

    const pa = players?.find((p: any) => p.id === m.player_a_id)
    const pb = players?.find((p: any) => p.id === m.player_b_id)
    console.log('Found players:', { pa, pb })
    
    if (!pa || !pb) {
      throw new Error('Player rows not found for undo')
    }

    // Revert both players to their rating_before and decrement matches_played
    const playerUpdates = [
      {
        id: m.player_a_id,
        name: pa.name,
        rating: m.player_a_rating_before,
        matches_played: Math.max(0, (pa.matches_played ?? 1) - 1),
        plays_am: pa.plays_am,
        plays_pm: pa.plays_pm,
        is_deleted: pa.is_deleted,
        updated_at: new Date().toISOString()
      },
      {
        id: m.player_b_id,
        name: pb.name,
        rating: m.player_b_rating_before,
        matches_played: Math.max(0, (pb.matches_played ?? 1) - 1),
        plays_am: pb.plays_am,
        plays_pm: pb.plays_pm,
        is_deleted: pb.is_deleted,
        updated_at: new Date().toISOString()
      }
    ]
    
    console.log('Updating players with:', playerUpdates)
    
    const { error: upErr } = await supabaseClient.from('players').upsert(playerUpdates)

    console.log('Player update result:', { upErr })

    if (upErr) {
      throw new Error(`Failed to revert players: ${upErr.message}`)
    }

    // Delete the match
    console.log('Deleting match:', m.id)
    const { error: delErr } = await supabaseClient
      .from('matches')
      .delete()
      .eq('id', m.id)

    console.log('Match deletion result:', { delErr })

    if (delErr) {
      throw new Error(`Failed to delete match: ${delErr.message}`)
    }

    // Bump league_state votes_until_sync back up by 1
    const { data: leagueState, error: leagueErr } = await supabaseClient
      .from('league_state')
      .select('*')
      .eq('id', 1)
      .single()

    if (!leagueErr && leagueState) {
      await supabaseClient
        .from('league_state')
        .update({
          votes_until_sync: leagueState.votes_until_sync + 1,
          updated_at: new Date().toISOString()
        })
        .eq('id', 1)
    }

    return new Response(JSON.stringify({ success: true }), { headers: { ...corsHeaders, 'Content-Type': 'application/json' }, status: 200 })
  } catch (error) {
    console.error('Undo error:', error)
    console.error('Error stack:', error instanceof Error ? error.stack : 'No stack trace')
    return new Response(
      JSON.stringify({ 
        error: error instanceof Error ? error.message : String(error),
        details: error instanceof Error ? error.stack : undefined
      }),
      { headers: { ...corsHeaders, 'Content-Type': 'application/json' }, status: 400 }
    )
  }
})
