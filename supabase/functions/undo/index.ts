import { serve } from "https://deno.land/std@0.168.0/http/server.ts"
import { createClient } from 'https://esm.sh/@supabase/supabase-js@2'

const corsHeaders = {
  'Access-Control-Allow-Origin': '*',
  'Access-Control-Allow-Headers': 'authorization, x-client-info, apikey, content-type',
  'Access-Control-Allow-Methods': 'POST, OPTIONS',
}

serve(async (req) => {
  // Handle CORS preflight requests
  if (req.method === 'OPTIONS') {
    return new Response('ok', { headers: corsHeaders })
  }

  try {
    const { match_id } = await req.json()

    // Validate input
    if (!match_id) {
      throw new Error('Missing required field: match_id')
    }

    // Initialize Supabase client with service role key for admin operations
    const supabaseClient = createClient(
      Deno.env.get('SUPABASE_URL') ?? '',
      Deno.env.get('SUPABASE_SERVICE_ROLE_KEY') ?? ''
    )

    // Get the match to undo
    const { data: match, error: matchError } = await supabaseClient
      .from('matches')
      .select('*')
      .eq('id', match_id)
      .single()

    if (matchError) {
      throw new Error(`Failed to fetch match: ${matchError.message}`)
    }

    if (!match) {
      throw new Error('Match not found')
    }

    // Get the players involved
    const { data: players, error: playersError } = await supabaseClient
      .from('players')
      .select('*')
      .in('id', [match.player_a_id, match.player_b_id])

    if (playersError) {
      throw new Error(`Failed to fetch players: ${playersError.message}`)
    }

    if (!players || players.length !== 2) {
      throw new Error('Could not find both players')
    }

    const playerA = players.find(p => p.id === match.player_a_id)
    const playerB = players.find(p => p.id === match.player_b_id)

    if (!playerA || !playerB) {
      throw new Error('Player data incomplete')
    }

    // Restore previous ratings and decrement matches played
    const { error: updateError } = await supabaseClient
      .from('players')
      .upsert([
        {
          id: match.player_a_id,
          name: playerA.name,
          rating: match.player_a_rating_before,
          matches_played: Math.max(0, playerA.matches_played - 1),
          plays_am: playerA.plays_am,
          plays_pm: playerA.plays_pm,
          is_ignored: playerA.is_ignored,
          updated_at: new Date().toISOString()
        },
        {
          id: match.player_b_id,
          name: playerB.name,
          rating: match.player_b_rating_before,
          matches_played: Math.max(0, playerB.matches_played - 1),
          plays_am: playerB.plays_am,
          plays_pm: playerB.plays_pm,
          is_ignored: playerB.is_ignored,
          updated_at: new Date().toISOString()
        }
      ])

    if (updateError) {
      throw new Error(`Failed to revert player ratings: ${updateError.message}`)
    }

    // Delete the match record
    const { error: deleteError } = await supabaseClient
      .from('matches')
      .delete()
      .eq('id', match_id)

    if (deleteError) {
      throw new Error(`Failed to delete match: ${deleteError.message}`)
    }

    return new Response(
      JSON.stringify({ 
        success: true,
        undoneMatch: {
          playerA: { id: match.player_a_id, restoredRating: match.player_a_rating_before },
          playerB: { id: match.player_b_id, restoredRating: match.player_b_rating_before }
        }
      }),
      { 
        headers: { ...corsHeaders, 'Content-Type': 'application/json' },
        status: 200 
      }
    )
  } catch (error) {
    console.error('Undo function error:', error)
    return new Response(
      JSON.stringify({ error: error instanceof Error ? error.message : String(error) }),
      { 
        headers: { ...corsHeaders, 'Content-Type': 'application/json' },
        status: 400 
      }
    )
  }
})