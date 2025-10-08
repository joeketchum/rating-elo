import { serve } from "https://deno.land/std@0.168.0/http/server.ts"
import { createClient } from 'https://esm.sh/@supabase/supabase-js@2'

const corsHeaders = {
  'Access-Control-Allow-Origin': '*',
  'Access-Control-Allow-Headers': 'authorization, x-client-info, apikey, content-type',
  'Access-Control-Allow-Methods': 'POST, OPTIONS',
}

// Elo rating calculation functions
function calculateExpectedScore(ratingA: number, ratingB: number): number {
  return 1 / (1 + Math.pow(10, (ratingB - ratingA) / 400))
}

function getKFactor(matchesPlayed: number, rating: number): number {
  if (matchesPlayed < 30) return 40
  if (rating < 2100) return 20
  return 10
}

function calculateNewRating(currentRating: number, kFactor: number, actualScore: number, expectedScore: number): number {
  return Math.round(currentRating + kFactor * (actualScore - expectedScore))
}

serve(async (req) => {
  // Handle CORS preflight requests
  if (req.method === 'OPTIONS') {
    return new Response('ok', { headers: corsHeaders })
  }

  try {
    const { a_id, b_id, winner } = await req.json()
    
    // Validate input
    if (!a_id || !b_id || !winner) {
      throw new Error('Missing required fields: a_id, b_id, winner')
    }

    // Initialize Supabase client with service role key for admin operations
    const supabaseClient = createClient(
      Deno.env.get('SUPABASE_URL') ?? '',
      Deno.env.get('SUPABASE_SERVICE_ROLE_KEY') ?? ''
    )

    // Fetch both players
    const { data: players, error: playersError } = await supabaseClient
      .from('players')
      .select('*')
      .in('id', [a_id, b_id])

    if (playersError) {
      throw new Error(`Failed to fetch players: ${playersError.message}`)
    }

    if (!players || players.length !== 2) {
      throw new Error('Could not find both players')
    }

    const playerA = players.find(p => p.id === a_id)
    const playerB = players.find(p => p.id === b_id)

    if (!playerA || !playerB) {
      throw new Error('Player data incomplete')
    }

    // Calculate Elo ratings
    const ratingA = playerA.rating
    const ratingB = playerB.rating
    const kFactorA = getKFactor(playerA.matches_played, ratingA)
    const kFactorB = getKFactor(playerB.matches_played, ratingB)
    
    const expectedA = calculateExpectedScore(ratingA, ratingB)
    const expectedB = calculateExpectedScore(ratingB, ratingA)
    
    // Determine actual scores (1 for win, 0.5 for draw, 0 for loss)
    let actualA: number, actualB: number
    if (winner === a_id) {
      actualA = 1
      actualB = 0
    } else if (winner === b_id) {
      actualA = 0
      actualB = 1
    } else {
      // Draw case
      actualA = 0.5
      actualB = 0.5
    }

    const newRatingA = calculateNewRating(ratingA, kFactorA, actualA, expectedA)
    const newRatingB = calculateNewRating(ratingB, kFactorB, actualB, expectedB)

    // Start transaction by updating both players
    const { error: updateError } = await supabaseClient
      .from('players')
      .upsert([
        {
          id: a_id,
          name: playerA.name,
          rating: newRatingA,
          matches_played: playerA.matches_played + 1,
          plays_am: playerA.plays_am,
          plays_pm: playerA.plays_pm,
          is_ignored: playerA.is_ignored,
          updated_at: new Date().toISOString()
        },
        {
          id: b_id,
          name: playerB.name,
          rating: newRatingB,
          matches_played: playerB.matches_played + 1,
          plays_am: playerB.plays_am,
          plays_pm: playerB.plays_pm,
          is_ignored: playerB.is_ignored,
          updated_at: new Date().toISOString()
        }
      ])

    if (updateError) {
      throw new Error(`Failed to update players: ${updateError.message}`)
    }

    // Record the match in matches table
    const { error: matchError } = await supabaseClient
      .from('matches')
      .insert({
        player_a_id: a_id,
        player_b_id: b_id,
        winner_id: winner,
        player_a_rating_before: ratingA,
        player_b_rating_before: ratingB,
        player_a_rating_after: newRatingA,
        player_b_rating_after: newRatingB,
        k_factor_used: kFactorA,
        played_at: new Date().toISOString()
      })

    if (matchError) {
      throw new Error(`Failed to record match: ${matchError.message}`)
    }

    // Update league state to decrease votes_until_sync
    const { data: leagueState, error: leagueError } = await supabaseClient
      .from('league_state')
      .select('*')
      .eq('id', 1)
      .single()

    if (!leagueError && leagueState) {
      await supabaseClient
        .from('league_state')
        .update({
          votes_until_sync: Math.max(0, leagueState.votes_until_sync - 1),
          updated_at: new Date().toISOString()
        })
        .eq('id', 1)
    }

    return new Response(
      JSON.stringify({ 
        success: true,
        match: {
          playerA: { id: a_id, oldRating: ratingA, newRating: newRatingA },
          playerB: { id: b_id, oldRating: ratingB, newRating: newRatingB },
          winner: winner
        }
      }),
      { 
        headers: { ...corsHeaders, 'Content-Type': 'application/json' },
        status: 200 
      }
    )
  } catch (error) {
    console.error('Edge function error:', error)
    return new Response(
      JSON.stringify({ error: error.message }),
      { 
        headers: { ...corsHeaders, 'Content-Type': 'application/json' },
        status: 400 
      }
    )
  }
})