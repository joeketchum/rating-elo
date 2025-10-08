const { createClient } = require('@supabase/supabase-js');

const supabaseUrl = 'https://rkuftaxaycdgirtjgfhz.supabase.co';
const supabaseKey = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InJrdWZ0YXhheWNkZ2lydGpnZmh6Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3MjgzNDE1ODcsImV4cCI6MjA0MzkxNzU4N30.4BOyU-DZozEKdOzcQ4QwmKGZq0TmQEh4OX8q_7wwdRY';

const supabase = createClient(supabaseUrl, supabaseKey);

async function cleanupPlayers() {
  try {
    console.log('Fetching all players...');
    
    // Get all players
    const { data: players, error: fetchError } = await supabase
      .from('players')
      .select('*')
      .order('id');
    
    if (fetchError) {
      console.error('Error fetching players:', fetchError);
      return;
    }
    
    console.log(`Found ${players.length} players`);
    
    // Identify players with invalid IDs (should be 1-103, anything else is hash-based)
    const validPlayers = players.filter(p => p.id >= 1 && p.id <= 103);
    const invalidPlayers = players.filter(p => p.id < 1 || p.id > 103);
    
    console.log(`Valid players (ID 1-103): ${validPlayers.length}`);
    console.log(`Invalid players (hash-based IDs): ${invalidPlayers.length}`);
    
    if (invalidPlayers.length > 0) {
      console.log('Invalid players to delete:', invalidPlayers.map(p => `${p.name} (ID: ${p.id})`));
      
      // Delete players with invalid IDs
      const invalidIds = invalidPlayers.map(p => p.id);
      
      const { error: deleteError } = await supabase
        .from('players')
        .delete()
        .in('id', invalidIds);
      
      if (deleteError) {
        console.error('Error deleting invalid players:', deleteError);
        return;
      }
      
      console.log(`✅ Successfully deleted ${invalidPlayers.length} players with invalid IDs`);
    } else {
      console.log('✅ No invalid players found - database is clean!');
    }
    
    // Show remaining valid players
    console.log('\nRemaining valid players:');
    validPlayers.forEach(p => {
      console.log(`  ${p.id}: ${p.name} (Rating: ${p.rating})`);
    });
    
  } catch (error) {
    console.error('Cleanup failed:', error);
  }
}

cleanupPlayers();