const { createClient } = require('@supabase/supabase-js');

const supabaseUrl = 'https://rkuftaxaycdgirtjgfhz.supabase.co';
const supabaseKey = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InJrdWZ0YXhheWNkZ2lydGpnZmh6Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3MjgzNDE1ODcsImV4cCI6MjA0MzkxNzU4N30.4BOyU-DZozEKdOzcQ4QwmKGZq0TmQEh4OX8q_7wwdRY';

const supabase = createClient(supabaseUrl, supabaseKey);

const playersData = [
  { id: 1, name: "Adam O'Reilly", rating: 1100, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 26, name: "Craig Leduc", rating: 1050, matches_played: 0, plays_am: true, plays_pm: false },
  { id: 102, name: "Zach Gutterman", rating: 1000, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 22, name: "Bryce Lapping", rating: 950, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 66, name: "Matt Lincoln", rating: 950, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 32, name: "Erik Zilli", rating: 925, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 76, name: "Mitchell Webb", rating: 875, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 104, name: "Gordon MacGregor", rating: 975, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 28, name: "Daniel Moore", rating: 850, matches_played: 0, plays_am: true, plays_pm: false },
  { id: 31, name: "Eric Wynalek", rating: 850, matches_played: 0, plays_am: true, plays_pm: false },
  { id: 47, name: "Joe Deblasio", rating: 850, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 65, name: "Matt Horton", rating: 850, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 81, name: "Omar Khatib", rating: 825, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 6, name: "Alex Kim", rating: 800, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 8, name: "Anatoly Akhunov", rating: 800, matches_played: 0, plays_am: true, plays_pm: false },
  { id: 45, name: "Jeff Brown", rating: 800, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 52, name: "Justin Webb", rating: 800, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 57, name: "Kyle Kim", rating: 800, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 61, name: "Mark Gould", rating: 800, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 74, name: "Mike Passaro", rating: 800, matches_played: 0, plays_am: true, plays_pm: false },
  { id: 96, name: "Ted Stevens", rating: 800, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 2, name: "Aj Rice", rating: 775, matches_played: 0, plays_am: true, plays_pm: false },
  { id: 3, name: "Alden Aikele", rating: 775, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 11, name: "Andy Dwyer", rating: 775, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 48, name: "Joe Ketchum", rating: 775, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 49, name: "Jordan English", rating: 775, matches_played: 0, plays_am: true, plays_pm: false },
  { id: 50, name: "Joshua Schneider", rating: 775, matches_played: 0, plays_am: true, plays_pm: false },
  { id: 55, name: "Kevin Corbin", rating: 775, matches_played: 0, plays_am: true, plays_pm: false },
  { id: 68, name: "Matthias Schroff", rating: 775, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 15, name: "Bill Taylor", rating: 750, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 17, name: "Brandt Levitt", rating: 750, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 5, name: "Alex Jofriet", rating: 725, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 33, name: "Fin Gold", rating: 725, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 69, name: "Michael Babcock", rating: 725, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 71, name: "Michael Lee", rating: 725, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 73, name: "Mike Barr", rating: 725, matches_played: 0, plays_am: true, plays_pm: false },
  { id: 97, name: "Tony Reckker", rating: 725, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 14, name: "Ashwin Dhurvas", rating: 700, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 23, name: "Cheryl Parsons", rating: 700, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 63, name: "Mark Van Hook", rating: 700, matches_played: 0, plays_am: true, plays_pm: false },
  { id: 64, name: "Martine Chev", rating: 700, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 70, name: "Michael Conte", rating: 700, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 18, name: "Brett Messier", rating: 675, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 24, name: "Chris Bolduc", rating: 675, matches_played: 0, plays_am: true, plays_pm: false },
  { id: 98, name: "Travis Senor", rating: 675, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 9, name: "Andrew Dinuzzo", rating: 650, matches_played: 0, plays_am: true, plays_pm: false },
  { id: 16, name: "Brad Riley", rating: 650, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 19, name: "Brian Borowski", rating: 650, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 25, name: "Chris Steward", rating: 650, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 27, name: "Dane Wedell", rating: 650, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 29, name: "David Ray", rating: 650, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 36, name: "Gideon Leconey", rating: 650, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 37, name: "Grant Batchelor", rating: 650, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 43, name: "Jason Jones", rating: 650, matches_played: 0, plays_am: true, plays_pm: false },
  { id: 72, name: "Michael Morrison", rating: 650, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 87, name: "Rick Sina", rating: 650, matches_played: 0, plays_am: true, plays_pm: false },
  { id: 4, name: "Alex Beeler", rating: 625, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 30, name: "Drey Vlady", rating: 625, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 10, name: "Andrew Miller", rating: 600, matches_played: 0, plays_am: true, plays_pm: false },
  { id: 90, name: "Ryan Mailhot", rating: 600, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 103, name: "Zach Straight", rating: 600, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 12, name: "Andy Gee", rating: 575, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 34, name: "Garrett William", rating: 575, matches_played: 0, plays_am: true, plays_pm: false },
  { id: 78, name: "Nicholas Artieri", rating: 575, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 95, name: "Steve Galavotti", rating: 575, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 40, name: "Ivan Weiskott", rating: 550, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 41, name: "Jaime Daza", rating: 550, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 54, name: "Kevin Chartrand", rating: 550, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 67, name: "Matthew Navarro", rating: 550, matches_played: 0, plays_am: true, plays_pm: false },
  { id: 80, name: "Nrupen Bhavsar", rating: 550, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 91, name: "Ryan Zimmerman", rating: 550, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 94, name: "Steve Dicks", rating: 550, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 99, name: "Tripp Green", rating: 550, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 56, name: "Kevin Miller", rating: 525, matches_played: 0, plays_am: true, plays_pm: false },
  { id: 59, name: "Lauren Horne", rating: 525, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 88, name: "Rick Zechini", rating: 525, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 100, name: "Tyler Barker", rating: 525, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 38, name: "Greg Gebhardt", rating: 500, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 53, name: "Kenneth Lilley", rating: 500, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 62, name: "Mark Huff", rating: 500, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 89, name: "Robert Inman", rating: 500, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 51, name: "Justin Johnson", rating: 475, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 79, name: "Nick Reichel", rating: 475, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 84, name: "Philip Stevenson", rating: 475, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 92, name: "Sean Whalen", rating: 475, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 13, name: "Andy Reuter", rating: 450, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 42, name: "Janet D. Aiken", rating: 450, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 75, name: "Mike Patten", rating: 450, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 83, name: "Pavel Moroz", rating: 450, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 93, name: "Shae Phelan", rating: 450, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 20, name: "Brian Ross", rating: 425, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 21, name: "Bryan Kearney", rating: 425, matches_played: 0, plays_am: true, plays_pm: false },
  { id: 44, name: "Jason Sivon", rating: 425, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 46, name: "Jessica Rachel", rating: 425, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 60, name: "Luke Meyer", rating: 425, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 85, name: "Pierre Santosuosso", rating: 425, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 86, name: "Raul Martinez", rating: 425, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 101, name: "Vincent Govan", rating: 425, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 35, name: "Georgie Ann Potts", rating: 400, matches_played: 0, plays_am: true, plays_pm: false },
  { id: 39, name: "Guada Gonzalez", rating: 375, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 58, name: "Laura Eischen", rating: 350, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 77, name: "Neil Jy Wen", rating: 350, matches_played: 0, plays_am: true, plays_pm: true },
  { id: 82, name: "Paul Gammariello", rating: 350, matches_played: 0, plays_am: false, plays_pm: true },
  { id: 7, name: "Alex Solt", rating: 300, matches_played: 0, plays_am: false, plays_pm: true }
];

async function updatePlayers() {
  try {
    console.log('Starting player update...');
    
    // Update players in batches
    const batchSize = 10;
    for (let i = 0; i < playersData.length; i += batchSize) {
      const batch = playersData.slice(i, i + batchSize);
      
      const { error } = await supabase
        .from('players')
        .upsert(batch, { 
          onConflict: 'id',
          ignoreDuplicates: false 
        });
      
      if (error) {
        console.error(`Error updating batch ${i / batchSize + 1}:`, error);
        throw error;
      }
      
      console.log(`Updated batch ${Math.floor(i / batchSize) + 1}/${Math.ceil(playersData.length / batchSize)}`);
    }
    
    console.log(`Successfully updated ${playersData.length} players!`);
    
    // Verify the update by fetching a few players
    const { data: samplePlayers } = await supabase
      .from('players')
      .select('id, name, rating, matches_played')
      .in('id', [1, 26, 102, 7])
      .order('rating', { ascending: false });
    
    console.log('\nSample players after update:');
    samplePlayers?.forEach(p => {
      console.log(`${p.name}: ${p.rating} (${p.matches_played} matches)`);
    });
    
  } catch (error) {
    console.error('Error updating players:', error);
  }
}

updatePlayers();