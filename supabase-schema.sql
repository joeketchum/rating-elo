-- Hockey Rating League Database Schema for Supabase

-- Enable Row Level Security
ALTER DATABASE postgres SET "app.jwt_secret" TO 'your-jwt-secret';

-- Players table
CREATE TABLE IF NOT EXISTS players (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL UNIQUE,
    rating INTEGER DEFAULT 1500,
    matches_played INTEGER DEFAULT 0,
    plays_am BOOLEAN DEFAULT true,
    plays_pm BOOLEAN DEFAULT true,
    is_ignored BOOLEAN DEFAULT false,
    is_deleted BOOLEAN DEFAULT false,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Matches history table
CREATE TABLE IF NOT EXISTS matches (
    id SERIAL PRIMARY KEY,
    player_a_id INTEGER REFERENCES players(id),
    player_b_id INTEGER REFERENCES players(id),
    winner_id INTEGER REFERENCES players(id),
    player_a_rating_before INTEGER NOT NULL,
    player_b_rating_before INTEGER NOT NULL,
    player_a_rating_after INTEGER NOT NULL,
    player_b_rating_after INTEGER NOT NULL,
    k_factor_used INTEGER NOT NULL,
    played_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- League state table (current match, settings, etc.)
CREATE TABLE IF NOT EXISTS league_state (
    id INTEGER PRIMARY KEY DEFAULT 1,
    current_match_player_a INTEGER REFERENCES players(id),
    current_match_player_b INTEGER REFERENCES players(id),
    votes_until_sync INTEGER DEFAULT 25,
    last_sync_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    CONSTRAINT single_row CHECK (id = 1)
);

-- Insert initial league state
INSERT INTO league_state (id) VALUES (1) ON CONFLICT (id) DO NOTHING;

-- Indexes for performance
CREATE INDEX IF NOT EXISTS idx_players_rating ON players(rating DESC);
CREATE INDEX IF NOT EXISTS idx_players_am ON players(plays_am) WHERE plays_am = true;
CREATE INDEX IF NOT EXISTS idx_players_pm ON players(plays_pm) WHERE plays_pm = true;
CREATE INDEX IF NOT EXISTS idx_matches_played_at ON matches(played_at DESC);
CREATE INDEX IF NOT EXISTS idx_matches_players ON matches(player_a_id, player_b_id);

-- Enable real-time subscriptions
ALTER PUBLICATION supabase_realtime ADD TABLE players;
ALTER PUBLICATION supabase_realtime ADD TABLE matches;
ALTER PUBLICATION supabase_realtime ADD TABLE league_state;

-- Row Level Security policies (allow all operations for now)
ALTER TABLE players ENABLE ROW LEVEL SECURITY;
ALTER TABLE matches ENABLE ROW LEVEL SECURITY;
ALTER TABLE league_state ENABLE ROW LEVEL SECURITY;

-- Allow all operations (you can restrict this later with authentication)
CREATE POLICY "Allow all operations on players" ON players FOR ALL USING (true);
CREATE POLICY "Allow all operations on matches" ON matches FOR ALL USING (true);
CREATE POLICY "Allow all operations on league_state" ON league_state FOR ALL USING (true);

-- Function to update the updated_at timestamp
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language 'plpgsql';

-- Triggers to automatically update timestamps
CREATE TRIGGER update_players_updated_at BEFORE UPDATE ON players
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_league_state_updated_at BEFORE UPDATE ON league_state
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();