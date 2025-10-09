-- Add is_deleted to players for soft-deletion semantics
ALTER TABLE IF EXISTS players
    ADD COLUMN IF NOT EXISTS is_deleted BOOLEAN DEFAULT false;

-- Ensure existing rows are false
UPDATE players SET is_deleted = false WHERE is_deleted IS NULL;