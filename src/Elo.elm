module Elo exposing (draw, initialRating, odds, sensitiveKFactor, win, dynamicKFactor, getKFactor, oddsAsPercentage)

{-| Calculate [Elo](https://en.wikipedia.org/wiki/Elo_rating_system) scores.
-}


{-| The initial rating for new players. Set to 1500 to provide better
room for growth and align with standard chess ratings. This gives more
psychological space for players to improve and reduces "rating floor" issues.
-}
initialRating : Int
initialRating =
    500


{-| A sensitive K-factor for players with few games. Use dynamicKFactor
for better scaling with many players and games.
-}
sensitiveKFactor : Int
sensitiveKFactor =
    32


{-| Dynamic K-factor based on games played and rating level.
This provides faster convergence for new players while maintaining
stability for experienced players in high-volume environments.
Adjusted for 500 base rating system with lots of voting.

- New players (0-20 games): K=40 for rapid rating discovery
- Developing players (21-50 games): K=24 for continued adjustment  
- Established players (50+ games): K=16 for stability
- High-rated players (800+): K=12 for minimal volatility (adjusted for 500 base)
-}
dynamicKFactor : Int -> Int -> Int
dynamicKFactor gamesPlayed currentRating =
    if gamesPlayed <= 20 then
        12  -- New players: more volatility
    else if gamesPlayed <= 50 then
        6   -- Developing players: moderate
    else if currentRating >= 800 then
        3   -- High-rated: very stable
    else
        3   -- Established: very stable


{-| Get the appropriate K-factor for a player based on their experience.
Uses dynamic K-factor by default for better scaling.
-}
getKFactor : Int -> Int -> Int
getKFactor gamesPlayed currentRating =
    dynamicKFactor gamesPlayed currentRating


{-| The odds that a will beat b

Uses a moderated curve that prevents extreme percentages while still
showing meaningful differences. The standard ELO formula can produce
99%+ win rates which isn't realistic for most sports.

To flip it, `1 - (odds a b)` or just flip the arguments.

-}
odds : Int -> Int -> Float
odds a b =
    let
        -- Use a smaller divisor (600 instead of 400) to moderate extreme differences
        -- This makes the curve less steep and more realistic for sports
        rA =
            10 ^ (toFloat a / 600)

        rB =
            10 ^ (toFloat b / 600)

        rawOdds = rA / (rA + rB)
        
        -- Apply additional moderation: cap extremes at 85%/15%
        -- This prevents unrealistic 99%+ predictions while maintaining skill differences
        minOdds = 0.15
        maxOdds = 0.85
    in
    if rawOdds < minOdds then
        minOdds
    else if rawOdds > maxOdds then
        maxOdds
    else
        rawOdds


{-| Convert odds to a readable percentage string for display
-}
oddsAsPercentage : Int -> Int -> String
oddsAsPercentage ratingA ratingB =
    let
        winChance = odds ratingA ratingB
        percentage = round (winChance * 100)
    in
    String.fromInt percentage ++ "%"


{-| One player won, the other player lost.
-}
win : Int -> { won : Int, lost : Int } -> { won : Int, lost : Int }
win kFactor { won, lost } =
    let
        winOdds = odds won lost
        loseOdds = odds lost won
        rawWinChange = toFloat kFactor * (1 - winOdds)
        rawLoseChange = toFloat kFactor * (0 - loseOdds)
        -- Cap rating change for expected wins/losses
        maxChange = 2
        winChange =
            if winOdds > 0.7 then
                clamp (-(toFloat 4)) (toFloat 4) rawWinChange
            else if winOdds < 0.3 then
                rawWinChange -- allow full K for upsets
            else
                clamp (-(toFloat kFactor)) (toFloat kFactor) rawWinChange
        loseChange =
            if loseOdds > 0.7 then
                clamp (-(toFloat 4)) (toFloat 4) rawLoseChange
            else if loseOdds < 0.3 then
                rawLoseChange
            else
                clamp (-(toFloat kFactor)) (toFloat kFactor) rawLoseChange
    in
    { won = toFloat won + winChange |> round
    , lost = toFloat lost + loseChange |> round
    }


{-| The players drew/tied.
-}
draw : Int -> { playerA : Int, playerB : Int } -> { playerA : Int, playerB : Int }
draw kFactor { playerA, playerB } =
    let
        oddsA = odds playerA playerB
        oddsB = odds playerB playerA
        rawAChange = toFloat kFactor * (0.5 - oddsA)
        rawBChange = toFloat kFactor * (0.5 - oddsB)
        maxChange = 2
        aChange =
            if oddsA > 0.7 then
                clamp (-(toFloat 4)) (toFloat 4) rawAChange
            else if oddsA < 0.3 then
                rawAChange
            else
                clamp (-(toFloat kFactor)) (toFloat kFactor) rawAChange
        bChange =
            if oddsB > 0.7 then
                clamp (-(toFloat 4)) (toFloat 4) rawBChange
            else if oddsB < 0.3 then
                rawBChange
            else
                clamp (-(toFloat kFactor)) (toFloat kFactor) rawBChange
    in
    { playerA = toFloat playerA + aChange |> round
    , playerB = toFloat playerB + bChange |> round
    }
