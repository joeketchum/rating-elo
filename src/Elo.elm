module Elo exposing (draw, initialRating, odds, sensitiveKFactor, win, dynamicKFactor, getKFactor)

{-| Calculate [Elo](https://en.wikipedia.org/wiki/Elo_rating_system) scores.
-}


{-| The initial rating for new players. Set to 1500 to provide better
room for growth and align with standard chess ratings. This gives more
psychological space for players to improve and reduces "rating floor" issues.
-}
initialRating : Int
initialRating =
    1500


{-| A sensitive K-factor for players with few games. Use dynamicKFactor
for better scaling with many players and games.
-}
sensitiveKFactor : Int
sensitiveKFactor =
    32


{-| Dynamic K-factor based on games played and rating level.
This provides faster convergence for new players while maintaining
stability for experienced players in high-volume environments.

- New players (0-20 games): K=40 for rapid rating discovery
- Developing players (21-50 games): K=24 for continued adjustment  
- Established players (50+ games): K=16 for stability
- High-rated players (1800+): K=12 for minimal volatility
-}
dynamicKFactor : Int -> Int -> Int
dynamicKFactor gamesPlayed currentRating =
    if gamesPlayed <= 20 then
        40  -- New players need rapid adjustment
    else if gamesPlayed <= 50 then
        24  -- Still learning, moderate adjustment
    else if currentRating >= 1800 then
        12  -- High-rated players get stability
    else
        16  -- Standard for established players


{-| Get the appropriate K-factor for a player based on their experience.
Uses dynamic K-factor by default for better scaling.
-}
getKFactor : Int -> Int -> Int
getKFactor gamesPlayed currentRating =
    dynamicKFactor gamesPlayed currentRating


{-| The odds that a will beat b

To flip it, `1 - (odds a b)` or just flip the arguments.

-}
odds : Int -> Int -> Float
odds a b =
    let
        rA =
            10 ^ (toFloat a / 400)

        rB =
            10 ^ (toFloat b / 400)
    in
    rA / (rA + rB)


{-| One player won, the other player lost.
-}
win : Int -> { won : Int, lost : Int } -> { won : Int, lost : Int }
win kFactor { won, lost } =
    { won = toFloat won + toFloat kFactor * (1 - odds won lost) |> round
    , lost = toFloat lost + toFloat kFactor * (0 - odds lost won) |> round
    }


{-| The players drew/tied.
-}
draw : Int -> { playerA : Int, playerB : Int } -> { playerA : Int, playerB : Int }
draw kFactor { playerA, playerB } =
    { playerA = toFloat playerA + toFloat kFactor * (0.5 - odds playerA playerB) |> round
    , playerB = toFloat playerB + toFloat kFactor * (0.5 - odds playerB playerA) |> round
    }
