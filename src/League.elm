module League exposing
    ( League, init, decoder, playersDecoder, encode
    , addPlayer, players, getPlayer, retirePlayer, updatePlayer
    , Match(..), currentMatch, nextMatch, nextMatchFiltered, startMatch, Outcome(..), finishMatch, kFactor, clearMatch
    , ignorePlayer, unignorePlayer, isPlayerIgnored
    , getPlayerRanking
    )

{-|

@docs League, init, decoder, encode

@docs addPlayer, players, getPlayer, retirePlayer

@docs Match, currentMatch, nextMatch, startMatch, Outcome, finishMatch, kFactor

-}

import Dict as ComparableDict
import Elo
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Player exposing (Player, PlayerId(..))
import Random exposing (Generator)
import Sort.Dict as Dict exposing (Dict)


type League
    = League
        { players : Dict PlayerId Player
        , currentMatch : Maybe Match
        , ignored : List PlayerId
        }


type Match
    = Match Player Player



-- LOADING AND SAVING


init : League
init =
    League
        { players = Dict.empty Player.idSorter
        , currentMatch = Nothing
        , ignored = []
        }


decoder : Decoder League
decoder =
    let
        ignoredDecoder =
            Decode.oneOf
                [ Decode.field "ignored" (Decode.list Decode.int) |> Decode.map (List.map PlayerId)
                , Decode.succeed []
                ]
    in
    Decode.oneOf
        [ Decode.map2
            (\newPlayers ignored ->
                League
                    { players = newPlayers
                    , currentMatch = Nothing
                    , ignored = ignored
                    }
            )
            playersDecoder
            ignoredDecoder
        , -- old format: only players as a dict
          Decode.dict Player.decoder
            |> Decode.map ComparableDict.toList
            |> Decode.map (List.map (\( _, player ) -> ( Player.id player, player )))
            |> Decode.map (Dict.fromList Player.idSorter)
            |> Decode.map (\playersDict -> League { players = playersDict, currentMatch = Nothing, ignored = [] })
        ]


playersDecoder : Decoder (Dict PlayerId Player)
playersDecoder =
    Decode.field "players" (Decode.list Player.decoder)
        |> Decode.map (List.map (\player -> ( Player.id player, player )))
        |> Decode.map (Dict.fromList Player.idSorter)


encode : League -> Encode.Value
encode (League league) =
    Encode.object
        [ ( "players", Encode.list Player.encode (Dict.values league.players) )
        , ( "ignored", Encode.list Encode.int (List.map (\(PlayerId i) -> i) league.ignored) )
        ]



-- PLAYERS


players : League -> List Player
players (League league) =
    Dict.values league.players


{-| Mark a player as ignored so they won't be scheduled for future matches.
-}
ignorePlayer : Player -> League -> League
ignorePlayer player (League league) =
    let
        pid = Player.id player
    in
    if List.member pid league.ignored then
        League league
    else
        League { league | ignored = pid :: league.ignored }


unignorePlayer : Player -> League -> League
unignorePlayer player (League league) =
    let
        pid = Player.id player
    in
    League { league | ignored = List.filter ((/=) pid) league.ignored }


isPlayerIgnored : Player -> League -> Bool
isPlayerIgnored player (League league) =
    List.member (Player.id player) league.ignored


getPlayer : PlayerId -> League -> Maybe Player
getPlayer id (League league) =
    Dict.get id league.players


addPlayer : Player -> League -> League
addPlayer player (League league) =
    League { league | players = Dict.insert (Player.id player) player league.players }


{-| -}
updatePlayer : Player -> League -> League
updatePlayer player (League league) =
    League { league | players = Dict.insert (Player.id player) player league.players }


retirePlayer : Player -> League -> League
retirePlayer player (League league) =
    League
        { league
            | players = Dict.remove (Player.id player) league.players
            , currentMatch =
                case league.currentMatch of
                    Nothing ->
                        Nothing

                    Just (Match a b) ->
                        if Player.id player == Player.id a || Player.id player == Player.id b then
                            Nothing

                        else
                            league.currentMatch
            , ignored = List.filter ((/=) (Player.id player)) league.ignored
        }



-- MATCHES


currentMatch : League -> Maybe Match
currentMatch (League league) =
    league.currentMatch


{-| Select the next match according to a two-phase system:

1.  If there are players who have less than the "play-in" number of matches
    (that is, the number of matches I feel are needed to get a good idea of
    the player's rough ranking) then choose among them randomly, favoring
    those who have played least. If there are no such players then choose
    among all the players, favoring players who have played less recently.

2.  Once the first player is chosen, choose a second player close to them
    by rank. The ideal matchup goes from a tie to a decisive "this player
    is ranked higher."

Edge case: If there are fewer than two unique players, we can't schedule a
new match.

-}
nextMatch : League -> Generator (Maybe Match)
nextMatch league =
    nextMatchFiltered (\_ -> True) league


-- Get player ranking by rating (1-indexed, higher rating = lower rank number)
getPlayerRanking : Player -> League -> Int  
getPlayerRanking player (League league) =
    Dict.values league.players
        |> List.sortBy (\p -> -(Player.rating p))  -- Sort by rating descending
        |> List.indexedMap (\index p -> (index + 1, p))  -- Convert to 1-indexed rankings
        |> List.filter (\(_, p) -> Player.id p == Player.id player)
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault 999  -- Default high ranking if not found


nextMatchFiltered : (Player -> Bool) -> League -> Generator (Maybe Match)
nextMatchFiltered allow (League league) =
    let
        allPlayersRaw =
            Dict.values league.players

        allPlayers =
            List.filter (\p -> allow p && not (List.member (Player.id p) league.ignored)) allPlayersRaw
    in
    case allPlayers of
        -- at least two
        a :: b :: rest ->
            let
                ( firstPossiblePlayer, restOfPossiblePlayers ) =
                    case List.filter (\player -> Player.matchesPlayed player <= playInMatches) allPlayers of
                        [] ->
                            ( a, b :: rest )

                        firstPlayIn :: restOfPlayIns ->
                            ( firstPlayIn, restOfPlayIns )

                mostMatchesAmongPossiblePlayers =
                    List.map Player.matchesPlayed (firstPossiblePlayer :: restOfPossiblePlayers)
                        |> List.maximum
                        |> Maybe.withDefault (Player.matchesPlayed firstPossiblePlayer)
            in
            Random.weighted
                ( toFloat (mostMatchesAmongPossiblePlayers - Player.matchesPlayed firstPossiblePlayer) ^ 2, firstPossiblePlayer )
                (List.map (\player -> ( toFloat (mostMatchesAmongPossiblePlayers - Player.matchesPlayed player) ^ 2, player )) restOfPossiblePlayers)
                |> Random.andThen
                    (\firstPlayer ->
                        let
                            allOpponents =
                                if firstPlayer == a then
                                    b :: rest
                                else if firstPlayer == b then
                                    a :: rest
                                else
                                    a :: b :: List.filter (\p -> p /= firstPlayer) rest

                            -- Filter opponents to within ±10 ranking positions for more competitive matches
                            firstPlayerRanking = getPlayerRanking firstPlayer (League league)
                            eligibleOpponents =
                                List.filter 
                                    (\opponent -> 
                                        let opponentRanking = getPlayerRanking opponent (League league)
                                        in abs (firstPlayerRanking - opponentRanking) <= 10
                                    ) 
                                    allOpponents

                            -- Fall back to all opponents if no eligible ones within range
                            (head, tail) = 
                                case eligibleOpponents of
                                    h :: t -> (h, t)
                                    [] -> 
                                        case allOpponents of
                                            h :: t -> (h, t)
                                            [] -> (firstPlayer, [])  -- This shouldn't happen

                            closestRatingDistance =
                                (head :: tail)
                                    |> List.map (\player -> abs (Player.rating firstPlayer - Player.rating player))
                                    |> List.minimum
                                    |> Maybe.withDefault 0
                                    
                            -- Add small constant to avoid division by zero and ensure all players have some chance
                            baseWeight = 10.0
                        in
                        Random.weighted
                            ( baseWeight + toFloat (500 - min 500 (abs (Player.rating firstPlayer - Player.rating head))), head )
                            (List.map (\player -> ( baseWeight + toFloat (500 - min 500 (abs (Player.rating firstPlayer - Player.rating player))), player )) tail)
                            |> Random.map (Tuple.pair firstPlayer)
                    )
                |> Random.andThen
                    (\( playerA, playerB ) ->
                        Random.map
                            (\flip ->
                                if flip then
                                    Match playerA playerB

                                else
                                    Match playerB playerA
                            )
                            (Random.uniform True [ False ])
                    )
                |> Random.map Just

        -- one or zero players
        _ ->
            Random.constant Nothing


startMatch : Match -> League -> League
startMatch (Match playerA playerB) (League league) =
    League
        { league
            | currentMatch =
                -- don't start a match with players that aren't in the
                -- league...
                Maybe.map2 Tuple.pair
                    (Dict.get (Player.id playerA) league.players)
                    (Dict.get (Player.id playerB) league.players)
                    |> Maybe.andThen
                        (\( gotA, gotB ) ->
                            -- ... or when the players are the same player
                            if gotA /= gotB then
                                Just (Match gotA gotB)

                            else
                                Nothing
                        )
        }


type Outcome
    = Win { won : Player, lost : Player }
    | Draw { playerA : Player, playerB : Player }


finishMatch : Outcome -> League -> League
finishMatch outcome league =
    case outcome of
        Win { won, lost } ->
            let
                newRatings =
                    Elo.win (kFactor league won)
                        { won = Player.rating won
                        , lost = Player.rating lost
                        }

                newPlayers =
                    updateRatingsIncludingPlayInPeriod
                        { playerA = newRatings.won
                        , playerB = newRatings.lost
                        }
                        { playerA = won
                        , playerB = lost
                        }
            in
            league
                |> updatePlayer newPlayers.playerA
                |> updatePlayer newPlayers.playerB
                |> clearMatch

        Draw { playerA, playerB } ->
            let
                newRatings =
                    Elo.draw (kFactor league (higherRankedPlayer playerA playerB))
                        { playerA = Player.rating playerA
                        , playerB = Player.rating playerB
                        }

                newPlayers =
                    updateRatingsIncludingPlayInPeriod
                        newRatings
                        { playerA = playerA
                        , playerB = playerB
                        }
            in
            league
                |> updatePlayer newPlayers.playerA
                |> updatePlayer newPlayers.playerB
                |> clearMatch


updateRatingsIncludingPlayInPeriod :
    { playerA : Int, playerB : Int }
    -> { playerA : Player, playerB : Player }
    -> { playerA : Player, playerB : Player }
updateRatingsIncludingPlayInPeriod ratings players_ =
    let
        playerAInPlayInPeriod =
            Player.matchesPlayed players_.playerA < playInMatches

        playerBInPlayInPeriod =
            Player.matchesPlayed players_.playerB < playInMatches
    in
    { playerA =
        if not playerAInPlayInPeriod && playerBInPlayInPeriod then
            players_.playerA

        else
            players_.playerA
                |> Player.setRating ratings.playerA
                |> Player.incrementMatchesPlayed
    , playerB =
        if not playerBInPlayInPeriod && playerAInPlayInPeriod then
            players_.playerB

        else
            players_.playerB
                |> Player.setRating ratings.playerB
                |> Player.incrementMatchesPlayed
    }


{-| -}
playInMatches : Int
playInMatches =
    12


{-| -}
kFactor : League -> Player -> Int
kFactor (League league) player =
    -- Use dynamic K-factor based on games played and rating for better scaling
    -- This provides optimal convergence for new players while maintaining
    -- stability for experienced players in high-volume environments
    Elo.getKFactor (Player.matchesPlayed player) (Player.rating player)


{-| Not 100% correct because of the rounding but good enough for our
purposes.
-}
percentile : Float -> List Int -> Maybe Int
percentile pct items =
    let
        sorted =
            List.sort items

        offset =
            pct * toFloat (List.length items)

        index =
            floor offset
    in
    if toFloat index == offset then
        sorted
            |> List.drop (index - 1)
            |> List.head

    else
        let
            fractionalPart =
                offset - toFloat index

            betweenThese =
                sorted
                    |> List.drop (index - 1)
                    |> List.take 2
        in
        case betweenThese of
            [ a, b ] ->
                Just (round (toFloat a + fractionalPart * (toFloat b - toFloat a)))

            _ ->
                Nothing


{-| -}
higherRankedPlayer : Player -> Player -> Player
higherRankedPlayer a b =
    if Player.rating a > Player.rating b then
        a

    else
        b


{-| -}
clearMatch : League -> League
clearMatch (League league) =
    League { league | currentMatch = Nothing }
