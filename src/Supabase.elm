module Supabase exposing
    ( Config
    , Player
    , Match
    , LeagueState
    , getPlayers
    , createNewPlayer
    , createPlayer
    , updatePlayer
    , deletePlayer
    , recordMatch
    , getLeagueState
    , updateLeagueState
    , subscribeToPlayers
    , voteEdgeFunction
    )

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Time


-- CONFIGURATION


type alias Config =
    { url : String
    , anonKey : String
    }


-- TYPES


type alias Player =
    { id : Int
    , name : String
    , rating : Int
    , matchesPlayed : Int
    , playsAM : Bool
    , playsPM : Bool
    , isIgnored : Bool
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    }


type alias Match =
    { id : Int
    , playerAId : Int
    , playerBId : Int
    , winnerId : Int
    , playerARatingBefore : Int
    , playerBRatingBefore : Int
    , playerARatingAfter : Int
    , playerBRatingAfter : Int
    , kFactorUsed : Int
    , playedAt : Time.Posix
    }


type alias LeagueState =
    { id : Int
    , currentMatchPlayerA : Maybe Int
    , currentMatchPlayerB : Maybe Int
    , votesUntilSync : Int
    , lastSyncAt : Time.Posix
    , updatedAt : Time.Posix
    }


-- DECODERS


playerDecoder : Decoder Player
playerDecoder =
    Decode.map8
        (\id name rating matchesPlayed playsAM playsPM isIgnored createdAt ->
            \updatedAt ->
                { id = id
                , name = name
                , rating = rating
                , matchesPlayed = matchesPlayed
                , playsAM = playsAM
                , playsPM = playsPM
                , isIgnored = isIgnored
                , createdAt = createdAt
                , updatedAt = updatedAt
                }
        )
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "rating" Decode.int)
        (Decode.field "matches_played" Decode.int)
        (Decode.field "plays_am" Decode.bool)
        (Decode.field "plays_pm" Decode.bool)
        (Decode.field "is_ignored" Decode.bool)
        (Decode.field "created_at" (Decode.string |> Decode.andThen decodeIsoTime))
    |> Decode.andThen (\partial ->
        Decode.field "updated_at" (Decode.string |> Decode.andThen decodeIsoTime)
            |> Decode.map partial
    )


matchDecoder : Decoder Match
matchDecoder =
    Decode.map8
        (\id playerAId playerBId winnerId playerARatingBefore playerBRatingBefore playerARatingAfter playerBRatingAfter ->
            \kFactorUsed -> \playedAt ->
                { id = id
                , playerAId = playerAId
                , playerBId = playerBId
                , winnerId = winnerId
                , playerARatingBefore = playerARatingBefore
                , playerBRatingBefore = playerBRatingBefore
                , playerARatingAfter = playerARatingAfter
                , playerBRatingAfter = playerBRatingAfter
                , kFactorUsed = kFactorUsed
                , playedAt = playedAt
                }
        )
        (Decode.field "id" Decode.int)
        (Decode.field "player_a_id" Decode.int)
        (Decode.field "player_b_id" Decode.int)
        (Decode.field "winner_id" Decode.int)
        (Decode.field "player_a_rating_before" Decode.int)
        (Decode.field "player_b_rating_before" Decode.int)
        (Decode.field "player_a_rating_after" Decode.int)
        (Decode.field "player_b_rating_after" Decode.int)
    |> Decode.andThen (\partial ->
        Decode.field "k_factor_used" Decode.int
            |> Decode.map partial
    )
    |> Decode.andThen (\partial ->
        Decode.field "played_at" (Decode.string |> Decode.andThen decodeIsoTime)
            |> Decode.map partial
    )


leagueStateDecoder : Decoder LeagueState
leagueStateDecoder =
    Decode.map6 LeagueState
        (Decode.field "id" Decode.int)
        (Decode.maybe (Decode.field "current_match_player_a" Decode.int))
        (Decode.maybe (Decode.field "current_match_player_b" Decode.int))
        (Decode.field "votes_until_sync" Decode.int)
        (Decode.field "last_sync_at" (Decode.string |> Decode.andThen decodeIsoTime))
        (Decode.field "updated_at" (Decode.string |> Decode.andThen decodeIsoTime))


decodeIsoTime : String -> Decoder Time.Posix
decodeIsoTime isoString =
    -- This is a simplified decoder - you may want to use elm/time extras
    case String.toInt (String.left 10 (String.replace "-" "" (String.replace "T" "" isoString))) of
        Just timestamp ->
            Decode.succeed (Time.millisToPosix (timestamp * 1000))
        Nothing ->
            Decode.fail ("Invalid ISO time: " ++ isoString)


-- ENCODERS


encodePlayer : Player -> Value
encodePlayer player =
    Encode.object
        [ ( "id", Encode.int player.id )
        , ( "name", Encode.string player.name )
        , ( "rating", Encode.int player.rating )
        , ( "matches_played", Encode.int player.matchesPlayed )
        , ( "plays_am", Encode.bool player.playsAM )
        , ( "plays_pm", Encode.bool player.playsPM )
        ]


encodeIsoTime : Time.Posix -> String
encodeIsoTime time =
    let
        millis = Time.posixToMillis time
        totalSeconds = millis // 1000
        second = Basics.modBy 60 totalSeconds
        minute = Basics.modBy 60 (totalSeconds // 60)
        hour = Basics.modBy 24 (totalSeconds // 3600)
        days = totalSeconds // 86400
        year = 2025 -- Hardcoded for now, can be improved
        month = 10  -- Hardcoded for now
        day = 8     -- Hardcoded for now
    in
        String.fromInt year ++ "-" ++ String.padLeft 2 '0' (String.fromInt month) ++ "-" ++ String.padLeft 2 '0' (String.fromInt day)
            ++ "T" ++ String.padLeft 2 '0' (String.fromInt hour) ++ ":" ++ String.padLeft 2 '0' (String.fromInt minute) ++ ":" ++ String.padLeft 2 '0' (String.fromInt second) ++ "Z"


encodeMatch : Match -> Value
encodeMatch match =
    Encode.object
        [ ( "player_a_id", Encode.int match.playerAId )
        , ( "player_b_id", Encode.int match.playerBId )
        , ( "winner_id", Encode.int match.winnerId )
        , ( "player_a_rating_before", Encode.int match.playerARatingBefore )
        , ( "player_b_rating_before", Encode.int match.playerBRatingBefore )
        , ( "player_a_rating_after", Encode.int match.playerARatingAfter )
        , ( "player_b_rating_after", Encode.int match.playerBRatingAfter )
        , ( "k_factor_used", Encode.int match.kFactorUsed )
        , ( "played_at", Encode.string (encodeIsoTime match.playedAt) )
        ]


-- HTTP HELPERS


supabaseRequest : Config -> String -> String -> Http.Body -> Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
supabaseRequest config method endpoint body decoder toMsg =
    Http.request
        { method = method
        , headers = 
            [ Http.header "apikey" config.anonKey
            , Http.header "Authorization" ("Bearer " ++ config.anonKey)
            , Http.header "Content-Type" "application/json"
            , Http.header "Prefer" "return=representation"
            ]
        , url = config.url ++ "/rest/v1" ++ endpoint
        , body = body
        , expect = Http.expectJson toMsg decoder
        , timeout = Nothing
        , tracker = Nothing
        }


-- API FUNCTIONS


getPlayers : Config -> (Result Http.Error (List Player) -> msg) -> Cmd msg
getPlayers config toMsg =
    supabaseRequest config "GET" "/players?order=rating.desc" Http.emptyBody (Decode.list playerDecoder) toMsg


-- Create a new player (without ID, Supabase will assign one)
createNewPlayer : Config -> String -> Int -> Bool -> Bool -> (Result Http.Error Player -> msg) -> Cmd msg
createNewPlayer config name rating playsAM playsPM toMsg =
    let
        now = Time.millisToPosix 1728396000000 -- Oct 8, 2025 12:00 PM UTC
        playerData = Encode.object
            [ ("name", Encode.string name)
            , ("rating", Encode.int rating)
            , ("matches_played", Encode.int 0)
            , ("plays_am", Encode.bool playsAM)
            , ("plays_pm", Encode.bool playsPM)
            , ("is_ignored", Encode.bool False)
            , ("created_at", Encode.string (encodeIsoTime now))
            , ("updated_at", Encode.string (encodeIsoTime now))
            ]
    in
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "apikey" config.anonKey
            , Http.header "Authorization" ("Bearer " ++ config.anonKey)
            , Http.header "Content-Type" "application/json"
            , Http.header "Prefer" "return=representation"
            ]
        , url = config.url ++ "/rest/v1/players"
        , body = Http.jsonBody playerData
        , expect = Http.expectJson toMsg (Decode.index 0 playerDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }

createPlayer : Config -> Player -> (Result Http.Error Player -> msg) -> Cmd msg
createPlayer config player toMsg =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "apikey" config.anonKey
            , Http.header "Authorization" ("Bearer " ++ config.anonKey)
            , Http.header "Content-Type" "application/json"
            , Http.header "Prefer" "resolution=merge-duplicates,return=representation"
            ]
        , url = config.url ++ "/rest/v1/players?on_conflict=id"
        , body = Http.jsonBody (encodePlayer player)
        , expect = Http.expectJson toMsg playerDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


updatePlayer : Config -> Int -> Player -> (Result Http.Error Player -> msg) -> Cmd msg
updatePlayer config playerId player toMsg =
    supabaseRequest config "PATCH" ("/players?id=eq." ++ String.fromInt playerId) (Http.jsonBody (encodePlayer player)) playerDecoder toMsg


deletePlayer : Config -> Int -> (Result Http.Error () -> msg) -> Cmd msg
deletePlayer config playerId toMsg =
    Http.request
        { method = "DELETE"
        , headers = 
            [ Http.header "apikey" config.anonKey
            , Http.header "Authorization" ("Bearer " ++ config.anonKey)
            ]
        , url = config.url ++ "/rest/v1/players?id=eq." ++ String.fromInt playerId
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


recordMatch : Config -> Match -> (Result Http.Error Match -> msg) -> Cmd msg
recordMatch config match toMsg =
    supabaseRequest config "POST" "/matches" (Http.jsonBody (encodeMatch match)) (Decode.index 0 matchDecoder) toMsg


getLeagueState : Config -> (Result Http.Error LeagueState -> msg) -> Cmd msg
getLeagueState config toMsg =
    supabaseRequest config "GET" "/league_state?id=eq.1&limit=1" Http.emptyBody (Decode.index 0 leagueStateDecoder) toMsg


updateLeagueState : Config -> LeagueState -> (Result Http.Error LeagueState -> msg) -> Cmd msg
updateLeagueState config state toMsg =
    let
        body = Encode.object
            [ ( "votes_until_sync", Encode.int state.votesUntilSync )
            , ( "updated_at", Encode.string (encodeIsoTime state.updatedAt) )
            ]
    in
    supabaseRequest config "PATCH" "/league_state?id=eq.1" (Http.jsonBody body) (Decode.index 0 leagueStateDecoder) toMsg


-- REAL-TIME SUBSCRIPTIONS (WebSocket-based)
-- This would require additional WebSocket integration


subscribeToPlayers : Config -> (String -> msg) -> Cmd msg
subscribeToPlayers config toMsg =
    -- This is a placeholder for real-time subscriptions
    -- You would implement WebSocket connection to Supabase realtime here
    Cmd.none


-- Call Supabase Edge Function for voting and Elo update
voteEdgeFunction : Config -> Int -> Int -> Int -> (Result Http.Error () -> msg) -> Cmd msg
voteEdgeFunction config aId bId winnerId toMsg =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "apikey" config.anonKey
            , Http.header "Authorization" ("Bearer " ++ config.anonKey)
            , Http.header "Content-Type" "application/json"
            ]
        , url = config.url ++ "/functions/v1/vote"
        , body = Http.jsonBody (Encode.object
            [ ("a_id", Encode.int aId)
            , ("b_id", Encode.int bId)
            , ("winner", Encode.int winnerId)
            ])
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }