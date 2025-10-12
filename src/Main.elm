port module Main exposing (..)

import Html.Styled exposing (Html, toUnstyled)
import Html.Styled as Html
import Html.Styled.Attributes as StyledAttributes exposing (css)
import Html.Styled.Events as Events
import Html.Styled.Keyed as Keyed
import Css
import Css.Media as Media
import Css.Reset
import Dict
import League exposing (League, Match, Outcome, getPlayer, currentMatch, init)
import Player exposing (Player)
import Supabase
import Config
import Elo
import History
import Time
import Set
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Task
import Process
import File
import File.Select as Select
import Browser
import Browser.Dom as Dom
import Browser.Events

type TimeFilter
    = All
    | AMOnly
    | PMOnly

type Msg
    = GotNextMatch (Maybe League.Match)
    | MatchFinished League.Outcome
    | PeriodicSync
    | TriggerReload
    | KeeperWantsToUndo
    | KeeperWantsToShowCustomMatchup
    | KeeperWantsToHideCustomMatchup
    | KeeperSelectedPlayerA Player
    | KeeperSelectedPlayerB Player
    | KeeperWantsToStartCustomMatch
    | KeeperUpdatedPlayerASearch String
    | KeeperUpdatedPlayerBSearch String
    | LoadedLeague (Result String League)
    | GotPlayers (Result Http.Error (List Supabase.Player))
    | ReceivedAutoSave Bool
    | ToggleAutoSave
    | ShowStatus String
    | ClearStatus
    | KeyPressed String
    | IgnoredKey
    | TogglePlayerAM Player
    | TogglePlayerPM Player
    | ConfirmTogglePlayerTime Player String Bool -- Player, "AM" or "PM", and new state
    | CancelTogglePlayerTime
    | SetTimeFilter TimeFilter
    | ReceivedTimeFilter String
    | ReceivedIgnoredPlayers String
    | ReceivedCurrentTime Time.Posix
    | MatchSaved (Result Http.Error ())
    | LeagueStateSaved (Result Http.Error Supabase.LeagueState)
    | PlayerACreated League.Outcome (Result Http.Error Supabase.Player)
    | PlayerBCreated League.Outcome (Result Http.Error Supabase.Player)
    | NewPlayerCreated (Result Http.Error Supabase.Player)
    | PlayerDeleted (Result Http.Error ())
    | KeeperUpdatedNewPlayerName String
    | KeeperWantsToAddNewPlayer
    | KeeperWantsToShowAddPlayerPopup
    | KeeperWantsToHideAddPlayerPopup
    | KeeperUpdatedAddPlayerName String
    | KeeperUpdatedAddPlayerRating String
    | KeeperToggledAddPlayerAM
    | KeeperToggledAddPlayerPM
    | KeeperConfirmedAddPlayer
    | CheckedExistingByName (Result Http.Error (List Supabase.Player))
    | KeeperWantsToRestorePlayer Int
    | PlayerRestored (Result Http.Error ())
    | KeeperWantsToRetirePlayer Player
    | ConfirmPlayerDeletion Player Int
    | CancelPlayerDeletion
    | KeeperWantsToIgnorePlayer Player
    | KeeperWantsToUnignorePlayer Player
    | KeeperWantsToSkipMatch





-- HELPER FUNCTIONS AND PORTS

-- CSS helper is now imported from Html.Styled.Attributes



-- JSON encode helper
encode : Int -> Encode.Value -> String
encode indent value =
    Encode.encode indent value

-- Generate checksum for league data
generateChecksum : League -> String
generateChecksum league =
    "checksum-" ++ String.fromInt (League.players league |> List.length)

-- Versioned league decoder
versionedLeagueDecoder : Decode.Decoder VersionedLeague
versionedLeagueDecoder =
    Decode.map4 VersionedLeague
        (Decode.field "version" Decode.int)
        (Decode.field "timestamp" Decode.int)
        (Decode.field "checksum" Decode.string)
        (Decode.field "league" League.decoder)

-- Port stubs for local storage (can be kept for local persistence)
port askForAutoSave : String -> Cmd msg
port askForTimeFilter : String -> Cmd msg
port askForIgnoredPlayers : String -> Cmd msg
port receiveAutoSave : (Bool -> msg) -> Sub msg
port receiveTimeFilter : (String -> msg) -> Sub msg
port receiveIgnoredPlayers : (String -> msg) -> Sub msg
port saveIgnoredPlayers : String -> Cmd msg

-- TYPES

type alias Flags = {}

type alias Model =
    { history : History.History League
    , newPlayerName : String
    , autoSave : Bool
    , status : Maybe String
    , isStatusTemporary : Bool
    , lastSynced : Maybe Time.Posix
    , shouldStartNextMatchAfterLoad : Bool
    , autoSaveInProgress : Bool
    , timeFilter : TimeFilter
    , pendingMatch : Maybe League.Match
    , ignoredPlayers : Set.Set String
    , customMatchupPlayerA : Maybe Player
    , customMatchupPlayerB : Maybe Player
    , showCustomMatchup : Bool
    , playerASearch : String
    , playerBSearch : String
    , playerASearchResults : List Player
    , playerBSearchResults : List Player
    , playerDeletionConfirmation : Maybe (Player, Int)
    , timeToggleConfirmation : Maybe (Player, String)
    , dataVersion : Int
    , lastModified : Int
    , showAddPlayerPopup : Bool
    , addPlayerName : String
    , addPlayerRating : String
    , addPlayerAM : Bool
    , addPlayerPM : Bool
    , addPlayerNotice : Maybe { message : String, restoreId : Maybe Int }
    , votesSinceLastSync : Int
    , isSyncing : Bool
    }

type alias VersionedLeague =
    { version : Int
    , timestamp : Int
    , checksum : String
    , league : League
    }



-- INIT

init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { history = History.init 50 League.init
        , newPlayerName = ""
        , autoSave = True
        , status = Nothing
        , isStatusTemporary = False
        , lastSynced = Nothing
        , shouldStartNextMatchAfterLoad = False
        , autoSaveInProgress = False
        , timeFilter = All
        , pendingMatch = Nothing
        , ignoredPlayers = Set.empty
        , customMatchupPlayerA = Nothing
        , customMatchupPlayerB = Nothing
        , showCustomMatchup = False
        , playerASearch = ""
        , playerBSearch = ""
        , playerASearchResults = []
        , playerBSearchResults = []
        , playerDeletionConfirmation = Nothing
        , timeToggleConfirmation = Nothing
        , dataVersion = 1
        , lastModified = 0
        , showAddPlayerPopup = False
        , addPlayerName = ""
        , addPlayerRating = "500"
        , addPlayerAM = True
        , addPlayerPM = True
    , addPlayerNotice = Nothing
        , votesSinceLastSync = 0
        , isSyncing = False
        }
        , Cmd.batch [ askForAutoSave "init", askForTimeFilter "init", askForIgnoredPlayers "init", Supabase.getPlayers Config.supabaseConfig GotPlayers ]
    )
        |> startNextMatchIfPossible

-- Convert Supabase.Player to Player
supabasePlayerToPlayer : Supabase.Player -> Player.Player
supabasePlayerToPlayer supabasePlayer =
    Player.Player
        { id = Player.PlayerId supabasePlayer.id
        , name = supabasePlayer.name
        , rating = supabasePlayer.rating
        , matches = supabasePlayer.matchesPlayed
        , am = supabasePlayer.playsAM
        , pm = supabasePlayer.playsPM
        }

-- Convert Player to Supabase.Player
toSupabasePlayer : Player.Player -> Supabase.Player
toSupabasePlayer player =
    let
        playerId = case Player.id player of
            Player.PlayerId id -> id
        now = Time.millisToPosix 1728396000000 -- Oct 8, 2025 12:00 PM UTC
    in
    { id = playerId
    , name = Player.name player
    , rating = Player.rating player
    , matchesPlayed = Player.matchesPlayed player
    , playsAM = True -- Default values - adjust as needed
    , playsPM = True
    , isDeleted = False
    , createdAt = now
    , updatedAt = now
    }

-- Convert League to Supabase.LeagueState
toSupabaseLeagueState : League -> Supabase.LeagueState
toSupabaseLeagueState league =
    { id = 1
    , currentMatchPlayerA =
        case League.currentMatch league of
            Just (League.Match a _) -> 
                case Player.id a of
                    Player.PlayerId id -> Just id
            _ -> Nothing
    , currentMatchPlayerB =
        case League.currentMatch league of
            Just (League.Match _ b) -> 
                case Player.id b of
                    Player.PlayerId id -> Just id
            _ -> Nothing
    , votesUntilSync = 25
    , lastSyncAt = Time.millisToPosix 0 -- Replace with actual time if available
    , updatedAt = Time.millisToPosix 0 -- Replace with actual time if available
    }

-- Create versioned league data for saving
createVersionedLeague : Model -> VersionedLeague
createVersionedLeague model =
    let
        currentLeague = History.current model.history
    in
    { version = model.dataVersion
    , timestamp = model.lastModified
    , checksum = generateChecksum currentLeague
    , league = currentLeague
    }



httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl u ->
            "Bad URL: " ++ u

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus s ->
            "Bad status: " ++ String.fromInt s

        Http.BadBody b ->
            "Bad body: " ++ b


-- Helper to check if voting should be disabled
isVotingDisabled : Model -> Bool
isVotingDisabled model =
    model.autoSaveInProgress || model.isSyncing


hasOpenPopup : Model -> Bool
hasOpenPopup model =
    model.showAddPlayerPopup || model.showCustomMatchup


maybeAutoSave : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
maybeAutoSave ( model, cmd ) =
    if model.autoSave then
        ( model, cmd )

    else
        ( model, cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveAutoSave ReceivedAutoSave
        , receiveTimeFilter ReceivedTimeFilter
        , receiveIgnoredPlayers ReceivedIgnoredPlayers
        , Browser.Events.onKeyDown keyDecoder
        ]

-- Decoder for keyboard events
keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.map KeyPressed


-- Helper function to check if player is locally ignored
isPlayerLocallyIgnored : Player -> Model -> Bool
isPlayerLocallyIgnored player model =
    let
        (Player.PlayerId idInt) = Player.id player
    in
    Set.member (String.fromInt idInt) model.ignoredPlayers


-- Create a predicate that only applies time filter (ignore filter now handled in display)
timePlayerFilter : Model -> (Player -> Bool)
timePlayerFilter model =
    \player ->
        case model.timeFilter of
            All -> True
            AMOnly -> Player.playsAM player
            PMOnly -> Player.playsPM player


-- Helper function to set a temporary status message that auto-fades after 2 seconds
setTemporaryStatus : String -> Model -> ( Model, Cmd Msg )
setTemporaryStatus message model =
    ( { model | status = Just message, isStatusTemporary = True }
    , Task.perform (\_ -> ClearStatus) (Process.sleep 3000)
    )


startNextMatchIfPossible : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
startNextMatchIfPossible ( model, cmd ) =
    if League.currentMatch (History.current model.history) /= Nothing then
        ( model, cmd )
    else
        ( model
        , Cmd.batch
            [ cmd
            , Random.generate GotNextMatch (
                League.nextMatchFiltered (\player -> not (isPlayerLocallyIgnored player model) && timePlayerFilter model player) (History.current model.history)
              )
            ]
        )






-- Helper function to handle match finishing without recursive update calls
handleMatchFinished : League.Outcome -> Model -> ( Model, Cmd Msg )
handleMatchFinished outcome model =
    if isVotingDisabled model then
        ( model, Cmd.none )
    else
        let
            -- Get current league BEFORE finishing the match to get the active players
            currentLeague = History.current model.history
        in
        case League.currentMatch currentLeague of
            Just (League.Match playerA playerB) ->
                let
                    -- Get player IDs  
                    playerAId = case Player.id playerA of
                        Player.PlayerId id -> id
                    playerBId = case Player.id playerB of
                        Player.PlayerId id -> id
                    winnerId = case outcome of
                        League.Win { won } -> 
                            case Player.id won of
                                Player.PlayerId id -> id
                        League.Draw _ -> playerAId -- For draws, use playerA as default
                    
                    -- Send match outcome to Edge Function
                    matchCmd = Supabase.voteEdgeFunction Config.supabaseConfig playerAId playerBId winnerId MatchSaved
                    
                    -- Update history after getting player info
                    updatedHistory = History.mapPush (League.finishMatch outcome) model.history
                    updatedModel = { model | history = updatedHistory }
                in
                ( updatedModel, matchCmd )
                |> startNextMatchIfPossible
                |> maybeAutoSave

            Nothing ->
                -- No current match, just ignore
                ( model, Cmd.none )

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TogglePlayerAM player ->
            -- Show confirmation dialog for both enabling and disabling AM
            ( { model | timeToggleConfirmation = Just (player, "AM") }
            , Cmd.none
            )

        TogglePlayerPM player ->
            -- Show confirmation dialog for both enabling and disabling PM
            ( { model | timeToggleConfirmation = Just (player, "PM") }
            , Cmd.none
            )

        ConfirmTogglePlayerTime player timeType newState ->
            let
                updatedLeague =
                    History.current model.history
                        |> \league ->
                            case League.getPlayer (Player.id player) league of
                                Just p ->
                                    let 
                                        newP = 
                                            if timeType == "AM" then
                                                Player.setAM newState p
                                            else
                                                Player.setPM newState p
                                    in
                                    League.updatePlayer newP league
                                Nothing ->
                                    league
            in
            ( { model | history = History.mapPush (\_ -> updatedLeague) model.history, timeToggleConfirmation = Nothing }
            , Cmd.none
            )
                |> startNextMatchIfPossible
                |> maybeAutoSave

        CancelTogglePlayerTime ->
            ( { model | timeToggleConfirmation = Nothing }
            , Cmd.none
            )
        KeeperUpdatedNewPlayerName newPlayerName ->
            ( { model | newPlayerName = newPlayerName }, Cmd.none )

        KeeperWantsToAddNewPlayer ->
            ( { model | showAddPlayerPopup = True }, Cmd.none )

        KeeperWantsToShowAddPlayerPopup ->
            ( { model | showAddPlayerPopup = True }, Cmd.none )

        KeeperWantsToHideAddPlayerPopup ->
            ( { model 
                | showAddPlayerPopup = False
                , addPlayerName = ""
                , addPlayerRating = "500"
                , addPlayerAM = True
                , addPlayerPM = True
              }, Cmd.none )

        KeeperUpdatedAddPlayerName name ->
            let
                trimmed = String.trim name
            in
            if String.length trimmed < 1 then
                ( { model | addPlayerName = name, addPlayerNotice = Nothing }, Cmd.none )
            else
                ( { model | addPlayerName = name, addPlayerNotice = Nothing }
                , Supabase.getPlayerByName Config.supabaseConfig trimmed CheckedExistingByName
                )

        KeeperUpdatedAddPlayerRating rating ->
            ( { model | addPlayerRating = rating }, Cmd.none )

        KeeperToggledAddPlayerAM ->
            ( { model | addPlayerAM = not model.addPlayerAM }, Cmd.none )

        KeeperToggledAddPlayerPM ->
            ( { model | addPlayerPM = not model.addPlayerPM }, Cmd.none )

        KeeperConfirmedAddPlayer ->
                        -- Before creating, ensure no active/deleted duplicate exists; creation path proceeds only when no exact name match exists.
                        let
                                rating = String.toInt model.addPlayerRating |> Maybe.withDefault 500
                        in
                        ( { model
                                | showAddPlayerPopup = False
                                , addPlayerName = ""
                                , addPlayerRating = "500"
                                , addPlayerAM = True
                                , addPlayerPM = True
                                , addPlayerNotice = Nothing
                            }
                        , Supabase.createNewPlayer Config.supabaseConfig model.addPlayerName rating model.addPlayerAM model.addPlayerPM NewPlayerCreated
                        )
        CheckedExistingByName result ->
            case result of
                Ok players ->
                    case players of
                        p :: _ ->
                            let
                                noticeRec =
                                    if p.isDeleted then
                                        { message = "This player already exists, but was deleted. Would you like to restore this player?"
                                        , restoreId = Just p.id
                                        }
                                    else
                                        { message = "This player already exists as an active player."
                                        , restoreId = Nothing
                                        }
                            in
                            ( { model | addPlayerNotice = Just noticeRec }, Cmd.none )
                        [] ->
                            ( { model | addPlayerNotice = Nothing }, Cmd.none )
                Err _ ->
                    -- On error, do not block; just clear notice
                    ( { model | addPlayerNotice = Nothing }, Cmd.none )

        KeeperWantsToRestorePlayer playerId ->
            ( model, Supabase.restorePlayer Config.supabaseConfig playerId PlayerRestored )

        PlayerRestored result ->
            case result of
                Ok _ ->
                    -- This branch is used for both "restore player" and undo completion
                    let
                        statusMsg =
                            if model.showAddPlayerPopup then
                                "Player restored"
                            else
                                "Undid last vote"
                    in
                    ( { model | addPlayerNotice = Nothing, showAddPlayerPopup = False, status = Just statusMsg, isStatusTemporary = True }
                    , Task.perform (\_ -> TriggerReload) (Process.sleep 300)
                    )
                Err err ->
                    ( { model | status = Just (if model.showAddPlayerPopup then
                                                    "Failed to restore player: " ++ httpErrorToString err
                                                else
                                                    "Failed to undo last vote: " ++ httpErrorToString err
                                            )
                      , isStatusTemporary = False }
                    , Cmd.none
                    )

        KeeperWantsToRetirePlayer player ->
            ( { model | playerDeletionConfirmation = Just (player, 1) }
            , Cmd.none
            )

        ConfirmPlayerDeletion player step ->
            if step == 2 then
                -- Final confirmation - try to delete from Supabase, but also mark as ignored locally
                let
                    (Player.PlayerId playerId) = Player.id player
                    -- Also mark player as ignored locally as a fallback
                    newIgnoredPlayers = Set.insert (String.fromInt playerId) model.ignoredPlayers
                    serializedIgnored = String.join "," (Set.toList newIgnoredPlayers)
                in
                ( { model 
                    | playerDeletionConfirmation = Nothing
                    , history = History.mapPush (League.retirePlayer player) model.history
                    , ignoredPlayers = newIgnoredPlayers
                  }
                , Cmd.batch 
                    [ Supabase.retirePlayer Config.supabaseConfig playerId PlayerDeleted
                    , saveIgnoredPlayers serializedIgnored
                    ]
                )
                    |> startNextMatchIfPossible
                    |> maybeAutoSave
            else
                -- Move to step 2 confirmation
                ( { model | playerDeletionConfirmation = Just (player, 2) }
                , Cmd.none
                )

        CancelPlayerDeletion ->
            ( { model | playerDeletionConfirmation = Nothing }
            , Cmd.none
            )

        KeeperWantsToIgnorePlayer player ->
            let
                (Player.PlayerId idInt) = Player.id player
                newIgnoredPlayers = Set.insert (String.fromInt idInt) model.ignoredPlayers
                serializedIgnored = String.join "," (Set.toList newIgnoredPlayers)
            in
            ( { model | ignoredPlayers = newIgnoredPlayers }
            , saveIgnoredPlayers serializedIgnored
            )
                |> startNextMatchIfPossible

        KeeperWantsToUnignorePlayer player ->
            let
                (Player.PlayerId idInt) = Player.id player
                newIgnoredPlayers = Set.remove (String.fromInt idInt) model.ignoredPlayers
                serializedIgnored = String.join "," (Set.toList newIgnoredPlayers)
            in
            ( { model | ignoredPlayers = newIgnoredPlayers }
            , saveIgnoredPlayers serializedIgnored
            )
                |> startNextMatchIfPossible

        KeeperWantsToSkipMatch ->
            ( { model | history = History.mapPush League.clearMatch model.history }
            , Cmd.none
            )
                |> startNextMatchIfPossible

        ClearStatus ->
            ( { model | status = Nothing, isStatusTemporary = False }
            , Cmd.none
            )

        GotNextMatch (Just match) ->
            ( { model | history = History.mapInPlace (League.startMatch match) model.history }
            , Cmd.none
            )

        GotNextMatch Nothing ->
            ( model, Cmd.none )

        MatchFinished outcome ->
            handleMatchFinished outcome model
        MatchSaved result ->
            case result of
                Ok _ -> 
                    let
                        newVoteCount = model.votesSinceLastSync + 1
                        shouldSync = newVoteCount >= 25
                    in
                    if shouldSync then
                        -- Sync every 25 votes to keep data fresh but not disruptive
                        ( { model | status = Just "Syncing data...", isStatusTemporary = False, votesSinceLastSync = 0 }
                        , Task.perform (\_ -> TriggerReload) (Process.sleep 200)
                        )
                    else
                        -- Just update vote count - no reload needed!
                        ( { model | status = Nothing, votesSinceLastSync = newVoteCount }
                        , Cmd.none
                        )
                Err err -> 
                    -- Show error and debug info together, persist for 10 seconds
                    let
                        debugInfo = case model.status of
                            Just s -> s
                            Nothing -> ""
                        errorMsg = "Failed to save match: " ++ httpErrorToString err ++ "\n" ++ debugInfo ++ " (Try voting again)"
                    in
                    ( { model | status = Just errorMsg, isStatusTemporary = False }
                    , Cmd.none
                    )

        LeagueStateSaved result ->
            case result of
                Ok _ -> ( model, Cmd.none )
                Err err -> ( { model | status = Just ("Failed to update league state: " ++ httpErrorToString err), isStatusTemporary = False }, Cmd.none )

        PlayerACreated _ result ->
            case result of
                Ok supabasePlayer ->
                    let
                        player = supabasePlayerToPlayer supabasePlayer
                    in
                    ( { model | history = History.mapPush (League.addPlayer player) model.history }
                    , Cmd.none
                    )
                        |> startNextMatchIfPossible
                        |> maybeAutoSave
                Err err ->
                    ( { model | status = Just ("Failed to create player: " ++ httpErrorToString err), isStatusTemporary = False }
                    , Cmd.none
                    )

        PlayerBCreated _ result ->
            -- Unused - Edge Function handles everything now
            ( model, Cmd.none )

        NewPlayerCreated result ->
            case result of
                Ok supabasePlayer ->
                    let
                        player = supabasePlayerToPlayer supabasePlayer
                    in
                    ( { model | history = History.mapPush (League.addPlayer player) model.history }
                    , Cmd.none
                    )
                        |> startNextMatchIfPossible
                        |> maybeAutoSave
                Err err ->
                    ( { model | status = Just ("Failed to create player: " ++ httpErrorToString err), isStatusTemporary = False }
                    , Cmd.none
                    )

        PlayerDeleted result ->
            case result of
                Ok _ ->
                    -- Player successfully retired in Supabase - trigger reload to sync
                    ( { model | status = Just "Player retired", isStatusTemporary = True }
                    , Task.perform (\_ -> TriggerReload) (Process.sleep 500)
                    )
                Err err ->
                    ( { model | status = Just ("Failed to retire player in database: " ++ httpErrorToString err), isStatusTemporary = False }
                    , Cmd.none
                    )

        PeriodicSync ->
            -- With Supabase, periodic sync is automatic - no action needed
            ( model, Cmd.none )
            
        TriggerReload ->
            -- Reload players from Supabase
            ( { model | votesSinceLastSync = 0, isSyncing = True }, Supabase.getPlayers Config.supabaseConfig GotPlayers )





        KeeperWantsToUndo ->
            let
                newHistory = History.goBack model.history |> Maybe.withDefault model.history
            in
            ( { model | history = newHistory }
            , Supabase.undoEdgeFunction Config.supabaseConfig (\res ->
                case res of
                    Ok _ -> PlayerRestored (Ok ()) -- reuse a success branch to trigger reload below
                    Err e -> PlayerRestored (Err e)
              )
            )


        KeeperWantsToShowCustomMatchup ->
            ( { model | showCustomMatchup = True, customMatchupPlayerA = Nothing, customMatchupPlayerB = Nothing, playerASearch = "", playerBSearch = "", playerASearchResults = [], playerBSearchResults = [] }
            , Cmd.none
            )

        KeeperWantsToHideCustomMatchup ->
            ( { model | showCustomMatchup = False, customMatchupPlayerA = Nothing, customMatchupPlayerB = Nothing, playerASearch = "", playerBSearch = "", playerASearchResults = [], playerBSearchResults = [] }
            , Cmd.none
            )

        KeeperSelectedPlayerA player ->
            ( { model | customMatchupPlayerA = Just player, playerASearch = Player.name player, playerASearchResults = [] }
            , Cmd.none
            )

        KeeperSelectedPlayerB player ->
            ( { model | customMatchupPlayerB = Just player, playerBSearch = Player.name player, playerBSearchResults = [] }
            , Cmd.none
            )

        KeeperUpdatedPlayerASearch searchText ->
            let
                currentLeague = History.current model.history
                allPlayers = 
                    currentLeague
                        |> League.players
                        |> List.filter (\p -> not (League.isPlayerIgnored p currentLeague))
                        |> List.sortBy Player.name
                searchResults = 
                    if String.length searchText < 2 then
                        []
                    else
                        allPlayers
                            |> List.filter (\p -> String.contains (String.toLower searchText) (String.toLower (Player.name p)))
                            |> List.take 8
            in
            ( { model | playerASearch = searchText, playerASearchResults = searchResults }
            , Cmd.none
            )

        KeeperUpdatedPlayerBSearch searchText ->
            let
                currentLeague = History.current model.history
                allPlayers = 
                    currentLeague
                        |> League.players
                        |> List.filter (\p -> not (League.isPlayerIgnored p currentLeague))
                        |> List.sortBy Player.name
                searchResults = 
                    if String.length searchText < 2 then
                        []
                    else
                        allPlayers
                            |> List.filter (\p -> String.contains (String.toLower searchText) (String.toLower (Player.name p)))
                            |> List.take 8
            in
            ( { model | playerBSearch = searchText, playerBSearchResults = searchResults }
            , Cmd.none
            )

        KeeperWantsToStartCustomMatch ->
            case (model.customMatchupPlayerA, model.customMatchupPlayerB) of
                (Just playerA, Just playerB) ->
                    if Player.id playerA == Player.id playerB then
                        setTemporaryStatus "Cannot match a player against themselves" model
                    else
                        let
                            -- Clear any existing match first, then start the custom match
                            updatedHistory = History.mapPush (League.clearMatch >> League.startMatch (League.Match playerA playerB)) model.history
                        in
                        ( { model 
                            | history = updatedHistory
                            , showCustomMatchup = False
                            , customMatchupPlayerA = Nothing
                            , customMatchupPlayerB = Nothing
                            , playerASearch = ""
                            , playerBSearch = ""
                            , playerASearchResults = []
                            , playerBSearchResults = []
                          }
                        , Cmd.none
                        )
                        
                _ ->
                    setTemporaryStatus "Please select both players for the custom match" model

        LoadedLeague (Ok league) ->
            -- Ignore local league data - always use Supabase data instead
            ( model, Cmd.none )

        LoadedLeague (Err problem) ->
            ( { model | status = Just ("Failed to load standings: " ++ problem), isStatusTemporary = False }
            , Cmd.none
            )

        GotPlayers result ->
            case result of
                Ok supabasePlayers ->
                    let
                        -- Filter out players with invalid hash-based IDs but keep ignored players for display
                        validIdPlayers = List.filter (\p -> p.id >= 1 && p.id < 1000000) supabasePlayers
                        invalidSupabasePlayers = List.filter (\p -> p.id < 1 || p.id >= 1000000) supabasePlayers
                        
                        players = List.map supabasePlayerToPlayer validIdPlayers
                        newLeague = List.foldl League.addPlayer League.init players
                        playerCount = List.length players
                        invalidCount = List.length invalidSupabasePlayers
                        
                        -- Preserve current match if syncing, otherwise start fresh
                        currentLeague = History.current model.history
                        activeMatch = League.currentMatch currentLeague
                        
                        finalLeague = case activeMatch of
                            Just match -> 
                                -- If there's a current match and we're syncing, preserve it
                                if model.isSyncing then
                                    League.startMatch match newLeague
                                else
                                    newLeague
                            Nothing -> 
                                newLeague
                        
                        statusMsg = 
                            if invalidCount > 0 then
                                "Filtered out " ++ String.fromInt invalidCount ++ " players with invalid IDs"
                            else
                                ""
                    in
                    let
                        updatedModel = { model | history = History.init 50 finalLeague, votesSinceLastSync = 0, isSyncing = False }
                    in
                    ( updatedModel
                    , if String.isEmpty statusMsg then
                        Cmd.none
                      else
                        Task.succeed (ShowStatus statusMsg) |> Task.perform identity
                    )
                        |> (if activeMatch == Nothing then startNextMatchIfPossible else identity)

                Err httpErr ->
                    ( { model | status = Just ("Failed to fetch players from Supabase: " ++ httpErrorToString httpErr), isStatusTemporary = False }
                    , Cmd.none
                    )



        ReceivedCurrentTime time ->
            let
                timestamp = Time.posixToMillis time
                updatedModel = { model | lastModified = timestamp }
            in
            ( updatedModel, Cmd.none )



        ReceivedAutoSave value ->
            ( { model | autoSave = value }, Cmd.none )



        ToggleAutoSave ->
            let
                newVal = not model.autoSave
            in
            ( { model | autoSave = newVal }
            , Task.succeed
                (ShowStatus
                    (if newVal then "Auto-save enabled" else "Auto-save disabled")
                )
                |> Task.perform identity
            )

        ShowStatus message ->
            ( { model | status = Just message, isStatusTemporary = False }
            , Cmd.none
            )

        KeyPressed key ->
            -- Ignore keyboard shortcuts if there's a popup open
            if hasOpenPopup model then
                ( model, Cmd.none )
            else
                case ( key, League.currentMatch (History.current model.history) ) of
                    ( "1", Just (League.Match playerA playerB) ) ->
                        -- Left player wins
                        if isVotingDisabled model then
                            setTemporaryStatus "Voting disabled during sync" model
                        else
                            let
                                outcome = League.Win { won = playerA, lost = playerB }
                            in
                            handleMatchFinished outcome model

                    ( "2", Just (League.Match playerA playerB) ) ->
                        -- Right player wins  
                        if isVotingDisabled model then
                            setTemporaryStatus "Voting disabled during sync" model
                        else
                            let
                                outcome = League.Win { won = playerB, lost = playerA }
                            in
                            handleMatchFinished outcome model

                    ( "0", Just (League.Match playerA playerB) ) ->
                        -- Tie/Draw
                        if isVotingDisabled model then
                            setTemporaryStatus "Voting disabled during sync" model
                        else
                            let
                                outcome = League.Draw { playerA = playerA, playerB = playerB }
                            in
                            handleMatchFinished outcome model

                    ( "Escape", _ ) ->
                        -- Skip match
                        ( { model | history = History.mapInPlace League.clearMatch model.history }
                        , Cmd.none
                        )
                            |> startNextMatchIfPossible

                    _ ->
                        ( model, Cmd.none )

        IgnoredKey ->
            ( model, Cmd.none )

        SetTimeFilter tf ->
            ( { model | timeFilter = tf, history = History.mapInPlace League.clearMatch model.history }
            , Cmd.none
            )
                |> startNextMatchIfPossible

        ReceivedTimeFilter raw ->
            let
                tf = parseFilter raw |> Maybe.withDefault All
            in
            ( { model | timeFilter = tf }
            , Cmd.none
            )

        ReceivedIgnoredPlayers raw ->
            let
                ignoredIds = 
                    if String.isEmpty raw then
                        Set.empty
                    else
                        String.split "," raw |> Set.fromList
            in
            ( { model | ignoredPlayers = ignoredIds }
            , Cmd.none
            )



-- VIEW

modernSansSerif : Css.Style
modernSansSerif =
    Css.fontFamilies [ "system-ui", "-apple-system", "BlinkMacSystemFont", "'Segoe UI'", "'Roboto'", "'Inter'", "'Helvetica Neue'", "Arial", "sans-serif" ]


view : Model -> Browser.Document Msg
view model =
    { title = "Hockey Rater 🏒"
    , body =
    [ Css.Reset.meyerV2
    , Css.Reset.borderBoxV201408
    , Html.node "style" [] [ Html.text "@keyframes spin { from { transform: rotate(0deg); } to { transform: rotate(360deg); } } .fade-overlay { animation: fadeIn 0.5s; } @keyframes fadeIn { from { opacity: 0; } to { opacity: 1; } }" ]

        , Html.div [ css [ Css.width (Css.pct 100) ] ]
            [ Html.main_
                [ css
                    [ Css.maxWidth (Css.px 1024)
                    , Css.margin2 Css.zero Css.auto
                    ]
                ]
                [ currentMatch model
                , filterBar model
                , rankings model
                ]
            ]
        ]
        ++ (if model.isSyncing then
                [ Html.div
                    [ css
                        [ Css.position Css.fixed
                        , Css.top Css.zero
                        , Css.left Css.zero
                        , Css.width (Css.pct 100)
                        , Css.height (Css.pct 100)
                        , Css.backgroundColor (Css.rgba 0 0 0 0.6)
                        , Css.displayFlex
                        , Css.alignItems Css.center
                        , Css.justifyContent Css.center
                        , Css.zIndex (Css.int 2000)
                        ]
                    , StyledAttributes.class "fade-overlay"
                    ]
                    [ Html.div
                        [ css
                            [ Css.backgroundColor (Css.hex "FFFFFF")
                            , Css.borderRadius (Css.px 12)
                            , Css.padding (Css.px 32)
                            , Css.boxShadow4 (Css.px 0) (Css.px 8) (Css.px 32) (Css.rgba 0 0 0 0.3)
                            , Css.textAlign Css.center
                            , modernSansSerif
                            ]
                        ]
                        [ Html.div
                            [ css
                                [ Css.width (Css.px 40)
                                , Css.height (Css.px 40)
                                , Css.border3 (Css.px 4) Css.solid (Css.hex "E5E7EB")
                                , Css.borderTopColor (Css.hex "3B82F6")
                                , Css.borderRadius (Css.pct 50)
                                , Css.property "animation" "spin 1s linear infinite"
                                , Css.margin2 (Css.px 0) Css.auto
                                , Css.marginBottom (Css.px 16)
                                ]
                            ] []
                        , Html.p
                            [ css
                                [ Css.fontSize (Css.px 16)
                                , Css.fontWeight (Css.int 500)
                                , Css.color (Css.hex "1F2937")
                                , Css.margin Css.zero
                                ]
                            ]
                            [ Html.text "Syncing with database..." ]
                        , Html.p
                            [ css
                                [ Css.fontSize (Css.px 14)
                                , Css.color (Css.hex "6B7280")
                                , Css.margin Css.zero
                                , Css.marginTop (Css.px 8)
                                ]
                            ]
                            [ Html.text "Your next match will start shortly" ]
                        ]
                    ]
                ]
            else
                []
           )
        ++ (case model.playerDeletionConfirmation of
                Just (player, step) ->
                    [ Html.div
                        [ css
                            [ Css.position Css.fixed
                            , Css.top Css.zero
                            , Css.left Css.zero
                            , Css.width (Css.pct 100)
                            , Css.height (Css.pct 100)
                            , Css.backgroundColor (Css.rgba 0 0 0 0.5)
                            , Css.displayFlex
                            , Css.alignItems Css.center
                            , Css.justifyContent Css.center
                            , Css.zIndex (Css.int 1500)
                            ]
                        ]
                        [ Html.div
                            [ css
                                [ Css.backgroundColor (Css.hex "FFFFFF")
                                , Css.borderRadius (Css.px 12)
                                , Css.padding (Css.px 24)
                                , Css.boxShadow4 (Css.px 0) (Css.px 8) (Css.px 32) (Css.rgba 0 0 0 0.3)
                                , Css.maxWidth (Css.px 400)
                                , Css.textAlign Css.center
                                , modernSansSerif
                                ]
                            ]
                            [ Html.h3
                                [ css 
                                    [ Css.margin2 (Css.px 0) (Css.px 0)
                                    , Css.marginBottom (Css.px 16)
                                    , Css.fontSize (Css.px 18)
                                    , Css.fontWeight (Css.int 600)
                                    , Css.color (Css.hex "DC2626")
                                    ]
                                ]
                                [ Html.text (if step == 1 then "Delete Player?" else "Final Warning!") ]
                            , Html.p
                                [ css 
                                    [ Css.margin2 (Css.px 0) (Css.px 0)
                                    , Css.marginBottom (Css.px 24)
                                    , Css.fontSize (Css.px 16)
                                    , Css.color (Css.hex "374151")
                                    ]
                                ]
                                [ Html.text (if step == 1 
                                    then "Are you sure you want to delete " ++ Player.name player ++ "?"
                                    else "Is that your final answer? This cannot be undone!")
                                ]
                            , Html.div
                                [ css [ Css.displayFlex, Css.justifyContent Css.center ] ]
                                [ Html.button
                                    [ css
                                        [ Css.backgroundColor (Css.hex "6B7280")
                                        , Css.color (Css.hex "FFFFFF")
                                        , Css.border Css.zero
                                        , Css.borderRadius (Css.px 6)
                                        , Css.padding2 (Css.px 8) (Css.px 16)
                                        , Css.cursor Css.pointer
                                        , Css.fontSize (Css.px 14)
                                        , Css.fontWeight (Css.int 500)
                                        , Css.marginRight (Css.px 12)
                                        , Css.hover [ Css.backgroundColor (Css.hex "4B5563") ]
                                        ]
                                    , Events.onClick CancelPlayerDeletion
                                    ]
                                    [ Html.text "Cancel" ]
                                , Html.button
                                    [ css
                                        [ Css.backgroundColor (Css.hex "DC2626")
                                        , Css.color (Css.hex "FFFFFF")
                                        , Css.border Css.zero
                                        , Css.borderRadius (Css.px 6)
                                        , Css.padding2 (Css.px 8) (Css.px 16)
                                        , Css.cursor Css.pointer
                                        , Css.fontSize (Css.px 14)
                                        , Css.fontWeight (Css.int 500)
                                        , Css.hover [ Css.backgroundColor (Css.hex "B91C1C") ]
                                        ]
                                    , Events.onClick (ConfirmPlayerDeletion player (step + 1))
                                    ]
                                    [ Html.text (if step == 1 then "Yes, Delete" else "Final Answer: DELETE") ]
                                ]
                            ]
                        ]
                    ]

                Nothing ->
                    []
           )
        ++ (case model.timeToggleConfirmation of
                Just (player, timeType) ->
                    let
                        currentState = if timeType == "AM" then Player.playsAM player else Player.playsPM player
                        newState = not currentState
                    in
                    [ Html.div
                        [ css
                            [ Css.position Css.fixed
                            , Css.top Css.zero
                            , Css.left Css.zero
                            , Css.width (Css.pct 100)
                            , Css.height (Css.pct 100)
                            , Css.backgroundColor (Css.rgba 0 0 0 0.5)
                            , Css.displayFlex
                            , Css.alignItems Css.center
                            , Css.justifyContent Css.center
                            , Css.zIndex (Css.int 1500)
                            ]
                        ]
                        [ Html.div
                            [ css
                                [ Css.backgroundColor (Css.hex "FFFFFF")
                                , Css.borderRadius (Css.px 12)
                                , Css.padding (Css.px 24)
                                , Css.boxShadow4 (Css.px 0) (Css.px 8) (Css.px 32) (Css.rgba 0 0 0 0.3)
                                , Css.maxWidth (Css.px 400)
                                , Css.textAlign Css.center
                                , modernSansSerif
                                ]
                            ]
                            [ Html.h3
                                [ css
                                    [ Css.margin2 (Css.px 0) (Css.px 0)
                                    , Css.marginBottom (Css.px 16)
                                    , Css.fontSize (Css.px 18)
                                    , Css.fontWeight (Css.int 600)
                                    , Css.color (Css.hex "DC2626")
                                    ]
                                ]
                                [ Html.text ("Confirm " ++ timeType ++ " Toggle") ]
                            , Html.p
                                [ css
                                    [ Css.margin2 (Css.px 0) (Css.px 0)
                                    , Css.marginBottom (Css.px 24)
                                    , Css.fontSize (Css.px 16)
                                    , Css.color (Css.hex "374151")
                                    ]
                                ]
                                [ Html.text ("Are you sure you want to " ++ (if newState then "enable" else "disable") ++ " " ++ timeType ++ " availability for " ++ Player.name player ++ "?") ]
                            , Html.div
                                [ css [ Css.displayFlex, Css.justifyContent Css.center ] ]
                                [ Html.button
                                    [ css
                                        [ Css.backgroundColor (Css.hex "6B7280")
                                        , Css.color (Css.hex "FFFFFF")
                                        , Css.border Css.zero
                                        , Css.borderRadius (Css.px 6)
                                        , Css.padding2 (Css.px 8) (Css.px 16)
                                        , Css.cursor Css.pointer
                                        , Css.fontSize (Css.px 14)
                                        , Css.fontWeight (Css.int 500)
                                        , Css.marginRight (Css.px 12)
                                        , Css.hover [ Css.backgroundColor (Css.hex "4B5563") ]
                                        ]
                                    , Events.onClick CancelTogglePlayerTime
                                    ]
                                    [ Html.text "Cancel" ]
                                , Html.button
                                    [ css
                                        [ Css.backgroundColor (Css.hex "F59E0B")
                                        , Css.color (Css.hex "FFFFFF")
                                        , Css.border Css.zero
                                        , Css.borderRadius (Css.px 6)
                                        , Css.padding2 (Css.px 8) (Css.px 16)
                                        , Css.cursor Css.pointer
                                        , Css.fontSize (Css.px 14)
                                        , Css.fontWeight (Css.int 500)
                                        , Css.hover [ Css.backgroundColor (Css.hex "D97706") ]
                                        ]
                                    , Events.onClick (ConfirmTogglePlayerTime player timeType newState)
                                    ]
                                    [ Html.text (if newState then "Yes, Enable " ++ timeType else "Yes, Disable " ++ timeType) ]
                                ]
                            ]
                        ]
                    ]

                Nothing ->
                    []
           )
        ++ (if model.showAddPlayerPopup then
                [ Html.div
                    [ css
                        [ Css.position Css.fixed
                        , Css.top Css.zero
                        , Css.left Css.zero
                        , Css.width (Css.pct 100)
                        , Css.height (Css.pct 100)
                        , Css.backgroundColor (Css.rgba 0 0 0 0.5)
                        , Css.displayFlex
                        , Css.alignItems Css.center
                        , Css.justifyContent Css.center
                        , Css.zIndex (Css.int 1500)
                        ]
                    ]
                    [ Html.div
                        [ css
                            [ Css.backgroundColor (Css.hex "FFFFFF")
                            , Css.borderRadius (Css.px 12)
                            , Css.padding (Css.px 32)
                            , Css.boxShadow4 (Css.px 0) (Css.px 20) (Css.px 25) (Css.rgba 0 0 0 0.15)
                            , Css.maxWidth (Css.px 500)
                            , Css.width (Css.pct 90)
                            , modernSansSerif
                            ]
                        ]
                        [ Html.h2
                            [ css
                                [ Css.fontSize (Css.px 24)
                                , Css.fontWeight (Css.int 600)
                                , Css.color (Css.hex "1F2937")
                                , Css.marginBottom (Css.px 24)
                                , Css.textAlign Css.center
                                ]
                            ]
                            [ Html.text "Add New Player" ]
                        , Html.div
                            [ css [ Css.marginBottom (Css.px 20) ] ]
                            [ Html.label
                                [ css
                                    [ Css.display Css.block
                                    , Css.fontSize (Css.px 14)
                                    , Css.fontWeight (Css.int 600)
                                    , Css.color (Css.hex "374151")
                                    , Css.marginBottom (Css.px 6)
                                    ]
                                ]
                                [ Html.text "Player Name" ]
                            , Html.input
                                [ StyledAttributes.type_ "text"
                                , StyledAttributes.placeholder "Enter player name"
                                , StyledAttributes.value model.addPlayerName
                                , Events.onInput KeeperUpdatedAddPlayerName
                                , css
                                    [ Css.width (Css.pct 100)
                                    , Css.padding2 (Css.px 12) (Css.px 16)
                                    , Css.border3 (Css.px 2) Css.solid (Css.hex "E5E7EB")
                                    , Css.borderRadius (Css.px 8)
                                    , Css.fontSize (Css.px 16)
                                    , Css.focus [ Css.borderColor (Css.hex "3B82F6"), Css.outline Css.none ]
                                    , Css.boxSizing Css.borderBox
                                    ]
                                ] []
                            ]
                        , Html.div
                            [ css [ Css.marginBottom (Css.px 20) ] ]
                            [ Html.label
                                [ css
                                    [ Css.display Css.block
                                    , Css.fontSize (Css.px 14)
                                    , Css.fontWeight (Css.int 600)
                                    , Css.color (Css.hex "374151")
                                    , Css.marginBottom (Css.px 6)
                                    ]
                                ]
                                [ Html.text "Estimated Rating" ]
                            , Html.input
                                [ StyledAttributes.type_ "number"
                                , StyledAttributes.placeholder "500"
                                , StyledAttributes.value model.addPlayerRating
                                , Events.onInput KeeperUpdatedAddPlayerRating
                                , css
                                    [ Css.width (Css.pct 100)
                                    , Css.padding2 (Css.px 12) (Css.px 16)
                                    , Css.border3 (Css.px 2) Css.solid (Css.hex "E5E7EB")
                                    , Css.borderRadius (Css.px 8)
                                    , Css.fontSize (Css.px 16)
                                    , Css.focus [ Css.borderColor (Css.hex "3B82F6"), Css.outline Css.none ]
                                    , Css.boxSizing Css.borderBox
                                    ]
                                ] []
                            ]
                        , Html.div
                            [ css [ Css.marginBottom (Css.px 24) ] ]
                            [ Html.label
                                [ css
                                    [ Css.display Css.block
                                    , Css.fontSize (Css.px 14)
                                    , Css.fontWeight (Css.int 600)
                                    , Css.color (Css.hex "374151")
                                    , Css.marginBottom (Css.px 12)
                                    ]
                                ]
                                [ Html.text "Available Times" ]
                            , Html.div
                                [ css [ Css.displayFlex, Css.justifyContent Css.spaceAround ] ]
                                [ Html.label
                                    [ css
                                        [ Css.displayFlex
                                        , Css.alignItems Css.center
                                        , Css.cursor Css.pointer
                                        , Css.fontSize (Css.px 16)
                                        ]
                                    ]
                                    [ Html.input
                                        [ StyledAttributes.type_ "checkbox"
                                        , StyledAttributes.checked model.addPlayerAM
                                        , Events.onClick KeeperToggledAddPlayerAM
                                        , css
                                            [ Css.width (Css.px 20)
                                            , Css.height (Css.px 20)
                                            , Css.marginRight (Css.px 8)
                                            ]
                                        ] []
                                    , Html.text "AM Games"
                                    ]
                                , Html.label
                                    [ css
                                        [ Css.displayFlex
                                        , Css.alignItems Css.center
                                        , Css.cursor Css.pointer
                                        , Css.fontSize (Css.px 16)
                                        ]
                                    ]
                                    [ Html.input
                                        [ StyledAttributes.type_ "checkbox"
                                        , StyledAttributes.checked model.addPlayerPM
                                        , Events.onClick KeeperToggledAddPlayerPM
                                        , css
                                            [ Css.width (Css.px 20)
                                            , Css.height (Css.px 20)
                                            , Css.marginRight (Css.px 8)
                                            ]
                                        ] []
                                    , Html.text "PM Games"
                                    ]
                                ]
                            ]
                        , Html.div
                            [ css [ Css.displayFlex, Css.justifyContent Css.spaceAround ] ]
                            [ Html.button
                                [ css
                                    [ Css.backgroundColor (Css.hex "6B7280")
                                    , Css.color (Css.hex "FFFFFF")
                                    , Css.border Css.zero
                                    , Css.borderRadius (Css.px 8)
                                    , Css.padding2 (Css.px 12) (Css.px 24)
                                    , Css.cursor Css.pointer
                                    , Css.fontSize (Css.px 16)
                                    , Css.fontWeight (Css.int 500)
                                    , Css.hover [ Css.backgroundColor (Css.hex "4B5563") ]
                                    , modernSansSerif
                                    ]
                                , Events.onClick KeeperWantsToHideAddPlayerPopup
                                ]
                                [ Html.text "Cancel" ]
                            , Html.button
                                [ css
                                    [ Css.backgroundColor (Css.hex "10B981")
                                    , Css.color (Css.hex "FFFFFF")
                                    , Css.border Css.zero
                                    , Css.borderRadius (Css.px 8)
                                    , Css.padding2 (Css.px 12) (Css.px 24)
                                    , Css.cursor Css.pointer
                                    , Css.fontSize (Css.px 16)
                                    , Css.fontWeight (Css.int 500)
                                    , Css.hover [ Css.backgroundColor (Css.hex "059669") ]
                                    , modernSansSerif
                                    , if String.isEmpty (String.trim model.addPlayerName) || Maybe.withDefault False (Maybe.map (\_ -> True) model.addPlayerNotice) then
                                        Css.batch [ Css.opacity (Css.num 0.5), Css.cursor Css.notAllowed ]
                                      else
                                        Css.batch []
                                    ]
                                , if String.isEmpty (String.trim model.addPlayerName) || Maybe.withDefault False (Maybe.map (\_ -> True) model.addPlayerNotice) then
                                    StyledAttributes.disabled True
                                  else
                                    Events.onClick KeeperConfirmedAddPlayer
                                ]
                                [ Html.text "Add Player" ]
                            ]
                                , case model.addPlayerNotice of
                                    Just note ->
                                        Html.div
                                            [ css
                                                [ Css.marginTop (Css.px 16)
                                                , Css.padding (Css.px 12)
                                                , Css.border3 (Css.px 1) Css.solid (Css.hex "FCD34D")
                                                , Css.backgroundColor (Css.hex "FEF9C3")
                                                , Css.borderRadius (Css.px 8)
                                                , modernSansSerif
                                                ]
                                            ]
                                            ([ Html.p [ css [ Css.margin Css.zero, Css.color (Css.hex "92400E") ] ] [ Html.text note.message ] ]
                                                ++ (case note.restoreId of
                                                        Just pid ->
                                                            [ Html.div [ css [ Css.marginTop (Css.px 8) ] ]
                                                                [ Html.button
                                                                    [ css
                                                                        [ Css.backgroundColor (Css.hex "10B981")
                                                                        , Css.color (Css.hex "FFFFFF")
                                                                        , Css.border Css.zero
                                                                        , Css.borderRadius (Css.px 6)
                                                                        , Css.padding2 (Css.px 8) (Css.px 16)
                                                                        , Css.cursor Css.pointer
                                                                        , Css.fontSize (Css.px 14)
                                                                        , Css.fontWeight (Css.int 600)
                                                                        , Css.hover [ Css.backgroundColor (Css.hex "059669") ]
                                                                        ]
                                                                    , Events.onClick (KeeperWantsToRestorePlayer pid)
                                                                    ]
                                                                    [ Html.text "Restore Player" ]
                                                                ]
                                                            ]
                                                        Nothing ->
                                                            []
                                                   )
                                            )
                                    Nothing ->
                                        Html.text ""
                        ]
                    ]
                ]
            else
                []
           )
        ++ (case model.status of
                Just message ->
                    [ Html.div
                        [ css
                            [ Css.position Css.fixed
                            , Css.top (Css.px 20)
                            , Css.right (Css.px 20)
                            , Css.backgroundColor (if model.autoSaveInProgress then Css.hex "EF4444" 
                                                       
                                                       else Css.hex "10B981")
                            , Css.color (Css.hex "FFFFFF")
                            , Css.padding4 (Css.px 12) (Css.px 16) (Css.px 12) (Css.px 16)
                            , Css.borderRadius (Css.px 8)
                            , Css.boxShadow4 (Css.px 0) (Css.px 4) (Css.px 12) (Css.rgba 0 0 0 0.15)
                            , Css.border3 (Css.px 1) Css.solid (Css.rgba 255 255 255 0.2)
                            , Css.property "animation" "slideInFromRight 0.3s ease-out"
                            , Css.fontSize (Css.px 14)
                            , Css.fontWeight (Css.int 500)
                            , Css.maxWidth (Css.px 300)
                            , Css.zIndex (Css.int 1000)
                            , modernSansSerif
                            ]
                        ]
                        [ Html.div 
                            [ css [ Css.displayFlex, Css.alignItems Css.center, Css.justifyContent Css.spaceBetween ] ] 
                            [ Html.span [] 
                                [ Html.text (message ++ 
                                    (if model.autoSaveInProgress then " (Voting disabled - autoSave)" 
                                     else if model.isSyncing then " (Voting disabled - syncing)"
                                     else "")) ]
                            , Html.button
                                [ css
                                    [ Css.backgroundColor Css.transparent
                                    , Css.border Css.zero
                                    , Css.color (Css.hex "FFFFFF")
                                    , Css.cursor Css.pointer
                                    , Css.fontSize (Css.px 18)
                                    , Css.fontWeight (Css.int 400)
                                    , Css.marginLeft (Css.px 12)
                                    , Css.padding Css.zero
                                    , Css.opacity (Css.num 0.8)
                                    , Css.hover [ Css.opacity (Css.num 1.0) ]
                                    ]
                                , Events.onClick ClearStatus
                                ]
                                [ Html.text "×" ]
                            ]
                        ]
                    ]

                Nothing ->
                    []
           )
            |> List.map toUnstyled
    }


filterBar : Model -> Html Msg
filterBar model =
    Html.div
        [ css [ Css.width (Css.pct 80), Css.margin2 Css.zero Css.auto, Css.marginTop (Css.px 10), Css.marginBottom (Css.px 10), Css.textAlign Css.center ] ]
        [ Html.span [ css [ Css.marginRight (Css.px 10), modernSansSerif, Css.fontWeight (Css.int 600) ] ] [ Html.text "Filter:" ]
        , toggleBtn (model.timeFilter == All) "All" (Just (SetTimeFilter All))
        , toggleBtn (model.timeFilter == AMOnly) "AM" (Just (SetTimeFilter AMOnly))
        , toggleBtn (model.timeFilter == PMOnly) "PM" (Just (SetTimeFilter PMOnly))
        ]

toggleBtn : Bool -> String -> Maybe Msg -> Html Msg
toggleBtn isOn label maybeMsg =
    Html.button
        [ css
            [ Css.padding2 (Css.px 6) (Css.px 12)
            , Css.margin2 Css.zero (Css.px 6)
            , Css.borderRadius (Css.px 9999)
            , Css.backgroundColor (Css.hex (if isOn then "3B82F6" else "6B7280"))
            , Css.color (Css.hex "FFF")
            , Css.border Css.zero
            , Css.cursor Css.pointer
            , modernSansSerif
            ]
        , case maybeMsg of
            Just m -> Events.onClick m
            Nothing -> StyledAttributes.disabled True
        ]
        [ Html.text label ]


customMatchupUI : Model -> Html Msg
customMatchupUI model =
    let
        searchInput searchValue onInput results onSelect placeholder selectedPlayer =
            Html.div [ css [ Css.position Css.relative, Css.marginBottom (Css.px 16) ] ]
                [ Html.input
                    [ css
                        [ Css.width (Css.pct 100)
                        , Css.padding (Css.px 12)
                        , Css.border3 (Css.px 2) Css.solid (case selectedPlayer of
                            Just _ -> Css.hex "10B981"
                            Nothing -> Css.hex "D1D5DB"
                          )
                        , Css.borderRadius (Css.px 8)
                        , Css.fontSize (Css.px 16)
                        , modernSansSerif
                        , Css.boxSizing Css.borderBox
                        , Css.focus [ Css.outline Css.none, Css.borderColor (Css.hex "3B82F6") ]
                        ]
                    , StyledAttributes.value searchValue
                    , StyledAttributes.placeholder placeholder
                    , Events.onInput onInput
                    ] []
                , if List.length results > 0 then
                    Html.div 
                        [ css 
                            [ Css.position Css.absolute
                            , Css.top (Css.pct 100)
                            , Css.left Css.zero
                            , Css.right Css.zero
                            , Css.backgroundColor (Css.hex "FFF")
                            , Css.border3 (Css.px 1) Css.solid (Css.hex "E5E7EB")
                            , Css.borderRadius (Css.px 8)
                            , Css.boxShadow5 Css.zero (Css.px 4) (Css.px 6) (Css.px -1) (Css.rgba 0 0 0 0.1)
                            , Css.zIndex (Css.int 10)
                            , Css.maxHeight (Css.px 200)
                            , Css.overflowY Css.auto
                            ]
                        ]
                        (List.map (\player ->
                            Html.button
                                [ css
                                    [ Css.width (Css.pct 100)
                                    , Css.padding (Css.px 12)
                                    , Css.textAlign Css.left
                                    , Css.border Css.zero
                                    , Css.backgroundColor Css.transparent
                                    , Css.cursor Css.pointer
                                    , Css.fontSize (Css.px 14)
                                    , modernSansSerif
                                    , Css.hover [ Css.backgroundColor (Css.hex "F3F4F6") ]
                                    , Css.displayFlex
                                    , Css.justifyContent Css.spaceBetween
                                    ]
                                , Events.onClick (onSelect player)
                                ]
                                [ Html.span [] [ Html.text (Player.name player) ]
                                , Html.span [ css [ Css.color (Css.hex "6B7280") ] ] 
                                    [ Html.text (String.fromInt (Player.rating player)) ]
                                ]
                        ) results)
                  else
                    Html.text ""
                ]
    in
    Html.div
        [ css 
            [ Css.backgroundColor (Css.hex "F9FAFB")
            , Css.border3 (Css.px 1) Css.solid (Css.hex "E5E7EB")
            , Css.borderRadius (Css.px 12)
            , Css.padding (Css.px 24)
            , Css.margin2 (Css.px 24) Css.auto
            , Css.maxWidth (Css.px 500)
            , modernSansSerif
            ]
        ]
        [ Html.div [ css [ Css.displayFlex, Css.justifyContent Css.spaceBetween, Css.alignItems Css.center, Css.marginBottom (Css.px 24) ] ]
            [ Html.h3 [ css [ Css.margin Css.zero, Css.fontSize (Css.px 20), Css.fontWeight (Css.int 600) ] ] 
                [ Html.text "Custom Match" ]
            , Html.button
                [ css
                    [ Css.backgroundColor Css.transparent
                    , Css.border Css.zero
                    , Css.fontSize (Css.px 24)
                    , Css.cursor Css.pointer
                    , Css.color (Css.hex "6B7280")
                    , Css.hover [ Css.color (Css.hex "374151") ]
                    ]
                , Events.onClick KeeperWantsToHideCustomMatchup
                ]
                [ Html.text "×" ]
            ]
        , Html.div [ css [ Css.marginBottom (Css.px 20) ] ]
            [ Html.label [ css [ Css.display Css.block, Css.fontSize (Css.px 14), Css.fontWeight (Css.int 600), Css.marginBottom (Css.px 8), Css.color (Css.hex "374151") ] ]
                [ Html.text "Player A" ]
            , searchInput model.playerASearch KeeperUpdatedPlayerASearch model.playerASearchResults KeeperSelectedPlayerA "Search for first player..." model.customMatchupPlayerA
            ]
        , Html.div [ css [ Css.marginBottom (Css.px 24) ] ]
            [ Html.label [ css [ Css.display Css.block, Css.fontSize (Css.px 14), Css.fontWeight (Css.int 600), Css.marginBottom (Css.px 8), Css.color (Css.hex "374151") ] ]
                [ Html.text "Player B" ]
            , searchInput model.playerBSearch KeeperUpdatedPlayerBSearch model.playerBSearchResults KeeperSelectedPlayerB "Search for second player..." model.customMatchupPlayerB
            ]
        , Html.div [ css [ Css.textAlign Css.center ] ]
            [ case (model.customMatchupPlayerA, model.customMatchupPlayerB) of
                (Just playerA, Just playerB) ->
                    if Player.id playerA == Player.id playerB then
                        Html.div [ css [ Css.color (Css.hex "EF4444"), Css.fontSize (Css.px 14), Css.marginBottom (Css.px 16) ] ]
                            [ Html.text "Please select two different players" ]
                    else
                        Html.div []
                            [ Html.div [ css [ Css.marginBottom (Css.px 16), Css.fontSize (Css.px 16), Css.color (Css.hex "374151"), Css.fontWeight (Css.int 500) ] ]
                                [ Html.text (Player.name playerA ++ " vs " ++ Player.name playerB) ]
                            , Html.button
                                [ css
                                    [ Css.backgroundColor (Css.hex "10B981")
                                    , Css.color (Css.hex "FFF")
                                    , Css.border Css.zero
                                    , Css.padding2 (Css.px 12) (Css.px 24)
                                    , Css.borderRadius (Css.px 8)
                                    , Css.fontSize (Css.px 16)
                                    , Css.fontWeight (Css.int 600)
                                    , Css.cursor Css.pointer
                                    , modernSansSerif
                                    , Css.hover [ Css.backgroundColor (Css.hex "059669") ]
                                    ]
                                , Events.onClick KeeperWantsToStartCustomMatch
                                ]
                                [ Html.text "Start Match" ]
                            ]
                _ ->
                    Html.div [ css [ Css.color (Css.hex "6B7280"), Css.fontSize (Css.px 14) ] ]
                        [ Html.text "Search and select both players to start the match" ]
            ]
        ]


currentMatch : Model -> Html Msg
currentMatch model =
    case League.currentMatch (History.current model.history) of
        Nothing ->
            Html.div
                [ css
                    [ modernSansSerif
                    , Css.textAlign Css.center
                    , Css.width (Css.pct 50)
                    , Css.margin2 (Css.px 32) Css.auto
                    ]
                ]
                [ Html.h1
                    [ css
                        [ Css.fontSize (Css.px 56)
                        , Css.marginBottom (Css.px 18)
                        , Css.fontWeight (Css.int 700)
                        , Css.fontStyle Css.italic
                        , Css.textTransform Css.uppercase
                        ]
                    ]
                    [ Html.text "Hockey Rater 🏒" ]
                , Html.p
                    [ css
                        [ Css.fontSize (Css.px 24)
                        , Css.lineHeight (Css.px 32)
                        , Css.marginBottom (Css.px 18)
                        ]
                    ]
                    [ Html.text "No current match. To get started, add at least two players!" ]
                ]

        Just (League.Match playerA playerB) ->
            let
                chanceAWins =
                    Elo.odds (Player.rating playerA) (Player.rating playerB)
            in
            Html.section
                [ css
                    [ Css.width (Css.pct 80)
                    , Css.margin2 (Css.px 32) Css.auto
                    ]
                ]
                [ Html.div
                    [ css
                        [ Css.position Css.relative
                        , Css.marginBottom (Css.px 20)
                        ]
                    ]
                    [ Html.div
                        [ css
                            [ Css.fontSize (Css.px 14)
                            , Css.fontWeight (Css.int 700)
                            , Css.textAlign Css.center
                            , Css.marginBottom (Css.px 12)
                            , Css.color (Css.hex "555")
                            , Css.letterSpacing (Css.px 1)
                            , Css.fontStyle Css.italic
                            , Css.textTransform Css.uppercase
                            , modernSansSerif
                            ]
                        ]
                        [ Html.div
                            [ css [ Css.displayFlex, Css.alignItems Css.center, Css.justifyContent Css.center ] ]
                            [ Html.span [ css [ Css.marginRight (Css.px 8) ] ] [ Html.text "🏒 HOCKEY RATER 🏒" ]
                            ]
                        ]
                    , Html.div
                        [ css
                            [ Css.position Css.relative
                            , Css.borderRadius (Css.px 20)
                            , Css.overflow Css.hidden
                            , Css.height (Css.px 32)
                            , Css.width (Css.pct 100)
                            , Css.backgroundColor (Css.hex "E8E8E8")
                            , Css.boxShadow6 Css.inset (Css.px 0) (Css.px 3) (Css.px 6) (Css.px 0) (Css.rgba 0 0 0 0.15)
                            , Css.border3 (Css.px 2) Css.solid (Css.hex "DDD")
                            ]
                        ]
                        [ Html.div
                            [ css
                                [ Css.width (Css.pct (max (if round (chanceAWins * 100) >= 100 then 20 else 16) (100 * chanceAWins)))
                                , Css.height (Css.pct 100)
                                , Css.backgroundColor (Css.hex "EF4444")  -- Modern red
                                , Css.position Css.absolute
                                , Css.left (Css.px 0)
                                , Css.top (Css.px 0)
                                , Css.boxShadow4 (Css.px 2) (Css.px 0) (Css.px 4) (Css.rgba 0 0 0 0.2)
                                ]
                            ]
                            []
                        , Html.div
                            [ css
                                [ Css.width (Css.pct (max (if round ((1 - chanceAWins) * 100) >= 100 then 20 else 16) (100 * (1 - chanceAWins))))
                                , Css.height (Css.pct 100)
                                , Css.backgroundColor (Css.hex "3B82F6")  -- Modern blue
                                , Css.position Css.absolute
                                , Css.right (Css.px 0)
                                , Css.top (Css.px 0)
                                , Css.boxShadow4 (Css.px -2) (Css.px 0) (Css.px 4) (Css.rgba 0 0 0 0.2)
                                ]
                            ]
                            []

                        , Html.div
                            [ css
                                [ Css.position Css.absolute
                                , Css.top (Css.px 4)
                                , Css.left (Css.px 8)
                                , Css.fontSize (Css.px 16)
                                , Css.fontWeight (Css.int 700)
                                , Css.fontStyle Css.italic
                                , Css.color (Css.hex "FFF")
                                , Css.textShadow4 (Css.px 1) (Css.px 1) (Css.px 2) (Css.rgba 0 0 0 0.7)
                                , modernSansSerif
                                ]
                            ]
                            [ Html.text (String.fromInt (round (chanceAWins * 100)) ++ "%") ]
                        , Html.div
                            [ css
                                [ Css.position Css.absolute
                                , Css.top (Css.px 4)
                                , Css.right (Css.px 8)
                                , Css.fontSize (Css.px 16)
                                , Css.fontWeight (Css.int 700)
                                , Css.fontStyle Css.italic
                                , Css.color (Css.hex "FFF")
                                , Css.textShadow4 (Css.px 1) (Css.px 1) (Css.px 2) (Css.rgba 0 0 0 0.7)
                                , modernSansSerif
                                ]
                            ]
                            [ Html.text (String.fromInt (round ((1 - chanceAWins) * 100)) ++ "%") ]
                        ]

                    ]
                , Html.div
                    [ css
                        [ Css.displayFlex
                        , Css.alignItems Css.center
                        , Css.paddingTop (Css.px 32)
                        , Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ]
                            [ Css.display Css.none ]
                        ]
                    ]
                    [ activePlayer playerA
                    , Html.p
                        [ css [ modernSansSerif, Css.width (Css.pct 20), Css.textAlign Css.center ] ]
                        [ Html.text "vs." ]
                    , activePlayer playerB
                    ]
                -- Mobile-specific stacked vote layout
                , Html.div
                    [ css
                        [ Css.display Css.none
                        , Css.marginTop (Css.px 24)
                        , Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ]
                            [ Css.display Css.block ]
                        ]
                    ]
                    [ -- Row 1: Player A with WINNER on the right
                      Html.div
                        [ css [ Css.displayFlex, Css.alignItems Css.center, Css.justifyContent Css.spaceBetween, Css.marginBottom (Css.px 10) ] ]
                        [ Html.div [ css [ Css.flexGrow (Css.num 1) ] ] [ activePlayerCompactWithIgnore playerA model ]
                        , Html.div []
                            [ redButtonLarge "WINNER"
                                (if isVotingDisabled model then Nothing else Just (MatchFinished (League.Win { lost = playerB, won = playerA })))
                            ]
                        ]
                    , -- Row 2: Player B with WINNER on the right
                      Html.div
                        [ css [ Css.displayFlex, Css.alignItems Css.center, Css.justifyContent Css.spaceBetween ] ]
                        [ Html.div [ css [ Css.flexGrow (Css.num 1) ] ] [ activePlayerCompactWithIgnore playerB model ]
                        , Html.div []
                            [ blueButtonLarge "WINNER"
                                (if isVotingDisabled model then Nothing else Just (MatchFinished (League.Win { won = playerB, lost = playerA })))
                            ]
                        ]
                    , -- Separator between players and the tie/skip row
                      Html.div
                        [ css [ Css.height (Css.px 4), Css.backgroundColor (Css.hex "D1D5DB"), Css.borderRadius (Css.px 2), Css.margin2 (Css.px 14) Css.zero ] ]
                        []
                    , -- Row 3: TIE and SKIP (centered on mobile)
                      Html.div
                        [ css [ Css.displayFlex, Css.alignItems Css.center, Css.justifyContent Css.center ] ]
                        [ Html.div [ css [ Css.marginRight (Css.px 4) ] ]
                            [ buttonCompact (Css.hex "1F2937") "TIE"
                                (if isVotingDisabled model then Nothing else Just (MatchFinished (League.Draw { playerA = playerA, playerB = playerB })))
                            ]
                        , Html.div [ css [ Css.marginLeft (Css.px 4) ] ]
                            [ buttonCompact (Css.hex "999") "SKIP" (Just KeeperWantsToSkipMatch) ]
                        ]
                    , -- Row 4 (new on mobile): CUSTOM MATCHUP and ADD PLAYER (centered)
                      Html.div
                        [ css [ Css.displayFlex, Css.alignItems Css.center, Css.justifyContent Css.center, Css.marginTop (Css.px 8) ] ]
                        [ Html.div [ css [ Css.marginRight (Css.px 4) ] ]
                            [ buttonCompact (Css.hex "6DD400") "CUSTOM" (Just KeeperWantsToShowCustomMatchup) ]
                        , Html.div [ css [ Css.marginLeft (Css.px 4) ] ]
                            [ buttonCompact (Css.hex "6DD400") "ADD PLAYER" (Just KeeperWantsToAddNewPlayer) ]
                        ]
                    ]
                , Html.div
                    [ css
                        [ Css.displayFlex
                        , Css.paddingTop (Css.px 32)
                        , Css.textAlign Css.center
                        , Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ]
                            [ Css.display Css.none ]
                        ]
                    ]
                    [ Html.div [ css [ Css.width (Css.pct 40) ] ]
                        [ redButton "WINNER" 
                            (if isVotingDisabled model then Nothing else Just (MatchFinished (League.Win { lost = playerB, won = playerA })))
                        ]
                    , Html.div [ css [ Css.width (Css.pct 20) ] ]
                        [ blackButton "TIE" 
                            (if isVotingDisabled model then Nothing else Just (MatchFinished (League.Draw { playerA = playerA, playerB = playerB })))
                        ]
                    , Html.div [ css [ Css.width (Css.pct 40) ] ]
                        [ blueButton "WINNER" 
                            (if isVotingDisabled model then Nothing else Just (MatchFinished (League.Win { won = playerB, lost = playerA })))
                        ]
                    ]
                , Html.div
                    [ css
                        [ Css.textAlign Css.center
                        , Css.marginTop (Css.px 8)
                        , modernSansSerif
                        , Css.color (Css.hex "6B7280")
                        , Css.fontSize (Css.px 12)
                        , Css.letterSpacing (Css.px 0.5)
                        , Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.display Css.none ]
                        ]
                    ]
                    [ Html.text "Shortcuts: Left (1) • Right (2) • Tie (0) • Skip (Esc)" ]
                , Html.div
                    [ css
                        [ Css.displayFlex
                        , Css.justifyContent Css.spaceBetween
                        , Css.paddingTop (Css.px 12)
                        ]
                    ]
                    [ Html.div [ css [ Css.width (Css.pct 40), Css.textAlign Css.center, Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.display Css.none ] ] ]
                        (if isPlayerLocallyIgnored playerA model then
                            [ zzzUnignoreButton (Just (KeeperWantsToUnignorePlayer playerA)) ]
                        else
                            [ zzzIgnoreButton (Just (KeeperWantsToIgnorePlayer playerA)) ]
                        )
                    , Html.div [ css [ Css.width (Css.pct 20), Css.textAlign Css.center ] ] [ Html.text "" ]
                    , Html.div [ css [ Css.width (Css.pct 40), Css.textAlign Css.center, Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.display Css.none ] ] ]
                        (if isPlayerLocallyIgnored playerB model then
                            [ zzzUnignoreButton (Just (KeeperWantsToUnignorePlayer playerB)) ]
                        else
                            [ zzzIgnoreButton (Just (KeeperWantsToIgnorePlayer playerB)) ]
                        )
                    ]
                , Html.div
                    [ css
                        [ Css.marginTop (Css.px 12)
                        , Css.display Css.none
                        , Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.display Css.block ]
                        ]
                    ]
                    [ if model.showCustomMatchup then customMatchupUI model else Html.text "" ]
                                , Html.div
                                        [ css
                                                [ Css.padding4 (Css.px 32) (Css.pct 20) Css.zero (Css.pct 20)
                                                , Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.display Css.none ]
                                                ]
                                        ]
                                        [ -- Desktop controls rows
                                                                                    Html.div
                                                                                        [ css [ Css.displayFlex, Css.justifyContent Css.center, Css.marginBottom (Css.px 12) ] ]
                                            [ blueButton "UNDO" (Maybe.map (\_ -> KeeperWantsToUndo) (History.peekBack model.history))
                                            
                                            , button (Css.hex "999") "SKIP" (Just KeeperWantsToSkipMatch)
                                            ]
                                                                                , Html.div
                                                                                        [ css [ Css.displayFlex, Css.justifyContent Css.center, Css.marginBottom (Css.px 8) ] ]
                                            [ greenButton "CUSTOM MATCHUP" (Just KeeperWantsToShowCustomMatchup)
                                            , greenButton "ADD PLAYER" (Just KeeperWantsToAddNewPlayer)
                                            ]
                                        , if model.showCustomMatchup then customMatchupUI model else Html.text ""
                                        ]
                ]


rankings : Model -> Html Msg
rankings model =
    let
        previousStandings =
            History.peekBack model.history
                |> Maybe.withDefault (History.current model.history)
                |> League.players
                |> List.sortBy (\player -> -(Player.rating player))
                |> List.indexedMap (\rank player -> ( Player.name player, rank ))
                |> Dict.fromList

        numericRank =
            Css.batch
                [ Css.fontWeight (Css.int 700)
                , Css.fontSize (Css.px 18)
                , Css.verticalAlign Css.middle
                , modernSansSerif
                ]

        numericDim =
            Css.batch
                [ Css.fontWeight (Css.int 500)
                , Css.fontSize (Css.px 12)
                , Css.verticalAlign Css.middle
                , Css.color (Css.hex "6B7280")
                , modernSansSerif
                ]

        textual =
            Css.batch
                [ Css.fontWeight (Css.int 500)
                , Css.fontSize (Css.px 16)
                , Css.lineHeight (Css.px 22)
                , Css.verticalAlign Css.middle
                , modernSansSerif
                , Css.paddingLeft (Css.px 15)
                -- default name text can wrap; specific cells can override
                , Css.whiteSpace Css.normal
                , Css.overflow Css.visible
                ]

        shrinkWidth =
            Css.batch
                [ Css.paddingLeft (Css.px 15)
                , Css.paddingRight (Css.px 15)
                , Css.width (Css.pct 1)
                , Css.maxWidth (Css.px 80)
                , Css.whiteSpace Css.noWrap
                ]

        left =
            Css.textAlign Css.left

        center =
            Css.textAlign Css.center

        header =
            Css.batch
                [ Css.paddingRight (Css.px 15)
                , Css.paddingLeft (Css.px 15)
                , Css.verticalAlign Css.middle
                , modernSansSerif
                , Css.fontWeight (Css.int 600)
                , Css.borderRight3 (Css.px 1) Css.solid (Css.hex "B1BECE")
                , Css.lastChild [ Css.borderRightWidth Css.zero ]
                ]
    in
    History.current model.history
        |> League.players
        |> List.filter (timePlayerFilter model)  -- Only filter by time, show ignored players grayed out
        |> List.sortBy (\player -> -(Player.rating player))  -- Then sort filtered list
        |> List.indexedMap  -- Then rank with no gaps (1, 2, 3, etc.)
            (\rank player ->
                let
                    previousRank =
                        Dict.get (Player.name player) previousStandings
                            |> Maybe.withDefault rank

                    isPlaying =
                        History.current model.history
                            |> League.currentMatch
                            |> Maybe.map (\(League.Match a b) -> Player.id player == Player.id a || Player.id player == Player.id b)
                            |> Maybe.withDefault False

                    isIgnored =
                        isPlayerLocallyIgnored player model
                in
                ( Player.htmlKey player
                , Html.tr
                    [ css [ Css.height (Css.px 40)
                          , Css.borderBottom3 (Css.px 1) Css.solid (Css.hex "E5E7EB")
                          , if isIgnored then 
                                Css.batch [ Css.opacity (Css.num 0.5), Css.backgroundColor (Css.hex "F9F9F9") ]
                            else 
                                Css.batch []
                          ] ]
                    [ Html.td
                        [ css
                            [ Css.verticalAlign Css.middle
                            , Css.textAlign Css.center
                            , Css.width (Css.px 24)
                            ]
                        ]
                        (if isPlaying then
                            [ circle (Css.hex "EFE700") ]

                         else if rank < previousRank then
                            [ upArrow (Css.hex "6DD400")
                            , Html.span
                                [ css [ modernSansSerif, Css.color (Css.hex "6DD400"), Css.fontSize (Css.px 14) ] ]
                                [ Html.text (String.fromInt (previousRank - rank)) ]
                            ]

                         else if rank > previousRank then
                            [ downArrow (Css.hex "E02020")
                            , Html.span
                                [ css [ modernSansSerif, Css.color (Css.hex "E02020"), Css.fontSize (Css.px 14) ] ]
                                [ Html.text (String.fromInt (abs (previousRank - rank))) ]
                            ]

                         else
                            [ Html.text "" ]
                        )
                    , Html.td [ css [ numericRank, center, Css.width (Css.px 60), Css.maxWidth (Css.px 60) ] ] [ Html.text (String.fromInt (rank + 1)) ]
                    , Html.td [ css [ textual, left, Css.width (Css.pct 25), Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.width (Css.pct 60) ] ] ]
                        [ Html.div [ css [ Css.displayFlex, Css.alignItems Css.center ] ]
                            [ Html.span [] [ Html.text (Player.name player) ]
                            , if isIgnored then
                                Html.span 
                                    [ css 
                                        [ Css.marginLeft (Css.px 8)
                                        , Css.padding2 (Css.px 2) (Css.px 6)
                                        , Css.backgroundColor (Css.hex "FEF3C7")
                                        , Css.color (Css.hex "92400E")
                                        , Css.fontSize (Css.px 10)
                                        , Css.fontWeight (Css.int 600)
                                        , Css.borderRadius (Css.px 4)
                                        , Css.textTransform Css.uppercase
                                        , modernSansSerif
                                        ]
                                    ] 
                                    [ Html.text "SNOOZED" ]
                              else
                                Html.text ""
                            ]
                        ]
                    , Html.td [ css [ numericDim, center, Css.width (Css.px 80), Css.maxWidth (Css.px 80), Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.display Css.none ] ] ] [ Html.text (String.fromInt (Player.rating player)) ]
                    , Html.td [ css [ numericDim, center, Css.width (Css.px 80), Css.maxWidth (Css.px 80), Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.display Css.none ] ] ] [ Html.text (String.fromInt (Player.matchesPlayed player)) ]
                    , Html.td [ css [ Css.verticalAlign Css.middle, Css.width (Css.pct 35) ] ]
                        [ Html.div
                            [ css
                                [ Css.displayFlex
                                , Css.flexWrap Css.wrap
                                , Css.alignItems Css.center
                                , Css.justifyContent Css.center
                                , Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.justifyContent Css.flexStart ]
                                ]
                            ]
                            (let
                                baseActions =
                                    if isPlayerLocallyIgnored player model then
                                        [ zzzUnignoreButtonSmall (Just (KeeperWantsToUnignorePlayer player)) ]
                                    else
                                        [ smallRedXButtonSmall (Just (KeeperWantsToRetirePlayer player))
                                        , Html.span [ css [ Css.paddingLeft (Css.px 6), Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.paddingLeft (Css.px 2) ] ] ] [ zzzIgnoreButtonSmall (Just (KeeperWantsToIgnorePlayer player)) ]
                                        ]
                            in
                            baseActions
                                ++ [ Html.span [ css [ Css.paddingLeft (Css.px 6), Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.paddingLeft (Css.px 2) ] ] ]
                                        [ toggleChipSmall "AM" (Player.playsAM player) (Css.hex "F59E0B") (TogglePlayerAM player) ]
                                   , Html.span [ css [ Css.paddingLeft (Css.px 4), Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.paddingLeft (Css.px 2) ] ] ]
                                        [ toggleChipSmall "PM" (Player.playsPM player) (Css.hex "8B5CF6") (TogglePlayerPM player) ]
                                   ]
                            )
                        ]
                    ]
                )
            )
        |> (::)
            ( "players-header"
            , Html.tr
                [ css [ Css.height (Css.px 40), Css.borderBottom3 (Css.px 2) Css.solid (Css.hex "D1D5DB") ] ]
                [ Html.th [ css [ Css.width (Css.px 24) ] ] []
                , Html.th [ css [ header, center, Css.width (Css.px 60), Css.maxWidth (Css.px 60) ] ] [ Html.text "RANK" ]
                , Html.th [ css [ header, left, Css.width (Css.pct 25), Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.width (Css.pct 60) ] ] ] [ Html.text "NAME" ]
                , Html.th [ css [ header, center, Css.width (Css.px 80), Css.maxWidth (Css.px 80), Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.display Css.none ] ] ] [ Html.text "RATING" ]
                , Html.th [ css [ header, center, Css.width (Css.px 80), Css.maxWidth (Css.px 80), Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.display Css.none ] ] ] [ Html.text "MATCHES" ]
                , Html.th [ css [ header, center, Css.width (Css.pct 35) ] ] [ Html.text "ACTIONS" ]
                ]
            )
        |> Keyed.node "table"
            [ css
                [ Css.width (Css.pct 80)
                , Css.margin2 Css.zero Css.auto
                , Css.borderCollapse Css.collapse
                , Css.tableLayout Css.auto
                , Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ]
                    [ Css.width (Css.pct 100) ]
                ]
            ]
        |> List.singleton
        |> Html.div
            [ css
                [ Css.width (Css.pct 80)
                , Css.margin2 Css.zero Css.auto
                , Css.overflowX Css.auto
                , Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ]
                    [ Css.width (Css.pct 100)
                    , Css.overflowX Css.auto
                    ]
                ]
            ]


-- UI helpers

button : Css.Color -> String -> Maybe Msg -> Html Msg
button baseColor label maybeMsg =
    Html.button
        [ css
            [ Css.paddingTop (Css.px 6)
            , Css.paddingBottom (Css.px 10)
            , Css.paddingLeft (Css.px 15)
            , Css.paddingRight (Css.px 15)
            , Css.margin2 Css.zero (Css.px 10)
            , Css.minWidth (Css.px 100)
            , case maybeMsg of
                Just _ -> Css.backgroundColor baseColor
                Nothing -> Css.backgroundColor (Css.hex "DDD")
            , Css.border Css.zero
            , Css.borderRadius (Css.px 4)
            , Css.boxShadow6 Css.inset Css.zero (Css.px -4) Css.zero Css.zero (Css.rgba 0 0 0 0.1)
            , Css.cursor Css.pointer
            , Css.fontSize (Css.px 14)
            , Css.fontWeight (Css.int 600)
            , Css.color (Css.hex "FFF")
            , modernSansSerif
            ]
        , case maybeMsg of
            Just m -> Events.onClick m
            Nothing -> StyledAttributes.disabled True
        ]
        [ Html.text label ]


blueButton : String -> Maybe Msg -> Html Msg
blueButton =
    button (Css.hex "3B82F6")


greenButton : String -> Maybe Msg -> Html Msg
greenButton =
    button (Css.hex "6DD400")


redButton : String -> Maybe Msg -> Html Msg
redButton =
    button (Css.hex "EF4444")


blackButton : String -> Maybe Msg -> Html Msg
blackButton =
    button (Css.hex "1F2937")





-- Large button variants (especially for mobile)

buttonLarge : Css.Color -> String -> Maybe Msg -> Html Msg
buttonLarge baseColor label maybeMsg =
    Html.button
        [ css
            [ Css.paddingTop (Css.px 12)
            , Css.paddingBottom (Css.px 16)
            , Css.paddingLeft (Css.px 18)
            , Css.paddingRight (Css.px 18)
            , Css.margin2 Css.zero (Css.px 10)
            , Css.minWidth (Css.px 140)
            , case maybeMsg of
                Just _ -> Css.backgroundColor baseColor
                Nothing -> Css.backgroundColor (Css.hex "DDD")
            , Css.border Css.zero
            , Css.borderRadius (Css.px 8)
            , Css.boxShadow6 Css.inset Css.zero (Css.px -4) Css.zero Css.zero (Css.rgba 0 0 0 0.1)
            , Css.cursor Css.pointer
            , Css.fontSize (Css.px 18)
            , Css.fontWeight (Css.int 700)
            , Css.color (Css.hex "FFF")
            , modernSansSerif
            ]
        , case maybeMsg of
            Just m -> Events.onClick m
            Nothing -> StyledAttributes.disabled True
        ]
        [ Html.text label ]

blueButtonLarge : String -> Maybe Msg -> Html Msg
blueButtonLarge =
    buttonLarge (Css.hex "3B82F6")


redButtonLarge : String -> Maybe Msg -> Html Msg
redButtonLarge =
    buttonLarge (Css.hex "EF4444")


blackButtonLarge : String -> Maybe Msg -> Html Msg
blackButtonLarge =
    buttonLarge (Css.hex "1F2937")


-- Compact large button for tight mobile rows (smaller minWidth/margins)
buttonCompact : Css.Color -> String -> Maybe Msg -> Html Msg
buttonCompact baseColor label maybeMsg =
    Html.button
        [ css
            [ Css.paddingTop (Css.px 10)
            , Css.paddingBottom (Css.px 12)
            , Css.paddingLeft (Css.px 12)
            , Css.paddingRight (Css.px 12)
            , Css.margin2 Css.zero (Css.px 4)
            , Css.minWidth (Css.px 100)
            , case maybeMsg of
                Just _ -> Css.backgroundColor baseColor
                Nothing -> Css.backgroundColor (Css.hex "DDD")
            , Css.border Css.zero
            , Css.borderRadius (Css.px 8)
            , Css.boxShadow6 Css.inset Css.zero (Css.px -4) Css.zero Css.zero (Css.rgba 0 0 0 0.1)
            , Css.cursor Css.pointer
            , Css.fontSize (Css.px 16)
            , Css.fontWeight (Css.int 700)
            , Css.color (Css.hex "FFF")
            , modernSansSerif
            ]
        , case maybeMsg of
            Just m -> Events.onClick m
            Nothing -> StyledAttributes.disabled True
        ]
        [ Html.text label ]


goldButton : String -> Maybe Msg -> Html Msg
goldButton label maybeMsg =
    Html.button
        [ css
            [ Css.paddingTop (Css.px 6)
            , Css.paddingBottom (Css.px 10)
            , Css.paddingLeft (Css.px 15)
            , Css.paddingRight (Css.px 15)
            , Css.margin2 Css.zero (Css.px 10)
            , Css.minWidth (Css.px 100)
            , case maybeMsg of
                Just _ -> Css.backgroundColor (Css.hex "EFE700")
                Nothing -> Css.backgroundColor (Css.hex "DDD")
            , Css.border Css.zero
            , Css.borderRadius (Css.px 4)
            , Css.boxShadow6 Css.inset Css.zero (Css.px -4) Css.zero Css.zero (Css.rgba 0 0 0 0.1)
            , Css.cursor Css.pointer
            , Css.fontSize (Css.px 14)
            , Css.fontWeight (Css.int 600)
            , Css.color (Css.hex "333")
            , modernSansSerif
            ]
        , case maybeMsg of
            Just m -> Events.onClick m
            Nothing -> StyledAttributes.disabled True
        ]
        [ Html.text label ]


smallRedXButton : Maybe Msg -> Html Msg
smallRedXButton maybeMsg =
    Html.button
        [ css
            [ Css.paddingTop (Css.px 4)
            , Css.paddingBottom (Css.px 4)
            , Css.paddingLeft (Css.px 8)
            , Css.paddingRight (Css.px 8)
            , Css.margin2 Css.zero (Css.px 6)
            , Css.minWidth (Css.px 36)
            , case maybeMsg of
                Just _ -> Css.backgroundColor (Css.hex "E02020")
                Nothing -> Css.backgroundColor (Css.hex "DDD")
            , Css.border Css.zero
            , Css.borderRadius (Css.px 4)
            , Css.boxShadow6 Css.inset Css.zero (Css.px -4) Css.zero Css.zero (Css.rgba 0 0 0 0.1)
            , Css.cursor Css.pointer
            , Css.fontSize (Css.px 14)
            , Css.fontWeight (Css.int 600)
            , Css.color (Css.hex "FFF")
            , modernSansSerif
            ]
        , case maybeMsg of
            Just m -> Events.onClick m
            Nothing -> StyledAttributes.disabled True
        ]
        [ Html.text "X" ]

-- Extra small X button for table
smallRedXButtonSmall : Maybe Msg -> Html Msg
smallRedXButtonSmall maybeMsg =
    Html.button
        [ css
            [ Css.padding2 (Css.px 2) (Css.px 8)
            , Css.margin2 Css.zero (Css.px 4)
            , Css.width (Css.px 40)
            , Css.boxSizing Css.borderBox
            , Css.textAlign Css.center
            , case maybeMsg of
                Just _ -> Css.backgroundColor (Css.hex "E02020")
                Nothing -> Css.backgroundColor (Css.hex "DDD")
            , Css.border Css.zero
            , Css.borderRadius (Css.px 9999)
            , Css.cursor Css.pointer
            , Css.fontSize (Css.px 11)
            , Css.fontWeight (Css.int 700)
            , Css.color (Css.hex "FFF")
            , modernSansSerif
            , Css.whiteSpace Css.noWrap
            , Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ]
                [ Css.padding2 (Css.px 2) (Css.px 6)
                , Css.marginRight (Css.px 2)
                , Css.marginLeft (Css.px 2)
                , Css.width (Css.px 40)
                , Css.boxSizing Css.borderBox
                , Css.fontSize (Css.px 10)
                ]
            ]
        , case maybeMsg of
            Just m -> Events.onClick m
            Nothing -> StyledAttributes.disabled True
        ]
        [ Html.text "X" ]


zzzIgnoreButton : Maybe Msg -> Html Msg
zzzIgnoreButton maybeMsg =
    Html.button
        [ css
            [ Css.paddingTop (Css.px 4)
            , Css.paddingBottom (Css.px 6)
            , Css.paddingLeft (Css.px 12)
            , Css.paddingRight (Css.px 12)
            , Css.margin2 Css.zero (Css.px 6)
            , Css.minWidth (Css.px 44)
            , Css.backgroundColor (Css.hex "6B7280") -- neutral gray
            , Css.border Css.zero
            , Css.borderRadius (Css.px 9999)
            , Css.cursor Css.pointer
            , Css.fontSize (Css.px 13)
            , Css.fontWeight (Css.int 600)
            , Css.color (Css.hex "FFF")
            , modernSansSerif
            , Css.hover
                [ Css.backgroundColor (Css.hex "4B5563") ]
            , Css.active
                [ Css.backgroundColor (Css.hex "374151") ]
            , Css.focus
                [ Css.outline3 (Css.px 2) Css.solid (Css.hex "93C5FD")
                , Css.outlineOffset (Css.px 2)
                ]
            ]
        , case maybeMsg of
            Just m -> Events.onClick m
            Nothing -> StyledAttributes.disabled True
        ]
        [ Html.text "Zzz" ]

-- Smaller Zzz buttons for table
zzzIgnoreButtonSmall : Maybe Msg -> Html Msg
zzzIgnoreButtonSmall maybeMsg =
    Html.button
        [ css
            [ Css.padding2 (Css.px 2) (Css.px 8)
            , Css.margin2 Css.zero (Css.px 4)
            , Css.width (Css.px 40)
            , Css.boxSizing Css.borderBox
            , Css.textAlign Css.center
            , Css.backgroundColor (Css.hex "6B7280")
            , Css.border Css.zero
            , Css.borderRadius (Css.px 9999)
            , Css.cursor Css.pointer
            , Css.fontSize (Css.px 11)
            , Css.fontWeight (Css.int 700)
            , Css.color (Css.hex "FFF")
            , modernSansSerif
            , Css.whiteSpace Css.noWrap
            , Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.paddingLeft (Css.px 6), Css.paddingRight (Css.px 6), Css.fontSize (Css.px 10), Css.width (Css.px 40), Css.boxSizing Css.borderBox ]
            , Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 400) ] ] [ Css.paddingLeft (Css.px 4), Css.paddingRight (Css.px 4), Css.width (Css.px 40) ]
            ]
        , case maybeMsg of
            Just m -> Events.onClick m
            Nothing -> StyledAttributes.disabled True
        ]
        [ Html.text "Zzz" ]

zzzUnignoreButton : Maybe Msg -> Html Msg
zzzUnignoreButton maybeMsg =
    Html.button
        [ css
            [ Css.paddingTop (Css.px 4)
            , Css.paddingBottom (Css.px 6)
            , Css.paddingLeft (Css.px 12)
            , Css.paddingRight (Css.px 12)
            , Css.margin2 Css.zero (Css.px 6)
            , Css.minWidth (Css.px 44)
            , Css.backgroundColor (Css.hex "374151") -- darker neutral
            , Css.border Css.zero
            , Css.borderRadius (Css.px 9999)
            , Css.cursor Css.pointer
            , Css.fontSize (Css.px 13)
            , Css.fontWeight (Css.int 600)
            , Css.color (Css.hex "FFF")
            , Css.textDecoration Css.lineThrough
            , modernSansSerif
            , Css.hover
                [ Css.backgroundColor (Css.hex "1F2937") ]
            , Css.active
                [ Css.backgroundColor (Css.hex "111827") ]
            , Css.focus
                [ Css.outline3 (Css.px 2) Css.solid (Css.hex "93C5FD")
                , Css.outlineOffset (Css.px 2)
                ]
            ]
        , case maybeMsg of
            Just m -> Events.onClick m
            Nothing -> StyledAttributes.disabled True
        ]
        [ Html.text "Zzz" ]

zzzUnignoreButtonSmall : Maybe Msg -> Html Msg
zzzUnignoreButtonSmall maybeMsg =
    Html.button
        [ css
            [ Css.padding2 (Css.px 2) (Css.px 8)
            , Css.margin2 Css.zero (Css.px 4)
            , Css.width (Css.px 40)
            , Css.boxSizing Css.borderBox
            , Css.textAlign Css.center
            , Css.backgroundColor (Css.hex "374151")
            , Css.border Css.zero
            , Css.borderRadius (Css.px 9999)
            , Css.cursor Css.pointer
            , Css.fontSize (Css.px 11)
            , Css.fontWeight (Css.int 700)
            , Css.color (Css.hex "FFF")
            , Css.textDecoration Css.lineThrough
            , modernSansSerif
            , Css.whiteSpace Css.noWrap
            , Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.paddingLeft (Css.px 6), Css.paddingRight (Css.px 6), Css.fontSize (Css.px 10), Css.width (Css.px 40), Css.boxSizing Css.borderBox ]
            , Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 400) ] ] [ Css.paddingLeft (Css.px 4), Css.paddingRight (Css.px 4), Css.width (Css.px 40) ]
            ]
        , case maybeMsg of
            Just m -> Events.onClick m
            Nothing -> StyledAttributes.disabled True
        ]
        [ Html.text "Zzz" ]


activePlayer : Player -> Html msg
activePlayer player =
    Html.div
        [ css [ Css.width (Css.pct 40), Css.maxWidth (Css.pct 45), Css.textAlign Css.center, modernSansSerif ] ]
        [ Html.h2
            [ css [ Css.fontSize (Css.px 26), Css.marginBottom (Css.px 6), Css.textTransform Css.uppercase, Css.fontWeight (Css.int 700), Css.fontStyle Css.italic ] ]
            [ Html.text (Player.name player) ]
        , availabilityBadges player
        ]


-- Compact player block for mobile rows
activePlayerCompact : Player -> Html msg
activePlayerCompact player =
    Html.div
        [ css [ modernSansSerif ] ]
        [ Html.h3
            [ css [ Css.fontSize (Css.px 20), Css.marginBottom (Css.px 4), Css.textTransform Css.uppercase, Css.fontWeight (Css.int 700), Css.fontStyle Css.italic ] ]
            [ Html.text (Player.name player) ]
        , availabilityBadges player
        ]

-- Compact player with ignore button for mobile comparison
activePlayerCompactWithIgnore : Player -> Model -> Html Msg
activePlayerCompactWithIgnore player model =
    Html.div
        [ css [ modernSansSerif ] ]
        [ Html.h3
            [ css [ Css.fontSize (Css.px 20), Css.marginBottom (Css.px 4), Css.textTransform Css.uppercase, Css.fontWeight (Css.int 700), Css.fontStyle Css.italic ] ]
            [ Html.text (Player.name player) ]
        , Html.div
            [ css [ Css.displayFlex, Css.justifyContent Css.center, Css.alignItems Css.center ] ]
            [ Html.span [ css [ Css.marginRight (Css.px 6) ] ] [ badge "AM" (Player.playsAM player) (Css.hex "F59E0B") ]
            , Html.span [ css [ Css.marginRight (Css.px 6) ] ] [ badge "PM" (Player.playsPM player) (Css.hex "8B5CF6") ]
            , if isPlayerLocallyIgnored player model then
                zzzUnignoreButtonTiny (Just (KeeperWantsToUnignorePlayer player))
              else
                zzzIgnoreButtonTiny (Just (KeeperWantsToIgnorePlayer player))
            ]
        ]


availabilityBadges : Player -> Html msg
availabilityBadges player =
    Html.div
        [ css [ Css.displayFlex, Css.justifyContent Css.center ] ]
        [ Html.span [ css [ Css.marginRight (Css.px 6) ] ] [ badge "AM" (Player.playsAM player) (Css.hex "F59E0B") ]
        , badge "PM" (Player.playsPM player) (Css.hex "8B5CF6")
        ]


badge : String -> Bool -> Css.Color -> Html msg
badge label isOn colorOn =
    Html.span
        [ css
            [ Css.display Css.inlineBlock
            , Css.padding2 (Css.px 2) (Css.px 8)
            , Css.borderRadius (Css.px 9999)
            , (if isOn then Css.backgroundColor colorOn else Css.backgroundColor (Css.hex "E5E7EB"))
            , (if isOn then Css.color (Css.hex "FFFFFF") else Css.color (Css.hex "6B7280"))
            , Css.fontSize (Css.px 12)
            , Css.fontWeight (Css.int 700)
            , Css.letterSpacing (Css.px 0.5)
            ]
        ]
        [ Html.text label ]

-- (AM/PM mini badges removed from rankings per request)


toggleChip : String -> Bool -> Css.Color -> Msg -> Html Msg
toggleChip label isOn colorOn msg =
    Html.button
        [ css
            [ Css.display Css.inlineBlock
            , Css.padding2 (Css.px 4) (Css.px 10)
            , Css.borderRadius (Css.px 9999)
            , (if isOn then Css.backgroundColor colorOn else Css.backgroundColor (Css.hex "E5E7EB"))
            , (if isOn then Css.color (Css.hex "FFFFFF") else Css.color (Css.hex "6B7280"))
            , Css.fontSize (Css.px 12)
            , Css.fontWeight (Css.int 700)
            , Css.border Css.zero
            , Css.cursor Css.pointer
            ]
        , Events.onClick msg
        ]
        [ Html.text label ]

-- Extra tiny Zzz button for mobile player cards
zzzIgnoreButtonTiny : Maybe Msg -> Html Msg
zzzIgnoreButtonTiny maybeMsg =
    Html.button
        [ css
            [ Css.paddingTop (Css.px 1)
            , Css.paddingBottom (Css.px 2)
            , Css.paddingLeft (Css.px 6)
            , Css.paddingRight (Css.px 6)
            , Css.margin2 Css.zero (Css.px 4)
            , Css.minWidth (Css.px 24)
            , Css.backgroundColor (Css.hex "6B7280")
            , Css.border Css.zero
            , Css.borderRadius (Css.px 9999)
            , Css.cursor Css.pointer
            , Css.fontSize (Css.px 9)
            , Css.fontWeight (Css.int 700)
            , Css.color (Css.hex "FFF")
            , modernSansSerif
            ]
        , case maybeMsg of
            Just m -> Events.onClick m
            Nothing -> StyledAttributes.disabled True
        ]
        [ Html.text "Zzz" ]

zzzUnignoreButtonTiny : Maybe Msg -> Html Msg
zzzUnignoreButtonTiny maybeMsg =
    Html.button
        [ css
            [ Css.paddingTop (Css.px 1)
            , Css.paddingBottom (Css.px 2)
            , Css.paddingLeft (Css.px 6)
            , Css.paddingRight (Css.px 6)
            , Css.margin2 Css.zero (Css.px 4)
            , Css.minWidth (Css.px 24)
            , Css.backgroundColor (Css.hex "374151")
            , Css.border Css.zero
            , Css.borderRadius (Css.px 9999)
            , Css.cursor Css.pointer
            , Css.fontSize (Css.px 9)
            , Css.fontWeight (Css.int 700)
            , Css.color (Css.hex "FFF")
            , Css.textDecoration Css.lineThrough
            , modernSansSerif
            ]
        , case maybeMsg of
            Just m -> Events.onClick m
            Nothing -> StyledAttributes.disabled True
        ]
        [ Html.text "Zzz" ]

-- Smaller toggle chip for table row
toggleChipSmall : String -> Bool -> Css.Color -> Msg -> Html Msg
toggleChipSmall label isOn colorOn msg =
    Html.button
        [ css
            [ Css.display Css.inlineBlock
            , Css.padding2 (Css.px 2) (Css.px 8)
            , Css.borderRadius (Css.px 9999)
            , (if isOn then Css.backgroundColor colorOn else Css.backgroundColor (Css.hex "E5E7EB"))
            , (if isOn then Css.color (Css.hex "FFFFFF") else Css.color (Css.hex "6B7280"))
            , Css.fontSize (Css.px 11)
            , Css.fontWeight (Css.int 700)
            , Css.border Css.zero
            , Css.cursor Css.pointer
            , modernSansSerif
            , Css.whiteSpace Css.noWrap
            , Css.textAlign Css.center
            , Css.width (Css.px 40)
            , Css.boxSizing Css.borderBox
            , Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.padding2 (Css.px 2) (Css.px 5), Css.fontSize (Css.px 10), Css.width (Css.px 40), Css.boxSizing Css.borderBox ]
            ]
        , Events.onClick msg
        ]
        [ Html.text label ]


upArrow : Css.Color -> Html msg
upArrow color =
    Html.div
        [ css
            [ Css.width Css.zero
            , Css.height Css.zero
            , Css.borderLeft3 (Css.px 5) Css.solid Css.transparent
            , Css.borderRight3 (Css.px 5) Css.solid Css.transparent
            , Css.borderBottom3 (Css.px 10) Css.solid color
            , Css.display Css.inlineBlock
            , Css.margin4 (Css.px 2) (Css.px 5) (Css.px 2) (Css.px 2)
            ]
        ]
        []


downArrow : Css.Color -> Html msg
downArrow color =
    Html.div
        [ css
            [ Css.width Css.zero
            , Css.height Css.zero
            , Css.borderLeft3 (Css.px 5) Css.solid Css.transparent
            , Css.borderRight3 (Css.px 5) Css.solid Css.transparent
            , Css.borderTop3 (Css.px 10) Css.solid color
            , Css.display Css.inlineBlock
            , Css.margin4 (Css.px 2) (Css.px 5) (Css.px 2) (Css.px 2)
            ]
        ]
        []


circle : Css.Color -> Html msg
circle color =
    Html.div
        [ css
            [ Css.width (Css.px 10)
            , Css.height (Css.px 10)
            , Css.borderRadius (Css.pct 100)
            , Css.backgroundColor color
            , Css.margin2 Css.zero Css.auto
            ]
        ]
        []


-- MAIN

main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


-- SERIALIZATION HELPERS

serializeFilter : TimeFilter -> String
serializeFilter tf =
    case tf of
        All -> "all"
        AMOnly -> "am"
        PMOnly -> "pm"


parseFilter : String -> Maybe TimeFilter
parseFilter s =
    case String.toLower s of
        "all" -> Just All
        "am" -> Just AMOnly
        "pm" -> Just PMOnly
        _ -> Nothing
