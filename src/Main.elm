port module Main exposing (..)

import Accessibility.Styled as Html exposing (Html)
import Process
import Browser exposing (Document)
import Time
import Css
import Css.Media as Media
import Css.Reset
import Dict exposing (Dict)
import Elo
import File exposing (File)
import File.Download as Download
import File.Select as Select
import History exposing (History)
import Html.Styled as WildWildHtml
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Html.Styled.Keyed as Keyed
import Json.Decode as Decode
import Json.Encode as Encode exposing (encode)
import Keyboard
import League exposing (League, isPlayerIgnored)
import List exposing (head)
import Player exposing (Player)
import Random
import Set exposing (Set)
import String
import Task
import Http


-- PORTS (local persistence)

port saveStandings : String -> Cmd msg
port askForStandings : String -> Cmd msg
port receiveStandings : (String -> msg) -> Sub msg

port saveAutoSave : Bool -> Cmd msg
port askForAutoSave : String -> Cmd msg
port receiveAutoSave : (Bool -> msg) -> Sub msg

-- PORTS (Drive via Apps Script)

port saveToPublicDrive : String -> Cmd msg
port loadFromPublicDrive : String -> Cmd msg  
port receivePublicDriveStatus : (String -> msg) -> Sub msg
port receiveMatchSaveComplete : (() -> msg) -> Sub msg

-- DEBUG PORT
port sendVoteCount : Int -> Cmd msg

-- PORTS (Persist time filter)
port saveTimeFilter : String -> Cmd msg
port askForTimeFilter : String -> Cmd msg
port receiveTimeFilter : (String -> msg) -> Sub msg

-- PORTS (Persist local ignore list)
port saveIgnoredPlayers : String -> Cmd msg
port askForIgnoredPlayers : String -> Cmd msg
port receiveIgnoredPlayers : (String -> msg) -> Sub msg


-- FLAGS / MODEL

type alias Flags =
    ()


type alias Model =
    { history : History League
    , newPlayerName : String
    , autoSave : Bool
    , status : Maybe String
    , lastSynced : Maybe String
    , votesUntilDriveSync : Int
    , shouldStartNextMatchAfterLoad : Bool
    , autoSaveInProgress : Bool
    , timeFilter : TimeFilter
    , ignoredPlayers : Set String  -- Player IDs that are locally ignored
    , customMatchupPlayerA : Maybe Player
    , customMatchupPlayerB : Maybe Player
    , showCustomMatchup : Bool
    , playerASearch : String
    , playerBSearch : String
    , playerASearchResults : List Player
    , playerBSearchResults : List Player
    }



-- MESSAGES

type Msg
    = KeeperUpdatedNewPlayerName String
    | KeeperWantsToAddNewPlayer
    | KeeperWantsToRetirePlayer Player
    | KeeperWantsToIgnorePlayer Player
    | KeeperWantsToUnignorePlayer Player
    | KeeperWantsToSkipMatch
    | GotNextMatch (Maybe League.Match)
    | MatchFinished League.Outcome
    | KeeperWantsToSaveStandings
    | KeeperWantsToSaveToDrive
    | KeeperWantsToLoadStandings
    | KeeperWantsToRefreshFromDrive
    | SelectedStandingsFile File
    | PeriodicSync
    | AutoSaveCompleted
    | AutoSaveTimeout
    | TriggerReload
    | KeeperWantsToUndo
    | KeeperWantsToRedo
    | KeeperWantsToShowCustomMatchup
    | KeeperWantsToHideCustomMatchup
    | KeeperSelectedPlayerA Player
    | KeeperSelectedPlayerB Player
    | KeeperWantsToStartCustomMatch
    | KeeperUpdatedPlayerASearch String
    | KeeperUpdatedPlayerBSearch String
    | LoadedLeague (Result String League)
    | GotPlayers (Result Http.Error League)
    | ReceivedStandings String
    | ReceivedAutoSave Bool
    | ToggleAutoSave
    | ShowStatus String
    | ClearStatus
    | ReceivedPublicDriveStatus String
    | IgnoredKey
    | TogglePlayerAM Player
    | TogglePlayerPM Player
    | SetTimeFilter TimeFilter
    | ReceivedTimeFilter String
    | ReceivedIgnoredPlayers String
type TimeFilter
    = All
    | AMOnly
    | PMOnly




-- INIT

init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { history = History.init 50 League.init
      , newPlayerName = ""
      , autoSave = True
      , status = Nothing
      , lastSynced = Nothing
    , votesUntilDriveSync = 25
      , shouldStartNextMatchAfterLoad = False
    , autoSaveInProgress = False
    , timeFilter = All
    , ignoredPlayers = Set.empty
    , customMatchupPlayerA = Nothing
    , customMatchupPlayerB = Nothing
    , showCustomMatchup = False
    , playerASearch = ""
    , playerBSearch = ""
    , playerASearchResults = []
    , playerBSearchResults = []
      }
        , Cmd.batch [ askForAutoSave "init", askForTimeFilter "init", askForIgnoredPlayers "init", loadFromPublicDrive "init" ]
    )
        |> startNextMatchIfPossible



-- HELPERS

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


maybeAutoSave : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
maybeAutoSave ( model, cmd ) =
    if model.autoSave then
        ( model
        , Cmd.batch
            [ cmd
            , saveStandings (encode 2 (League.encode (History.current model.history)))
            ]
        )

    else
        ( model, cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    case League.currentMatch (History.current model.history) of
        Just (League.Match left right) ->
            Keyboard.downs
                (\rawKey ->
                    case Keyboard.navigationKey rawKey of
                        Just Keyboard.ArrowLeft ->
                            MatchFinished (League.Win { won = left, lost = right })

                        Just Keyboard.ArrowRight ->
                            MatchFinished (League.Win { won = right, lost = left })

                        Just Keyboard.ArrowUp ->
                            MatchFinished (League.Draw { playerA = left, playerB = right })

                        _ ->
                            -- Try other keys: Escape to skip, digits 1/2/0 for vote shortcuts
                            let
                                keyStr = Keyboard.rawValue rawKey
                            in
                            if keyStr == "Escape" || keyStr == "Esc" then
                                if model.autoSaveInProgress then
                                    IgnoredKey
                                else
                                    KeeperWantsToSkipMatch
                            else
                                case Keyboard.characterKeyUpper rawKey of
                                    Just (Keyboard.Character "1") ->
                                        MatchFinished (League.Win { won = left, lost = right })

                                    Just (Keyboard.Character "2") ->
                                        MatchFinished (League.Win { won = right, lost = left })

                                    Just (Keyboard.Character "0") ->
                                        MatchFinished (League.Draw { playerA = left, playerB = right })

                                    _ ->
                                        IgnoredKey
                )

        Nothing ->
            Sub.batch
                [ receiveStandings ReceivedStandings
                , receiveAutoSave ReceivedAutoSave
                , receivePublicDriveStatus ReceivedPublicDriveStatus
                , receiveMatchSaveComplete (\_ -> AutoSaveCompleted)
                , Time.every (30 * 1000) (\_ -> PeriodicSync) -- Every 30 seconds
                , receiveTimeFilter ReceivedTimeFilter
                , receiveIgnoredPlayers ReceivedIgnoredPlayers
                ]


-- Helper function to check if player is locally ignored
isPlayerLocallyIgnored : Player -> Model -> Bool
isPlayerLocallyIgnored player model =
    let
        (Player.PlayerId idInt) = Player.id player
    in
    Set.member (String.fromInt idInt) model.ignoredPlayers


-- Create a predicate that combines time filter and local ignore filter
combinedPlayerFilter : Model -> (Player -> Bool)
combinedPlayerFilter model =
    \player ->
        not (isPlayerLocallyIgnored player model) &&
        case model.timeFilter of
            All -> True
            AMOnly -> Player.playsAM player
            PMOnly -> Player.playsPM player


startNextMatchIfPossible : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
startNextMatchIfPossible ( model, cmd ) =
    if League.currentMatch (History.current model.history) /= Nothing then
        ( model, cmd )

    else
        ( model
        , Cmd.batch
            [ cmd
            , Random.generate GotNextMatch (
                League.nextMatchFiltered (combinedPlayerFilter model) (History.current model.history)
              )
            ]
        )


maybeSaveToDriveAfterVote : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
maybeSaveToDriveAfterVote ( model, cmd ) =
    let
        newCount = model.votesUntilDriveSync - 1
    in
    if newCount <= 0 then
        -- Save to Drive and don't start next match until reload completes
    ( { model | votesUntilDriveSync = 25, status = Just "Saving to Google Sheets...", autoSaveInProgress = True }
        , Cmd.batch
            [ saveToPublicDrive (encode 0 (League.encode (History.current model.history)))
            , Task.succeed (ShowStatus "Auto-saving to Drive...") |> Task.perform identity
            , Process.sleep 10000 |> Task.perform (\_ -> AutoSaveTimeout) -- 10 second timeout
            , sendVoteCount 25  -- Reset to 25
            ]
        )
    else
        ( { model | votesUntilDriveSync = newCount }, Cmd.batch [ cmd, sendVoteCount newCount ] )



-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TogglePlayerAM player ->
            let
                updatedLeague =
                    History.current model.history
                        |> \league ->
                            case League.getPlayer (Player.id player) league of
                                Just p ->
                                    let newP = Player.setAM (not (Player.playsAM p)) p in
                                    League.updatePlayer newP league
                                Nothing ->
                                    league
            in
            ( { model | history = History.mapPush (\_ -> updatedLeague) model.history }
            , Cmd.none
            )
                |> startNextMatchIfPossible
                |> maybeAutoSave

        TogglePlayerPM player ->
            let
                updatedLeague =
                    History.current model.history
                        |> \league ->
                            case League.getPlayer (Player.id player) league of
                                Just p ->
                                    let newP = Player.setPM (not (Player.playsPM p)) p in
                                    League.updatePlayer newP league
                                Nothing ->
                                    league
            in
            ( { model | history = History.mapPush (\_ -> updatedLeague) model.history }
            , Cmd.none
            )
                |> startNextMatchIfPossible
                |> maybeAutoSave
        KeeperUpdatedNewPlayerName newPlayerName ->
            ( { model | newPlayerName = newPlayerName }, Cmd.none )

        KeeperWantsToAddNewPlayer ->
            ( { model
                | history =
                    History.mapPush (League.addPlayer (Player.init model.newPlayerName)) model.history
                , newPlayerName = ""
              }
            , Cmd.none
            )
                |> startNextMatchIfPossible
                |> maybeAutoSave

        KeeperWantsToRetirePlayer player ->
            ( { model | history = History.mapPush (League.retirePlayer player) model.history }
            , Cmd.none
            )
                |> startNextMatchIfPossible
                |> maybeAutoSave

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

        GotNextMatch (Just match) ->
            ( { model | history = History.mapInPlace (League.startMatch match) model.history }
            , Cmd.none
            )

        GotNextMatch Nothing ->
            ( model, Cmd.none )

        MatchFinished outcome ->
            if model.autoSaveInProgress then
                -- Ignore votes during auto-save to prevent race conditions
                ( model, Cmd.none )
            else
                ( { model | history = History.mapPush (League.finishMatch outcome) model.history }
                , Cmd.none
                )
                    |> maybeSaveToDriveAfterVote
                    |> startNextMatchIfPossible
                    |> maybeAutoSave

        KeeperWantsToSaveStandings ->
            ( model
            , Cmd.batch
                [ Download.string
                    "standings.json"
                    "application/json"
                    (encode 2 (League.encode (History.current model.history)))
                , Task.succeed (ShowStatus "Exported rankings") |> Task.perform identity
                ]
            )

        KeeperWantsToSaveToDrive ->
            ( model
            , Cmd.batch
                [ saveToPublicDrive (encode 2 (League.encode (History.current model.history)))
                , Task.succeed (ShowStatus "Saving to Drive...") |> Task.perform identity
                ]
            )

        KeeperWantsToRefreshFromDrive ->
            ( model
            , Cmd.batch
                [ loadFromPublicDrive ""
                , Task.succeed (ShowStatus "Refreshing from Drive...") |> Task.perform identity
                ]
            )

        PeriodicSync ->
            -- Only sync if no current match (don't interrupt voting)
            if League.currentMatch (History.current model.history) == Nothing then
                ( model, loadFromPublicDrive "" )
            else
                ( model, Cmd.none )

        AutoSaveCompleted ->
            -- Save completed, reload data. Only continue to next match if it was an auto-save
            if model.autoSaveInProgress then
                -- This was an auto-save, continue to next match after reload
                ( { model | shouldStartNextMatchAfterLoad = True, autoSaveInProgress = False, status = Just "Auto-save completed! Reloading data..." }
                , Process.sleep 1500 |> Task.perform (\_ -> TriggerReload)
                )
            else
                -- This was a manual save, just reload without starting next match
                ( { model | shouldStartNextMatchAfterLoad = False, status = Just "Manual save completed! Reloading data..." }
                , Process.sleep 1000 |> Task.perform (\_ -> TriggerReload)
                )

        AutoSaveTimeout ->
            -- Auto-save timed out, reset state and continue
            if model.autoSaveInProgress then
                ( { model | autoSaveInProgress = False, status = Just "Auto-save timed out. Voting re-enabled." }
                , Cmd.none
                )
            else
                -- Timeout arrived after completion, ignore it
                ( model, Cmd.none )

        TriggerReload ->
            -- Trigger the actual reload after save completion delay
            ( model, loadFromPublicDrive "" )

        KeeperWantsToLoadStandings ->
            ( model, Select.file [ "application/json" ] SelectedStandingsFile )

        SelectedStandingsFile file ->
            ( model
            , File.toString file
                |> Task.andThen
                    (\jsonString ->
                        case Decode.decodeString League.decoder jsonString of
                            Ok decoded ->
                                Task.succeed decoded

                            Err err ->
                                Task.fail (Decode.errorToString err)
                    )
                |> Task.attempt LoadedLeague
            )

        KeeperWantsToUndo ->
            ( { model | history = History.goBack model.history |> Maybe.withDefault model.history }
            , Cmd.none
            )

        KeeperWantsToRedo ->
            ( { model | history = History.goForward model.history |> Maybe.withDefault model.history }
            , Cmd.none
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
                        ( { model | status = Just "Cannot match a player against themselves" }
                        , Cmd.none
                        )
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
                            , status = Just ("Custom match: " ++ Player.name playerA ++ " vs " ++ Player.name playerB)
                          }
                        , Cmd.none
                        )
                        
                _ ->
                    ( { model | status = Just "Please select both players for the custom match" }
                    , Cmd.none
                    )

        LoadedLeague (Ok league) ->
            ( { model | history = History.init 50 league }
            , Task.succeed (ShowStatus "Imported rankings") |> Task.perform identity
            )
                |> startNextMatchIfPossible
                |> maybeAutoSave

        LoadedLeague (Err problem) ->
            ( { model | status = Just ("Failed to load standings: " ++ problem) }
            , Cmd.none
            )

        GotPlayers result ->
            case result of
                Ok league ->
                    ( { model | history = History.init 50 league }
                    , Task.succeed (ShowStatus "Standings loaded from Drive") |> Task.perform identity
                    )
                        |> startNextMatchIfPossible

                Err httpErr ->
                    -- If Drive fetch fails (or Drive file is malformed), ask the host to return
                    -- the last saved public copy from localStorage as a fallback.
                    ( { model | status = Just ("Failed to fetch players from Drive: " ++ httpErrorToString httpErr) }
                    , Cmd.batch
                        [ loadFromPublicDrive ""
                        , Task.succeed (ShowStatus "Failed to load Drive, falling back to saved public copy") |> Task.perform identity
                        ]
                    )

        ReceivedStandings jsonString ->
            case Decode.decodeString League.decoder jsonString of
                Ok league ->
                    let
                        updatedModel = { model | history = History.init 50 league, shouldStartNextMatchAfterLoad = False, autoSaveInProgress = False }
                        baseResult = ( updatedModel, Task.succeed (ShowStatus "Standings loaded") |> Task.perform identity )
                    in
                    baseResult
                        |> startNextMatchIfPossible
                        |> maybeAutoSave

                Err _ ->
                    ( { model | status = Just "Saved standings malformed or unreadable", shouldStartNextMatchAfterLoad = False, autoSaveInProgress = False }
                    , Cmd.none
                    )

        ReceivedAutoSave value ->
            ( { model | autoSave = value }, Cmd.none )

        ReceivedPublicDriveStatus msgStr ->
            let
                parts = String.split "|" msgStr
                maybeTs =
                    case List.drop 1 parts |> head of
                        Just t -> Just t
                        Nothing -> Nothing
            in
            ( { model
                | status = Just (List.head parts |> Maybe.withDefault msgStr)
                , lastSynced = maybeTs
              }
            , Cmd.none
            )

        ToggleAutoSave ->
            let
                newVal = not model.autoSave
            in
            ( { model | autoSave = newVal }
            , Cmd.batch
                [ saveAutoSave newVal
                , Task.succeed
                    (ShowStatus
                        (if newVal then "Auto-save enabled" else "Auto-save disabled")
                    )
                    |> Task.perform identity
                ]
            )

        ShowStatus message ->
            ( { model | status = Just message }
            , Process.sleep 2000 |> Task.perform (\_ -> ClearStatus)
            )

        ClearStatus ->
            ( { model | status = Nothing }, Cmd.none )

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


view : Model -> Document Msg
view model =
    { title = "Hockey Rater 🏒"
    , body =
        [ Css.Reset.meyerV2
        , Css.Reset.borderBoxV201408

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
                , Html.section
                    [ css [ Css.textAlign Css.center, Css.marginTop (Css.px 32) ] ]
                    [ blueButton "EXPORT RANKINGS" (Just KeeperWantsToSaveStandings)
                    , blueButton "SAVE TO DRIVE" (Just KeeperWantsToSaveToDrive)
                    , blueButton "REFRESH FROM DRIVE" (Just KeeperWantsToRefreshFromDrive)
                    ]
                ]
            ]
        ]
        ++ (case model.status of
                Just message ->
                    [ Html.div
                        [ css
                            [ Css.position Css.fixed
                            , Css.top (Css.px 20)
                            , Css.right (Css.px 20)
                            , Css.backgroundColor (if model.autoSaveInProgress then Css.hex "EF4444" else Css.hex "10B981")
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
                                [ Html.text (message ++ (if model.autoSaveInProgress then " (Voting disabled)" else "")) ]
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
            |> List.map Html.toUnstyled
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
            Nothing -> Attributes.disabled True
        ]
        [ Html.text label ]


customMatchupUI : Model -> Html Msg
customMatchupUI model =
    let
        searchInput searchValue onInput results onSelect placeholder selectedPlayer =
            Html.div [ css [ Css.position Css.relative, Css.marginBottom (Css.px 16) ] ]
                [ Html.inputText searchValue
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
                    , Attributes.placeholder placeholder
                    , Events.onInput onInput
                    ]
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
                                (if model.autoSaveInProgress then Nothing else Just (MatchFinished (League.Win { lost = playerB, won = playerA })))
                            ]
                        ]
                    , -- Row 2: Player B with WINNER on the right
                      Html.div
                        [ css [ Css.displayFlex, Css.alignItems Css.center, Css.justifyContent Css.spaceBetween ] ]
                        [ Html.div [ css [ Css.flexGrow (Css.num 1) ] ] [ activePlayerCompactWithIgnore playerB model ]
                        , Html.div []
                            [ blueButtonLarge "WINNER"
                                (if model.autoSaveInProgress then Nothing else Just (MatchFinished (League.Win { won = playerB, lost = playerA })))
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
                                (if model.autoSaveInProgress then Nothing else Just (MatchFinished (League.Draw { playerA = playerA, playerB = playerB })))
                            ]
                        , Html.div [ css [ Css.marginLeft (Css.px 4) ] ]
                            [ buttonCompact (Css.hex "999") "SKIP" (Just KeeperWantsToSkipMatch) ]
                        ]
                    , -- Row 4 (new on mobile): CUSTOM MATCHUP and SAVE (centered)
                      Html.div
                        [ css [ Css.displayFlex, Css.alignItems Css.center, Css.justifyContent Css.center, Css.marginTop (Css.px 8) ] ]
                        [ Html.div [ css [ Css.marginRight (Css.px 4) ] ]
                            [ buttonCompact (Css.hex "6DD400") "CUSTOM" (Just KeeperWantsToShowCustomMatchup) ]
                        , Html.div [ css [ Css.marginLeft (Css.px 4) ] ]
                            [ buttonCompact (Css.hex "6DD400") "SAVE" (Just KeeperWantsToSaveToDrive) ]
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
                            (if model.autoSaveInProgress then Nothing else Just (MatchFinished (League.Win { lost = playerB, won = playerA })))
                        ]
                    , Html.div [ css [ Css.width (Css.pct 20) ] ]
                        [ blackButton "TIE" 
                            (if model.autoSaveInProgress then Nothing else Just (MatchFinished (League.Draw { playerA = playerA, playerB = playerB })))
                        ]
                    , Html.div [ css [ Css.width (Css.pct 40) ] ]
                        [ blueButton "WINNER" 
                            (if model.autoSaveInProgress then Nothing else Just (MatchFinished (League.Win { won = playerB, lost = playerA })))
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
                                            , blueButton "REDO" (Maybe.map (\_ -> KeeperWantsToRedo) (History.peekForward model.history))
                                            , button (Css.hex "999") "SKIP" (Just KeeperWantsToSkipMatch)
                                            ]
                                                                                , Html.div
                                                                                        [ css [ Css.displayFlex, Css.justifyContent Css.center, Css.marginBottom (Css.px 8) ] ]
                                            [ greenButton "CUSTOM MATCHUP" (Just KeeperWantsToShowCustomMatchup)
                                            , greenButton "SAVE" (Just KeeperWantsToSaveToDrive)
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
        |> List.sortBy (\player -> -(Player.rating player))
        |> List.indexedMap
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
                in
                ( Player.htmlKey player
                , Html.tr
                    [ css [ Css.height (Css.px 40), Css.borderBottom3 (Css.px 1) Css.solid (Css.hex "E5E7EB") ] ]
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
                        [ Html.span [] [ Html.text (Player.name player) ] ]
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
        |> (\tableGuts ->
                tableGuts
                    ++
                        [ ( "add-player-form"
                          , Html.tr
                                [ css [ Css.height (Css.px 60) ] ]
                                [ Html.td [] []
                                                                , Html.td [ css [ numericDim, shrinkWidth, center, Css.color (Css.hex "A6A6A6") ] ] [ Html.text "-" ]
                                                                , Html.td [ css [ numericDim, shrinkWidth, center, Css.color (Css.hex "A6A6A6") ] ] [ Html.text (String.fromInt Elo.initialRating) ]
                                                                , Html.td [ css [ numericDim, shrinkWidth, center, Css.color (Css.hex "A6A6A6") ] ] [ Html.text "0" ]
                                , Html.td
                                    [ css [ textual, left ] ]
                                    [ Html.inputText model.newPlayerName
                                        [ Events.onInput KeeperUpdatedNewPlayerName
                                        , Events.on "keydown"
                                            (Decode.field "key" Decode.string
                                                |> Decode.andThen
                                                    (\key ->
                                                        case key of
                                                            "Enter" ->
                                                                Decode.succeed KeeperWantsToAddNewPlayer

                                                            _ ->
                                                                Decode.fail "ignoring"
                                                    )
                                            )
                                        , css
                                            [ Css.border Css.zero
                                            , Css.fontSize (Css.px 18)
                                            , Css.padding2 (Css.px 5) (Css.px 15)
                                            , Css.width (Css.calc (Css.pct 100) Css.minus (Css.px 15))
                                            , Css.boxShadow6 Css.inset Css.zero (Css.px 1) (Css.px 2) Css.zero (Css.rgba 0 0 0 0.5)
                                            , Css.borderRadius (Css.px 5)
                                            ]
                                        ]
                                                                        ]
                                                                , Html.td [ css [ numericDim, shrinkWidth, center ] ] [ greenButton "ADD" (Just KeeperWantsToAddNewPlayer) ]
                                ]
                          )
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
            Nothing -> Attributes.disabled True
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


-- Save icon (small) for top-right quick save
saveIconButton : Maybe Msg -> Html Msg
saveIconButton maybeMsg =
    Html.button
        [ css
            [ Css.padding2 (Css.px 6) (Css.px 10)
            , Css.border Css.zero
            , Css.borderRadius (Css.px 6)
            , case maybeMsg of
                Just _ -> Css.backgroundColor (Css.hex "3B82F6")
                Nothing -> Css.backgroundColor (Css.hex "B0C4FF")
            , Css.color (Css.hex "FFF")
            , Css.cursor Css.pointer
            , Css.fontWeight (Css.int 700)
            , Css.display Css.inlineBlock
            , modernSansSerif
            ]
        , case maybeMsg of
            Just m -> Events.onClick m
            Nothing -> Attributes.disabled True
        ]
        [ Html.span [] [ Html.text "💾" ]
        , Html.span [ css [ Css.marginLeft (Css.px 6) ] ] [ Html.text "Save" ]
        ]


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
            Nothing -> Attributes.disabled True
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
            Nothing -> Attributes.disabled True
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
            Nothing -> Attributes.disabled True
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
            Nothing -> Attributes.disabled True
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
            Nothing -> Attributes.disabled True
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
            Nothing -> Attributes.disabled True
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
            Nothing -> Attributes.disabled True
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
            Nothing -> Attributes.disabled True
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
            Nothing -> Attributes.disabled True
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
            Nothing -> Attributes.disabled True
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
            Nothing -> Attributes.disabled True
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