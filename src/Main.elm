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
type TimeFilter
    = All
    | AMOnly
    | PMOnly




-- INIT

init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        url =
            "https://www.googleapis.com/drive/v3/files/1dMiPZqpcj7sMr9aKMxNhWKQNc2vzcJJD?alt=media&key=AIzaSyCuUxgmuh4ca0E-KQjE3VB-m5G4hm2c5Bc"

        httpRequest =
            Http.get
                { url = url
                , expect = Http.expectJson GotPlayers League.decoder
                }
    in
    ( { history = History.init 50 League.init
      , newPlayerName = ""
      , autoSave = True
      , status = Nothing
      , lastSynced = Nothing
    , votesUntilDriveSync = 25
      , shouldStartNextMatchAfterLoad = False
    , autoSaveInProgress = False
    , timeFilter = All
      }
    , Cmd.batch [ askForAutoSave "init", askForTimeFilter "init", httpRequest ]
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
                ]


startNextMatchIfPossible : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
startNextMatchIfPossible ( model, cmd ) =
    if League.currentMatch (History.current model.history) /= Nothing then
        ( model, cmd )

    else
        ( model
        , Cmd.batch
            [ cmd
            , Random.generate GotNextMatch (
                case model.timeFilter of
                    All ->
                        League.nextMatch (History.current model.history)

                    AMOnly ->
                        League.nextMatchFiltered Player.playsAM (History.current model.history)

                    PMOnly ->
                        League.nextMatchFiltered Player.playsPM (History.current model.history)
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
            ( { model | history = History.mapPush (League.ignorePlayer player) model.history }
            , Cmd.none
            )
                |> startNextMatchIfPossible
                |> maybeAutoSave

        KeeperWantsToUnignorePlayer player ->
            ( { model | history = History.mapPush (League.unignorePlayer player) model.history }
            , Cmd.none
            )
                |> startNextMatchIfPossible
                |> maybeAutoSave

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
                    if model.shouldStartNextMatchAfterLoad then
                        baseResult
                            |> startNextMatchIfPossible
                            |> maybeAutoSave
                    else
                        baseResult
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
            , Process.sleep 3500 |> Task.perform (\_ -> ClearStatus)
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
                , Html.section
                    [ css [ Css.width (Css.pct 80), Css.margin2 (Css.px 8) Css.auto, Css.textAlign Css.right ] ]
                    [ saveIconButton (Just KeeperWantsToSaveToDrive) ]
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
                            , Css.backgroundColor (if model.autoSaveInProgress then Css.hex "E02020" else Css.hex "333")
                            , Css.color (Css.hex "FFF")
                            , Css.padding4 (Css.px 8) (Css.px 12) (Css.px 8) (Css.px 12)
                            , Css.borderRadius (Css.px 6)
                            , modernSansSerif
                            ]
                        ]
                        [ Html.div [] 
                            [ Html.span [] [ Html.text message ]
                            , if model.autoSaveInProgress then Html.span [ css [ Css.marginLeft (Css.px 8) ] ] [ Html.text "(Voting disabled)" ] else Html.text ""
                            ]
                        , Html.div []
                            (case model.lastSynced of
                                Just ts ->
                                    [ Html.span
                                        [ css
                                            [ Css.fontSize (Css.px 12)
                                            , Css.marginTop (Css.px 6)
                                            , Css.display Css.inlineBlock
                                            ]
                                        ]
                                        [ Html.text ("Last-synced: " ++ ts) ]
                                    ]

                                Nothing ->
                                    []
                            )
                        , Html.span [ css [ Css.marginLeft (Css.px 8) ] ]
                            [ smallRedXButton (Just ClearStatus) ]
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
                        [ Html.div [ css [ Css.flexGrow (Css.num 1) ] ] [ activePlayerCompact playerA ]
                        , Html.div []
                            [ blueButtonLarge "WINNER"
                                (if model.autoSaveInProgress then Nothing else Just (MatchFinished (League.Win { lost = playerB, won = playerA })))
                            ]
                        ]
                    , -- Row 2: TIE full width
                      Html.div [ css [ Css.marginBottom (Css.px 10) ] ]
                        [ blueButtonLarge "TIE"
                            (if model.autoSaveInProgress then Nothing else Just (MatchFinished (League.Draw { playerA = playerA, playerB = playerB })))
                        ]
                    , -- Row 3: Player B with WINNER on the right
                      Html.div
                        [ css [ Css.displayFlex, Css.alignItems Css.center, Css.justifyContent Css.spaceBetween ] ]
                        [ Html.div [ css [ Css.flexGrow (Css.num 1) ] ] [ activePlayerCompact playerB ]
                        , Html.div []
                            [ blueButtonLarge "WINNER"
                                (if model.autoSaveInProgress then Nothing else Just (MatchFinished (League.Win { won = playerB, lost = playerA })))
                            ]
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
                        [ blueButton "WINNER" 
                            (if model.autoSaveInProgress then Nothing else Just (MatchFinished (League.Win { lost = playerB, won = playerA })))
                        ]
                    , Html.div [ css [ Css.width (Css.pct 20) ] ]
                        [ blueButton "TIE" 
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
                        (if isPlayerIgnored playerA (History.current model.history) then
                            [ zzzUnignoreButton (Just (KeeperWantsToUnignorePlayer playerA)) ]
                        else
                            [ zzzIgnoreButton (Just (KeeperWantsToIgnorePlayer playerA)) ]
                        )
                    , Html.div [ css [ Css.width (Css.pct 20), Css.textAlign Css.center ] ]
                        [ Html.text "" ]
                    , Html.div [ css [ Css.width (Css.pct 40), Css.textAlign Css.center, Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.display Css.none ] ] ]
                        (if isPlayerIgnored playerB (History.current model.history) then
                            [ zzzUnignoreButton (Just (KeeperWantsToUnignorePlayer playerB)) ]
                        else
                            [ zzzIgnoreButton (Just (KeeperWantsToIgnorePlayer playerB)) ]
                        )
                    ]
                , Html.div
                    [ css
                        [ Css.displayFlex
                        , Css.padding4 (Css.px 32) (Css.pct 20) Css.zero (Css.pct 20)
                        , Css.justifyContent Css.spaceAround
                        ]
                    ]
                    [ -- Desktop controls: Undo/Redo/Skip same size
                                            Html.div
                                                [ css [ Css.displayFlex, Css.justifyContent Css.spaceAround
                              , Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.display Css.none ]
                              ]
                        ]
                        [ blueButton "UNDO" (Maybe.map (\_ -> KeeperWantsToUndo) (History.peekBack model.history))
                        , blueButton "REDO" (Maybe.map (\_ -> KeeperWantsToRedo) (History.peekForward model.history))
                        , button (Css.hex "999") "SKIP" (Just KeeperWantsToSkipMatch)
                        ]
                    , -- Mobile: large Skip button only
                      Html.div
                        [ css [ Css.display Css.none
                              , Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ]
                                  [ Css.display Css.block ]
                              , Css.width (Css.pct 100)
                              , Css.textAlign Css.center
                              ]
                        ]
                        [ buttonLarge (Css.hex "999") "SKIP" (Just KeeperWantsToSkipMatch) ]
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
                , Css.whiteSpace Css.noWrap
                , Css.overflow Css.hidden
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
                    , Html.td [ css [ numericRank, shrinkWidth, center ] ] [ Html.text (String.fromInt (rank + 1)) ]
                    , Html.td [ css [ textual, left ] ]
                        [ Html.span [] [ Html.text (Player.name player) ] ]
                    , Html.td [ css [ numericDim, shrinkWidth, center, Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.display Css.none ] ] ] [ Html.text (String.fromInt (Player.rating player)) ]
                    , Html.td [ css [ numericDim, shrinkWidth, center, Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.display Css.none ] ] ] [ Html.text (String.fromInt (Player.matchesPlayed player)) ]
                    , Html.td [ css [ textual, shrinkWidth, center, Css.whiteSpace Css.noWrap ] ]
                        (let baseActions =
                                if isPlayerIgnored player (History.current model.history) then
                                    [ zzzUnignoreButtonSmall (Just (KeeperWantsToUnignorePlayer player)) ]
                                else
                                    [ smallRedXButtonSmall (Just (KeeperWantsToRetirePlayer player))
                                    , Html.span [ css [ Css.paddingLeft (Css.px 6) ] ] [ zzzIgnoreButtonSmall (Just (KeeperWantsToIgnorePlayer player)) ]
                                    ]
                         in
                         baseActions
                            ++ [ Html.span [ css [ Css.paddingLeft (Css.px 6) ] ]
                                    [ toggleChipSmall "AM" (Player.playsAM player) (Css.hex "F59E0B") (TogglePlayerAM player) ]
                               , Html.span [ css [ Css.paddingLeft (Css.px 4) ] ]
                                    [ toggleChipSmall "PM" (Player.playsPM player) (Css.hex "8B5CF6") (TogglePlayerPM player) ]
                               ]
                        )
                    ]
                )
            )
        |> (::)
            ( "players-header"
            , Html.tr
                [ css [ Css.height (Css.px 40), Css.borderBottom3 (Css.px 2) Css.solid (Css.hex "D1D5DB") ] ]
                [ Html.th [ css [ Css.width (Css.px 20) ] ] []
                , Html.th [ css [ header, center ] ] [ Html.text "RANK" ]
                , Html.th [ css [ header, left ] ] [ Html.text "NAME" ]
                , Html.th [ css [ header, center, Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.display Css.none ] ] ] [ Html.text "RATING" ]
                , Html.th [ css [ header, center, Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.display Css.none ] ] ] [ Html.text "MATCHES" ]
                , Html.th [ css [ header, center ] ] [ Html.text "ACTIONS" ]
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
                , Css.tableLayout Css.fixed
                , Css.overflowX Css.hidden
                , Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ]
                    [ Css.width (Css.pct 100) ]
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
    button (Css.hex "0091FF")


greenButton : String -> Maybe Msg -> Html Msg
greenButton =
    button (Css.hex "6DD400")


redButton : String -> Maybe Msg -> Html Msg
redButton =
    button (Css.hex "E02020")


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
            , Css.display Css.inlineFlex
            , Css.alignItems Css.center
            , Css.gap (Css.px 6)
            , modernSansSerif
            ]
        , case maybeMsg of
            Just m -> Events.onClick m
            Nothing -> Attributes.disabled True
        ]
        [ -- Simple floppy disk emoji as icon; could swap for SVG later
          Html.span [] [ Html.text "💾" ]
        , Html.span [] [ Html.text "Save" ]
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
    buttonLarge (Css.hex "0091FF")


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
            [ Css.paddingTop (Css.px 2)
            , Css.paddingBottom (Css.px 2)
            , Css.paddingLeft (Css.px 6)
            , Css.paddingRight (Css.px 6)
            , Css.margin2 Css.zero (Css.px 4)
            , Css.minWidth (Css.px 28)
            , case maybeMsg of
                Just _ -> Css.backgroundColor (Css.hex "E02020")
                Nothing -> Css.backgroundColor (Css.hex "DDD")
            , Css.border Css.zero
            , Css.borderRadius (Css.px 4)
            , Css.cursor Css.pointer
            , Css.fontSize (Css.px 12)
            , Css.fontWeight (Css.int 700)
            , Css.color (Css.hex "FFF")
            , modernSansSerif
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
            [ Css.paddingTop (Css.px 2)
            , Css.paddingBottom (Css.px 4)
            , Css.paddingLeft (Css.px 8)
            , Css.paddingRight (Css.px 8)
            , Css.margin2 Css.zero (Css.px 4)
            , Css.minWidth (Css.px 36)
            , Css.backgroundColor (Css.hex "6B7280")
            , Css.border Css.zero
            , Css.borderRadius (Css.px 9999)
            , Css.cursor Css.pointer
            , Css.fontSize (Css.px 11)
            , Css.fontWeight (Css.int 700)
            , Css.color (Css.hex "FFF")
            , modernSansSerif
            , Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.paddingLeft (Css.px 6), Css.paddingRight (Css.px 6), Css.fontSize (Css.px 10) ]
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
            [ Css.paddingTop (Css.px 2)
            , Css.paddingBottom (Css.px 4)
            , Css.paddingLeft (Css.px 8)
            , Css.paddingRight (Css.px 8)
            , Css.margin2 Css.zero (Css.px 4)
            , Css.minWidth (Css.px 36)
            , Css.backgroundColor (Css.hex "374151")
            , Css.border Css.zero
            , Css.borderRadius (Css.px 9999)
            , Css.cursor Css.pointer
            , Css.fontSize (Css.px 11)
            , Css.fontWeight (Css.int 700)
            , Css.color (Css.hex "FFF")
            , Css.textDecoration Css.lineThrough
            , modernSansSerif
            , Media.withMedia [ Media.only Media.screen [ Media.maxWidth (Css.px 640) ] ] [ Css.paddingLeft (Css.px 6), Css.paddingRight (Css.px 6), Css.fontSize (Css.px 10) ]
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

-- Smaller toggle chip for table row
toggleChipSmall : String -> Bool -> Css.Color -> Msg -> Html Msg
toggleChipSmall label isOn colorOn msg =
    Html.button
        [ css
            [ Css.display Css.inlineBlock
            , Css.padding2 (Css.px 2) (Css.px 6)
            , Css.borderRadius (Css.px 9999)
            , (if isOn then Css.backgroundColor colorOn else Css.backgroundColor (Css.hex "E5E7EB"))
            , (if isOn then Css.color (Css.hex "FFFFFF") else Css.color (Css.hex "6B7280"))
            , Css.fontSize (Css.px 10)
            , Css.fontWeight (Css.int 700)
            , Css.border Css.zero
            , Css.cursor Css.pointer
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
