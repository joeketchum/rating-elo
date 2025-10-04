port module Main exposing (..)

import Accessibility.Styled as Html exposing (Html)
import Process
import Browser exposing (Document)
import Time
import Css
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
      , votesUntilDriveSync = 20
      , shouldStartNextMatchAfterLoad = False
      , autoSaveInProgress = False
      }
    , Cmd.batch [ askForAutoSave "init", httpRequest ]
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
                            IgnoredKey
                )

        Nothing ->
            Sub.batch
                [ receiveStandings ReceivedStandings
                , receiveAutoSave ReceivedAutoSave
                , receivePublicDriveStatus ReceivedPublicDriveStatus
                , receiveMatchSaveComplete (\_ -> AutoSaveCompleted)
                , Time.every (30 * 1000) (\_ -> PeriodicSync) -- Every 30 seconds
                ]


startNextMatchIfPossible : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
startNextMatchIfPossible ( model, cmd ) =
    if League.currentMatch (History.current model.history) /= Nothing then
        ( model, cmd )

    else
        ( model
        , Cmd.batch
            [ cmd
            , Random.generate GotNextMatch (League.nextMatch (History.current model.history))
            ]
        )


maybeSaveToDriveAfterVote : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
maybeSaveToDriveAfterVote ( model, cmd ) =
    let
        newCount = model.votesUntilDriveSync - 1
    in
    if newCount <= 0 then
        -- Save to Drive and don't start next match until reload completes
        ( { model | votesUntilDriveSync = 20, status = Just "Saving to Google Sheets...", autoSaveInProgress = True }
        , Cmd.batch
            [ saveToPublicDrive (encode 0 (League.encode (History.current model.history)))
            , Task.succeed (ShowStatus "Auto-saving to Drive...") |> Task.perform identity
            , Process.sleep 10000 |> Task.perform (\_ -> AutoSaveTimeout) -- 10 second timeout
            , sendVoteCount 20  -- Reset to 20
            ]
        )
    else
        ( { model | votesUntilDriveSync = newCount }, Cmd.batch [ cmd, sendVoteCount newCount ] )



-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                ( { model | shouldStartNextMatchAfterLoad = True, autoSaveInProgress = False, status = Just "Auto-save completed! Loading next match..." }
                , loadFromPublicDrive ""
                )
            else
                -- This was a manual save, just reload without starting next match
                ( { model | shouldStartNextMatchAfterLoad = False, status = Just "Manual save completed! Reloading data..." }
                , loadFromPublicDrive ""
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



-- VIEW

modernSansSerif : Css.Style
modernSansSerif =
    Css.fontFamilies [ "system-ui", "-apple-system", "BlinkMacSystemFont", "'Segoe UI'", "'Roboto'", "'Inter'", "'Helvetica Neue'", "Arial", "sans-serif" ]


view : Model -> Document Msg
view model =
    { title = "The Rating Game ❤️"
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
                , rankings model
                , Html.section
                    [ css [ Css.textAlign Css.center, Css.marginTop (Css.px 32) ] ]
                    [ blueButton "Export rankings" (Just KeeperWantsToSaveStandings)
                    , blueButton "Save to Drive" (Just KeeperWantsToSaveToDrive)
                    , blueButton "Refresh from Drive" (Just KeeperWantsToRefreshFromDrive)
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
                        [ Css.fontSize (Css.px 32)
                        , Css.marginBottom (Css.px 18)
                        , Css.fontWeight (Css.int 600)
                        ]
                    ]
                    [ Html.text "The Rating Game ❤️" ]
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
                            , modernSansSerif
                            ]
                        ]
                        [ Html.text "⚔️ BATTLE PREDICTION ⚔️" ]
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
                                , Css.backgroundColor (Css.hex "FF6B6B")
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
                                , Css.backgroundColor (Css.hex "4ECDC4")
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
                        ]
                    ]
                    [ activePlayer playerA
                    , Html.p
                        [ css [ modernSansSerif, Css.width (Css.pct 20), Css.textAlign Css.center ] ]
                        [ Html.text "vs." ]
                    , activePlayer playerB
                    ]
                , Html.div
                    [ css
                        [ Css.displayFlex
                        , Css.paddingTop (Css.px 32)
                        , Css.textAlign Css.center
                        ]
                    ]
                    [ Html.div [ css [ Css.width (Css.pct 40) ] ]
                        [ blueButton "Winner!" 
                            (if model.autoSaveInProgress then Nothing else Just (MatchFinished (League.Win { lost = playerB, won = playerA })))
                        ]
                    , Html.div [ css [ Css.width (Css.pct 20) ] ]
                        [ blueButton "Tie!" 
                            (if model.autoSaveInProgress then Nothing else Just (MatchFinished (League.Draw { playerA = playerA, playerB = playerB })))
                        ]
                    , Html.div [ css [ Css.width (Css.pct 40) ] ]
                        [ blueButton "Winner!" 
                            (if model.autoSaveInProgress then Nothing else Just (MatchFinished (League.Win { won = playerB, lost = playerA })))
                        ]
                    ]
                , Html.div
                    [ css
                        [ Css.displayFlex
                        , Css.justifyContent Css.spaceBetween
                        , Css.paddingTop (Css.px 12)
                        ]
                    ]
                    [ Html.div [ css [ Css.width (Css.pct 40), Css.textAlign Css.center ] ]
                        [ goldButton "Ignore" (Just (KeeperWantsToIgnorePlayer playerA)) ]
                    , Html.div [ css [ Css.width (Css.pct 20), Css.textAlign Css.center ] ]
                        [ Html.text "" ]
                    , Html.div [ css [ Css.width (Css.pct 40), Css.textAlign Css.center ] ]
                        [ goldButton "Ignore" (Just (KeeperWantsToIgnorePlayer playerB)) ]
                    ]
                , Html.div
                    [ css
                        [ Css.displayFlex
                        , Css.padding4 (Css.px 32) (Css.pct 20) Css.zero (Css.pct 20)
                        , Css.justifyContent Css.spaceAround
                        ]
                    ]
                    [ blueButton "Undo" (Maybe.map (\_ -> KeeperWantsToUndo) (History.peekBack model.history))
                    , blueButton "Redo" (Maybe.map (\_ -> KeeperWantsToRedo) (History.peekForward model.history))
                    , button (Css.hex "999") "Skip" (Just KeeperWantsToSkipMatch)
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

        numeric =
            Css.batch
                [ Css.fontWeight (Css.int 600)
                , Css.fontSize (Css.px 21)
                , Css.verticalAlign Css.middle
                , modernSansSerif
                ]

        textual =
            Css.batch
                [ Css.fontWeight (Css.int 500)
                , Css.fontSize (Css.px 18)
                , Css.lineHeight (Css.px 24)
                , Css.verticalAlign Css.middle
                , modernSansSerif
                , Css.paddingLeft (Css.px 15)
                ]

        shrinkWidth =
            Css.batch
                [ Css.paddingLeft (Css.px 15)
                , Css.paddingRight (Css.px 15)
                , Css.width (Css.pct 1)
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
                    [ css [ Css.height (Css.px 60) ] ]
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
                    , Html.td [ css [ numeric, shrinkWidth, center ] ] [ Html.text (String.fromInt (rank + 1)) ]
                    , Html.td [ css [ numeric, shrinkWidth, center ] ] [ Html.text (String.fromInt (Player.rating player)) ]
                    , Html.td [ css [ numeric, shrinkWidth, center ] ] [ Html.text (String.fromInt (Player.matchesPlayed player)) ]
                    , Html.td [ css [ textual, left ] ] [ Html.text (Player.name player) ]
                    , Html.td [ css [ textual, shrinkWidth, center ] ]
                        (if isPlayerIgnored player (History.current model.history) then
                            [ zzzUnignoreButton (Just (KeeperWantsToUnignorePlayer player)) ]

                         else
                            [ smallRedXButton (Just (KeeperWantsToRetirePlayer player))
                            , Html.span [ css [ Css.paddingLeft (Css.px 8) ] ]
                                [ zzzIgnoreButton (Just (KeeperWantsToIgnorePlayer player)) ]
                            ]
                        )
                    ]
                )
            )
        |> (::)
            ( "players-header"
            , Html.tr
                [ css [ Css.height (Css.px 45) ] ]
                [ Html.th [ css [ Css.width (Css.px 20) ] ] []
                , Html.th [ css [ header, center ] ] [ Html.text "Rank" ]
                , Html.th [ css [ header, center ] ] [ Html.text "Rating" ]
                , Html.th [ css [ header, center ] ] [ Html.text "Matches" ]
                , Html.th [ css [ header, left ] ] [ Html.text "Name" ]
                , Html.th [ css [ header, center ] ] [ Html.text "Actions" ]
                ]
            )
        |> (\tableGuts ->
                tableGuts
                    ++
                        [ ( "add-player-form"
                          , Html.tr
                                [ css [ Css.height (Css.px 60) ] ]
                                [ Html.td [] []
                                , Html.td [ css [ numeric, shrinkWidth, center, Css.color (Css.hex "A6A6A6") ] ] [ Html.text "-" ]
                                , Html.td [ css [ numeric, shrinkWidth, center, Css.color (Css.hex "A6A6A6") ] ] [ Html.text (String.fromInt Elo.initialRating) ]
                                , Html.td [ css [ numeric, shrinkWidth, center, Css.color (Css.hex "A6A6A6") ] ] [ Html.text "0" ]
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
                                , Html.td [ css [ numeric, shrinkWidth, center ] ] [ greenButton "Add" (Just KeeperWantsToAddNewPlayer) ]
                                ]
                          )
                        ]
           )
        |> Keyed.node "table"
            [ css
                [ Css.width (Css.pct 80)
                , Css.margin2 Css.zero Css.auto
                , Css.borderCollapse Css.collapse
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


activePlayer : Player -> Html msg
activePlayer player =
    Html.h2
        [ css
            [ Css.width (Css.pct 40)
            , Css.maxWidth (Css.pct 45)
            , Css.textAlign Css.center
            , Css.fontSize (Css.px 24)
            , modernSansSerif
            ]
        ]
        [ Html.text (Player.name player) ]


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
