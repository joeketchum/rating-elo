port module Main exposing (..)

import Accessibility.Styled as Html exposing (Html)
import Browser exposing (Document)
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
import Player exposing (Player)
import Random
import Task

-- Ports for persisting standings in the browser (localStorage)
port saveStandings : String -> Cmd msg

-- Elm will send a simple request (any string) and JS should reply via `receiveStandings`
port askForStandings : String -> Cmd msg

-- JS can send saved standings JSON strings into Elm through this port
port receiveStandings : (String -> msg) -> Sub msg

-- Auto-save preference ports
port saveAutoSave : Bool -> Cmd msg
port askForAutoSave : String -> Cmd msg
port receiveAutoSave : (Bool -> msg) -> Sub msg


type alias Flags =
    ()


type alias Model =
    { history : History League

    -- view state: new player form
    , newPlayerName : String
    , autoSave : Bool
    , status : Maybe String
    }


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
    | KeeperWantsToLoadStandings
    | SelectedStandingsFile File
    | KeeperWantsToUndo
    | KeeperWantsToRedo
    | LoadedLeague (Result String League)
    | ReceivedStandings String
    | ReceivedAutoSave Bool
    | ToggleAutoSave
    | ShowStatus String
    | ClearStatus
    | IgnoredKey


init : Flags -> ( Model, Cmd Msg )
init _ =
        ( { history = History.init 50 League.init
            , newPlayerName = ""
            , autoSave = False
            , status = Nothing
            }
        , askForAutoSave "init"
        )
                |> startNextMatchIfPossible


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeeperUpdatedNewPlayerName newPlayerName ->
            ( { model | newPlayerName = newPlayerName }
            , Cmd.none
            )

        KeeperWantsToAddNewPlayer ->
            ( { model
                | history = History.mapPush (League.addPlayer (Player.init model.newPlayerName)) model.history
                , newPlayerName = ""
              }
            , Cmd.none
            )
                |> startNextMatchIfPossible

        KeeperWantsToRetirePlayer player ->
            ( { model | history = History.mapPush (League.retirePlayer player) model.history }
            , Cmd.none
            )
                |> startNextMatchIfPossible

        KeeperWantsToIgnorePlayer player ->
            ( { model | history = History.mapPush (League.ignorePlayer player) model.history }
            , Cmd.none
            )
                |> startNextMatchIfPossible

        KeeperWantsToUnignorePlayer player ->
            ( { model | history = History.mapPush (League.unignorePlayer player) model.history }
            , Cmd.none
            )
                |> startNextMatchIfPossible

        KeeperWantsToSkipMatch ->
            -- Clear the current match without updating ratings; then get
            -- the next one.
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
            ( { model | history = History.mapPush (League.finishMatch outcome) model.history }
            , Cmd.none
            )
                |> startNextMatchIfPossible

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

        KeeperWantsToLoadStandings ->
            ( model
            , Select.file [ "application/json" ] SelectedStandingsFile
            )

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

        LoadedLeague (Err problem) ->
            -- show an error
            ( { model | status = Just ("Failed to load standings: " ++ problem) }
            , Cmd.none
            )

        ReceivedStandings jsonString ->
            case Decode.decodeString League.decoder jsonString of
                Ok league ->
                    ( { model | history = History.init 50 league }
                    , Task.succeed (ShowStatus "Standings loaded") |> Task.perform identity
                    )
                        |> startNextMatchIfPossible
                        |> maybeAutoSave
                Err _ ->
                    -- show malformed error
                    ( { model | status = Just "Saved standings malformed or unreadable" }
                    , Cmd.none
                    )

        ReceivedAutoSave value ->
            ( { model | autoSave = value }
            , Cmd.none
            )

        ToggleAutoSave ->
            let
                newVal = not model.autoSave
            in
            ( { model | autoSave = newVal }
            , Cmd.batch [ saveAutoSave newVal, Task.succeed (ShowStatus (if newVal then "Auto-save enabled" else "Auto-save disabled")) |> Task.perform identity ]
            )

        ShowStatus message ->
            ( { model | status = Just message }
            , Cmd.none
            )

        ClearStatus ->
            ( { model | status = Nothing }
            , Cmd.none
            )

        IgnoredKey ->
            ( model, Cmd.none )


maybeAutoSave : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
maybeAutoSave ( model, cmd ) =
    if model.autoSave then
        ( model, Cmd.batch [ cmd, saveStandings (encode 2 (League.encode (History.current model.history))) ] )
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
                ]


startNextMatchIfPossible : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
startNextMatchIfPossible ( model, cmd ) =
    if League.currentMatch (History.current model.history) /= Nothing then
        -- there's a match already in progress; no need to overwrite it.
        ( model, cmd )

    else
        ( model
        , Cmd.batch
            [ cmd
            , Random.generate GotNextMatch (League.nextMatch (History.current model.history))
            ]
        )


openSans : Css.Style
openSans =
    Css.fontFamilies [ "'Open Sans'", "sans-serif" ]


view : Model -> Document Msg
view model =
    { title = "The Rating Game ❤️"
    , body =
        [ Css.Reset.meyerV2
        , Css.Reset.borderBoxV201408
        , WildWildHtml.node "style" [] [ Html.text """
            @font-face {
                font-family: "Open Sans";
                src: url("/fonts/OpenSans-Regular-webfont.woff");
                font-weight: 500;
            }

            @font-face {
                font-family: "Open Sans";
                src: url("/fonts/OpenSans-Semibold-webfont.woff");
                font-weight: 600;
            }
          """ ]
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
                    , goldButton (if model.autoSave then "Auto-save: On" else "Auto-save: Off") (Just ToggleAutoSave)
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
                            , Css.backgroundColor (Css.hex "333")
                            , Css.color (Css.hex "FFF")
                            , Css.padding4 (Css.px 8) (Css.px 12) (Css.px 8) (Css.px 12)
                            , Css.borderRadius (Css.px 6)
                            , openSans
                            ]
                        ]
                        [ Html.span [] [ Html.text message ]
                        , Html.span [ css [ Css.marginLeft (Css.px 8) ] ] [ smallRedXButton (Just ClearStatus) ]
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
                    [ openSans
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
                , blueButton "Load Standings" (Just KeeperWantsToLoadStandings)
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
                        [ Css.borderRadius (Css.px 5)
                        , Css.overflow Css.hidden
                        , Css.height (Css.px 5)
                        , Css.width (Css.pct 100)
                        , Css.backgroundColor (Css.hex "EEE")
                        ]
                    ]
                    [ Html.div
                        [ css
                            [ Css.width (Css.pct (100 * chanceAWins))
                            , Css.height (Css.pct 100)
                            , Css.backgroundColor (Css.hex "6DD400")
                            ]
                        ]
                        []
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
                        [ css
                            [ openSans
                            , Css.width (Css.pct 20)
                            , Css.textAlign Css.center
                            ]
                        ]
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
                    [ Html.div
                        [ css [ Css.width (Css.pct 40) ] ]
                        [ blueButton "Winner!" (Just (MatchFinished (League.Win { lost = playerB, won = playerA }))) ]
                    , Html.div
                        [ css [ Css.width (Css.pct 20) ] ]
                        [ blueButton "Tie!" (Just (MatchFinished (League.Draw { playerA = playerA, playerB = playerB }))) ]
                    , Html.div
                        [ css [ Css.width (Css.pct 40) ] ]
                        [ blueButton "Winner!" (Just (MatchFinished (League.Win { won = playerB, lost = playerA }))) ]
                    ]
                , Html.div
                    [ css
                        [ Css.displayFlex
                        , Css.justifyContent Css.spaceBetween
                        , Css.paddingTop (Css.px 12)
                        ]
                    ]
                    [ Html.div
                        [ css [ Css.width (Css.pct 40), Css.textAlign Css.center ] ]
                        [ goldButton "Ignore" (Just (KeeperWantsToIgnorePlayer playerA)) ]
                    , Html.div
                        [ css [ Css.width (Css.pct 20), Css.textAlign Css.center ] ]
                        [ Html.text "" ]
                    , Html.div
                        [ css [ Css.width (Css.pct 40), Css.textAlign Css.center ] ]
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
                Just _ ->
                    Css.backgroundColor baseColor

                Nothing ->
                    Css.backgroundColor (Css.hex "DDD")
            , Css.border Css.zero
            , Css.borderRadius (Css.px 4)
            , Css.boxShadow6 Css.inset Css.zero (Css.px -4) Css.zero Css.zero (Css.rgba 0 0 0 0.1)
            , Css.cursor Css.pointer

            -- font
            , Css.fontSize (Css.px 14)
            , Css.fontWeight (Css.int 600)
            , Css.color (Css.hex "FFF")
            ]
        , case maybeMsg of
            Just msg ->
                Events.onClick msg

            Nothing ->
                Attributes.disabled True
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
                Just _ ->
                    Css.backgroundColor (Css.hex "EFE700")

                Nothing ->
                    Css.backgroundColor (Css.hex "DDD")
            , Css.border Css.zero
            , Css.borderRadius (Css.px 4)
            , Css.boxShadow6 Css.inset Css.zero (Css.px -4) Css.zero Css.zero (Css.rgba 0 0 0 0.1)
            , Css.cursor Css.pointer

            -- font
            , Css.fontSize (Css.px 14)
            , Css.fontWeight (Css.int 600)
            , Css.color (Css.hex "333")
            ]
        , case maybeMsg of
            Just msg ->
                Events.onClick msg

            Nothing ->
                Attributes.disabled True
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
                Just _ ->
                    Css.backgroundColor (Css.hex "E02020")

                Nothing ->
                    Css.backgroundColor (Css.hex "DDD")
            , Css.border Css.zero
            , Css.borderRadius (Css.px 4)
            , Css.boxShadow6 Css.inset Css.zero (Css.px -4) Css.zero Css.zero (Css.rgba 0 0 0 0.1)
            , Css.cursor Css.pointer

            -- font
            , Css.fontSize (Css.px 14)
            , Css.fontWeight (Css.int 600)
            , Css.color (Css.hex "FFF")
            ]
        , case maybeMsg of
            Just msg ->
                Events.onClick msg

            Nothing ->
                Attributes.disabled True
        ]
        [ Html.text "X" ]


activePlayer : Player -> Html msg
activePlayer player =
    Html.h2
        [ css
            [ Css.width (Css.pct 40)
            , Css.maxWidth (Css.pct 45)
            , Css.textAlign Css.center
            , Css.fontSize (Css.px 24)
            , openSans
            ]
        ]
        [ Html.text (Player.name player) ]


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
                , openSans
                ]

        textual =
            Css.batch
                [ Css.fontWeight (Css.int 500)
                , Css.fontSize (Css.px 18)
                , Css.lineHeight (Css.px 24)
                , Css.verticalAlign Css.middle
                , openSans
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

                -- font
                , openSans
                , Css.fontWeight (Css.int 600)

                -- separators
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
                                [ css
                                    [ openSans
                                    , Css.color (Css.hex "6DD400")
                                    , Css.fontSize (Css.px 14)
                                    ]
                                ]
                                [ Html.text (String.fromInt (previousRank - rank)) ]
                            ]

                         else if rank > previousRank then
                            [ downArrow (Css.hex "E02020")
                            , Html.span
                                [ css
                                    [ openSans
                                    , Css.color (Css.hex "E02020")
                                    , Css.fontSize (Css.px 14)
                                    ]
                                ]
                                [ Html.text (String.fromInt (abs (previousRank - rank))) ]
                            ]

                         else
                            [ Html.text "" ]
                        )
                    , Html.td
                        [ css [ numeric, shrinkWidth, center ] ]
                        [ Html.text (String.fromInt (rank + 1)) ]
                    , Html.td
                        [ css [ numeric, shrinkWidth, center ] ]
                        [ Html.text (String.fromInt (Player.rating player)) ]
                    , Html.td
                        [ css [ numeric, shrinkWidth, center ] ]
                        [ Html.text (String.fromInt (Player.matchesPlayed player)) ]
                    , Html.td
                        [ css [ textual, left ] ]
                        [ Html.text (Player.name player) ]
                    , Html.td
                        [ css [ textual, shrinkWidth, center ] ]
                        (if isPlayerIgnored player (History.current model.history) then
                            [ greenButton "Unignore" (Just (KeeperWantsToUnignorePlayer player)) ]

                         else
                            [ redButton "Delete" (Just (KeeperWantsToRetirePlayer player))
                            , Html.span [ css [ Css.paddingLeft (Css.px 8) ] ] [ goldButton "Ignore" (Just (KeeperWantsToIgnorePlayer player)) ]
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
                    ++ [ ( "add-player-form"
                         , Html.tr
                            [ css [ Css.height (Css.px 60) ] ]
                            [ Html.td [] []
                            , Html.td
                                [ css [ numeric, shrinkWidth, center, Css.color (Css.hex "A6A6A6") ] ]
                                [ Html.text "-" ]
                            , Html.td
                                [ css [ numeric, shrinkWidth, center, Css.color (Css.hex "A6A6A6") ] ]
                                [ Html.text (String.fromInt Elo.initialRating) ]
                            , Html.td
                                [ css [ numeric, shrinkWidth, center, Css.color (Css.hex "A6A6A6") ] ]
                                [ Html.text "0" ]
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
                            , Html.td
                                [ css [ numeric, shrinkWidth, center ] ]
                                [ greenButton "Add" (Just KeeperWantsToAddNewPlayer) ]
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


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
