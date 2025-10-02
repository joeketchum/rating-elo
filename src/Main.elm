module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, h2, h3, li, span, text, ul)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Set exposing (Set)


-- DOMAIN


type alias Item =
    { id : Int
    , name : String
    }


type alias Model =
    { items : Dict Int Item
    , ratings : Dict Int Float
    , currentPair : Maybe ( Int, Int )
    , ignored : Set Int
    }


type Msg
    = ChooseLeft
    | ChooseRight
    | SkipCurrent
    | SkipAndIgnore Int
    | NextPair



-- INIT


initialItems : List Item
initialItems =
    [ { id = 1, name = "Player A" }
    , { id = 2, name = "Player B" }
    , { id = 3, name = "Player C" }
    , { id = 4, name = "Player D" }
    , { id = 5, name = "Player E" }
    , { id = 6, name = "Player F" }
    ]


initialRating : Float
initialRating =
    1200


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ids =
            List.map .id initialItems

        mItems =
            Dict.fromList (List.map (\i -> ( i.id, i )) initialItems)

        mRatings =
            Dict.fromList (List.map (\i -> ( i.id, initialRating )) initialItems)

        pair =
            pickNextPair { items = mItems, ratings = mRatings, currentPair = Nothing, ignored = Set.empty }
    in
    ( { items = mItems
      , ratings = mRatings
      , currentPair = pair
      , ignored = Set.empty
      }
    , Cmd.none
    )



-- UPDATE


kFactor : Float
kFactor =
    24


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChooseLeft ->
            case model.currentPair of
                Just ( l, r ) ->
                    let
                        rl =
                            Dict.get l model.ratings |> Maybe.withDefault initialRating

                        rr =
                            Dict.get r model.ratings |> Maybe.withDefault initialRating

                        ( rl2, rr2 ) =
                            eloUpdate ( rl, rr ) ( 1, 0 )

                        ratings2 =
                            model.ratings
                                |> Dict.insert l rl2
                                |> Dict.insert r rr2

                        model2 =
                            { model | ratings = ratings2 }
                    in
                    ( { model2 | currentPair = pickNextPair model2 }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ChooseRight ->
            case model.currentPair of
                Just ( l, r ) ->
                    let
                        rl =
                            Dict.get l model.ratings |> Maybe.withDefault initialRating

                        rr =
                            Dict.get r model.ratings |> Maybe.withDefault initialRating

                        ( rl2, rr2 ) =
                            eloUpdate ( rl, rr ) ( 0, 1 )

                        ratings2 =
                            model.ratings
                                |> Dict.insert l rl2
                                |> Dict.insert r rr2

                        model2 =
                            { model | ratings = ratings2 }
                    in
                    ( { model2 | currentPair = pickNextPair model2 }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SkipCurrent ->
            -- do not change ratings; just move on
            ( { model | currentPair = pickNextPair model }, Cmd.none )

        SkipAndIgnore pid ->
            let
                ignored2 =
                    Set.insert pid model.ignored

                model2 =
                    { model | ignored = ignored2 }
            in
            -- no rating change; blacklist and move on
            ( { model2 | currentPair = pickNextPair model2 }, Cmd.none )

        NextPair ->
            ( { model | currentPair = pickNextPair model }, Cmd.none )



-- ELO


expectedScore : Float -> Float -> Float
expectedScore ra rb =
    1 / (1 + 10 ^ ((rb - ra) / 400))


eloUpdate : ( Float, Float ) -> ( Int, Int ) -> ( Float, Float )
eloUpdate ( ra, rb ) ( sa, sb ) =
    let
        ea =
            expectedScore ra rb

        eb =
            1 - ea

        ra2 =
            ra + kFactor * (toFloat sa - ea)

        rb2 =
            rb + kFactor * (toFloat sb - eb)
    in
    ( ra2, rb2 )



-- PAIRING


eligibleIds : Model -> List Int
eligibleIds model =
    model.items
        |> Dict.keys
        |> List.filter (\id -> not (Set.member id model.ignored))


pickNextPair : Model -> Maybe ( Int, Int )
pickNextPair model =
    let
        ids =
            eligibleIds model

        -- simple, deterministic selector: take the top two by "most similar rating"
        -- fall back to just first two if you prefer
        rated id =
            Dict.get id model.ratings |> Maybe.withDefault initialRating

        sorted =
            List.sortBy rated ids
    in
    case sorted of
        a :: b :: _ ->
            Just ( a, b )

        _ ->
            Nothing



-- VIEW


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


view : Model -> Html Msg
view model =
    div [ style "font-family" "system-ui, -apple-system, Segoe UI, Roboto, Helvetica, Arial, sans-serif", style "padding" "16px" ]
        [ h2 [] [ text "Elo Anything — Preview with Skip" ]
        , case model.currentPair of
            Nothing ->
                viewNoPair model

            Just pair ->
                viewPair model pair
        , h3 [ style "margin-top" "24px" ] [ text "Leaderboard" ]
        , viewLeaderboard model
        ]


viewNoPair : Model -> Html Msg
viewNoPair model =
    let
        remaining =
            eligibleIds model |> List.length
    in
    div []
        [ text <|
            if remaining < 2 then
                "Not enough eligible players to compare. You may have skipped too many. (Reset your ignore list to continue.)"

            else
                "Loading next pair…"
        , div [ style "margin-top" "12px" ]
            [ button [ onClick NextPair, baseBtn ] [ text "Find Next Pair" ] ]
        ]


viewPair : Model -> ( Int, Int ) -> Html Msg
viewPair model ( leftId, rightId ) =
    let
        left =
            Dict.get leftId model.items

        right =
            Dict.get rightId model.items
    in
    div [ style "display" "grid", style "grid-template-columns" "1fr 1fr", style "gap" "16px", style "align-items" "start", style "margin-top" "12px" ]
        [ viewPlayerCard model leftId left True
        , viewPlayerCard model rightId right False
        , div [ style "grid-column" "1 / span 2", style "text-align" "center", style "margin-top" "8px" ]
            [ button [ onClick SkipCurrent, baseBtn ] [ text "Skip (I don’t know either)" ] ]
        ]


viewPlayerCard : Model -> Int -> Maybe Item -> Bool -> Html Msg
viewPlayerCard model pid mItem isLeft =
    let
        name_ =
            mItem |> Maybe.map .name |> Maybe.withDefault ("Player #" ++ String.fromInt pid)

        rating_ =
            Dict.get pid model.ratings |> Maybe.withDefault initialRating
    in
    div [ style "border" "1px solid #ddd", style "border-radius" "12px", style "padding" "12px" ]
        [ h3 [ style "margin" "0 0 8px 0" ] [ text name_ ]
        , span [ style "display" "inline-block", style "margin-bottom" "8px", style "color" "#666" ]
            [ text ("Rating: " ++ String.fromFloat (round1 rating_)) ]
        , div [ style "display" "flex", style "gap" "8px", style "flex-wrap" "wrap" ]
            button ([ onClick (if isLeft then ChooseLeft else ChooseRight) ] ++ primaryBtn)
    [ text (if isLeft then "◀︎ Left wins" else "Right wins ▶︎") ]

button ([ onClick (SkipAndIgnore pid) ] ++ linkBtn)
    [ text "Skip and don’t ask about this player again" ]
            ]
        ]


viewLeaderboard : Model -> Html Msg
viewLeaderboard model =
    let
        rows =
            Dict.toList model.ratings
                |> List.sortBy (\(_, r) -> -r)
                |> List.map
                    (\( id, r ) ->
                        let
                            name_ =
                                Dict.get id model.items |> Maybe.map .name |> Maybe.withDefault ("Player #" ++ String.fromInt id)

                            muted =
                                if Set.member id model.ignored then
                                    " (ignored)"
                                else
                                    ""
                        in
                        li []
                            [ text (name_ ++ muted ++ " — " ++ String.fromFloat (round1 r)) ]
                    )
    in
    ul [] rows



-- STYLES (tiny inline helpers)


baseBtn : Html.Attribute msg
baseBtn =
    style "padding" "8px 12px"


primaryBtn : List (Html.Attribute msg)
primaryBtn =
    [ baseBtn
    , style "background" "black"
    , style "color" "white"
    , style "border-radius" "8px"
    , style "border" "1px solid black"
    , style "cursor" "pointer"
    ]


linkBtn : List (Html.Attribute msg)
linkBtn =
    [ baseBtn
    , style "background" "transparent"
    , style "border" "none"
    , style "color" "#0a58ca"
    , style "text-decoration" "underline"
    , style "cursor" "pointer"
    ]


round1 : Float -> Float
round1 x =
    (toFloat (round (x * 10))) / 10
