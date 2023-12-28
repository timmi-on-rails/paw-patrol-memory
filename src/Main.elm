module Main exposing (main)

import Browser
import Html exposing (div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Process
import Random
import Random.List
import Task


type Card
    = Rubble
    | Zuma
    | Chase
    | Marshall
    | Rocky
    | Skye
    | Everest
    | Tracker
    | Rex
    | Liberty


allCards : List Card
allCards =
    [ Rubble
    , Zuma
    , Chase
    , Marshall
    , Rocky
    , Skye
    , Everest
    , Tracker
    , Rex
    , Liberty
    ]


type Place
    = Empty
    | Hidden Card
    | Open Card


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( ShufflingCards
    , Random.generate NewCards (Random.List.shuffle (allCards ++ allCards))
    )


type Turn
    = MayorGoodway
    | MayorHumdinger


type Model
    = ShufflingCards
    | WaitForClick GameData
    | Freeze GameData


type alias GameData =
    { places : List Place
    , turn : Turn
    }


type Msg
    = NewCards (List Card)
    | CardClicked Int
    | Turn


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( ShufflingCards, NewCards cards ) ->
            ( WaitForClick
                { places = cards |> List.map Hidden
                , turn = MayorGoodway
                }
            , Cmd.none
            )

        ( WaitForClick g, CardClicked index ) ->
            let
                newGameData =
                    { g | places = showCard index g.places }
            in
            if List.length (visibleCards newGameData.places) > 1 then
                ( Freeze newGameData
                , Task.perform (\_ -> Turn) <|
                    Process.sleep
                        (if isMatch newGameData.places then
                            500

                         else
                            2000
                        )
                )

            else
                ( WaitForClick newGameData, Cmd.none )

        ( Freeze g, Turn ) ->
            ( WaitForClick <| processTurn g, Cmd.none )

        _ ->
            ( model, Cmd.none )


processTurn : GameData -> GameData
processTurn g =
    let
        winCard =
            case visibleCards g.places of
                [] ->
                    Nothing

                y :: ys ->
                    if List.all ((==) y) ys then
                        Just y

                    else
                        Nothing

        newPlaces =
            List.map
                (\p ->
                    case ( p, winCard ) of
                        ( Open c, Just wc ) ->
                            if wc == c then
                                Empty

                            else
                                Hidden c

                        ( Open c, Nothing ) ->
                            Hidden c

                        _ ->
                            p
                )
                g.places
    in
    { g
        | places = newPlaces
        , turn =
            case winCard of
                Just _ ->
                    g.turn

                Nothing ->
                    next g.turn
    }


showCard : Int -> List Place -> List Place
showCard index =
    List.indexedMap
        (\i p ->
            if i == index then
                case p of
                    Hidden c ->
                        Open c

                    _ ->
                        p

            else
                p
        )


isMatch : List Place -> Bool
isMatch places =
    case visibleCards places of
        x :: y :: _ ->
            x == y

        _ ->
            False


visibleCards : List Place -> List Card
visibleCards =
    List.foldl
        (\place cards ->
            case place of
                Open c ->
                    c :: cards

                _ ->
                    cards
        )
        []


next : Turn -> Turn
next turn =
    case turn of
        MayorGoodway ->
            MayorHumdinger

        MayorHumdinger ->
            MayorGoodway


view : Model -> Html.Html Msg
view model =
    case model of
        WaitForClick g ->
            viewGame g

        Freeze g ->
            viewGame g

        _ ->
            text "unexpected state"


viewGame : GameData -> Html.Html Msg
viewGame g =
    div []
        [ viewTurn g.turn
        , div
            [ style "display" "grid"
            , style "grid-template-columns" "repeat(5, min-content)"
            , style "grid-gap" "10px"
            ]
            (g.places |> List.indexedMap viewPlace)
        ]


viewTurn : Turn -> Html.Html msg
viewTurn t =
    case t of
        MayorGoodway ->
            text "Bürgermeisterin Gutherz"

        MayorHumdinger ->
            text "Bürgermeister Besserwisser"


viewPlace : Int -> Place -> Html.Html Msg
viewPlace index place =
    div
        (onClick (CardClicked index)
            :: (case place of
                    Empty ->
                        []

                    Hidden _ ->
                        [ class "card", class "card-hidden" ]

                    Open card ->
                        [ class "card", class <| cardToClass card ]
               )
        )
        []


cardToClass : Card -> String
cardToClass card =
    case card of
        Rubble ->
            "card-rubble"

        Zuma ->
            "card-zuma"

        Chase ->
            "card-chase"

        Marshall ->
            "card-marshall"

        Rocky ->
            "card-rocky"

        Skye ->
            "card-skye"

        Everest ->
            "card-everest"

        Tracker ->
            "card-tracker"

        Rex ->
            "card-rex"

        Liberty ->
            "card-liberty"
