module Main exposing (main)

import Bitwise exposing (and)
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
    | Game GameData


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
    case msg of
        NewCards cards ->
            ( Game
                { places = cards |> List.map Hidden
                , turn = MayorGoodway
                }
            , Cmd.none
            )

        CardClicked index ->
            case model of
                Game g ->
                    if List.length (visibleCards g.places) < 2 then
                        let
                            newGameData =
                                { g | places = showCard index g.places }
                        in
                        ( Game newGameData
                        , if List.length (visibleCards newGameData.places) > 1 then
                            Task.perform (\_ -> Turn) <|
                                Process.sleep
                                    (if isMatch newGameData.places then
                                        1000

                                     else
                                        3000
                                    )

                          else
                            Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Turn ->
            case model of
                Game g ->
                    ( Game
                        { g
                            | places = hideAll g.places
                            , turn = next g.turn
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


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


hideAll : List Place -> List Place
hideAll =
    List.map
        (\p ->
            case p of
                Open c ->
                    Hidden c

                _ ->
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
        Game g ->
            div
                [ style "display" "grid"
                , style "grid-template-columns" "repeat(5, min-content)"
                , style "grid-gap" "10px"
                ]
                (g.places |> List.indexedMap viewPlace)

        _ ->
            text "test"


viewPlace : Int -> Place -> Html.Html Msg
viewPlace index place =
    div
        [ onClick (CardClicked index)
        , class "card"
        , class
            (case place of
                Empty ->
                    "empty"

                Hidden _ ->
                    "card-hidden"

                Open card ->
                    cardToClass card
            )
        ]
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
