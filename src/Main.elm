module Main exposing (main)

import Browser
import Html exposing (button, div, text)
import Html.Attributes exposing (class, classList)
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
    | Finished GameData


type alias GameData =
    { places : List Place
    , turn : Turn
    }


type Place
    = TakenByMayorGoodway Card
    | TakenByMayorHumdinger Card
    | Hidden Card
    | Open Card


type Msg
    = NewCards (List Card)
    | CardClicked Int
    | Turn
    | NewGame


type GameMode
    = NextCard
    | TurnWin
    | TurnLoose
    | FinishedTotal


gameMode : GameData -> GameMode
gameMode g =
    if
        g.places
            |> List.all
                (\x ->
                    case x of
                        TakenByMayorGoodway _ ->
                            True

                        TakenByMayorHumdinger _ ->
                            True

                        _ ->
                            False
                )
    then
        FinishedTotal

    else
        case visibleCards g.places of
            [ x, y ] ->
                if x == y then
                    TurnWin

                else
                    TurnLoose

            _ ->
                NextCard


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
            case gameMode newGameData of
                TurnWin ->
                    ( Freeze newGameData
                    , Task.perform (\_ -> Turn) <|
                        Process.sleep 500
                    )

                TurnLoose ->
                    ( Freeze newGameData
                    , Task.perform (\_ -> Turn) <|
                        Process.sleep 2000
                    )

                NextCard ->
                    ( WaitForClick newGameData, Cmd.none )

                FinishedTotal ->
                    ( model, Cmd.none )

        ( Freeze g, Turn ) ->
            let
                newGame =
                    processTurn g
            in
            ( case gameMode newGame of
                FinishedTotal ->
                    Finished newGame

                _ ->
                    WaitForClick <| newGame
            , Cmd.none
            )

        ( Finished _, NewGame ) ->
            init ()

        _ ->
            ( model, Cmd.none )


processTurn : GameData -> GameData
processTurn g =
    let
        gMode =
            gameMode g

        newPlaces =
            List.map
                (\p ->
                    case ( p, gMode ) of
                        ( Open c, TurnWin ) ->
                            case g.turn of
                                MayorGoodway ->
                                    TakenByMayorGoodway c

                                MayorHumdinger ->
                                    TakenByMayorHumdinger c

                        ( Open c, _ ) ->
                            Hidden c

                        _ ->
                            p
                )
                g.places
    in
    { g
        | places = newPlaces
        , turn =
            case gMode of
                TurnWin ->
                    next g.turn

                TurnLoose ->
                    next g.turn

                _ ->
                    g.turn
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


visibleCards : List Place -> List Card
visibleCards =
    List.filterMap
        (\place ->
            case place of
                Open c ->
                    Just c

                _ ->
                    Nothing
        )


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

        Finished g ->
            viewFinish g

        _ ->
            text "unexpected state"


viewFinish : GameData -> Html.Html Msg
viewFinish g =
    div []
        [ text <|
            "Mayor Gutherz: "
                ++ String.fromInt
                    ((g.places
                        |> List.map
                            (\x ->
                                case x of
                                    TakenByMayorGoodway _ ->
                                        1

                                    _ ->
                                        0
                            )
                        |> List.sum
                     )
                        // 2
                    )
        , text <|
            "Mayor Besserwisser: "
                ++ String.fromInt
                    ((g.places
                        |> List.map
                            (\x ->
                                case x of
                                    TakenByMayorHumdinger _ ->
                                        1

                                    _ ->
                                        0
                            )
                        |> List.sum
                     )
                        // 2
                    )
        , button [ onClick NewGame ] [ text "Again" ]
        ]


viewGame : GameData -> Html.Html Msg
viewGame g =
    div
        [ class "main-container" ]
        [ div [ class "mayor-row" ]
            [ viewAvatar "goodway" (g.turn == MayorGoodway)
            , viewAvatar "humdinger" (g.turn == MayorHumdinger)
            ]
        , div
            [ class "card-container" ]
            (g.places |> List.indexedMap viewPlace)
        ]


viewAvatar : String -> Bool -> Html.Html msg
viewAvatar cl active =
    div
        [ class "avatar"
        , class ("avatar-" ++ cl)
        , classList [ ( "avatar-inactive", not active ) ]
        ]
        []


viewPlace : Int -> Place -> Html.Html Msg
viewPlace index place =
    div
        (onClick (CardClicked index)
            :: (case place of
                    TakenByMayorGoodway _ ->
                        [ class "card", class "card-missing" ]

                    TakenByMayorHumdinger _ ->
                        [ class "card", class "card-missing" ]

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
