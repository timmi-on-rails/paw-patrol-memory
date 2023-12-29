module Main exposing (main)

import Browser
import Html exposing (button, div, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Process
import Random
import Random.List
import Task


type Pup
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


allPups : List Pup
allPups =
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
    ( ShufflingPups
    , Random.generate ShuffledPups (Random.List.shuffle (allPups ++ allPups))
    )


type Player
    = MayorGoodway
    | MayorHumdinger


type Model
    = ShufflingPups
    | WaitForClick GameData
    | Freeze GameData
    | GameOver GameData


type alias GameData =
    { places : List Tile
    , currentPlayer : Player
    }


type Tile
    = TakenByMayorGoodway Pup
    | TakenByMayorHumdinger Pup
    | Hidden Pup
    | Open Pup


type Msg
    = ShuffledPups (List Pup)
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
        ( ShufflingPups, ShuffledPups cards ) ->
            ( WaitForClick
                { places = cards |> List.map Hidden
                , currentPlayer = MayorGoodway
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
                    GameOver newGame

                _ ->
                    WaitForClick <| newGame
            , Cmd.none
            )

        ( GameOver _, NewGame ) ->
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
                            case g.currentPlayer of
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
        , currentPlayer =
            case gMode of
                TurnWin ->
                    next g.currentPlayer

                TurnLoose ->
                    next g.currentPlayer

                _ ->
                    g.currentPlayer
    }


showCard : Int -> List Tile -> List Tile
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


visibleCards : List Tile -> List Pup
visibleCards =
    List.filterMap
        (\place ->
            case place of
                Open c ->
                    Just c

                _ ->
                    Nothing
        )


next : Player -> Player
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

        GameOver g ->
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
            [ viewAvatar "goodway" (g.currentPlayer == MayorGoodway)
            , viewAvatar "humdinger" (g.currentPlayer == MayorHumdinger)
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


viewPlace : Int -> Tile -> Html.Html Msg
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


cardToClass : Pup -> String
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
