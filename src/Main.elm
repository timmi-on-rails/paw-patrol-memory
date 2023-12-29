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
    ( ShufflingTiles
    , Random.generate ShuffledTiles (Random.List.shuffle (allPups ++ allPups))
    )


type Player
    = MayorGoodway
    | MayorHumdinger


type Model
    = ShufflingTiles
    | WaitForClick (GameData Pup Player)
    | Freeze (GameData Pup Player)
    | GameOver (GameData Pup Player)


type alias GameData a b =
    { tiles : List (Tile a b)
    , currentPlayer : b
    }


type Tile a b
    = Taken b a
    | Hidden a
    | Open a


type Msg
    = ShuffledTiles (List Pup)
    | CardClicked Int
    | Turn
    | NewGame


type GameMode
    = NextCard
    | TurnWin
    | TurnLoose
    | FinishedTotal


gameMode : GameData a b -> GameMode
gameMode g =
    let
        visibleCards : List (Tile a b) -> List a
        visibleCards =
            List.filterMap
                (\place ->
                    case place of
                        Open c ->
                            Just c

                        _ ->
                            Nothing
                )
    in
    if
        g.tiles
            |> List.all
                (\x ->
                    case x of
                        Taken _ _ ->
                            True

                        _ ->
                            False
                )
    then
        FinishedTotal

    else
        case visibleCards g.tiles of
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
        ( ShufflingTiles, ShuffledTiles cards ) ->
            ( WaitForClick
                { tiles = cards |> List.map Hidden
                , currentPlayer = MayorGoodway
                }
            , Cmd.none
            )

        ( WaitForClick g, CardClicked index ) ->
            let
                newGameData =
                    { g | tiles = showCard index g.tiles }
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


processTurn : GameData a Player -> GameData a Player
processTurn g =
    let
        gMode =
            gameMode g

        newPlaces =
            List.map
                (\p ->
                    case ( p, gMode ) of
                        ( Open c, TurnWin ) ->
                            Taken g.currentPlayer c

                        ( Open c, _ ) ->
                            Hidden c

                        _ ->
                            p
                )
                g.tiles
    in
    { g
        | tiles = newPlaces
        , currentPlayer =
            case gMode of
                TurnWin ->
                    next g.currentPlayer

                TurnLoose ->
                    next g.currentPlayer

                _ ->
                    g.currentPlayer
    }


showCard : Int -> List (Tile a b) -> List (Tile a b)
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


viewFinish : GameData a Player -> Html.Html Msg
viewFinish g =
    div []
        [ text <|
            "Mayor Gutherz: "
                ++ String.fromInt
                    ((g.tiles
                        |> List.map
                            (\x ->
                                case x of
                                    Taken player _ ->
                                        if player == MayorGoodway then
                                            1

                                        else
                                            0

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
                    ((g.tiles
                        |> List.map
                            (\x ->
                                case x of
                                    Taken player _ ->
                                        if player == MayorHumdinger then
                                            1

                                        else
                                            0

                                    _ ->
                                        0
                            )
                        |> List.sum
                     )
                        // 2
                    )
        , button [ onClick NewGame ] [ text "Again" ]
        ]


viewGame : GameData Pup Player -> Html.Html Msg
viewGame g =
    div
        [ class "main-container" ]
        [ div [ class "mayor-row" ]
            [ viewAvatar "goodway" (g.currentPlayer == MayorGoodway)
            , viewAvatar "humdinger" (g.currentPlayer == MayorHumdinger)
            ]
        , div
            [ class "card-container" ]
            (g.tiles |> List.indexedMap viewPlace)
        ]


viewAvatar : String -> Bool -> Html.Html msg
viewAvatar cl active =
    div
        [ class "avatar"
        , class ("avatar-" ++ cl)
        , classList [ ( "avatar-inactive", not active ) ]
        ]
        []


viewPlace : Int -> Tile Pup Player -> Html.Html Msg
viewPlace index place =
    div
        (onClick (CardClicked index)
            :: (case place of
                    Taken _ _ ->
                        [ class "card", class "card-missing" ]

                    Hidden _ ->
                        [ class "card", class "card-hidden" ]

                    Open card ->
                        [ class "card", class <| cssClass card ]
               )
        )
        []


cssClass : Pup -> String
cssClass card =
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
