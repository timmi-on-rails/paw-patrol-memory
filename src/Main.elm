module Main exposing (main)

import Browser
import Html exposing (button, div, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Memory
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
    | WaitForClick (Memory.Model Pup Player)
    | Freeze (Memory.Model Pup Player)
    | GameOver (Memory.Model Pup Player)


type Msg
    = ShuffledTiles (List Pup)
    | CardClicked Int
    | Turn
    | RestartGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( ShufflingTiles, ShuffledTiles cards ) ->
            ( WaitForClick <| Memory.init cards ( MayorGoodway, [ MayorHumdinger ] )
            , Cmd.none
            )

        ( WaitForClick g, CardClicked index ) ->
            let
                newGameData =
                    Memory.showCard index g
            in
            case Memory.gameMode newGameData of
                Memory.TurnWin ->
                    ( Freeze newGameData
                    , Task.perform (\_ -> Turn) <|
                        Process.sleep 500
                    )

                Memory.TurnLoose ->
                    ( Freeze newGameData
                    , Task.perform (\_ -> Turn) <|
                        Process.sleep 2000
                    )

                Memory.NextCard ->
                    ( WaitForClick newGameData, Cmd.none )

                Memory.GameOver ->
                    ( model, Cmd.none )

        ( Freeze g, Turn ) ->
            let
                newGame =
                    Memory.processTurn g
            in
            ( case Memory.gameMode newGame of
                Memory.GameOver ->
                    GameOver newGame

                _ ->
                    WaitForClick newGame
            , Cmd.none
            )

        ( GameOver _, RestartGame ) ->
            init ()

        _ ->
            ( model, Cmd.none )


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


viewFinish : Memory.Model a Player -> Html.Html Msg
viewFinish g =
    div []
        [ text <|
            "Mayor Gutherz: "
                ++ String.fromInt
                    ((Memory.tiles g
                        |> List.map
                            (\x ->
                                case x of
                                    Memory.Taken player _ ->
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
                    ((Memory.tiles g
                        |> List.map
                            (\x ->
                                case x of
                                    Memory.Taken player _ ->
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
        , button [ onClick RestartGame ] [ text "Again" ]
        ]


viewGame : Memory.Model Pup Player -> Html.Html Msg
viewGame g =
    div
        [ class "main-container" ]
        [ div [ class "mayor-row" ]
            [ viewAvatar "goodway" (Memory.currentPlayer g == MayorGoodway)
            , viewAvatar "humdinger" (Memory.currentPlayer g == MayorHumdinger)
            ]
        , div
            [ class "card-container" ]
            (Memory.tiles g |> List.indexedMap viewTile)
        ]


viewAvatar : String -> Bool -> Html.Html msg
viewAvatar cl active =
    div
        [ class "avatar"
        , class ("avatar-" ++ cl)
        , classList [ ( "avatar-inactive", not active ) ]
        ]
        []


viewTile : Int -> Memory.Tile Pup Player -> Html.Html Msg
viewTile index tile =
    div
        (onClick (CardClicked index)
            :: (case tile of
                    Memory.Taken _ _ ->
                        [ class "card", class "card-missing" ]

                    Memory.Hidden _ ->
                        [ class "card", class "card-hidden" ]

                    Memory.Open card ->
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
