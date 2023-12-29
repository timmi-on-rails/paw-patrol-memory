-- Abstract memory game model


module Memory exposing (GameData, GameMode(..), Tile(..), currentPlayer, gameMode, init, processTurn, showCard, tiles)


type alias Nonempty a =
    ( a, List a )


type GameData a b
    = GameData
        { tiles : List (Tile a b)
        , players : Nonempty b
        }


tiles : GameData a b -> List (Tile a b)
tiles (GameData g) =
    g.tiles


init : List a -> Nonempty b -> GameData a b
init t players =
    GameData
        { tiles = t |> List.map Hidden
        , players = players
        }


type Tile a b
    = Taken b a
    | Hidden a
    | Open a


type GameMode
    = NextCard
    | TurnWin
    | TurnLoose
    | GameOver


gameMode : GameData a b -> GameMode
gameMode (GameData g) =
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
        GameOver

    else
        case visibleCards g.tiles of
            [ x, y ] ->
                if x == y then
                    TurnWin

                else
                    TurnLoose

            _ ->
                NextCard


processTurn : GameData a b -> GameData a b
processTurn g =
    let
        gMode =
            gameMode g

        newPlaces =
            List.map
                (\p ->
                    case ( p, gMode ) of
                        ( Open c, TurnWin ) ->
                            Taken (currentPlayer g) c

                        ( Open c, _ ) ->
                            Hidden c

                        _ ->
                            p
                )
                (tiles g)

        (GameData gd) =
            g
    in
    GameData
        { gd
            | tiles = newPlaces
            , players =
                case gMode of
                    TurnLoose ->
                        next gd.players

                    _ ->
                        gd.players
        }


currentPlayer : GameData a b -> b
currentPlayer (GameData a) =
    a.players |> Tuple.first


showCard : Int -> GameData a b -> GameData a b
showCard index (GameData g) =
    GameData
        { g
            | tiles =
                g.tiles
                    |> List.indexedMap
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
        }


next : Nonempty a -> Nonempty a
next ( head, tail ) =
    case tail of
        x :: xs ->
            ( x, xs ++ [ head ] )

        [] ->
            ( head, [] )
