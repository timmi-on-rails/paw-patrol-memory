-- Abstract memory game model


module Memory exposing (..)


type alias Nonempty a =
    ( a, List a )


type alias GameData a b =
    { tiles : List (Tile a b)
    , players : Nonempty b
    }


type Tile a b
    = Taken b a
    | Hidden a
    | Open a


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
                            Taken (currentPlayer g.players) c

                        ( Open c, _ ) ->
                            Hidden c

                        _ ->
                            p
                )
                g.tiles
    in
    { g
        | tiles = newPlaces
        , players =
            case gMode of
                TurnLoose ->
                    next g.players

                _ ->
                    g.players
    }


currentPlayer : Nonempty a -> a
currentPlayer =
    Tuple.first


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


next : Nonempty a -> Nonempty a
next ( head, tail ) =
    case tail of
        x :: xs ->
            ( x, xs ++ [ head ] )

        [] ->
            ( head, [] )
