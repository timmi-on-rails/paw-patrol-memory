-- Abstract memory game model


module Memory exposing (GameMode(..), Model, Tile(..), currentPlayer, gameMode, init, processTurn, showCard, tiles)


type alias NonEmpty a =
    ( a, List a )


type Model a b
    = Model
        { tiles : List (Tile a b)
        , players : NonEmpty b
        }


tiles : Model a b -> List (Tile a b)
tiles (Model g) =
    g.tiles


init : List a -> NonEmpty b -> Model a b
init t players =
    Model
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


gameMode : Model a b -> GameMode
gameMode (Model g) =
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


processTurn : Model a b -> Model a b
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

        (Model gd) =
            g
    in
    Model
        { gd
            | tiles = newPlaces
            , players =
                case gMode of
                    TurnLoose ->
                        next gd.players

                    _ ->
                        gd.players
        }


currentPlayer : Model a b -> b
currentPlayer (Model a) =
    a.players |> Tuple.first


showCard : Int -> Model a b -> Model a b
showCard index (Model g) =
    Model
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


next : NonEmpty a -> NonEmpty a
next ( head, tail ) =
    case tail of
        x :: xs ->
            ( x, xs ++ [ head ] )

        [] ->
            ( head, [] )
