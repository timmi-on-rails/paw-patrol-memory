module Main exposing (main)

import Browser
import Html exposing (div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)


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


type Place
    = Empty
    | Hidden Card
    | Open Card


type alias Model =
    { places : List Place }


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
    ( { places =
            [ Hidden Rubble
            , Hidden Zuma
            , Hidden Chase
            , Hidden Marshall
            , Hidden Rocky
            , Hidden Skye
            , Hidden Everest
            , Hidden Tracker
            , Hidden Rex
            , Hidden Liberty
            , Hidden Rubble
            , Hidden Zuma
            , Hidden Chase
            , Hidden Marshall
            , Hidden Rocky
            , Hidden Skye
            , Hidden Everest
            , Hidden Tracker
            , Hidden Rex
            , Hidden Liberty
            ]
      }
    , Cmd.none
    )


type Msg
    = Toggle Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle index ->
            ( { model
                | places =
                    model.places
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
            , Cmd.none
            )


view : Model -> Html.Html Msg
view model =
    div
        [ style "display" "grid"
        , style "grid-template-columns" "repeat(5, min-content)"
        , style "grid-gap" "10px"
        ]
        (model.places |> List.indexedMap viewPlace)


viewPlace : Int -> Place -> Html.Html Msg
viewPlace index place =
    div
        [ onClick (Toggle index)
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
