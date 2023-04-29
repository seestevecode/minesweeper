module Main exposing (main)

import Browser
import Element as Ui
import Html exposing (Html)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, view = view, update = update }


type alias Model =
    Cell


type alias Cell =
    { floor : CellFloor, ceiling : CellCeiling }


type CellFloor
    = Bomb
    | BombCount Int


type CellCeiling
    = Flagged
    | Covered
    | Uncovered


type Msg
    = NoOp


init : Model
init =
    Cell (BombCount 2) Uncovered


view : Model -> Html Msg
view model =
    Ui.layout [] <| viewCell model


viewCell : Cell -> Ui.Element Msg
viewCell { floor, ceiling } =
    Ui.el [] <|
        Ui.text <|
            case ( floor, ceiling ) of
                ( _, Covered ) ->
                    "C"

                ( _, Flagged ) ->
                    "F"

                ( Bomb, _ ) ->
                    "B"

                ( BombCount bc, _ ) ->
                    String.fromInt bc


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model
