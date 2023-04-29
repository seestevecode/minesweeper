module Main exposing (main)

import Browser
import Element as Ui
import Element.Border as Border
import Html exposing (Html)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, view = view, update = update }


type alias Model =
    Grid


type alias Grid =
    List Cell


type alias Cell =
    { coord : CellCoord, floor : CellFloor, ceiling : CellCeiling }


type alias CellCoord =
    ( Int, Int )


type CellFloor
    = Bomb
    | BombCount Int


type CellCeiling
    = Flagged
    | Covered
    | Uncovered


type Msg
    = NoOp


gridHeight : Int
gridHeight =
    2


init : Model
init =
    [ Cell ( 1, 1 ) Bomb Covered
    , Cell ( 1, 2 ) Bomb Uncovered
    , Cell ( 1, 3 ) Bomb Flagged
    , Cell ( 2, 1 ) (BombCount 2) Covered
    , Cell ( 2, 2 ) (BombCount 3) Flagged
    , Cell ( 2, 3 ) (BombCount 0) Uncovered
    ]


view : Model -> Html Msg
view model =
    Ui.layout [] <| viewGrid model


viewGrid : Grid -> Ui.Element Msg
viewGrid grid =
    let
        viewRow row =
            grid
                |> List.filter (\cell -> Tuple.first cell.coord == row)
                |> List.map viewCell
                |> Ui.row [ Ui.spacing 5 ]
    in
    List.range 1 gridHeight
        |> List.map viewRow
        |> Ui.column [ Ui.spacing 5 ]


viewCell : Cell -> Ui.Element Msg
viewCell { floor, ceiling } =
    Ui.el
        [ Border.solid
        , Border.width 1
        , Ui.width <| Ui.px 50
        , Ui.height <| Ui.px 50
        ]
    <|
        Ui.el [ Ui.centerX, Ui.centerY ] <|
            Ui.text <|
                case ( floor, ceiling ) of
                    ( _, Covered ) ->
                        "C"

                    ( _, Flagged ) ->
                        "F"

                    ( Bomb, Uncovered ) ->
                        "B"

                    ( BombCount bombCount, Uncovered ) ->
                        String.fromInt bombCount


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model
