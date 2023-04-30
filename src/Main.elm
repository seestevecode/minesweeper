module Main exposing (main)

import Browser
import Element as Ui
import Element.Border as Border
import Html exposing (Html)
import List.Extra as ListX
import Random
import Random.List


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { grid : Grid }


type alias Grid =
    List Cell


type alias Cell =
    { coord : Coord, floor : CellFloor, ceiling : CellCeiling }


type alias Coord =
    ( Int, Int )


type CellFloor
    = Mine
    | MineCount Int


type CellCeiling
    = Flagged
    | Covered
    | Uncovered


type Msg
    = NewGrid (List Coord)


init : () -> ( Model, Cmd Msg )
init _ =
    let
        floors =
            List.repeat (gridHeight * gridWidth) (MineCount 0)

        ceilings =
            List.repeat (gridHeight * gridWidth) Uncovered
    in
    ( { grid = List.map3 Cell allCoords floors ceilings }
    , Random.generate NewGrid mineGenerator
    )


gridHeight : Int
gridHeight =
    9


gridWidth : Int
gridWidth =
    9


totalMines : Int
totalMines =
    10


allCoords : List Coord
allCoords =
    ListX.lift2 Tuple.pair (List.range 1 gridHeight) (List.range 1 gridWidth)


mineGenerator : Random.Generator (List Coord)
mineGenerator =
    allCoords |> Random.List.shuffle |> Random.map (List.take totalMines)


view : Model -> Html Msg
view model =
    Ui.layout [] <| Ui.column [] <| [ viewGrid model.grid ]


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

                    ( Mine, Uncovered ) ->
                        "M"

                    ( MineCount mineCount, Uncovered ) ->
                        String.fromInt mineCount


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGrid mines ->
            ( { model
                | grid =
                    model.grid
                        |> placeMines mines
                        |> placeMineCounts
                        |> List.map (\cell -> { cell | ceiling = Covered })
              }
            , Cmd.none
            )


placeMines : List Coord -> Grid -> Grid
placeMines mines grid =
    List.map
        (\cell ->
            if List.member cell.coord mines then
                { cell | floor = Mine }

            else
                cell
        )
        grid


placeMineCounts : Grid -> Grid
placeMineCounts grid =
    List.map
        (\cell ->
            if cell.floor /= Mine then
                { cell | floor = MineCount <| countMines cell.coord grid }

            else
                cell
        )
        grid


countMines : Coord -> Grid -> Int
countMines coord grid =
    List.filter (\cell -> List.member cell.coord (getNeighbours coord)) grid
        |> List.filter (\cell -> cell.floor == Mine)
        |> List.length


getNeighbours : Coord -> List Coord
getNeighbours ( coordCol, coordRow ) =
    let
        columns =
            List.range (max (coordCol - 1) 1) (min (coordCol + 1) gridHeight)

        rows =
            List.range (max (coordRow - 1) 1) (min (coordRow + 1) gridWidth)
    in
    ListX.lift2 Tuple.pair columns rows
        |> List.filter (\coord -> coord /= ( coordCol, coordRow ))


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none
