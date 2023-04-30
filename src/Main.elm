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
    { grid : Grid, bombCoords : List ( Int, Int ) }


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
    = NewBombs (List ( Int, Int ))


init : () -> ( Model, Cmd Msg )
init _ =
    let
        floors =
            List.repeat (gridHeight * gridWidth) (BombCount 0)

        ceilings =
            List.repeat (gridHeight * gridWidth) Uncovered
    in
    ( { grid = List.map3 Cell allCoords floors ceilings
      , bombCoords = []
      }
    , Random.generate NewBombs bombGenerator
    )


gridHeight : Int
gridHeight =
    9


gridWidth : Int
gridWidth =
    9


totalBombs : Int
totalBombs =
    10


allCoords : List ( Int, Int )
allCoords =
    ListX.lift2 Tuple.pair (List.range 1 gridHeight) (List.range 1 gridWidth)


bombGenerator : Random.Generator (List ( Int, Int ))
bombGenerator =
    allCoords |> Random.List.shuffle |> Random.map (List.take totalBombs)


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

                    ( Bomb, Uncovered ) ->
                        "B"

                    ( BombCount bombCount, Uncovered ) ->
                        String.fromInt bombCount


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewBombs bombs ->
            ( { model
                | grid = model.grid |> placeBombs bombs |> placeBombCounts
                , bombCoords = bombs
              }
            , Cmd.none
            )


placeBombs : List ( Int, Int ) -> Grid -> Grid
placeBombs bombs grid =
    List.map
        (\cell ->
            if List.member cell.coord bombs then
                { cell | floor = Bomb }

            else
                cell
        )
        grid


placeBombCounts : Grid -> Grid
placeBombCounts grid =
    List.map
        (\cell ->
            if cell.floor /= Bomb then
                { cell | floor = BombCount <| countBombs cell.coord grid }

            else
                cell
        )
        grid


countBombs : ( Int, Int ) -> Grid -> Int
countBombs coord grid =
    List.filter (\cell -> List.member cell.coord (getNeighbours coord)) grid
        |> List.filter (\cell -> cell.floor == Bomb)
        |> List.length


getNeighbours : ( Int, Int ) -> List ( Int, Int )
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
