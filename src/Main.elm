module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Element as Ui
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
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
    | UncoverCell Coord


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
                |> Ui.row [ Ui.spacing 2 ]
    in
    List.range 1 gridHeight
        |> List.map viewRow
        |> Ui.column [ Ui.spacing 2 ]


viewCell : Cell -> Ui.Element Msg
viewCell cell =
    Input.button
        (cellAttributes cell)
        { onPress = Just <| UncoverCell cell.coord
        , label = cellLabel cell
        }


cellAttributes : Cell -> List (Ui.Attribute Msg)
cellAttributes cell =
    let
        commonAttributes =
            [ Ui.width <| Ui.px 50
            , Ui.height <| Ui.px 50
            , Font.bold
            , Font.size 30
            ]

        attributes =
            case ( cell.floor, cell.ceiling ) of
                ( _, Covered ) ->
                    [ Background.color <| Ui.rgb255 128 128 128
                    ]

                ( _, Flagged ) ->
                    []

                ( Mine, Uncovered ) ->
                    [ Background.color <| Ui.rgb255 200 200 200 ]

                ( MineCount mineCount, Uncovered ) ->
                    [ Background.color <| Ui.rgb255 200 200 200
                    , Font.color <|
                        Maybe.withDefault (Ui.rgb255 0 0 0) <|
                            Dict.get mineCount mineCountColours
                    ]
    in
    commonAttributes ++ attributes


mineCountColours : Dict Int Ui.Color
mineCountColours =
    Dict.fromList
        [ ( 0, Ui.rgb255 0 0 0 )
        , ( 1, Ui.rgb255 0 0 255 )
        , ( 2, Ui.rgb255 0 123 0 )
        , ( 3, Ui.rgb255 255 0 0 )
        , ( 4, Ui.rgb255 0 0 123 )
        , ( 5, Ui.rgb255 123 0 0 )
        , ( 6, Ui.rgb255 0 123 123 )
        , ( 7, Ui.rgb255 0 0 0 )
        , ( 8, Ui.rgb255 0 0 255 )
        ]


cellLabel : Cell -> Ui.Element Msg
cellLabel cell =
    Ui.el [ Ui.centerX, Ui.centerY ] <|
        case ( cell.floor, cell.ceiling ) of
            ( _, Covered ) ->
                Ui.none

            ( _, Flagged ) ->
                Ui.text "F"

            ( Mine, Uncovered ) ->
                Ui.text "M"

            ( MineCount 0, Uncovered ) ->
                Ui.none

            ( MineCount mineCount, Uncovered ) ->
                Ui.text <| String.fromInt mineCount


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

        UncoverCell coord ->
            ( { model
                | grid =
                    model.grid
                        |> List.map
                            (\cell ->
                                if cell.coord == coord then
                                    { cell | ceiling = Uncovered }

                                else
                                    cell
                            )
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
