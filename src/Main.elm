module Main exposing (Hole(..), Model, Msg(..), init, main, subscriptions, update, view)

import Array exposing (Array)
import Array.Extra
import Browser
import Browser.Events exposing (onAnimationFrame)
import Config exposing (Config)
import Html exposing (Html)
import Random
import Svg exposing (Svg)
import Svg.Attributes



-- MODEL & TYPES


type Hole
    = Hole


type Direction
    = North
    | East
    | South
    | West


type Turn
    = Left
    | Right


type alias Rule =
    Array Turn


type alias Coords =
    { x : Int
    , y : Int
    }


type alias Ant =
    { position : Coords
    , rule : Array Turn
    , pointsTo : Direction
    }


type alias Grid =
    Array Int


type alias Anthill =
    { grid : Grid
    , width : Int
    , height : Int
    , maxStates : Int
    }


type Status
    = Initializing
    | Running


type alias Model =
    { ants : List Ant
    , anthill : Anthill
    , state : Status
    }



-- MESSAGES


type Msg
    = Tick
    | AntsInitialized (List Ant)



-- INIT


randomRuleGenerator : Int -> Random.Generator Rule
randomRuleGenerator ruleLength =
    Random.list ruleLength
        (Random.uniform Left [ Right ])
        |> Random.map Array.fromList


randomPositionGenerator : Int -> Int -> Random.Generator Coords
randomPositionGenerator width height =
    Random.pair (Random.int 0 width) (Random.int 0 height)
        |> Random.map
            (\( x, y ) -> Coords x y)


randomOrientationGenerator : Random.Generator Direction
randomOrientationGenerator =
    Random.uniform North [ East, South, West ]


randomAntGenerator : Int -> Int -> Int -> Int -> Random.Generator (List Ant)
randomAntGenerator width height ruleLength length =
    Random.map3
        Ant
        (randomPositionGenerator width height)
        (randomRuleGenerator ruleLength)
        randomOrientationGenerator
        |> Random.list length


initAnt : Int -> Ant
initAnt maxStates =
    { position = Coords 0 0
    , rule = Array.repeat maxStates Left
    , pointsTo = North
    }


initGrid : Int -> Int -> Array Int
initGrid width height =
    Array.repeat (width * height) 0


init : Config -> ( Model, Cmd Msg )
init { maxStates, numberOfAnts, gridWidth, gridHeight } =
    let
        ants =
            List.repeat maxStates <| initAnt maxStates

        anthill =
            { grid = initGrid gridWidth gridHeight
            , width = gridWidth
            , height = gridHeight
            , maxStates = maxStates
            }
    in
    { ants = ants
    , anthill = anthill
    , state = Initializing
    }
        |> withCommand
            (Random.generate AntsInitialized <|
                randomAntGenerator gridWidth gridHeight maxStates numberOfAnts
            )



-- VIEW


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


documentWithTitle : String -> Html msg -> Document msg
documentWithTitle title body =
    Document title [ body ]


colors : Array String
colors =
    let
        { maxStates } =
            Config.values
    in
    List.range 0 maxStates
        |> List.reverse
        |> List.map
            (\v ->
                let
                    s =
                        toFloat v
                            / toFloat maxStates
                            * 255
                            |> String.fromFloat
                in
                "rgb("
                    ++ s
                    ++ ","
                    ++ s
                    ++ ","
                    ++ s
                    ++ ")"
            )
        |> Array.fromList


viewCell : Int -> Int -> Int -> (Int -> Int -> Svg msg)
viewCell cellwidth width height index state =
    let
        strCellwidth =
            String.fromInt cellwidth

        x =
            modBy width index
                * cellwidth
                |> String.fromInt

        y =
            index
                // width
                * cellwidth
                |> String.fromInt

        fill =
            Array.get state colors
                |> Maybe.withDefault "#FF0000"
    in
    Svg.rect
        [ Svg.Attributes.width strCellwidth
        , Svg.Attributes.height strCellwidth
        , Svg.Attributes.x x
        , Svg.Attributes.y y
        , Svg.Attributes.fill fill
        ]
        []


viewAnt : Int -> Int -> Int -> (Ant -> Svg msg)
viewAnt cellwidth width height { position } =
    let
        strCellwidth =
            String.fromInt cellwidth

        x =
            position.x
                |> (*) cellwidth
                |> String.fromInt

        y =
            position.y
                |> (*) cellwidth
                |> String.fromInt
    in
    Svg.rect
        [ Svg.Attributes.width strCellwidth
        , Svg.Attributes.height strCellwidth
        , Svg.Attributes.x x
        , Svg.Attributes.y y
        , Svg.Attributes.fill "#0000FF"
        ]
        []


viewAnthill : Int -> Model -> Html msg
viewAnthill cellwidth { anthill, ants } =
    let
        width =
            anthill.width
                * cellwidth
                |> String.fromInt

        height =
            anthill.height
                * cellwidth
                |> String.fromInt

        viewbox =
            "0 0 " ++ width ++ " " ++ height

        root =
            Svg.svg
                [ Svg.Attributes.width width
                , Svg.Attributes.height height
                , Svg.Attributes.viewBox viewbox
                ]

        renderedCells =
            Array.indexedMap
                (viewCell cellwidth anthill.width anthill.height)
                anthill.grid
                |> Array.toList

        renderedAnts =
            List.map
                (viewAnt cellwidth anthill.width anthill.height)
                ants
    in
    root
        (renderedCells ++ renderedAnts)


view : Config -> Model -> Document Msg
view { cellwidth } model =
    viewAnthill cellwidth model
        |> documentWithTitle "Anthill"



-- UPDATE


withCommand : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCommand msg model =
    ( model, msg )


updateDirection : Anthill -> Ant -> Ant
updateDirection anthill ant =
    let
        { pointsTo, rule } =
            ant

        index =
            anthill.width
                * ant.position.y
                + ant.position.x

        state =
            Array.get index anthill.grid
                |> Maybe.withDefault 0

        turnTo =
            Array.get state rule
                |> Maybe.withDefault Left

        nextDirection =
            case turnTo of
                Left ->
                    case pointsTo of
                        North ->
                            West

                        East ->
                            North

                        South ->
                            East

                        West ->
                            South

                Right ->
                    case pointsTo of
                        North ->
                            East

                        East ->
                            South

                        South ->
                            West

                        West ->
                            North
    in
    { ant | pointsTo = nextDirection }


updatePosition : Anthill -> Ant -> Ant
updatePosition { width, height } ant =
    let
        { x, y } =
            ant.position

        nextPosition =
            case ant.pointsTo of
                North ->
                    Coords
                        x
                        (if y - 1 < 0 then
                            height - 1

                         else
                            y - 1
                        )

                East ->
                    Coords
                        (if (x - 1) < 0 then
                            width - 1

                         else
                            x - 1
                        )
                        y

                South ->
                    Coords
                        x
                        (modBy height (y + 1))

                West ->
                    Coords
                        (modBy width (x + 1))
                        y
    in
    { ant | position = nextPosition }


updateGrid : Ant -> Anthill -> Anthill
updateGrid { position } anthill =
    let
        { grid, width, maxStates } =
            anthill

        { x, y } =
            position

        index =
            (y * width) + x

        nextGrid =
            Array.Extra.update
                index
                (modBy maxStates << (+) 1)
                grid
    in
    { anthill | grid = nextGrid }


update : Config -> Msg -> Model -> ( Model, Cmd Msg )
update config msg model =
    case msg of
        Tick ->
            let
                nextAnts =
                    List.map
                        (updatePosition model.anthill << updateDirection model.anthill)
                        model.ants

                nextAnthill =
                    List.foldl
                        updateGrid
                        model.anthill
                        model.ants
            in
            { model
                | ants = nextAnts
                , anthill = nextAnthill
            }
                |> withCommand Cmd.none

        AntsInitialized randomizedAnts ->
            { model
                | ants = randomizedAnts
                , state = Running
            }
                |> withCommand Cmd.none



-- SUBSCRIPTIONS


subscriptions : Config -> Model -> Sub Msg
subscriptions _ { state } =
    case state of
        Running ->
            onAnimationFrame <| always Tick

        _ ->
            Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = always <| init Config.values
        , view = view Config.values
        , update = update Config.values
        , subscriptions = subscriptions Config.values
        }
