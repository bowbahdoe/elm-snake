module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Json.Decode as Decode
import Set exposing (Set)
import Svg exposing (Svg, svg)
import Svg.Attributes
import Time


type alias Coordinate =
    ( Int, Int )


type Face
    = Up
    | Down
    | Left
    | Right


type alias Snake =
    { head : Coordinate
    , tail : List Coordinate
    , face : Face
    }


w : Int
w =
    10


h : Int
h =
    10


outOfBounds : Coordinate -> Bool
outOfBounds ( x, y ) =
    x < 0 || x >= w || y < 0 || y >= h


pixelWidth : Int
pixelWidth =
    1200


pixelHeight : Int
pixelHeight =
    1200


squareWidth : Int
squareWidth =
    pixelWidth // w


squareHeight : Int
squareHeight =
    pixelHeight // h


type alias Board =
    { food : Set Coordinate
    , snake : Snake
    }


startingBoard : Board
startingBoard =
    { food = Set.fromList [ ( 2, 3 ) ]
    , snake =
        { head = ( 4, 4 )
        , tail = [ ( 4, 5 ), ( 4, 6 ) ]
        , face = Up
        }
    }


type TickResult
    = GameOver Board
    | Continue Board
    | AteFood Board


butLast : List a -> List a
butLast list =
    List.take (List.length list - 1) list


move : Snake -> Snake
move snake =
    let
        { head, tail, face } =
            snake

        ( headX, headY ) =
            head

        newHead =
            case face of
                Up ->
                    ( headX, headY - 1 )

                -- This is weird, but its how the coordinates work when rendering
                Down ->
                    ( headX, headY + 1 )

                Left ->
                    ( headX - 1, headY )

                Right ->
                    ( headX + 1, headY )

        newTail =
            snake.head :: butLast snake.tail
    in
    { snake | head = newHead, tail = newTail }


isDirectTurnaround : Face -> Face -> Bool
isDirectTurnaround faceA faceB =
    case ( faceA, faceB ) of
        ( Up, Down ) ->
            True

        ( Down, Up ) ->
            True

        ( Left, Right ) ->
            True

        ( Right, Left ) ->
            True

        _ ->
            False


tick : Maybe Face -> Board -> TickResult
tick userIntent board =
    let
        currentSnake =
            board.snake

        snakeToMove =
            case userIntent of
                Nothing ->
                    currentSnake

                Just face ->
                    if isDirectTurnaround face currentSnake.face then
                        currentSnake

                    else
                        { currentSnake | face = face }

        nextSnakeUngrown =
            move snakeToMove

        { result, nextFood, nextSnake } =
            if outOfBounds nextSnakeUngrown.head then
                { result = GameOver, nextFood = board.food, nextSnake = nextSnakeUngrown }

            else
                { result = Continue, nextFood = board.food, nextSnake = nextSnakeUngrown }
    in
    result { board | food = nextFood, snake = nextSnake }


permutations : List a -> List b -> List ( a, b )
permutations listA listB =
    let
        pairWithAll a bs =
            List.map (Tuple.pair a) bs

        pairAllWithAll l1 l2 =
            List.foldl
                (\a acc -> acc ++ pairWithAll a l2)
                []
                l1
    in
    pairAllWithAll listA listB


allCells : Set Coordinate
allCells =
    Set.fromList <|
        permutations (List.range 0 w) (List.range 0 h)


type alias Color =
    String


cellToSquare : Color -> Coordinate -> Svg msg
cellToSquare color coordinate =
    let
        ( x, y ) =
            coordinate
    in
    Svg.rect
        [ Svg.Attributes.fill color
        , Svg.Attributes.x (String.fromInt (x * squareWidth))
        , Svg.Attributes.y (String.fromInt (y * squareHeight))
        , Svg.Attributes.width (String.fromInt squareWidth)
        , Svg.Attributes.height (String.fromInt squareHeight)
        ]
        []


renderBoard : Board -> Html msg
renderBoard { food, snake } =
    svg
        [ Svg.Attributes.viewBox <|
            "0 0 "
                ++ String.fromInt pixelWidth
                ++ " "
                ++ String.fromInt pixelWidth
        , Svg.Attributes.width "600"
        , Svg.Attributes.height "600"
        ]
        (List.map (cellToSquare "blue") (Set.toList food)
            ++ [ cellToSquare "black" snake.head ]
            ++ List.map (cellToSquare "black") snake.tail
        )


type alias SnakeGame =
    { board : Board
    , userIntent : Maybe Face
    , gameOver : Bool
    }


startingGame : SnakeGame
startingGame =
    { board = startingBoard
    , userIntent = Nothing
    , gameOver = False
    }


view : SnakeGame -> Html msg
view { board, userIntent } =
    renderBoard board


type Msg
    = NoOp
    | Tick
    | Go Face


update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick ->
            if model.gameOver then
                ( model, Cmd.none )

            else
                let
                    ( nextBoard, gameOver ) =
                        case tick model.userIntent model.board of
                            Continue board ->
                                ( board, False )

                            GameOver board ->
                                ( board, True )

                            AteFood board ->
                                ( board, True )
                in
                ( { model
                    | board = nextBoard
                    , gameOver = gameOver
                    , userIntent = Nothing
                  }
                , Cmd.none
                )

        Go face ->
            ( { model | userIntent = Just face }, Cmd.none )


subscriptions model =
    Sub.batch
        [ Time.every 500 (\_ -> Tick)
        , Browser.Events.onKeyDown
            (Decode.map
                (\k ->
                    if k == "w" then
                        Go Up

                    else if k == "a" then
                        Go Left

                    else if k == "d" then
                        Go Right

                    else if k == "s" then
                        Go Down

                    else
                        NoOp
                )
                (Decode.field "key" Decode.string)
            )
        ]


main : Program () SnakeGame Msg
main =
    Browser.element
        { init = \_ -> ( startingGame, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
