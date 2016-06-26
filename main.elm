import Element exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Time exposing (..)
import Text
import Keyboard
import Char exposing (..)

segmentDim = 3.0
(width, height) = (600, 600)
fps = 50

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Direction 
  = Up
  | Down
  | Left
  | Right

type alias Position = (Float, Float)

pos : Float -> Float -> Position
pos = (,)

type alias Player =
  { color: Color
  , pos: Position
  , direction: Direction
  , path: List Position }

initPlayerA : Player
initPlayerA = 
  { color = Color.yellow
  , pos = pos 0 0
  , direction = Right
  , path = []
  }

-- step 1: define your Model
type Model 
  = NotStarted 
  | Started Player

-- step 2: define Msg that can trigger updates to Model
type Msg
  = Tick Time
  | KeyPress Keyboard.KeyCode

-- step 3: define the initial state for the app
init : (Model, Cmd Msg)
init = (NotStarted, Cmd.none)

-- step 4: define your subscriptions - WebSocket, Keyboard, etc.
subscriptions : Model -> Sub Msg
subscriptions model = 
  case model of
    NotStarted ->
      Keyboard.presses KeyPress

    Started _ ->
      Sub.batch 
        [ Keyboard.presses KeyPress
        , Time.every (Time.inMilliseconds (1000/fps)) Tick
        ]

-- step 5: how to render your model
view : Model -> Html Msg
view model =
  let bg = rect (toFloat width) (toFloat height) |> filled black
      content =
        case model of
          NotStarted -> 
            [txt "press SPACE to start\n use [a,w,s,d] to control bike"]

          Started playerA -> 
            let path = List.map (\pos -> rect segmentDim segmentDim |> filled playerA.color |> move pos) playerA.path
                head = rect (segmentDim*2) (segmentDim*2) |> filled white |> move playerA.pos
            in head::path
  in collage width height (bg::content)
     |> Element.toHtml

-- step 6: implement state transition
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case model of
    NotStarted ->
      case msg of
        KeyPress 32 -> (Started initPlayerA, Cmd.none)
        _ -> (model, Cmd.none)

    Started playerA -> 
      case msg of
        KeyPress keyCode -> 
          let newDir = getNewDirection keyCode playerA.direction
              newPlayerA = { playerA | direction = newDir }
          in (Started newPlayerA, Cmd.none)
        
        Tick _ ->
          let newPos  = getNewSegment playerA.pos playerA.direction
              newPath = playerA.pos::playerA.path
              newPlayerA = { playerA | pos = newPos, path = newPath }
              gameOver = isGameOver newPos newPath
          in if gameOver then
               (NotStarted, Cmd.none)
             else
               (Started newPlayerA, Cmd.none)

txt : String -> Form
txt msg =
  msg
  |> Text.fromString
  |> Text.color white
  |> Text.monospace
  |> Element.centered
  |> Collage.toForm

getNewDirection : Char.KeyCode -> Direction -> Direction
getNewDirection keyCode currentDir =
  let (changeableDirs, newDir) =
    case fromCode keyCode of
      'a' -> ([ Up, Down ], Left)
      'w' -> ([ Left, Right ], Up)
      'd' -> ([ Up, Down ], Right)
      's' -> ([ Left, Right ], Down)
      _  -> ([], currentDir)
  in if List.any ((==) currentDir) changeableDirs then newDir else currentDir

getNewSegment : (Float, Float) -> Direction -> (Float, Float)
getNewSegment (x, y) direction =
  case direction of
    Up    -> (x, y+segmentDim)
    Down  -> (x, y-segmentDim)
    Left  -> (x-segmentDim, y)
    Right -> (x+segmentDim, y)

isGameOver : Position -> List Position -> Bool
isGameOver newHead path =
  List.any ((==) newHead) path   -- hit the wall
  || fst newHead > (width / 2)   -- hit bottom
  || snd newHead > (height / 2)  -- hit top
  || fst newHead < (-width / 2)  -- hit left
  || snd newHead < (-height / 2) -- hit right