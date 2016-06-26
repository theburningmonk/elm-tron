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
  , pos = pos (-width/2) 0
  , direction = Right
  , path = []
  }

initPlayerB : Player
initPlayerB = 
  { color = Color.blue
  , pos = pos (width/2) 0
  , direction = Left
  , path = []
  }

type alias Controls = 
  { up: Char
  , down: Char
  , left: Char
  , right: Char
  }

playerAControls : Controls
playerAControls =
  { up = 'w'
  , down = 's'
  , left = 'a'
  , right = 'd'
  }

playerBControls : Controls
playerBControls =
  { up = 'i'
  , down = 'k'
  , left = 'j'
  , right = 'l'
  }

type Outcome
  = PlayerAWins
  | PlayerBWins
  | Draw

-- step 1: define your Model
type Model 
  = NotStarted 
  | Started Player Player
  | Ended Outcome Player Player

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

    Started _ _ ->
      Sub.batch 
        [ Keyboard.presses KeyPress
        , Time.every (Time.inMilliseconds (1000/fps)) Tick
        ]

    Ended _ _ _ -> 
      Keyboard.presses KeyPress

-- step 5: how to render your model
draw : Player -> List Form
draw {color, pos, path} =
  let tail = List.map (\pos -> rect segmentDim segmentDim |> filled color |> move pos) path
      head = rect (segmentDim*2) (segmentDim*2) |> filled white |> move pos
  in head::tail

view : Model -> Html Msg
view model =
  let bg = rect (toFloat width) (toFloat height) |> filled black
      content =
        case model of
          NotStarted -> 
            let msg = 
              "press SPACE to start\n" ++ 
              "\nP1 CONTROLS: A, W, S, D" ++
              "\nP2 CONTROLS: J, I, K, L"
            in [txt msg]

          Started playerA playerB -> 
            (draw playerA) ++ (draw playerB)

          Ended outcome playerA playerB ->
            let outcomeMsg =
                  case outcome of
                    Draw ->
                      "it's a DRAW"
                    PlayerAWins ->
                      "Player A WIN"
                    PlayerBWins ->
                      "Player B WIN"
                msg = 
                  outcomeMsg ++
                  "\npress SPACE to restart"
            in (draw playerA) ++ (draw playerB) ++ [txt msg]
  in collage width height (bg::content)
     |> Element.toHtml

-- step 6: implement state transition
changeDirection : Player -> Controls -> Char.KeyCode -> Player
changeDirection player controls keyCode =
  let newDir = getNewDirection controls keyCode player.direction
  in { player | direction = newDir }

step : Player -> Player
step player =
  let newPos = getNewSegment player.pos player.direction
      newPath = player.pos::player.path
  in { player | pos = newPos, path = newPath }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case model of
    NotStarted ->
      case msg of
        KeyPress 32 -> (Started initPlayerA initPlayerB, Cmd.none)
        _ -> (model, Cmd.none)

    Started playerA playerB -> 
      case msg of
        KeyPress keyCode -> 
          let newPlayerA = changeDirection playerA playerAControls keyCode
              newPlayerB = changeDirection playerB playerBControls keyCode
          in (Started newPlayerA newPlayerB, Cmd.none)
        
        Tick _ ->
          let newPlayerA = step playerA
              newPlayerB = step playerB
              outcome = evaluate newPlayerA newPlayerB
          in case outcome of
              Just x ->
                (Ended x newPlayerA newPlayerB, Cmd.none)
              Nothing ->                
                (Started newPlayerA newPlayerB, Cmd.none)

    Ended _ playerA playerB ->
      case msg of
        KeyPress 32 -> (Started initPlayerA initPlayerB, Cmd.none)
        _ -> (model, Cmd.none)

txt : String -> Form
txt msg =
  msg
  |> Text.fromString
  |> Text.color white
  |> Text.monospace
  |> Element.centered
  |> Collage.toForm

getNewDirection : Controls -> Char.KeyCode -> Direction -> Direction
getNewDirection controls keyCode currentDir =
  let (changeableDirs, newDir) =
    let key = fromCode keyCode
    in if key == controls.left then
          ([ Up, Down ], Left)
       else if key == controls.up then 
          ([ Left, Right ], Up)
       else if key == controls.right then 
          ([ Up, Down ], Right)
       else if key == controls.down then
          ([ Left, Right ], Down)
       else ([], currentDir)
  in if List.any ((==) currentDir) changeableDirs then newDir else currentDir

getNewSegment : (Float, Float) -> Direction -> (Float, Float)
getNewSegment (x, y) direction =
  case direction of
    Up    -> (x, y+segmentDim)
    Down  -> (x, y-segmentDim)
    Left  -> (x-segmentDim, y)
    Right -> (x+segmentDim, y)

hitTheWalls : Player -> Bool
hitTheWalls {pos} =
  fst pos > (width / 2)      -- hit right
  || snd pos > (height / 2)  -- hit top
  || fst pos < (-width / 2)  -- hit left
  || snd pos < (-height / 2) -- hit bottom

hitThePath : Player -> Player -> Bool
hitThePath player other =
  List.any ((==) player.pos) player.path ||
  List.any ((==) player.pos) other.path

headOnCollission : Player -> Player -> Bool
headOnCollission player other =
  player.pos == other.pos

evaluate : Player -> Player -> Maybe Outcome
evaluate playerA playerB =
  if hitTheWalls playerA || hitThePath playerA playerB then
    Just PlayerBWins
  else if hitTheWalls playerB || hitThePath playerB playerA then
    Just PlayerAWins
  else if headOnCollission playerA playerB then
    Just Draw
  else Nothing