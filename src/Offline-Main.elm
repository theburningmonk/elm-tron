import Html.App as Html
import Time exposing (..)
import Keyboard
import Types exposing (..)
import State exposing (..)
import View exposing (..)

gameConfig : GameConfig
gameConfig = 
  { width = 600
  , height = 600
  , fps = 50
  , segmentDim = 3
  }

main =
  Html.program
    { init = init
    , view = view gameConfig
    , update = update
    , subscriptions = subscriptions
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
        , Time.every (Time.inMilliseconds (1000/gameConfig.fps)) Tick
        ]

    Ended _ _ _ -> 
      Keyboard.presses KeyPress

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case model of
    NotStarted ->
      case msg of
        KeyPress 32 -> 
          let playerA = initPlayerA gameConfig
              playerB = initPlayerB gameConfig
          in (Started playerA playerB, Cmd.none)
        _ -> (model, Cmd.none)

    Started playerA playerB -> 
      case msg of
        KeyPress keyCode -> 
          let newPlayerA = changeDirection playerA playerAControls keyCode
              newPlayerB = changeDirection playerB playerBControls keyCode
          in (Started newPlayerA newPlayerB, Cmd.none)
        
        Tick _ ->
          let newPlayerA = step gameConfig playerA
              newPlayerB = step gameConfig playerB
              outcome = evaluate gameConfig newPlayerA newPlayerB
          in case outcome of
              Just x ->
                (Ended x newPlayerA newPlayerB, Cmd.none)
              Nothing ->                
                (Started newPlayerA newPlayerB, Cmd.none)

    Ended _ playerA playerB ->
      case msg of
        KeyPress 32 -> 
          let playerA = initPlayerA gameConfig
              playerB = initPlayerB gameConfig
          in (Started playerA playerB, Cmd.none)
        _ -> (model, Cmd.none)