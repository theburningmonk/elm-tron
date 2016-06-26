module Types exposing (..) 

import Color exposing (..)

type alias GameConfig =
  { width: Int
  , height: Int
  , fps: Float
  , segmentDim: Float }

type Direction 
  = Up
  | Down
  | Left
  | Right

type alias Position = (Float, Float)

type alias Player =
  { color: Color
  , pos: Position
  , direction: Direction
  , path: List Position }


type alias Controls = 
  { up: Char
  , down: Char
  , left: Char
  , right: Char
  }

type Outcome
  = PlayerAWins
  | PlayerBWins
  | Draw

type Model 
  = NotStarted 
  | Started Player Player
  | Ended Outcome Player Player