module State exposing (initPlayerA, initPlayerB, step, changeDirection, evaluate)

import Color exposing (..)
import Char
import Types exposing (..)

initPlayerA : GameConfig -> Player
initPlayerA {width} = 
  { color = Color.yellow
  , pos = pos (toFloat width / -2.0) 0
  , direction = Right
  , path = []
  }

initPlayerB : GameConfig -> Player
initPlayerB {width} = 
  { color = Color.blue
  , pos = pos (toFloat width / 2.0) 0
  , direction = Left
  , path = []
  }

pos : Float -> Float -> Types.Position
pos = (,)

changeDirection : Player -> Controls -> Char.KeyCode -> Player
changeDirection player controls keyCode =
  let newDir = getNewDirection controls keyCode player.direction
  in { player | direction = newDir }

getNewDirection : Controls -> Char.KeyCode -> Types.Direction -> Types.Direction
getNewDirection controls keyCode currentDir =
  let (changeableDirs, newDir) =
    let key = Char.fromCode keyCode
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

step : GameConfig -> Player -> Player
step gameConfig player =
  let newPos = getNewSegment gameConfig player.pos player.direction
      newPath = player.pos::player.path
  in { player | pos = newPos, path = newPath }

getNewSegment : GameConfig -> (Float, Float) -> Types.Direction -> (Float, Float)
getNewSegment {segmentDim} (x, y) direction =
  case direction of
    Up    -> (x, y+segmentDim)
    Down  -> (x, y-segmentDim)
    Left  -> (x-segmentDim, y)
    Right -> (x+segmentDim, y)

hitTheWalls : GameConfig -> Player -> Bool
hitTheWalls {width, height} {pos} =
  fst pos > (toFloat width / 2)      -- hit right
  || snd pos > (toFloat height / 2)  -- hit top
  || fst pos < (toFloat width / -2)  -- hit left
  || snd pos < (toFloat height / -2) -- hit bottom

hitThePath : Player -> Player -> Bool
hitThePath player other =
  List.any ((==) player.pos) player.path ||
  List.any ((==) player.pos) other.path

headOnCollission : Player -> Player -> Bool
headOnCollission player other =
  player.pos == other.pos

evaluate : GameConfig -> Player -> Player -> Maybe Outcome
evaluate gameConfig playerA playerB =
  if hitTheWalls gameConfig playerA || hitThePath playerA playerB then
    Just PlayerBWins
  else if hitTheWalls gameConfig playerB || hitThePath playerB playerA then
    Just PlayerAWins
  else if headOnCollission playerA playerB then
    Just Draw
  else Nothing