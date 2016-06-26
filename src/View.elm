module View exposing (txt, view)

import Collage exposing (..)
import Color exposing (..)
import Element
import Html exposing (..)
import Text
import Types exposing (..)

txt : String -> Form
txt msg =
  msg
  |> Text.fromString
  |> Text.color white
  |> Text.monospace
  |> Element.centered
  |> Collage.toForm

drawPlayer : GameConfig -> Player -> List Form
drawPlayer {segmentDim} {color, pos, path} =
  let tail = List.map (\pos -> rect segmentDim segmentDim |> filled color |> move pos) path
      head = rect (segmentDim*2) (segmentDim*2) |> filled white |> move pos
  in head::tail

draw : GameConfig -> List Player -> List Form
draw gameConfig players =
  List.concatMap (drawPlayer gameConfig) players

view : GameConfig -> Model -> Html a
view gameConfig model =
  let (width, height) = (gameConfig.width, gameConfig.height)
      bg = rect (toFloat width) (toFloat height) |> filled black
      content =
        case model of
          NotStarted -> 
            let msg = 
              "press SPACE to start\n" ++ 
              "\nP1 CONTROLS: A, W, S, D" ++
              "\nP2 CONTROLS: J, I, K, L"
            in [txt msg]

          Started playerA playerB -> 
            draw gameConfig [playerA, playerB]

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
            in draw gameConfig [playerA, playerB] ++ [txt msg]
  in collage width height (bg::content)
     |> Element.toHtml