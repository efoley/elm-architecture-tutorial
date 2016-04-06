module Expr where

import Char
import Html exposing (..)
import Html.Attributes exposing (style, placeholder, value)
import Html.Events exposing (on, targetValue)
import Parser exposing (..)
import Parser.Char exposing (..)
import Parser.Number exposing (float, integer, natural)


-- MODEL

type alias Model = String

-- Expressions

type alias Pos = { row : Int, col : Int }

type alias Fun1 = String
type alias Fun2 = String

type Expr = Val Float
          | Ref Pos
          | Range Pos Pos

debugParse : String -> String
debugParse s =
  let r = parse expr s
  in case r of
      Ok expr -> debugExpr expr
      Err err -> "Error: " ++ err

debugExpr : Expr -> String
debugExpr expr =
  case expr of
    Val f -> "(Val " ++ toString f ++ ")"
    Ref pos -> "(Ref " ++ strPos pos ++ ")"
    Range pos1 pos2 -> "(Range " ++ strPos pos1 ++ " " ++ strPos pos2 ++ ")"

strPos : Pos -> String
strPos pos = (toString <| Char.fromCode <| Char.toCode 'A' + pos.col) ++ (toString pos.row) 

val : Parser Expr
val = (map toFloat integer) `or` float |> map Val

crToPos : Char -> Int -> Pos
crToPos c r = { row=r, col=(Char.toCode c - Char.toCode 'A') }

pos : Parser Pos
pos = map crToPos (map Char.toUpper <| upper `or` lower) |> andMap natural

ref : Parser Expr
ref = map Ref pos

range : Parser Expr
range = map Range pos <* symbol ':' |> andMap pos

expr : Parser Expr
expr = choice [val, range, ref]

-- UPDATE

type Action = SetExpr String

update : Action -> Model -> Model
update (SetExpr x) model = x

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ input
      [ placeholder "A1" -- TODO EDF add "=" and strip in decideAction
      , value model
      , on "input" targetValue (message address)
      , myStyle
      ] 
      []
    , div [ myStyle ] [ text <| debugParse model ]
    ]

message : Signal.Address Action -> String -> Signal.Message
message address s = Signal.message address (SetExpr s)

myStyle : Attribute
myStyle =
  style
    [ ("font-size", "20px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", "200px")
    , ("text-align", "left")
    ]
