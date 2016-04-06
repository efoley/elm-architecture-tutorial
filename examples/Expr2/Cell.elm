module Cell where

import Expr
import List exposing (filter, head, map)
import Html exposing (..)
import Html.Attributes exposing (style, placeholder, value)
import Html.Events exposing (on, targetValue)


-- MODEL

type alias Pos = Expr.Pos

type alias Model = 
  { value : String
  , expr : Expr.Expr
  , err : Maybe String
  , pos : Pos
  , eval : Expr.Eval
  }

-- UPDATE

type Action = SetExpr String

nan : Float
nan = 0/0

initModel : Int -> Int -> Model
initModel irow icol = { value="0", expr=Expr.Const 0, err=Nothing, pos = { row = irow, col = icol }, eval=Expr.Unknown}


update : Action -> Model -> Model
update (SetExpr s) model = 
  let r = Expr.parse s 
  in case r of
      Ok e -> { model | value=s, expr=e, err=Nothing, eval=Expr.Unknown }
      Err m -> { model | value=s, expr=Expr.Const nan, err=Just m, eval=Expr.Unknown }

updateFromContext : Expr.Context -> Model -> Model
updateFromContext context model =
  --let e = context |> filter (fst >> (==) model.pos) |> head |> Maybe.withDefault Expr.Unknown
  let e = Expr.deref context model.pos
  in { model | eval = e }

-- VIEW

debugMessage : Model -> String
debugMessage m =
  let peval e = case e of
                  Expr.Unknown -> "???"
                  Expr.Error -> "###"
                  Expr.Val f -> toString f
  in 
      case m.err of
        Nothing -> Expr.debugPrint m.expr ++ " = " ++ peval m.eval
        Just e -> e

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ input
      [ placeholder "A1" -- TODO EDF add "=" and strip in decideAction
      , value model.value
      , on "input" targetValue (message address)
      , myStyle "50px"
      ] 
      []
    , div [ myStyle "100px" ] [ text <| debugMessage model ]
    ]

message : Signal.Address Action -> String -> Signal.Message
message address s = Signal.message address (SetExpr s)

myStyle : String -> Attribute
myStyle w =
  style
    [ ("font-size", "20px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", w )
    , ("text-align", "left")
    ]
