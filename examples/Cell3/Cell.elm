module Cell where

import Html exposing (..)
import Html.Attributes exposing (style, placeholder, value)
import Html.Events exposing (on, targetValue)
import String exposing (toFloat)


-- MODEL
type alias Pos = { row : Int, col : Int }

type alias Model = 
  { value : Float
  , expr : String
  , pos : Pos
  }


-- UPDATE

type Action = SetConst Float

update : Action -> Model -> Model
update (SetConst x) model = { model | value = x }

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ input
      [ placeholder "x"
      , value ("=" ++ toString model.value)
      , on "input" targetValue (message address)
      , subCellStyle
      ] 
      []
    , div [ subCellStyle ] [ text (toString model.value) ]
    ]

-- TODO refactor
message : Signal.Address Action -> String -> Signal.Message
message address s = Signal.message address (decideAction s)

decideAction : String -> Action
decideAction s = 
  case (String.toFloat (String.dropLeft 1 s)) of 
    Ok x -> SetConst x
    Err _ -> SetConst 0 -- ignore error for now

subCellStyle : Attribute
subCellStyle =
  style
    [ ("font-size", "20px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("text-align", "center")
    ]
