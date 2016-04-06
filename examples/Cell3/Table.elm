module Table where

import Cell exposing (Pos)
import Html exposing (..)
import Html.Attributes exposing (style, placeholder, value)
import Html.Events exposing (on, targetValue)
import List exposing (append, concatMap, filter, head, map, repeat, take)
import Maybe exposing (withDefault)

type alias Model =
  { nrows : Int
  , ncols : Int
  , cells : List Cell.Model
  }

renumberRows : (Int -> Bool) -> (Int -> Int) -> List Cell.Model -> List Cell.Model
renumberRows cond f = map <| 
    \cell ->
      if cond cell.pos.row then { cell | pos = { row = (f cell.pos.row), col = cell.pos.col } } else cell

newRowCells : Int -> Int -> List Cell.Model
newRowCells ncols irow =
  [1..ncols] |> map (\icol -> { value = 0, pos = { row = irow, col = icol } })

addRowCells : Int -> Int -> List Cell.Model -> List Cell.Model
addRowCells ncols irow = append (newRowCells ncols irow)


addRow : Int -> Model -> Model
addRow irow model =
  { model
    | nrows = model.nrows + 1
    , cells = model.cells |> renumberRows ((>=) irow) ((+) 1)
                          |> addRowCells model.ncols irow
  }

removeRow : Int -> Model -> Model
removeRow irow model = 
  { model
    | nrows = model.nrows - 1
    , cells = model.cells |> filter (\cell -> cell.pos.row /= irow)
                          |> renumberRows ((>) irow) ((-) 1)
  }

-- TODO EDF
addCol : Int -> Model -> Model
addCol icol = identity

-- TODO EDF
removeCol : Int -> Model -> Model
removeCol icol = identity

getCellAt : Pos -> Model -> Cell.Model
getCellAt pos model = 
  model.cells |> filter (\cell -> cell.pos==pos) |> head |> withDefault { value = 0, pos = { row=0, col=0 } }

setCellAt : Pos -> Model -> Cell.Model -> Model
setCellAt pos model newCell =
  let maybeReplace cell = if cell.pos==pos then newCell else cell
  in { model | cells = map maybeReplace model.cells }

editCell : Pos -> Cell.Action -> Model -> Model
editCell pos action model = 
  let oldCell = getCellAt pos model
      newCell = Cell.update action oldCell
  in setCellAt pos model newCell


initModel : Int -> Int -> Model
initModel nrows ncols = 
  let newCells = [1..nrows] |> concatMap (\irow -> newRowCells ncols irow)
  in { nrows = nrows, ncols = ncols, cells = newCells }

type Action
    = AddRow Int
    | RemoveRow Int
    | AddCol Int
    | RemoveCol Int
    | EditCell Pos Cell.Action

update : Action -> Model -> Model
update action = 
  case action of
    AddRow irow -> addRow irow
    RemoveRow irow -> removeRow irow
    AddCol icol -> addCol icol
    RemoveCol icol -> removeCol icol
    EditCell pos a -> editCell pos a

view : Signal.Address Action -> Model -> Html
view address model =
  let thCol icol = th [] [text <| toString icol]
      --tdCell pos = td [] [text <| toString (getCellAt pos model).value]
      tdCell pos = td [] [ Cell.view (Signal.forwardTo address <| EditCell pos) <| getCellAt pos model ]
      trRow irow = tr [] <| List.map (\c -> tdCell {row=irow, col=c}) [1..model.ncols]
  in
    div []
    [ table []
      [ thead [] [ tr [] (List.map thCol [1..model.ncols]) ]
      , tbody [] (List.map trRow [1..model.nrows])
      ]
    ]

