module Table where

import Cell exposing (Pos)
import Expr exposing (Expr)
import Html exposing (..)
import Html.Attributes exposing (style, placeholder, value)
import Html.Events exposing (on, targetValue)
import List exposing (append, concatMap, filter, foldl, head, map, member, repeat, take)
import Maybe exposing (withDefault)

type alias Model =
  { nrows : Int
  , ncols : Int
  , cells : List Cell.Model
  , evalErr : Maybe String
  }

type alias TopoState =
  { frontier : List Cell.Model
  , scells : List Cell.Model
  }

topoVisitCell : Model -> Cell.Model -> TopoState -> Result String TopoState
topoVisitCell tmodel cell state =
  if List.member cell state.frontier
  then Err ("Cycle found at " ++ Expr.strPos cell.pos)
  else if List.member cell state.scells
  then Ok state
  else 
    let state' = { state | frontier = cell::state.frontier }
        refs = Debug.log ("refs are: ") (Expr.refs cell.expr)
        f pos r = -- TODO EDF use Result.andThen here!!
          case r of
            Ok s -> topoVisitCell tmodel (getCellAt pos tmodel) (Debug.log "recurse tvc" s)
            _ -> r
    in foldl f (Ok state') refs |> Result.map (\s -> { s | frontier=state.frontier, scells = cell::s.scells})

-- | Topological sort of cells by reference. Leaf nodes are in front.
topoCells : Model -> Result String (List Cell.Model)
topoCells tmodel =  
  let f cell r = 
    case r of 
      Ok tstate -> topoVisitCell tmodel cell { frontier=[], scells=tstate.scells }
      _ -> r
  in foldl f (Ok {frontier=[], scells=[]}) tmodel.cells |> (flip Result.andThen) (\tstate -> Ok <| List.reverse tstate.scells)

-- | Errors when circular references exist.
evalTable : Model -> Model
evalTable model = 
  case topoCells model of
    Err e -> Debug.log ("evalTable error: " ++ e) { model | evalErr=Just e, cells=map (\c -> {c | eval=Expr.Error }) model.cells }
    Ok cells ->
      Debug.log ("cells are " ++ toString cells) (foldl (\cell context -> (cell.pos, Expr.eval context cell.expr)::context) [] cells |> 
        \context -> updateFromContext context model)

updateFromContext : Expr.Context -> Model -> Model
updateFromContext context model = { model | cells = map (Cell.updateFromContext context) model.cells }

renumberRows : (Int -> Bool) -> (Int -> Int) -> List Cell.Model -> List Cell.Model
renumberRows cond f = map <| 
    \cell ->
      if cond cell.pos.row then { cell | pos = { row = (f cell.pos.row), col = cell.pos.col } } else cell

--type alias Model = 
--  { value : String
--  , expr : Expr.Expr
--  , err : Maybe String
--  , pos : Pos
--  , eval : Expr.Eval
--  }

newRowCells : Int -> Int -> List Cell.Model
newRowCells ncols irow =
  [1..ncols] |> map (\icol -> Cell.initModel irow icol)

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

-- modifying cells

getCellAt : Pos -> Model -> Cell.Model
getCellAt pos model = 
  model.cells |> filter (\cell -> cell.pos==pos) |> head |> withDefault (Cell.initModel 0 0)

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
  in { nrows = nrows, ncols = ncols, cells = newCells, evalErr = Nothing }

type Action
    = AddRow Int
    | RemoveRow Int
    | AddCol Int
    | RemoveCol Int
    | EditCell Pos Cell.Action

update : Action -> Model -> Model
update action m = 
  let m' = 
      case action of
        AddRow irow -> addRow irow m
        RemoveRow irow -> removeRow irow m
        AddCol icol -> addCol icol m
        RemoveCol icol -> removeCol icol m
        EditCell pos a -> editCell pos a m
  in evalTable m'


viewCell : Signal.Address Action -> Pos -> Cell.Model -> Html
viewCell address pos cell = 
  Cell.view (Signal.forwardTo address <| EditCell pos) <| cell

view : Signal.Address Action -> Model -> Html
view address model =
  let --evals = evaluate model
      thCol icol = th [] [text <| Expr.strCol icol]
      tdCell pos = td [] [ viewCell address pos (getCellAt pos model) ]
      trRow irow = tr [] <| (td [] [text <| toString irow])::(List.map (\c -> tdCell {row=irow, col=c}) [1..model.ncols])
  in
    div []
    [ table []
      [ thead [] [ tr [] <| (th [] [text " "])::(List.map thCol [1..model.ncols]) ]
      , tbody [] (List.map trRow [1..model.nrows])
      ]
    ]

