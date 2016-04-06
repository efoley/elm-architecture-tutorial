module Expr where

import Char
import Debug
import List exposing (filter, head)
import Parser exposing (..)
import Parser.Char exposing (..)
import Parser.Number exposing (float, integer, natural)
import Result exposing (withDefault)


-- Expressions

type alias Pos = { row : Int, col : Int }

type alias Fun1 = String
type alias Fun2 = String

type Expr = Const Float
          | Ref Pos
          | Range Pos Pos

type Eval = Unknown
          | Error
          | Val Float

type alias Context = List (Pos, Eval)


parse : String -> Result String Expr
parse s = Parser.parse expr s

debugParse : String -> String
debugParse s =
  let r = parse s
  in case r of
      Ok expr -> debugPrint expr
      Err err -> "Error: " ++ err

debugPrint : Expr -> String
debugPrint expr =
  case expr of
    Const f -> "(Const " ++ toString f ++ ")"
    Ref pos -> "(Ref " ++ strPos pos ++ ")"
    Range pos1 pos2 -> "(Range " ++ strPos pos1 ++ " " ++ strPos pos2 ++ ")"

refs : Expr -> List Pos
refs expr = 
  case expr of
    Const _ -> []
    Ref p -> [p]
    Range p1 p2 -> Debug.crash "not implemented"

deref : Context -> Pos -> Eval
deref context pos = context |> List.filter (fst >> (==) pos) |> List.map snd |> List.head |> Maybe.withDefault Unknown

eval : Context -> Expr -> Eval
eval context expr =
  case expr of
    Const x -> Val x
    Ref p -> Debug.log ("pos is " ++ (toString p) ++ "\ncontext is " ++ (toString context)) <| deref context p
    Range p1 p2 -> Debug.crash "not implemented"

strCol : Int -> String
strCol icol = toString <| Char.fromCode <| Char.toCode 'A' + (icol-1)

strPos : Pos -> String
strPos pos = (strCol pos.col) ++ (toString pos.row) 

val : Parser Expr
val = (map toFloat integer) `or` float |> map Const

crToPos : Char -> Int -> Pos
crToPos c r = { row=r, col=(Char.toCode c - Char.toCode 'A' + 1)  }

pos : Parser Pos
pos = map crToPos (map Char.toUpper <| upper `or` lower) |> andMap natural

ref : Parser Expr
ref = map Ref pos

range : Parser Expr
range = map Range pos <* symbol ':' |> andMap pos

expr : Parser Expr
expr = choice [val, range, ref]
