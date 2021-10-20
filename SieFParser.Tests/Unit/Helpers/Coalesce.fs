module Coalesce

open FParsecPlayground
open Xunit
open Helpers
open FsUnit
open Xunit

type Result =
    | Left
    | Right
    
let someLeft _ = Some Left
let right _ = Right
let none _ = None

[<Fact>]
let ``returns the result of the left argument if the left returns some`` () =
    someLeft <|>% right
    <| () |> should equal <| Left
    
[<Fact>]
let ``returns the result of the right argument if the left returns none`` () =
    none <|>% right
    <| () |> should equal <| Right