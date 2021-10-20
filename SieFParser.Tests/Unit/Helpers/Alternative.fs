module Alternative

open FParsecPlayground
open Xunit
open Helpers
open FsUnit
open Xunit

type Result =
    | Left
    | Right
    
let someLeft _ = Some Left
let someRight _ = Some Right
let none _ = None

[<Fact>]
let ``returns the result of the left argument if both return some`` () =
    someLeft <|> someRight
    <| () |> should equal <| Some Left
    
[<Fact>]
let ``returns the result of the left argument if the left returns some`` () =
    someLeft <|> none
    <| () |> should equal <| Some Left
    
[<Fact>]
let ``returns the result of the right argument if the left returns none`` () =
    none <|> someRight
    <| () |> should equal <| Some Right
    
[<Fact>]
let ``returns none if both return none`` () =
    none <|> none
    <| () |> should equal <| None