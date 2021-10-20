module HeadOrNone

open AutoFixture
open AutoFixture.Xunit2
open FParsecPlayground
open Xunit
open Helpers
open FsUnit
open TestTools

[<Sealed>]
type EmptyListAttribute() =
    inherit AutoDataAttribute(fun () -> Fixture() |> withValue List.empty<int>)
    
[<Theory; EmptyList>]
let ``returns none if the list is empty`` (list : int list) =
    headOrNone list
    |> should equal None
    
[<Theory; AutoDataExtended>]
let ``returns head of the list`` (list : int list) =
    headOrNone list
    |> should equal <| Some list.Head