module FParsecPlayground.Helpers

open System
open System.Globalization
open FSharpPlus

let headOrNone = function
    | head::_ -> Some head
    | _ -> None
    
let (<|>) left right =
    fun arg ->
        match left arg with
        | Some value -> Some value
        | None -> right arg
            
let (<|>%) left right =
    fun arg ->
        match left arg with
        | Some value -> value
        | None -> right arg
        
let choose options =
    options
    |> List.reduce (<|>)
    
let (|Int|_|) (str : string) =
    match Int32.TryParse(str) with
    | true, value -> Some value
    | _ -> None
        
let (|InvariantDecimal|_|) (str : string) =
    match Decimal.TryParse(str, NumberStyles.Number, CultureInfo.InvariantCulture) with
    | true, value -> Some value
    | _ -> None
    
let (|CompactDate|_|) (str : string) =
    match Date.TryParseExact(str, "yyyyMMdd", null, DateTimeStyles.None) with
    | true, value -> Some value
    | _ -> None
    
let except exclude (source : 'a list) =
    source
    |> filter (not << (fun item -> exclude |> List.contains item))