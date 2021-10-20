module FParsecPlayground.Helpers

open System
open System.Globalization

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
    
let (|TryInt|_|) (str : string) =
    match Int32.TryParse(str) with
    | true, value -> Some value
    | _ -> None
        
let (|TryDecimal|_|) (str : string) =
    match Decimal.TryParse(str, NumberStyles.Number, CultureInfo.InvariantCulture) with
    | true, value -> Some value
    | _ -> None
    
let (|TryDate|_|) (str : string) =
    match Date.TryParseExact(str, "yyyyMMdd", null, DateTimeStyles.None) with
    | true, value -> Some value
    | _ -> None
    
open FSharpPlus
    
let except exclude (source : 'a list) =
    source
    |> filter (not << (fun item -> exclude |> List.contains item))