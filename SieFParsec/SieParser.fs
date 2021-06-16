module FParsecPlayground.SieParser

open System
open FParsec
open FSharpPlus.Internals

[<AutoOpen>]
module Types =
    type SieValue =
        | Value of string
        | ObjectList of string list
        
    let sieValue = function
        | Value value -> Some value
        | _ -> None
        
    let objectList = function
        | ObjectList list -> Some list
        | _ -> None
        
    let plainValues (values : SieValue list) =
        values
        |> List.choose sieValue
        
    type SieBase = {
        Tag : string
        Values : SieValue list
        Children : SieBase list
    }

[<AutoOpen>]
module Chars =
    let doubleQuote = pchar '"'
    let curlyOpen = skipChar '{'
    let curlyClose = skipChar '}'
    let hashtag = skipChar '#'
    let escapedDoubleQuote =
        skipChar '\\' >>. doubleQuote
    let nonSpace = satisfy (not << Char.IsWhiteSpace)
    let lineSpace = satisfy (fun x -> Char.IsWhiteSpace(x) && (x <> '\n'))
    let lineSpaces = skipMany lineSpace
    let eol = newline |>> ignore <|> eof


let manyBetween first last values = first >>. manyTill values last
let manyCharsBetween2 boundary chars = boundary >>. manyCharsTill chars boundary

let value = many1Chars nonSpace
let quotedString =
    (escapedDoubleQuote <|> anyChar)
    |> manyCharsBetween2 doubleQuote
let tag = hashtag >>. value
let listOfObjects =
    lineSpaces >>. (quotedString <|> value) .>> lineSpaces
    |> manyBetween curlyOpen curlyClose
    
let sieValues =
    choice [
        (listOfObjects |>> ObjectList)
        (quotedString |>> Value)
        (value |>> Value)
    ]
    
let sieEntity, sieEntityRef = createParserForwardedToRef()

let children =
    sieEntity
    |> manyBetween (spaces >>. curlyOpen .>> spaces) (curlyClose .>> spaces)
    
do sieEntityRef :=
    parse {
        let! tag = tag |> between spaces lineSpaces
        
        let! values =
            (sieValues .>> lineSpaces)
            |> manyBetween lineSpaces (lineSpaces >>. eol)
        
        let! children = (attempt children) <|>% list.Empty
        
        return { Tag = tag; Values = values; Children = children }
    }
    
let sieParser =
    many sieEntity

let parseSie input =
    match run sieParser input with
    | Success(result, _, _) -> Right result
    | Failure(error, _, _) -> Left error