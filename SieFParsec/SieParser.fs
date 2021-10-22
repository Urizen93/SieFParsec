module FParsecPlayground.SieParser

open System
open FParsec
open FParsec.Pipes

[<AutoOpen>]
module Types =
    type SieValue =
        | Value of string
        | ObjectList of string list
        
    let private sieValue = function
        | Value value -> Some value
        | _ -> None
        
    let plainValues = List.choose sieValue
        
    type SieRecord =
        | Base of SieBase
        | NonParsed of string
        
    and SieBase = {
        Tag : string
        Values : SieValue list
        Children : SieRecord list
    }

[<AutoOpen>]
module private Chars =
    let doubleQuote = %'"'
    let curlyOpen = %'{'
    let curlyClose = %'}'
    let notCurlyClose = satisfy (fun ch -> ch <> '}')
    let hashtag = skipChar '#'
    let escapedDoubleQuote =
        %'\\' >>. doubleQuote
    let nonSpace = satisfy (not << Char.IsWhiteSpace)
    let lineSpace = satisfy (fun ch -> Char.IsWhiteSpace(ch) && (ch <> '\n'))
    let lineSpaces = skipMany lineSpace
    let eol = newline |>> ignore <|> eof

[<AutoOpen>]
module private Parsers =
    let manyBetween first last values = first >>. manyTill values last
    let manyCharsBetween2 boundary chars = boundary >>. manyCharsTill chars boundary

    let value = many1Chars nonSpace
    let quotedString =
        (attempt escapedDoubleQuote <|> anyChar)
        |> manyCharsBetween2 doubleQuote
    let tag = hashtag >>. value
    let listOfObjects =
        (quotedString <|> many1Chars (lookAhead notCurlyClose >>. nonSpace)) .>> lineSpaces
        |> manyBetween (curlyOpen .>> lineSpaces) curlyClose
        
    let sieValue = %[
        (listOfObjects |>> ObjectList)
        (quotedString |>> Value)
        (value |>> Value)]
        
    let sieBase, sieBaseRef = createParserForwardedToRef()

    let skipTillNonSpace =
        spaces >>. (lookAhead nonSpace |>> ignore <|> eof)
        
    let trim (str : string) =
        str.Trim()
        
    let nonParsed =
        many1CharsTill anyChar (lookAhead hashtag <|> eof)
        |>> trim

    let sieRecord = %[
        attempt sieBase |>> Base
        nonParsed |>> NonParsed]

    let children =
        (sieRecord .>> skipTillNonSpace)
        |> manyBetween (spaces >>. curlyOpen .>> spaces) curlyClose
        
    do sieBaseRef :=
        parse {
            let! tag = tag
            
            let! values =
                (sieValue .>> lineSpaces)
                |> manyBetween lineSpaces eol
            
            let! children = (attempt children) <|>% list.Empty
            
            return { Tag = tag; Values = values; Children = children }
        }

    let sieParser =
        sepEndBy sieRecord skipTillNonSpace
        
let parseSie input encoding =
    match runParserOnStream sieParser () "" input encoding with
    | Success(result, _, _) -> Choice1Of2 result
    | Failure(error, _, _) -> Choice2Of2 error