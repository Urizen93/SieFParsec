module FParsecPlayground.TypeParsers

open SieParser
open FSharpPlus
open FParsecPlayground.Helpers
open FParsecPlayground.Tags

let ignoreTags tags sie =
    match tags |> List.contains sie.Tag with
    | true -> Some Ignored
    | _ -> None
    
let ignoreContent tags sie =
    match tags |> List.contains sie.Tag with
    | true -> Some IgnoredContent
    | false -> None

let companyID = function
    | { Tag = ORGNR; Values = values }
        -> values |> plainValues |> headOrNone |> map Orgnr
    | _ -> None
    
let companyName = function
    | { Tag = FNAMN; Values = values }
        -> values |> plainValues |> headOrNone |> map Fnamn
    | _ -> None
    
let createTransaction (values : SieValue list) =
    match values |> plainValues with
    | account
      :: TryDecimal balance
      :: TryDate date
      :: description
      :: _
       -> Some {
            Account = account
            Balance = balance
            Date = Some date
            Description = description
       }
    | account
      :: TryDecimal balance
      :: _
      :: description
      :: _
       -> Some {
            Account = account
            Balance = balance
            Date = None
            Description = description
       }
    | _ -> None
    
let transaction = function
    | { Tag = TRANS; Values = values; }
        -> createTransaction values |> map Trans
    | _ -> None
    
let createVerification values transactions =
    match values |> plainValues with
    | serie
      :: version
      :: TryDate date
      :: text
      :: _
      -> Some {
          Serie = serie
          Version = version
          Date = date
          Text = text
          Transactions = transactions
      }
    | _ -> None

let mapVoucherContent (transactions : SieBase list) =
    transactions
    |> map (transaction <|> ignoreContent [RTRANS; BTRANS] <|>% UnknownContent)
    |> filter (not << isIgnoredContent)

let voucher = function
    | { Tag = VER; Values = values; Children = children }
        -> mapVoucherContent children
        |> createVerification values
        |> map Ver
    | _ -> None
    
let account = function
    | { Tag = KONTO; Values = values }
     -> match values |> plainValues with
        | account::name::_ -> Some <| Konto { Code = account; Name = name }
        | _ -> None
    | _ -> None
    
let createBalance (values : SieValue list) =
    match values |> plainValues with
        | TryInt year
          ::account
          ::TryDecimal balance
          ::_ -> Some { YearIndex = year; Account = account; Balance = balance }
        | _ -> None
    
let openingBalance = function
    | { Tag = IB; Values = values } -> createBalance values |> map Ib
    | _ -> None
    
let closingBalance = function
    | { Tag = UB; Values = values } -> createBalance values |> map Ub
    | _ -> None
    
let resultAccount = function
    | { Tag = RES; Values = values } -> createBalance values |> map Res
    | _ -> None
    
let financialYear = function
    | { Tag = RAR; Values = values }
     -> match values |> plainValues with
        | TryInt year
          ::TryDate start
          ::TryDate finish
          ::_ -> Some <| Rar { YearIndex = year; Start = start; End = finish }
        | _ -> None
    | _ -> None
    
let uselessTags = allTags |> except [ VER; KONTO; IB; UB; RES; ORGNR; FNAMN; RAR ]
    
let all = [
    voucher
    account
    openingBalance
    closingBalance
    resultAccount
    companyID
    companyName
    financialYear
    ignoreTags uselessTags
]

let mapAll (sie : SieBase list) =
    sie
    |> map (choose all <|>% Unknown)
    |> filter (not << isIgnored)
    |> List.partition (function | Unknown _ -> false | _ -> true)