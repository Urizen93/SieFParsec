module FParsecPlayground.TypeParsers

open SieParser
open FSharpPlus
open FParsecPlayground.Helpers
open FParsecPlayground.Tags

let companyID = function
    | Base { Tag = ORGNR; Values = values }
        -> values |> plainValues |> headOrNone |> map Orgnr |> map Sie
    | _ -> None
    
let companyName = function
    | Base { Tag = FNAMN; Values = values }
        -> values |> plainValues |> headOrNone |> map Fnamn |> map Sie
    | _ -> None
    
let sieType = function
    | Base { Tag = SIETYP; Values = values }
        -> values |> plainValues |> headOrNone |> map SieTyp |> map Sie
    | _ -> None
    
let private createTransaction (values : SieValue list) =
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
    | Base { Tag = TRANS; Values = values; }
        -> createTransaction values |> map Trans |> map Info
    | _ -> None
    
let private createVerification values transactions : Verification option =
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

let ignoreContent tags sie =
    match sie with
    | Base sieBase -> 
        match  tags |> List.contains sieBase.Tag with
        | true -> Some IgnoredContent
        | false -> None
    | _ -> None

let mapVoucherContent (transactions : SieRecord list) =
    transactions
    |> map (transaction <|> ignoreContent [RTRANS; BTRANS] <|>% UnknownContent)
    |> filter (not << isIgnoredContent)

let voucher = function
    | Base { Tag = VER; Values = values; Children = children }
        -> mapVoucherContent children
        |> createVerification values
        |> map Ver
        |> map Sie
    | _ -> None
    
let account = function
    | Base { Tag = KONTO; Values = values }
     -> match values |> plainValues with
        | account::name::_ -> Some <| Konto { Code = account; Name = name } |> map Sie
        | _ -> None
    | _ -> None
    
let private createBalance (values : SieValue list) =
    match values |> plainValues with
        | TryInt year
          ::account
          ::TryDecimal balance
          ::_ -> Some { YearIndex = year; Account = account; Balance = balance }
        | _ -> None
    
let openingBalance = function
    | Base { Tag = IB; Values = values } -> createBalance values |> map Ib |> map Sie
    | _ -> None
    
let closingBalance = function
    | Base { Tag = UB; Values = values } -> createBalance values |> map Ub |> map Sie
    | _ -> None
    
let resultAccount = function
    | Base { Tag = RES; Values = values } -> createBalance values |> map Res |> map Sie
    | _ -> None
    
let financialYear = function
    | Base { Tag = RAR; Values = values }
     -> match values |> plainValues with
        | TryInt year
          ::TryDate start
          ::TryDate finish
          ::_ -> Some <| Rar { YearIndex = year; Start = start; End = finish } |> map Sie
        | _ -> None
    | _ -> None
    
let uselessTags = allTags |> except [ VER; KONTO; IB; UB; RES; ORGNR; FNAMN; RAR; TRANS; BTRANS; RTRANS; SIETYP ]
    
let ignoreTags tags sie =
    match sie with
    | Base sieBase -> 
        match tags |> List.contains sieBase.Tag with
        | true -> Some Ignored
        | _ -> None
    | _ -> None
    
let all = [
    voucher
    account
    openingBalance
    closingBalance
    resultAccount
    companyID
    companyName
    financialYear
    sieType
    ignoreTags uselessTags
]

open SieDocumentBuilder

let document sie =
    sie
    |> map (choose all <|>% Unknown)
    |> List.fold consume create
    |> build