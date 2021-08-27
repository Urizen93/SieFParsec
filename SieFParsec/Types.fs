[<AutoOpen>]
module FParsecPlayground.Types

open System
open FParsecPlayground.SieParser.Types
open FSharpPlus

type Transaction = {
    Account : string
    Balance : decimal
    Date : Date option
    Description : string
}

type TransactionInfo =
    | Trans of Transaction

type VerificationContent =
    | IgnoredContent
    | UnknownContent of SieRecord
    | Info of TransactionInfo

type Verification = {
    Serie : string
    Version : string
    Date : Date
    Text : string
    Transactions : VerificationContent list
}

type Voucher = {
    Serie : string
    Version : string
    Date : Date
    Text : string
    Transactions : TransactionInfo list
}

type Account = {
    Code : string
    Name : string
}

type Balance = {
    YearIndex : int
    Account : string
    Balance : decimal
}

type FinancialYear = {
    YearIndex : int
    Start : Date
    End : Date
}

type SieEntity =
    | Orgnr of string
    | Fnamn of string
    | Ver of Verification
    | Konto of Account
    | Ib of Balance
    | Ub of Balance
    | Rar of FinancialYear
    | Res of Balance

type AnySieEntity =
    | Ignored
    | Unknown of SieRecord
    | Sie of SieEntity

let isIgnoredContent = function
    | IgnoredContent -> true
    | _ -> false
    
let isIgnored = function
    | Ignored -> true
    | _ -> false
    
let createVoucher (value : Verification) =
    let transactions, badTransactions =
        value.Transactions
        |> List.filter (not << isIgnoredContent)
        |> List.partitionMap (function
        | Info x -> Choice1Of2 x
        | UnknownContent x -> Choice2Of2 x
        | IgnoredContent _ -> raise <| Exception "Ignored values should be filtered out by now!")
    ({Serie = value.Serie
      Version = value.Version
      Date = value.Date
      Text = value.Text
      Transactions = transactions},
      badTransactions)
    
type SieDocument = {
    No : string
    Name : string
    Period : FinancialYear
    Accounts : Account list
    Ingoing : Balance list
    Outgoing : Balance list
    Res : Balance list
    Vouchers : Voucher list
    BadRecords : SieRecord list
}

type SieDocumentBuilder = {
    mutable No : string option
    mutable Name : string option
    mutable Period : FinancialYear option
    mutable Accounts : Account seq
    mutable Ingoing : Balance seq
    mutable Outgoing : Balance seq
    mutable Res : Balance seq
    mutable Vouchers : Voucher seq
    mutable BadRecords : SieRecord seq
}

let createDocumentBuilder : SieDocumentBuilder =
    { No = None; Name = None; Period = None
      Accounts = Seq.empty; Ingoing = Seq.empty; Outgoing = Seq.empty
      Res = Seq.empty; Vouchers = Seq.empty; BadRecords = Seq.empty; }

let withNo value builder =
    match builder.No with
    | Some existing when existing = value -> ()
    | Some existing -> raise <| Exception $"Found %s{value}, but there already is %s{existing}"
    | None -> builder.No <- Some value
    builder
    
let withName value builder =
    match builder.Name with
    | Some existing when existing = value -> ()
    | Some existing -> raise <| Exception $"Found %s{value}, but there already is %s{existing}"
    | None -> builder.Name <- Some value
    builder
    
let withPeriod value builder =
    match builder.Period with
    | Some existing when existing = value -> ()
    | Some existing -> raise <| Exception $"Found %A{value}, but there already is %A{existing}"
    | None -> builder.Period <- Some value
    builder
    
let (+) a b =
    Seq.append a <| Seq.singleton b
    
let withAccount value builder =
    builder.Accounts <- builder.Accounts + value
    builder
    
let withIngoing value builder =
    builder.Ingoing <- builder.Ingoing + value
    builder
    
let withOutgoing value builder =
    builder.Outgoing <- builder.Outgoing + value
    builder
    
let withRes value builder =
    builder.Res <- builder.Res + value
    builder
    
let withBadRecord value builder =
    builder.BadRecords <- builder.BadRecords + value
    builder
    
let withBadRecords values builder =
    builder.BadRecords <- Seq.append builder.BadRecords values
    builder
    
let withVoucher value builder =
    let voucher, badTransactions = createVoucher value
    builder.Vouchers <- builder.Vouchers + voucher
    builder |> withBadRecords badTransactions
    
let build builder : SieDocument option =
    monad {
        let! no = builder.No
        let name = builder.Name |> Option.defaultValue no
        let! period = builder.Period
        
        return { No = no
                 Name = name
                 Period = period
                 Accounts = builder.Accounts |> List.ofSeq
                 Ingoing = builder.Ingoing |> List.ofSeq
                 Outgoing = builder.Outgoing |> List.ofSeq
                 Res = builder.Res |> List.ofSeq
                 Vouchers = builder.Vouchers |> List.ofSeq
                 BadRecords = builder.BadRecords |> List.ofSeq }
    }