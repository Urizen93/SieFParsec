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

let createVoucher (verification : Verification) transactions = {
    Serie = verification.Serie
    Version = verification.Version
    Date = verification.Date
    Text = verification.Text
    Transactions = transactions }

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

type Company = {
    ID : string
    Name : string
}

let createCompany id name : Company option = monad {
    let! id = id
    let name = name |> Option.defaultValue id
    return { ID = id; Name = name }
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
    
let fromVerification (value : Verification) =
    value.Transactions
    |> List.choose (function
        | Info x -> Some <| Choice1Of2 x
        | UnknownContent x -> Some <| Choice2Of2 x
        | IgnoredContent _ -> None)
    |> List.partitionMap id
    |> mapItem1 (createVoucher value)
    
type SieDocument = {
    Type : string option
    Company : Company
    Period : FinancialYear
    Accounts : Account list
    Ingoing : Balance list
    Outgoing : Balance list
    Res : Balance list
    Vouchers : Voucher list
    BadRecords : SieRecord list
}

type SieDocumentBuilder = {
    mutable Type : string option
    mutable No : string option
    mutable Name : string option
    mutable Period : FinancialYear option
    mutable Accounts : Account list
    mutable Ingoing : Balance list
    mutable Outgoing : Balance list
    mutable Res : Balance list
    mutable Vouchers : Voucher list
    mutable BadRecords : SieRecord list
}

let createDocumentBuilder : SieDocumentBuilder =
    { Type = None; No = None; Name = None; Period = None
      Accounts = List.empty; Ingoing = List.empty; Outgoing = List.empty
      Res = List.empty; Vouchers = List.empty; BadRecords = List.empty; }

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
    
let withAccount value builder =
    builder.Accounts <- value :: builder.Accounts
    builder
    
let withIngoing value builder =
    builder.Ingoing <- value :: builder.Ingoing
    builder
    
let withOutgoing value builder =
    builder.Outgoing <- value :: builder.Outgoing
    builder
    
let withRes value builder =
    builder.Res <- value :: builder.Res
    builder
    
let withBadRecord (value : SieRecord) builder =
    builder.BadRecords <- value :: builder.BadRecords
    builder
    
let withBadRecords values builder =
    builder.BadRecords <- List.append values builder.BadRecords
    builder 
    
let withVoucher value builder =
    let voucher, badTransactions = fromVerification value
    builder.Vouchers <- voucher :: builder.Vouchers 
    builder |> withBadRecords badTransactions
    
let build builder : SieDocument option = monad {
    let! company = createCompany builder.No builder.Name
    let! period = builder.Period
    
    return { Type = builder.Type
             Company = company
             Period = period
             Accounts = builder.Accounts |> List.rev
             Ingoing = builder.Ingoing |> List.rev
             Outgoing = builder.Outgoing |> List.rev
             Res = builder.Res |> List.rev
             Vouchers = builder.Vouchers |> List.rev
             BadRecords = builder.BadRecords |> List.rev }
}