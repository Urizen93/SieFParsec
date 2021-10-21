[<AutoOpen>]
module FParsecPlayground.Types

open System
open FParsecPlayground.SieParser.Types
open FSharpPlus
open FSharpPlus.Data

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

let private createVoucher (verification : Verification) transactions = {
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