[<AutoOpen>]
module FParsecPlayground.Types

open System
open SieParser

type Transaction = {
    Account : string
    Balance : decimal
    Date : Date option
    Description : string
}
    
type VerificationContent =
    | IgnoredContent
    | UnknownContent of SieBase
    | Trans of Transaction

type Verification = {
    Serie : string
    Version : string
    Date : Date
    Text : string
    Transactions : VerificationContent list
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
    | Ignored
    | Unknown of SieBase
    | Orgnr of string
    | Fnamn of string
    | Ver of Verification
    | Konto of Account
    | Ib of Balance
    | Ub of Balance
    | Rar of FinancialYear
    | Res of Balance
    
let isIgnoredContent = function
    | IgnoredContent -> true
    | _ -> false
    
let isIgnored = function
    | Ignored -> true
    | _ -> false