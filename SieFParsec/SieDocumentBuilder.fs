module FParsecPlayground.SieDocumentBuilder

open System
open FParsecPlayground
open FParsecPlayground.SieParser
open FSharpPlus

//this one is mutable in order to reduce GC pressure
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

[<AutoOpen>]
module private Mutators =
    let withType value builder =
        builder.Type <- value

    let withNo value builder =
        match builder.No with
        | Some existing when existing = value -> ()
        | Some existing -> raise <| Exception $"Found %s{value}, but there already is %s{existing}"
        | None -> builder.No <- Some value
        
    let withName value builder =
        match builder.Name with
        | Some existing when existing = value -> ()
        | Some existing -> raise <| Exception $"Found %s{value}, but there already is %s{existing}"
        | None -> builder.Name <- Some value
        
    let withPeriod value builder =
        match builder.Period with
        | Some existing when existing = value -> ()
        | Some existing -> raise <| Exception $"Found %A{value}, but there already is %A{existing}"
        | None -> builder.Period <- Some value
        
    let withAccount value builder =
        builder.Accounts <- value :: builder.Accounts
        
    let withIngoing value builder =
        builder.Ingoing <- value :: builder.Ingoing
        
    let withOutgoing value builder =
        builder.Outgoing <- value :: builder.Outgoing
        
    let withRes value builder =
        builder.Res <- value :: builder.Res
        
    let withBadRecords values builder =
        builder.BadRecords <- List.append values builder.BadRecords
        
    let withVoucher value builder =
        let voucher, badTransactions = fromVerification value
        builder.Vouchers <- voucher :: builder.Vouchers 
        builder |> withBadRecords badTransactions
        
    let withBadRecord (value : SieRecord) builder =
        builder.BadRecords <- value :: builder.BadRecords
        builder
        
    let addEntity (entity : SieEntity) builder =
        match entity with
        | Orgnr no -> builder |> withNo no
        | Fnamn name -> builder |> withName name
        | Ver v -> builder |> withVoucher v
        | Konto v -> builder |> withAccount v
        | Ib v -> builder |> withIngoing v
        | Ub v -> builder |> withOutgoing v
        | Rar year -> match year with
                      | { YearIndex = 0 } -> builder |> withPeriod year
                      | _ -> ()
        | Res v -> builder |> withRes v
        builder
        
    let handleEntity value =
        match value with
        | Sie sie -> addEntity sie
        | Unknown unknown -> withBadRecord unknown
        | Ignored _ -> id
    
let consume builder value =
    builder |> handleEntity value
    
let build builder = monad {
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

let create =
    { Type = None; No = None; Name = None; Period = None
      Accounts = List.empty; Ingoing = List.empty; Outgoing = List.empty
      Res = List.empty; Vouchers = List.empty; BadRecords = List.empty; }