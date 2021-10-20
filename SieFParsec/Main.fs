module FParsecPlayground.Main

open System.IO
open System.Text
open FSharpPlus.Internals
open FSharpPlus

let readStream filePath =
    File.OpenRead(filePath)
    
let writeLines filePath lines =
    File.WriteAllLines(filePath, lines)
        
[<EntryPoint>]
let main _ =
    let sieFile = readStream "C:\\sie\\big_sie1.se"
    
    match SieParser.parseSie sieFile (CodePagesEncodingProvider.Instance.GetEncoding 437) with
    | Right result ->
        printfn $"%A{(TypeParsers.document result |> Option.map (fun (document : SieDocument) -> document.BadRecords))}"
    | Left error-> printfn $"%s{error}"
    0