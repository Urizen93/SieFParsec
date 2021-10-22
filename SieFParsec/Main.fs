module FParsecPlayground.Main

open System
open System.IO
open FSharpPlus

let readStream filePath =
    File.OpenRead(filePath)
    
let writeLines filePath lines =
    File.WriteAllLines(filePath, lines)
        
[<EntryPoint>]
let main _ =
    let sieFiles =
        "C:\\sie\\sie"
        |> Directory.GetFiles
        |> Seq.ofArray
        |> Seq.sort
    
    for sieFilePath in sieFiles do
        use sieFile = readStream sieFilePath
        let encoding = SieEncoding.guessFromStream sieFile
        printfn $"document {sieFilePath} | encoding {encoding.CodePage} {encoding.EncodingName}"
        
        sieFile.Position <- 0L
        match SieParser.parseSie sieFile encoding with
        | Choice1Of2 result ->
            match TypeParsers.document result with
            | Choice1Of2 document ->
                match document.BadRecords with
                | [] ->
                    printfn "Document seems to be ok"
                    File.Delete sieFilePath
                | badRecords ->
                    printfn "There are bad records in the document"
                    printfn $"%A{badRecords}"
                    printfn "Hit enter to remove the document and proceed - or quit the program to investigate!"
                    ignore <| Console.ReadLine ()
                    File.Delete sieFilePath
                    printfn $"%s{sieFilePath} deleted, moving forward..."
            | Choice2Of2 fail ->
                printfn $"There is a problem with the document %A{fail}"
                printfn "Hit enter to proceed - or quit the program to investigate!"
                ignore <| Console.ReadLine ()
        | Choice2Of2 error ->
            printfn $"%s{error}"
            printfn "Hit enter to proceed - or quit the program to investigate!"
            ignore <| Console.ReadLine ()
    0