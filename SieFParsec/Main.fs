module FParsecPlayground.Main

open FSharpPlus.Internals

let encoding = System.Text.CodePagesEncodingProvider.Instance.GetEncoding(437)

let readLines filePath =
    System.IO.File.ReadAllText(filePath, encoding)
    
let writeLines filePath lines =
    System.IO.File.WriteAllLines(filePath, lines)
        
[<EntryPoint>]
let main _ =
    let sieFile = readLines "C:\\sie\\big_sie1.se"
    
    match SieParser.parseSie sieFile with
    | Right result ->
        match TypeParsers.document result with
        | Some document -> printfn $"%A{document.BadRecords}"
        | None -> printfn "Failed to parse!"
        
    | Left error-> printfn $"%s{error}"
    0