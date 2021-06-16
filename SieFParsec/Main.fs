module FParsecPlayground.Main

open FSharpPlus.Internals

let encoding = System.Text.CodePagesEncodingProvider.Instance.GetEncoding(437)

let readLines filePath =
    System.IO.File.ReadAllText(filePath, encoding)
    
let writeLines filePath lines =
    System.IO.File.WriteAllLines(filePath, lines)
        
[<EntryPoint>]
let main _ =
    let sieFile = readLines "C:\\sie\\sie.se"
    
    let input = "#VER \"24\" 3103708 20170102 \"Ankommande betalningar - 10100\" 20170102
{
    #TRANS 1204 {   } 9390.00 \"20170102\"  \"6794681\" 
    #TRANS 1208 {   } -9390.00 \"20170102\"  \"6794681\" 
}"
    
    match SieParser.parseSie sieFile with
    | Right result ->
        let parsed, unknown = TypeParsers.mapAll result
        
        match unknown with
        | [] -> printfn "ALL TAGS ARE PARSED"
        | _ ->  printfn $"THERE ARE SOME UNKNOWN TAGS: %A{unknown}"
        
        printfn "--------------------------"
        printfn $"%d{parsed.Length}"
        for p in parsed do printfn $"%A{p}"
        
    | Left error-> printfn $"%s{error}"
    0