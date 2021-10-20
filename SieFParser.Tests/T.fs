module SieFParser.Tests.T

open FParsecPlayground
open Xunit
open SieEncoding

let readStream = System.IO.File.OpenRead

[<Fact>]
let test () =
    let sieFile = readStream "C:\\sie\\big_sie1.se"
    let encoding = guessFromStream sieFile
    SieParser.parseSie sieFile encoding