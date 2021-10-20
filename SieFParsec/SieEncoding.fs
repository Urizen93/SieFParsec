module SieEncoding

open System.IO
open System.Text
open System.Text.RegularExpressions

let private defaultEncoding = CodePagesEncodingProvider.Instance.GetEncoding 437

let private b2s (b : byte) = b |> Array.singleton |> defaultEncoding.GetString

let private encodingSpecialCharacters =
    Regex($"{b2s(228uy)}|{b2s(218uy)}|{b2s(246uy)}", RegexOptions.Compiled);
        
let private encodingSpecialCharacters2 =
    Regex("[ÅåÄäÖö]", RegexOptions.Compiled)
    
let private guessEncoding (input : string) =
    match encodingSpecialCharacters.IsMatch(input) with
    | true -> Some <| CodePagesEncodingProvider.Instance.GetEncoding 1252
    | _ -> None
        
let private guessByLines (reader : StreamReader) =
    match reader.ReadLine() with
    | null -> None
    | line when encodingSpecialCharacters2.IsMatch(line) -> None
    | line -> Some (guessEncoding line, reader)
    
let guess reader =
    reader
    |> Seq.unfold guessByLines
    |> Seq.choose id
    |> Seq.tryHead
    |> Option.defaultValue defaultEncoding
    
let guessFromStream (stream : Stream) =
    guess |> using (new StreamReader(stream, defaultEncoding))