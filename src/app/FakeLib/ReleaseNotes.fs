namespace Fake

open System
open System.Text.RegularExpressions

type ReleaseNotes =
    { AssemblyVersion: string
      NugetVersion: string
      Notes: string list }
    override x.ToString() = sprintf "%A" x

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ReleaseNotes =
    open FParsec

    let private nugetRegex = getRegEx @"([0-9]+.)+[0-9]+(-[a-zA-Z]+)?"
    let private assemblyRegex = getRegEx @"([0-9]+.)+[0-9]+"

    /// Parse simple release notes sequence
    let private parseSimpleReleaseNotes (text: seq<string>) = 
        let lastLine = text |> Seq.last
        let assemblyVersion, nugetVersion = assemblyRegex.Match (lastLine), nugetRegex.Match (lastLine)
        if not assemblyVersion.Success
        then failwith "Unable to parse valid Assembly version from release notes."
        let notes = 
            lastLine.Substring (nugetVersion.Index + nugetVersion.Length)
            |> trimChars [|' '; '-'|]
            |> split '.'
            |> List.map trim
            |> List.filter isNotNullOrEmpty
            |> List.map (fun x -> x + ".")
        { AssemblyVersion = assemblyVersion.Value; NugetVersion = nugetVersion.Value; Notes = notes }

    /// Parse "complex" release notes text sequence
    let private parseComplexReleaseNotes (text: seq<string>) =
        let rec loop notes = function
            | [] -> failwithf "No header in %A" text
            | h :: t -> 
                if "#" <* h then h, notes
                else loop (h :: notes) t

        let header, notes = loop [] (text |> Seq.map (trimChars [|' '; '*'|]) |> List.ofSeq |> List.rev)
        let assemblyVer, nugetVer = assemblyRegex.Match header, nugetRegex.Match header
        if not assemblyVer.Success then failwith "Unable to parse valid Assembly version from release notes."
        { AssemblyVersion = assemblyVer.Value; NugetVersion = nugetVer.Value; Notes = notes }
    
    /// Parse a Release Notes text - Either simple or "complex" format
    /// See: https://github.com/fsharp/FAKE/issues/171
    /// <param name="data">Release notes text</param>
    let parseReleaseNotes' (data: seq<string>) = 
        let data = data |> Seq.toList |> List.filter (not << isNullOrWhiteSpace)
        match data with
        | [] -> failwith "Empty Realease file."
        | h :: _ -> 
            let (|Simple|Complex|Invalid|) = function '*' -> Simple | '#' -> Complex | _ -> Invalid
            let firstNonEmptyChar = h.Trim([|'-'; ' '|]).[0]
            match firstNonEmptyChar with
            | Simple -> parseSimpleReleaseNotes data
            | Complex -> parseComplexReleaseNotes data
            | Invalid -> failwith "Invalid Release Notes format."


    let parseReleaseNotes (data: string) =
// * 1.1.9 - Infer booleans for ints that only manifest 0 and 1. Support for partially overriding the Schema in CsvProvider.
// * 1.1.10 - Support for heterogeneous XML attributes. Make CsvFile re-entrant.
        let inline separated delimiter (items: string seq) = String.Join(delimiter, Array.ofSeq items)

        let version = sepBy1 puint32 (pchar '.') |>> fun x -> x |> Seq.map string |> separated "."
        let notes = sepEndBy1 (many1Chars (noneOf "*")) (pchar '.')
        let releaseNote = pchar '*' >>. spaces >>. version .>> skipManySatisfy (fun c -> c = ' ' || c = '-') >>. notes
        let releaseNotes = many1 releaseNote

        match run releaseNotes data with
        | Success (result, _, _) -> result
        | Failure (error, _, _) -> failwith error