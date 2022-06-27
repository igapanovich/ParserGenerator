namespace HnkParserGenerator.CodeGen

open System
open System.IO
open System.Text.RegularExpressions
open HnkParserGenerator

type internal ParserDefinition = {
    symbolTypes : DefaultingMap<string, string option>
    productions : Production<string> Set
}

module internal ParserDefinition =
    type private LineData =
        | Productions of Production<string> list
        | Typing of symbol : string * symbolType : string
        | Blank

    let private typingRegex = Regex("^\s*(?<symbol>[A-Za-z]+)\s*:\s*(?<type>.+?)\s*$")
    let private (|AsTyping|_|) (line : string) =
        let m = typingRegex.Match(line)
        if m.Success then
            let symbol = m.Groups["symbol"].Value
            let type_ = m.Groups["type"].Value
            Some (Typing (symbol, type_))
        else
            None

    let private productionRegex = Regex("^\s*(?<symbol>[A-Za-z]+)\s*->\s*(?<into>.+?)\s*$")
    let private (|AsProduction|_|) (epsilon : string) (line : string) =
        let m = productionRegex.Match(line)
        if m.Success then
            let symbol = m.Groups["symbol"].Value
            let into = m.Groups["into"].Value
            let intoAlternatives =
                into.Split('|')
                |> Seq.map (fun i -> i.Trim().Split(' ') |> List.ofArray)
                |> List.ofSeq

            let validSymbol text =
                text |> Seq.forall (fun c -> c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z')

            let valid =
                intoAlternatives
                |> List.forall(fun i ->
                    i = [epsilon] ||
                    not i.IsEmpty && i |> List.forall validSymbol)

            if not valid then None else

            intoAlternatives
            |> List.map (fun intoSymbols ->
                let intoSymbols =
                    if intoSymbols = [epsilon]
                    then []
                    else intoSymbols
                { from = symbol
                  into = intoSymbols})
            |> Productions
            |> Some
        else
            None

    let private (|AsBlankLine|_|) (line : string) =
        if line |> Seq.forall Char.IsWhiteSpace
        then Some ()
        else None

    let private parseLine (epsilon : string) (line : string) =
        match line with
        | AsProduction epsilon p -> Ok p
        | AsTyping t -> Ok t
        | AsBlankLine -> Ok Blank
        | _ -> Error "Malformed line. Expected a production (A -> B c | D) or a typing (A : T)."

    let parse (epsilon : string) (stream : Stream) =
        let reader = new StreamReader(stream)

        let parseResult =
            seq {
                while not reader.EndOfStream do
                    reader.ReadLine()
            }
            |> Seq.map (parseLine epsilon)
            |> Seq.mapi (fun i res -> res |> Result.mapError (fun er -> $"Error at line %i{i + 1}: %s{er}"))
            |> Result.fromSeqOfResults

        match parseResult with
        | Error lineErrors ->
            lineErrors
            |> String.concat Environment.NewLine
            |> Error
        | Ok lines ->
            let symbolTypes =
                lines
                |> Seq.choose (function
                    | Typing (s, t) -> Some (s, Some t)
                    | _ -> None)
                |> DefaultingMap.ofSeq None

            let productions =
                lines
                |> Seq.choose (function
                    | Productions p -> Some p
                    | _ -> None)
                |> Seq.collect id
                |> Set.ofSeq

            if productions.IsEmpty then Error "No productions defined" else

            Ok { symbolTypes = symbolTypes; productions = productions }