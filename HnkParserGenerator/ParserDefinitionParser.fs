namespace HnkParserGenerator.CodeGen

open System
open System.IO
open System.Text.RegularExpressions
open HnkParserGenerator
open HnkParserGenerator.ParserDefinition

module internal ParserDefinitionParser =
    let private typingRegex = Regex("^(?<symbol>[a-zA-Z]+)\s*:\s*(?<type>.+)$")

    let private productionRegex =
        Regex("^(?<from>[a-zA-Z]+)(\.(?<fromCase>[a-zA-Z]+))?\s*->\s*(?<into>[a-zA-Z ]*)$")

    let private commentRegex = Regex("^//")

    let private (|IsMatch|_|) (regex: Regex) (text: string) =
        let m = regex.Match(text)
        if m.Success then
            Some m
        else
            None

    let private parseLine (line: string) : Result<DefinitionItem<_>, string> option =
        match line with
        | IsMatch productionRegex m ->
            let from = m.Groups["from"].Value

            let into =
                m
                    .Groups[ "into" ]
                    .Value.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                |> List.ofArray

            let fromCaseGroup = m.Groups["fromCase"]

            let cases =
                if fromCaseGroup.Success then
                    (fromCaseGroup.Value, into)
                    |> Set.singleton
                    |> Many
                else
                    Single into

            { from = from; cases = cases }
            |> Production
            |> Ok
            |> Some
        | IsMatch typingRegex m ->
            let symbol = m.Groups["symbol"].Value
            let type_ = m.Groups["type"].Value

            { terminal = symbol; type_ = type_ }
            |> Typing
            |> Ok
            |> Some
        | IsMatch commentRegex _ ->
            None
        | _ ->
            Error line
            |> Some

    let private seqSplit2 (f: 'a -> Choice<'b, 'c>) (seq: #seq<'a>) : 'b list * 'c list =
        let bs = System.Collections.Generic.List()
        let cs = System.Collections.Generic.List()

        for item in seq do
            match f item with
            | Choice1Of2 b -> bs.Add(b)
            | Choice2Of2 c -> cs.Add(c)

        let bs = bs |> List.ofSeq
        let cs = cs |> List.ofSeq

        bs, cs

    let private lineFormatError errorLines =
        let errorLinesStr = errorLines |> String.concat Environment.NewLine

        Error(
            "Expected syntax:\n"
            + "\tSymbolName : Type\n"
            + "\tSingleCaseNonTerminal -> Symbol Symbol Symbol\n"
            + "\tMultipleCaseNonTerminal.CaseName -> Symbol Symbol Symbol\n"
            + $"All symbols must be English letters of any case.{Environment.NewLine}"
            + $"Invalid lines are:{Environment.NewLine}{errorLinesStr}"
        )

    let parse (stream: Stream) : Result<ParserDefinition<_>, string> =
        let reader = new StreamReader(stream)

        let definitionItems =
            seq {
                while not reader.EndOfStream do
                    reader.ReadLine()
            }
            |> Seq.filter (String.IsNullOrWhiteSpace >> not)
            |> Seq.choose parseLine


        let definitionItems, errorLines =
            definitionItems
            |> seqSplit2 (function
                | Ok p -> Choice1Of2 p
                | Error e -> Choice2Of2 e)

        if errorLines.Length > 0 then
            lineFormatError errorLines
        else

            let typings, productions =
                definitionItems
                |> seqSplit2 (function
                    | Typing t -> Choice1Of2 t
                    | Production p -> Choice2Of2 p)

            let uniqueTypingCount =
                typings
                |> Seq.map (fun t -> t.terminal)
                |> Seq.distinct
                |> Seq.length

            if uniqueTypingCount < typings.Length then
                Error "Symbols must have only one typing"
            else

                let typings = typings |> Set.ofList

                let productions =
                    seq {
                        for from, cases in productions |> Seq.groupBy (fun p -> p.from) do

                            cases
                            |> Seq.map Ok
                            |> Seq.reduce (fun p1 p2 ->
                                match p1, p2 with
                                | Error e, _
                                | _, Error e -> Error e
                                | Ok { cases = Single _ }, _
                                | _, Ok { cases = Single _ } ->
                                    Error
                                        $"Symbol {from} has an unnamed case and some other case. Unnamed case must be the only case of a symbol."
                                | Ok { cases = Many cases1 }, Ok { cases = Many cases2 } ->
                                    let cases = cases1 + cases2

                                    if cases.Count < cases1.Count + cases2.Count then
                                        Error $"Symbol {from} has repeating case names"
                                    else
                                        Ok { from = from; cases = Many cases })
                    }

                let productions, errors =
                    productions
                    |> seqSplit2 (function
                        | Ok p -> Choice1Of2 p
                        | Error e -> Choice2Of2 e)

                if errors.Length > 0 then
                    errors
                    |> String.concat Environment.NewLine
                    |> Error
                else
                    let productions = productions |> Set.ofList

                    Ok
                        { typings = typings
                          productions = productions }
