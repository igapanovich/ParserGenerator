open System
open System.IO
open ParserGenerator
open ParserGenerator.ParserDefinition
open ParserGenerator.CodeGen
open ParserGenerator.CodeGen.CodeGenerator
open ParserGenerator.LALR

let eof = "$"

[<EntryPoint>]
let main argv =
    if argv.Length < 3 then printf "arguments required: <path to definition file> <path to output file> <module full name>"; 1
    else

    let definitionFilePath = argv[0]
    let outputFilePath = argv[1]
    let moduleFullName = argv[2]

    use definitionFile = File.OpenRead definitionFilePath

    let parserDefinition = ParserDefinitionParser.parse definitionFile

    match parserDefinition with
    | Error error ->
        Console.WriteLine error
        1
    | Ok parserDefinition ->
        let grammarProductions =
            parserDefinition.productions
            |> Seq.collect (fun p ->
                match p.cases with
                | Single into ->
                    { from = p.from; into = into }
                    |> Seq.singleton
                | Many cases ->
                    cases
                    |> Seq.map (fun (_, into) ->
                        { from = p.from; into = into })
                )
            |> Set.ofSeq

        let grammar = Grammar.fromProductions grammarProductions

        let automaton = Automaton.create eof grammar

        let parsingTable = ParsingTable.create automaton

        let args =
            { newLine = Environment.NewLine
              eofSymbol = eof
              symbolToIdentifier = id
              parsingTable = parsingTable
              parserModuleName = moduleFullName
              parserDefinition = parserDefinition }

        use outputFile = File.Create outputFilePath

        CodeGenerator.generate args outputFile
        0