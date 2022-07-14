open System
open System.IO
open HnkParserGenerator
open HnkParserGenerator.ParserDefinition
open HnkParserGenerator.CodeGen
open HnkParserGenerator.CodeGen.CodeGenerator
open HnkParserGenerator.LALR

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
        let grammar =
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
            Grammar.fromProductions grammarProductions

        let parsingTable =
            Automaton.create eof grammar
            |> ParsingTable.create

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