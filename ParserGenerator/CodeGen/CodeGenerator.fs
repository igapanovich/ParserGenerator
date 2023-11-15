module internal ParserGenerator.CodeGen.CodeGenerator

open ParserGenerator.LALR
open ParserGenerator
open ParserGenerator.CodeGen.Code
open System.IO
open ParserGenerator.CodeGen.CodeModel
open ParserGenerator.ParserDefinition

let private blankLine = Line ""

type CodeGenArgs<'s when 's: comparison> =
    { newLine: string
      eofSymbol: 's
      symbolToIdentifier: 's -> string
      parsingTable: ParsingTable<'s>
      parserModuleName: string
      parserDefinition: ParserDefinition<'s> }

type private Context<'s when 's: comparison> =
    { terminalTypes: TypeAliasDeclaration list
      nonTerminalTypes: SumTypeDeclaration list
      getNonTerminalConstructorForProduction : Production<'s> -> SumTypeDeclaration * SumTypeConstructor
      inputType: SumTypeDeclaration
      inputTypeSignature: TypeSignature
      getInputCaseForSymbol : 's -> SumTypeConstructor
      unexpectedItemType: SumTypeDeclaration
      unexpectedItemTypeSignature: TypeSignature
      expectedItemType: SumTypeDeclaration
      expectedItemTypeSignature: TypeSignature
      getExpectedItemCaseForSymbol : 's -> SumTypeConstructor
      errorType: RecordDeclaration
      errorTypeSignature: TypeSignature
      startingSymbol: 's
      eofSymbol : 's
      getSymbolType: 's -> TypeSignature option
      startingState : State<'s>
      getStateNumber : State<'s> -> int
      gotoTable : (State<'s> * 's * State<'s>) list
      actionTable : (State<'s> * ('s * Action<'s>) list) list }

let private createContext (args: CodeGenArgs<'s>) : Context<'s> =
    let allTerminals =
        let terminalsFromGrammar = args.parsingTable.grammar.terminals
        let terminalsFromTypings = args.parserDefinition.typings |> Set.map (fun t -> t.terminal)
        terminalsFromGrammar + terminalsFromTypings

    let terminalTypesWithSymbols =
        allTerminals
        |> Seq.map (fun terminal ->
            let typing =
                args.parserDefinition.typings
                |> Seq.tryFind (fun typing -> typing.terminal = terminal)
            let t =
                match typing with
                | Some typing when typing.type_ <> "unit" ->
                    { name = args.symbolToIdentifier typing.terminal
                      aliasedType = PlainType typing.type_ }
                    |> Some
                | _ -> None
            (terminal, t))
        |> List.ofSeq

    let terminalAliasTypes =
        terminalTypesWithSymbols
        |> List.choose snd

    let getTypeAliasForTerminal symbol =
        terminalTypesWithSymbols
        |> Seq.choose (fun (s, t) ->
            if s = symbol
            then Some t
            else None)
        |> Seq.exactlyOne

    let nonTerminalTypesWithSymbols =
        [ for prod in args.parserDefinition.productions do
              let name = args.symbolToIdentifier prod.from

              let productionIntoToType (into: list<'s>) =
                  let typeString =
                      into
                      |> Seq.filter (fun symbol -> args.parsingTable.grammar.nonTerminals.Contains(symbol) || getTypeAliasForTerminal symbol <> None)
                      |> Seq.map args.symbolToIdentifier
                      |> String.concat " * "

                  if typeString.Length = 0 then
                      None
                  else
                      typeString
                      |> PlainType
                      |> Some

              let ctors =
                  match prod.cases with
                  | Single into ->
                      let ctor =
                          { SumTypeConstructor.name = name
                            type_ = productionIntoToType into }
                      [ into, ctor ]
                  | Many cases ->
                      cases
                      |> Seq.map (fun (caseName, into) ->
                          let ctor =
                              { SumTypeConstructor.name = caseName
                                type_ = productionIntoToType into }
                          (into, ctor))
                      |> List.ofSeq

              let t =
                  { SumTypeDeclaration.name = name
                    constructors = ctors |> List.map snd }

              (prod.from, ctors, t) ]


    let nonTerminalTypes = nonTerminalTypesWithSymbols |> List.map (fun (_, _, t) -> t)

    let getNonTerminalConstructorForProduction (production : Production<'s>) =
        nonTerminalTypesWithSymbols
        |> Seq.collect (fun (from, ctors, t) ->
            ctors
            |> Seq.choose (fun (into, case) ->
                if from = production.from && into = production.into
                then Some (t, case)
                else None))
        |> Seq.exactlyOne

    let inputTypeConstructorsWithSymbols =
        allTerminals
        |> Seq.map (fun terminal ->
            let type_ =
                getTypeAliasForTerminal terminal
                |> Option.map (fun typeAlias -> PlainType typeAlias.name)

            let ctor =
              { SumTypeConstructor.name = args.symbolToIdentifier terminal
                type_ = type_ }
            (terminal, ctor))
        |> List.ofSeq

    let inputType =
        let ctors =
            inputTypeConstructorsWithSymbols
            |> List.map snd

        { name = "InputItem"
          constructors = ctors }

    let inputTypeSignature = PlainType inputType.name

    let getInputCaseForSymbol symbol =
        inputTypeConstructorsWithSymbols
        |> Seq.choose (fun (s, ctor) ->
            if s = symbol
            then Some ctor
            else None)
        |> Seq.exactlyOne

    let getSymbolType symbol =
        if args.parsingTable.grammar.terminals.Contains(symbol) then
            terminalTypesWithSymbols
            |> Seq.choose (fun (s, t) ->
                if s = symbol
                then Some (t |> Option.map (fun t -> t.name))
                else None)
            |> Seq.head
        else
            nonTerminalTypesWithSymbols
            |> Seq.choose (fun (s, _, t) ->
                if s = symbol
                then Some (Some t.name)
                else None)
            |> Seq.head
        |> Option.map PlainType

    let startingSymbol =
        let nonTerminals =
            args.parsingTable.grammar.productions
            |> Seq.map (fun p -> p.from)
            |> Seq.distinct

        let startingSymbols =
            nonTerminals
            |> Seq.filter (fun s ->
                args.parsingTable.grammar.productions
                |> Seq.exists (fun p -> p.from <> s && p.into |> List.contains s)
                |> not)
            |> List.ofSeq

        match startingSymbols with
        | [] -> failwith "None of the symbols qualify to be a starting symbol"
        | [ s ] -> s
        | _ -> failwith $"More than one potential starting symbol: {startingSymbols}"

    let states =
        args.parsingTable.action
        |> Map.toSeq
        |> Seq.map fst
        |> List.ofSeq

    let getStateNumber s = args.parsingTable.stateNumber |> Map.find s

    let startingState =
        states
        |> Seq.find (fun state ->
            state.configurations
            |> Seq.exists (fun cfg ->
                cfg.production.from = args.parsingTable.grammar.startingSymbol &&
                cfg |> Configuration.isStarting))

    let gotoTable =
        args.parsingTable.goto
        |> Map.toSeq
        |> Seq.collect (fun (currentState, gotos) ->
            gotos
            |> Map.toSeq
            |> Seq.map (fun (symbol, nextState) ->
                (currentState, symbol, nextState)))
        |> List.ofSeq

    let actionTable =
        args.parsingTable.action
        |> Map.toSeq
        |> Seq.map (fun (currentState, actions) ->
            let actions =
                actions
                |> Map.toSeq
                |> List.ofSeq
            (currentState, actions))
        |> List.ofSeq

    let unexpectedItemType =
        { SumTypeDeclaration.name = "Unexpected"
          constructors =
              [ { SumTypeConstructor.name = "EndOfStream"; type_ = None }
                { SumTypeConstructor.name = "InputItem"; type_ = Some inputTypeSignature } ]}

    let unexpectedItemTypeSignature = PlainType unexpectedItemType.name

    let expectedItemTypeConstructorsWithSymbols =
        inputTypeConstructorsWithSymbols
        |> List.map (fun (s, ctor) ->
            let ctor = { SumTypeConstructor.name = ctor.name; type_ = None }
            (s, ctor))

    let expectedItemTypeConstructorsWithSymbols =
        (args.eofSymbol, { SumTypeConstructor.name = "EndOfStream"; type_ = None })
        :: expectedItemTypeConstructorsWithSymbols

    let expectedItemType =
        { SumTypeDeclaration.name = "ExpectedItem"
          constructors =
              expectedItemTypeConstructorsWithSymbols
              |> List.map snd }

    let expectedItemTypeSignature = PlainType expectedItemType.name

    let getExpectedItemCaseForSymbol symbol =
        expectedItemTypeConstructorsWithSymbols
        |> Seq.choose (fun (s, ctor) ->
            if s = symbol
            then Some ctor
            else None)
        |> Seq.exactlyOne

    let errorType =
        { RecordDeclaration.name = "ParseError"
          fields =
              [ { RecordField.name = "unexpected"; type_ = unexpectedItemTypeSignature }
                { RecordField.name = "expected"; type_ = GenericInstanceType("list", [expectedItemTypeSignature]) } ]}

    let errorTypeSignature = PlainType errorType.name

    { terminalTypes = terminalAliasTypes
      nonTerminalTypes = nonTerminalTypes
      getNonTerminalConstructorForProduction = getNonTerminalConstructorForProduction
      inputType = inputType
      inputTypeSignature = inputTypeSignature
      getInputCaseForSymbol = getInputCaseForSymbol
      unexpectedItemType = unexpectedItemType
      unexpectedItemTypeSignature = unexpectedItemTypeSignature
      expectedItemType = expectedItemType
      expectedItemTypeSignature = expectedItemTypeSignature
      getExpectedItemCaseForSymbol = getExpectedItemCaseForSymbol
      errorType = errorType
      errorTypeSignature = errorTypeSignature
      getSymbolType = getSymbolType
      startingSymbol = startingSymbol
      startingState = startingState
      eofSymbol = args.eofSymbol
      getStateNumber = getStateNumber
      gotoTable = gotoTable
      actionTable = actionTable }

let rec private typeSignatureToStr (ts: TypeSignature) : string =
    match ts with
    | PlainType t -> t
    | GenericInstanceType (t, targs) ->
        let targs =
            targs
            |> Seq.map typeSignatureToStr
            |> String.concat ", "

        $"{t}<{targs}>"

let private writeTypeAlias (t : TypeAliasDeclaration) : Code =
    Line $"type {t.name} = {typeSignatureToStr t.aliasedType}"

let private writeSumType (t : SumTypeDeclaration) : Code =
    code {
        Line $"type {t.name} ="
        Indented <| code {
            for ctor in t.constructors do
                match ctor.type_ with
                | Some t -> Line $"| {ctor.name} of {typeSignatureToStr t}"
                | None -> Line $"| {ctor.name}"
        }
    }

let private writeRecord (r : RecordDeclaration) : Code =
    code {
        Line $"type {r.name} = {{"
        Indented <| code {
            for field in r.fields do
                Line $"{field.name} : {typeSignatureToStr field.type_}"
        }
        Line "}"
    }

let private writeParseFunction (context : Context<'s>) : Code =
    let inputTypeStr = typeSignatureToStr context.inputTypeSignature

    let resultTypeStr =
        context.startingSymbol
        |> context.getSymbolType
        |> Option.get
        |> typeSignatureToStr

    let errorResultTypeStr =
        context.errorTypeSignature
        |> typeSignatureToStr

    let startingStateNumber = context.startingState |> context.getStateNumber

    let failwithInvalidState = "failwith \"Parser is in an invalid state. This is a bug in the parser generator.\""

    let inputParamName = "input"
    let inputEnumeratorVarName = "inputEnumerator"
    let lhsStackVarName = "lhsStack"
    let stateStackVarName = "stateStack"
    let resultVarName = "result"
    let acceptedVarName = "accepted"
    let expectedVarName = "expected"
    let lookaheadVarName = "lookahead"
    let lookaheadIsEofVarName = "lookaheadIsEof"
    let keepGoingVarName = "keepGoing"
    let reductionResultVarName = "reductionResult"

    let expectedVarTypeStr = typeSignatureToStr (GenericInstanceType("list", [context.expectedItemTypeSignature]))
    let unexpectedVarTypeStr = typeSignatureToStr context.unexpectedItemTypeSignature

    let writeShift (lookahead : 's) (newState : State<'s>) : Code =
        let inputConstructor = context.getInputCaseForSymbol lookahead
        let ctorArgString =
            match inputConstructor.type_ with
            | Some _ -> " x"
            | None -> ""
        code {
            Line $"| {typeSignatureToStr context.inputTypeSignature}.{inputConstructor.name}{ctorArgString} ->"
            Indented <| code {
                Line "// shift"
                if inputConstructor.type_ <> None then
                    Line $"{lhsStackVarName}.Push(x)"
                Line $"if {inputEnumeratorVarName}.MoveNext() then"
                Indented <| code {
                    Line $"{lookaheadVarName} <- {inputEnumeratorVarName}.Current"
                }
                Line "else"
                Indented <| code {
                    Line $"{lookaheadIsEofVarName} <- true"
                }
                Line $"{stateStackVarName}.Push({context.getStateNumber newState})"
            }
        }

    let writeReduction (production : ParserGenerator.Production<'s>) : Code =
        let args =
            production.into
            |> Seq.choose context.getSymbolType
            |> Seq.mapi (fun i t -> ($"arg{i + 1}", t) )
            |> List.ofSeq

        let argListStr =
            args
            |> Seq.map fst
            |> String.concat ", "

        let nonTerminalType, nonTerminalCtor = context.getNonTerminalConstructorForProduction production
        let ctorStr = $"{nonTerminalType.name}.{nonTerminalCtor.name}"

        code {
            for _ = 1 to production.into.Length do
                Line $"{stateStackVarName}.Pop() |> ignore"
            for argName, argType in args |> List.rev do
                Line $"let {argName} = {lhsStackVarName}.Pop() :?> {typeSignatureToStr argType}"

            match args.Length with
            | 0 -> Line $"let {reductionResultVarName} = {ctorStr}"
            | 1 -> Line $"let {reductionResultVarName} = {ctorStr} {argListStr}"
            | _ -> Line $"let {reductionResultVarName} = {ctorStr} ({argListStr})"
        }

    let writeReduce (lookahead : 's) (production : ParserGenerator.Production<'s>) : Code =
        let goto =
            context.gotoTable
            |> Seq.choose (fun (src, nonTerminal, dst) ->
                if nonTerminal = production.from
                then Some (src, dst)
                else None)

        code {
            if lookahead = context.eofSymbol then
                Line $"| _ when {lookaheadIsEofVarName} ->"
            else
                let inputCase = context.getInputCaseForSymbol lookahead
                Line $"| {typeSignatureToStr context.inputTypeSignature}.{inputCase.name} _ ->"

            Indented <| code {
                Line "// reduce"

                writeReduction production

                Line $"{lhsStackVarName}.Push({reductionResultVarName})"
                Line "let nextState ="
                Indented <| code {
                    Line $"match {stateStackVarName}.Peek() with"
                    for src, dest in goto do
                        Line $"| {context.getStateNumber src} -> {context.getStateNumber dest}"
                    Line $"| _ -> {failwithInvalidState}"
                }
                Line $"{stateStackVarName}.Push(nextState)"
            }
        }

    let writeAccept (production : ParserGenerator.Production<'s>) : Code =
        code {
            Line $"| _ when {lookaheadIsEofVarName} ->"
            Indented <| code {
                Line "// accept"
                writeReduction production
                Line $"{resultVarName} <- {reductionResultVarName}"
                Line $"{acceptedVarName} <- true"
                Line $"{keepGoingVarName} <- false"
            }
        }

    code {
        Line $"let parse ({inputParamName}: #seq<{inputTypeStr}>) : Result<{resultTypeStr}, {errorResultTypeStr}> ="
        Indented <| code {
            Line $"use {inputEnumeratorVarName} = {inputParamName}.GetEnumerator()"
            Line $"let {lhsStackVarName} = System.Collections.Stack(50)"
            Line $"let {stateStackVarName} = System.Collections.Generic.Stack<int>(50)"
            Line $"let mutable {resultVarName} = Unchecked.defaultof<{resultTypeStr}>"
            Line $"let mutable {acceptedVarName} = false"
            Line $"let mutable {expectedVarName} = Unchecked.defaultof<{expectedVarTypeStr}>"
            blankLine
            Line $"{stateStackVarName}.Push({startingStateNumber})"
            blankLine
            Line $"let mutable {lookaheadVarName}, {lookaheadIsEofVarName} ="
            Indented <| code {
                Line $"if {inputEnumeratorVarName}.MoveNext()"
                Line $"then ({inputEnumeratorVarName}.Current, false)"
                Line $"else (Unchecked.defaultof<{inputTypeStr}>, true)"
            }
            blankLine
            Line $"let mutable {keepGoingVarName} = true"
            Line $"while {keepGoingVarName} do"
            Indented <| code {
                Line $"match {stateStackVarName}.Peek() with"
                for state, stateActions in context.actionTable do
                    Line $"| {context.getStateNumber state} ->"
                    Indented <| code {
                        Line $"match {lookaheadVarName} with"

                        let stateActions =
                            stateActions
                            |> List.sortBy (fun (s, _) -> if s = context.eofSymbol then 0 else 1)

                        let hasEofAction =
                            match stateActions with
                            | (symbol, _) :: _ when symbol = context.eofSymbol -> true
                            | _ -> false

                        let expectedSymbolListStr =
                            stateActions
                            |> Seq.map (fun (s, _) ->
                                let ctor = context.getExpectedItemCaseForSymbol s
                                $"{typeSignatureToStr context.expectedItemTypeSignature}.{ctor.name}")
                            |> String.concat "; "
                            |> sprintf "[ %s ]"

                        if not hasEofAction then
                            Line $"| _ when {lookaheadIsEofVarName} ->"
                            Indented <| code {
                                Line "// error"
                                Line $"{expectedVarName} <- {expectedSymbolListStr}"
                                Line $"{keepGoingVarName} <- false"
                            }

                        for lookahead, action in stateActions do
                            match action with
                            | Shift newState -> writeShift lookahead newState
                            | Reduce production -> writeReduce lookahead production
                            | Accept production -> writeAccept production

                        if stateActions.Length < context.inputType.constructors.Length then
                            Line "| _ ->"
                            Indented <| code {
                                Line "// error"
                                Line $"{expectedVarName} <- {expectedSymbolListStr}"
                                Line $"{keepGoingVarName} <- false"
                            }
                    }
                Line $"| _ -> {failwithInvalidState}"
            }
            blankLine
            Line $"if {acceptedVarName}"
            Line $"then Ok {resultVarName}"
            Line "else Error {"
            Indented <| code {
                Line $"unexpected = if {lookaheadIsEofVarName} then {unexpectedVarTypeStr}.EndOfStream else {unexpectedVarTypeStr}.InputItem {lookaheadVarName}"
                Line $"expected = {expectedVarName}"
            }
            Line "}"
        }
    }

let generate (args: CodeGenArgs<'s>) (stream: Stream) : unit =
    let context = createContext args

    let parserCode =
        code {
            Line $"module internal rec {args.parserModuleName}"
            blankLine
            Line "(*"
            Line (args.parsingTable.ToString())
            Line "*)"
            blankLine
            for t in context.terminalTypes do
                writeTypeAlias t
            blankLine
            for t in context.nonTerminalTypes do
                writeSumType t
                blankLine
            writeSumType context.inputType
            blankLine
            writeSumType context.unexpectedItemType
            blankLine
            writeSumType context.expectedItemType
            blankLine
            writeRecord context.errorType
            blankLine
            writeParseFunction context
        }

    let writer = new StreamWriter(stream)

    Code.write args.newLine writer parserCode

    writer.Flush()
