module internal HnkParserGenerator.CodeGen.CodeGenerator

open HnkParserGenerator.LALR
open HnkParserGenerator
open HnkParserGenerator.CodeGen.Code
open System.IO

let private comment str = Line $"// %s{str}"

let private blankLine = Line ""

let private header = code {
    comment "---------------------------------------------------------------------"
    comment "This code was generated by a tool."
    comment "Changes to this file may cause incorrect behavior and will be lost if"
    comment "the code is regenerated."
    comment "---------------------------------------------------------------------"
}

let private moduleDecl name = Line $"module internal %s{name}"

let private sumTypeDecl name cases = code {
    Line $"type {name} ="
    Indented <| code {
        for name, type_ in cases do
            match type_ with
            | Some type_ -> Line $"| {name} of ({type_})"
            | None -> Line $"| {name}"
    }
}

let private recordDecl name fields = code {
    let fieldLine (name, type_) = Line $"{name} : {type_}"

    Line $"type %s{name} = {{"
    Indented (Block (fields |> List.map fieldLine))
    Line "}"
}

let private failwithInvalidState = "failwithInvalidState ()"

let private idtTerminal = "Terminal"
let private idtReducer = "Reducer"

let private idReducer = "reducer"
let private idInputEnumerator = "inputEnumerator"
let private idLhsStack = "lhsStack"
let private idStateStack = "stateStack"
let private idResult = "result"
let private idAccepted = "accepted"
let private idLookahead = "lookahead"
let private idLookaheadIsEof = "lookaheadIsEof"
let private idKeepGoing = "keepGoing"
let private idReductionResult = "reduced"

type private Ident =
    | Ident of string
    override this.ToString() = let (Ident str) = this in str

type private Type =
    | Type of string
    override this.ToString() = let (Type str) = this in str

type private Context<'s when 's : comparison> =
    { eof : 's
      resultType : Type
      toIdent : 's -> Ident
      getType : 's -> Type option
      getNum : State<'s> -> int
      startingStateNum : int
      productions : Production<'s> list
      terminalCases : (Ident * Type option) list
      reducerFields : (Ident * Type) list
      gotoTable : (State<'s> * 's * State<'s>) list
      actionTable : (State<'s> * ('s * Action<'s>) list) list }

let private symbolToTerminalCase toIdent s = $"T_{toIdent s}"

let private productionToReducerFieldName toIdent production =
    (toIdent production.from).ToString() +
        "_" +
        (production.into
        |> Seq.map (toIdent >> string)
        |> String.concat "_")
    |> Ident

let private shift ctx lookahead newState =
    let caseName = symbolToTerminalCase ctx.toIdent lookahead
    let isLookaheadTyped = ctx.getType lookahead <> None
    code {
        if isLookaheadTyped
        then Line $"| {caseName} x ->"
        else Line $"| {caseName} ->"
        Indented <| code {
            comment "shift"
            if isLookaheadTyped then
                Line $"{idLhsStack}.Push(x)"
            Line $"if {idInputEnumerator}.MoveNext()"
            Line $"then {idLookahead} <- {idInputEnumerator}.Current"
            Line $"else {idLookaheadIsEof} <- true"
            Line $"{idStateStack}.Push({ctx.getNum newState})"
        }
    }

let private applyReduction ctx production =
    let args =
        production.into
        |> Seq.choose ctx.getType
        |> Seq.mapi (fun i t -> ($"arg{i + 1}", t) )
        |> List.ofSeq

    let argListStr =
        args
        |> Seq.map fst
        |> String.concat ", "

    let reducerField = productionToReducerFieldName ctx.toIdent production

    code {
        for _ = 1 to production.into.Length do
            Line $"{idStateStack}.Pop() |> ignore"
        for argName, argType in args |> List.rev do
            Line $"let {argName} = {idLhsStack}.Pop() :?> {argType}"

        if not args.IsEmpty then
            Line $"let reductionArgs = ({argListStr})"
            Line $"let {idReductionResult} = {idReducer}.{reducerField} reductionArgs"
        else
            Line $"let {idReductionResult} = {idReducer}.{reducerField}"
    }

let private reduce ctx lookahead production =
    let goto = ctx.gotoTable |> List.filter (fun (_, s, _) -> s = production.from)
    let isLookaheadTyped = ctx.getType lookahead <> None

    code {
        if lookahead = ctx.eof then Line $"| _ when {idLookaheadIsEof} ->"
        elif isLookaheadTyped then Line $"| {symbolToTerminalCase ctx.toIdent lookahead} _ ->"
        else Line $"| {symbolToTerminalCase ctx.toIdent lookahead} ->"

        Indented <| code {
            comment "reduce"
            applyReduction ctx production
            Line $"{idLhsStack}.Push({idReductionResult})"
            Line "let nextState ="
            Indented <| code {
                Line $"match {idStateStack}.Peek() with"
                for src, _, dest in goto do
                    Line $"| {ctx.getNum src} -> {ctx.getNum dest}"
                Line $"| _ -> {failwithInvalidState}"
            }
            Line $"{idStateStack}.Push(nextState)"
        }
    }

let private accept ctx production =
    code {
        Line $"| _ when {idLookaheadIsEof} ->"
        Indented <| code {
            comment "accept"
            applyReduction ctx production
            Line $"{idResult} <- {idReductionResult}"
            Line $"{idAccepted} <- true"
            Line $"{idKeepGoing} <- false"
        }
    }

let private parseFunction ctx = code {
    Line $"let parse ({idReducer} : {idtReducer}) (input : {idtTerminal} seq) : Result<{ctx.resultType}, string> ="
    Indented <| code {
        Line $"use {idInputEnumerator} = input.GetEnumerator()"
        Line $"let {idLhsStack} = System.Collections.Stack(50)"
        Line $"let {idStateStack} = System.Collections.Generic.Stack<int>(50)"
        Line $"let mutable {idResult} = Unchecked.defaultof<{ctx.resultType}>"
        Line $"let mutable {idAccepted} = false"
        blankLine
        Line $"{idStateStack}.Push({ctx.startingStateNum})"
        blankLine
        Line $"let mutable ({idLookahead}, {idLookaheadIsEof}) ="
        Indented <| code {
            Line $"if {idInputEnumerator}.MoveNext()"
            Line $"then ({idInputEnumerator}.Current, false)"
            Line $"else (Unchecked.defaultof<{idtTerminal}>, true)"
        }
        blankLine
        Line $"let mutable {idKeepGoing} = true"
        Line $"while {idKeepGoing} do"
        Indented <| code {
            Line $"match {idStateStack}.Peek() with"
            for state, stateActions in ctx.actionTable do
                Line $"| {ctx.getNum state} ->"
                Indented <| code {
                    Line $"match {idLookahead} with"

                    let stateActions = stateActions |> List.sortBy (fun (s, _) -> if s = ctx.eof then 0 else 1)

                    for lookahead, action in stateActions do
                        match action with
                        | Shift newState -> shift ctx lookahead newState
                        | Reduce production -> reduce ctx lookahead production
                        | Accept production -> accept ctx production

                    Line "| _ ->"
                    Indented <| code {
                        comment "error"
                        Line $"{idKeepGoing} <- false"
                    }
                }
            Line $"| _ -> {failwithInvalidState}"
        }
        blankLine
        Line $"if {idAccepted}"
        Line $"then Ok {idResult}"
        Line "else Error \"TODO error reporting\""
    }
}

type CodeGenArgs<'s when 's : comparison> =
    { newLine : string
      eofSymbol : 's
      symbolTypes : DefaultingMap<'s, string option>
      symbolToIdentifier : 's -> string
      parsingTable : ParsingTable<'s>
      parserModuleName : string }

let private createContext args =
    let toIdent = args.symbolToIdentifier >> Ident

    let getType s = args.symbolTypes |> DefaultingMap.find s |> Option.map Type

    let resultType =
        args.parsingTable.grammar.startingSymbol
        |> getType
        |> Option.defaultWith (fun () -> failwith "Starting symbol must have its type specified")

    let states =
        args.parsingTable.action
        |> Map.toSeq
        |> Seq.map fst
        |> List.ofSeq

    let stateNumbers =
        states
        |> Seq.mapi (fun i state -> (state, i))
        |> Map.ofSeq

    let getNum s = stateNumbers |> Map.find s

    let startingStateNum =
        states
        |> Seq.find (fun state ->
            state.configurations
            |> Seq.exists (fun cfg ->
                cfg.production.from = args.parsingTable.grammar.startingSymbol &&
                cfg |> Configuration.isStarting))
        |> getNum

    let productions =
        args.parsingTable.grammar.productions
        |> List.ofSeq

    let terminalCases =
        args.parsingTable.grammar.terminals
        |> Seq.map (fun t ->
            let name = symbolToTerminalCase toIdent t |> Ident
            let type_ = getType t
            (name, type_))
        |> Seq.sort
        |> List.ofSeq

    let reducerFields =
        args.parsingTable.grammar.productions
        |> Seq.map (fun p ->
            let name = productionToReducerFieldName toIdent p

            let type_ =
                let resultType =
                    getType p.from
                    |> Option.defaultWith (fun () -> failwith "non-terminals must have their type specified")

                let argType =
                    p.into
                    |> Seq.choose (fun s -> getType s |> Option.map (fun t -> $"({t})"))
                    |> String.concat " * "

                if argType <> ""
                then Type $"{argType} -> {resultType}"
                else Type $"{resultType}"

            (name, type_))
        |> Seq.sortBy fst
        |> List.ofSeq

    let gotoTable =
        args.parsingTable.goto
        |> Map.toSeq
        |> Seq.collect (fun (src, stateGoto) ->
            stateGoto
            |> Map.toSeq
            |> Seq.map (fun (symbol, dest) ->
                (src, symbol, dest)))
        |> List.ofSeq

    let actionTable =
        args.parsingTable.action
        |> Map.toSeq
        |> Seq.map (fun (state, stateActions) ->
            let actions =
                stateActions
                |> Map.toSeq
                |> List.ofSeq
            (state, actions))
        |> List.ofSeq

    { eof = args.eofSymbol
      resultType = resultType
      toIdent = toIdent
      getType = getType
      getNum = getNum
      startingStateNum = startingStateNum
      productions = productions
      terminalCases = terminalCases
      reducerFields = reducerFields
      gotoTable = gotoTable
      actionTable = actionTable }

let generate (args : CodeGenArgs<'s>) (stream : Stream) : unit =
    let writer = new StreamWriter(stream)

    let ctx = createContext args

    let parserCode =
        code {
            header
            moduleDecl args.parserModuleName
            blankLine
            sumTypeDecl idtTerminal ctx.terminalCases
            blankLine
            recordDecl idtReducer ctx.reducerFields
            blankLine
            Line "let private failwithInvalidState () = failwith \"Parser is in an invalid state. This is a bug in the parser generator.\""
            blankLine
            parseFunction ctx
        }

    parserCode |> write args.newLine writer

    writer.Flush()