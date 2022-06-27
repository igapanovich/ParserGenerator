namespace HnkParserGenerator
open HnkParserGenerator.LALR

type internal Action<'s when 's : comparison> =
    | Shift of State<'s>
    | Reduce of Production<'s>
    | Accept of Production<'s>

type internal ParsingTable<'s when 's : comparison> =
    private {
        grammar : Grammar<'s>
        goto : Map<State<'s>, Map<'s, State<'s>>>
        action : Map<State<'s>, Map<'s, Action<'s>>>
    }
    override this.ToString() =
        let stateNumbers =
            this.action
            |> Map.toSeq
            |> Seq.map fst
            |> Seq.mapi (fun i state -> (state, i))
            |> Map.ofSeq

        let productionNumbers =
            this.action
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.collect Map.toSeq
            |> Seq.map snd
            |> Seq.choose (function Reduce prod -> Some prod | _ -> None)
            |> Seq.distinct
            |> Seq.mapi (fun i p -> (p, i))
            |> Map.ofSeq

        let sb = System.Text.StringBuilder()

        let appendLine str = sb.AppendLine(str) |> ignore

        appendLine "STATES"
        for state, num in stateNumbers |> Map.toSeq do
            appendLine $"   {num} {state}"

        appendLine "PRODUCTIONS"
        for prod, num in productionNumbers |> Map.toSeq do
            appendLine $"   {num} {prod}"

        appendLine "ACTION (State | Lookahead | Action)"
        for state, stateAction in this.action |> Map.toSeq do
            for symbol, action in stateAction |> Map.toSeq do
                let action =
                    match action with
                    | Accept _ -> "acc"
                    | Shift state -> $"s{stateNumbers[state]}"
                    | Reduce prod -> $"r{productionNumbers[prod]}"
                appendLine $"   {stateNumbers[state]} {symbol} {action}"

        appendLine "GOTO (Source state | Symbol | Destination state)"
        for src, stateGoto in this.goto |> Map.toSeq do
            for symbol, dest in stateGoto |> Map.toSeq do
                appendLine $"   {stateNumbers[src]} {symbol} {stateNumbers[dest]}"

        sb.ToString()

module internal ParsingTable =
    let create (automaton : Automaton<_>) =
        let action =
            seq {
                for state in automaton.states do
                    let finalConfigurations = state.configurations |> Set.filter Configuration.isFinal

                    let stateActions =
                        seq {
                            for cfg in finalConfigurations do
                                for lookahead in cfg.lookahead do
                                    let action =
                                        if cfg.production.from = automaton.grammar.startingSymbol
                                        then Accept cfg.production
                                        else Reduce cfg.production

                                    yield (lookahead, action)

                            for transition in automaton.transitions do
                                if
                                    transition.sourceState = state &&
                                    automaton.grammar.terminals.Contains(transition.symbol)
                                then
                                    yield (transition.symbol, Shift transition.destinationState)
                        } |> Map.ofSeq

                    yield (state, stateActions)
            } |> Map.ofSeq

        let goto =
            seq {
                for state in automaton.states do
                    let nonFinalConfigurations = state.configurations |> Set.filter (not << Configuration.isFinal)

                    let stateGoto =
                        seq {
                            for cfg in nonFinalConfigurations do
                                match Configuration.getSymbolAfterCursor cfg with
                                | None -> ()
                                | Some symbol ->
                                    if automaton.grammar.nonTerminals.Contains(symbol) then
                                        let transitionOnSymbol =
                                            automaton.transitions
                                            |> Seq.filter (fun tr -> tr.sourceState = state && tr.symbol = symbol)
                                            |> Seq.exactlyOne

                                        yield (symbol, transitionOnSymbol.destinationState)
                        } |> Map.ofSeq

                    if stateGoto |> Map.isEmpty |> not then
                        yield (state, stateGoto)
            } |> Map.ofSeq

        { grammar = automaton.grammar
          action = action
          goto = goto }
