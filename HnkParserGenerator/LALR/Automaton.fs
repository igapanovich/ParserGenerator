namespace HnkParserGenerator.LALR

open System.Collections.Generic
open HnkParserGenerator

type internal Automaton<'symbol when 'symbol : comparison> =
    private {
        _grammar : Grammar<'symbol>
        _transitions : StateTransition<'symbol> Set
        _states : State<'symbol> Set
    }
    member this.grammar = this._grammar
    member this.transitions = this._transitions
    member this.states = this._states

    override this.ToString() =
        let stateNumbers =
            this.states
            |> Seq.mapi (fun i state -> (state, i))
            |> Map.ofSeq

        let sb = System.Text.StringBuilder()

        let appendLine str = sb.AppendLine(str) |> ignore

        appendLine "States"
        for state in this.states do
            appendLine $"   {stateNumbers[state]} {state}"

        appendLine "Transitions"
        for transition in this.transitions do
            appendLine (
                $"   {stateNumbers[transition.sourceState]}" +
                $" --{transition.symbol}--> " +
                $"{stateNumbers[transition.destinationState]}")

        sb.ToString()

module internal Automaton =
    type private AugmentedSymbol<'s when 's : comparison> =
        | PlainSymbol of 's
        | TransitionalSymbol of 's * LR0.State<'s> * LR0.State<'s>

    type private LookaheadKey<'s when 's : comparison> = LR0.State<'s> * LR0.Configuration<'s>

    let private createAugmentedGrammar
            (lr0 : LR0.Automaton<'s>)
            (grammar : Grammar<'s>)
            : Grammar<AugmentedSymbol<'s>> =

        let tryTransition state symbol : LR0.State<'s> option =
            let transitionsFromStateOnSymbol =
                lr0.transitions
                |> Seq.filter (fun tr -> tr.sourceState = state && tr.symbol = symbol)
                |> List.ofSeq

            match transitionsFromStateOnSymbol with
            | [] -> None
            | [tr] -> Some tr.destinationState
            | _ -> failwith "LR0 contains reduce/reduce conflict"

        let rec traceAndAugmentSymbols state inputSymbols reverseResult : AugmentedSymbol<'s> list =
            match inputSymbols with
            | [] -> reverseResult |> List.rev
            | symbol :: inputSymbolsRest ->
                let state' = tryTransition state symbol |> Option.get
                let symbol' =
                    if grammar.nonTerminals |> Set.contains symbol
                    then TransitionalSymbol (symbol, state, state')
                    else PlainSymbol symbol

                let reverseResult = symbol' :: reverseResult

                traceAndAugmentSymbols state' inputSymbolsRest reverseResult

        let augmentedProductions : Set<Production<AugmentedSymbol<'s>>> =
            seq {
                for state in lr0.states do
                    let startConfigs = state.configurations |> Set.filter (fun cfg -> cfg.cursorOffset = 0)
                    for config in startConfigs do
                        // config: A -> . w
                        let A = config.production.from
                        let w = config.production.into

                        let transitionStateOnA = tryTransition state A

                        let A' =
                            match transitionStateOnA with
                            | None -> PlainSymbol A
                            | Some trState -> TransitionalSymbol (A, state, trState)

                        let w' = traceAndAugmentSymbols state w []

                        yield {
                            from = A'
                            into = w'
                        }
            } |> Set.ofSeq

        Grammar.fromProductions augmentedProductions

    type private FirstEntry<'s> =
        | Symbol of 's
        | Epsilon

    // Define FIRST*(ω) as follows:
    // ● FIRST*(ε) = { ε }
    // ● FIRST*(tω) = { t }
    // ● If ε ∉ FIRST(A):
    //  – FIRST*(Aω) = FIRST(A)
    // ● If ε ∈ FIRST(A):
    //  – FIRST*(Aω) = (FIRST(A) - { ε }) ∪ FIRST*(ω)
    let rec private firstOfString grammar first str =
        match str with
        | [] -> Epsilon |> Set.singleton
        | t :: _ when grammar.terminals.Contains(t) -> Symbol t |> Set.singleton
        | A :: w ->
            let firstA = first A |> Set.ofSeq
            if firstA.Contains(Epsilon) then
                (firstA |> Set.filter ((<>) Epsilon))
                +
                firstOfString grammar first w
            else
                firstA

    let private createFirstSets (grammar : Grammar<_>) =
        let firstSets =
            grammar.nonTerminals
            |> Seq.map (fun s -> (s, HashSet()))
            |> Map.ofSeq

        // Initially, for all nonterminals A, set FIRST(A) = { t | A → tω for some ω }
        for production in grammar.productions do
            match production.into with
            | t :: _ when grammar.terminals.Contains(t) ->
                firstSets[production.from].Add(Symbol t) |> ignore
            | _ -> ()

        let epsilonProductions = grammar.productions |> Set.filter (fun p -> p.into = [])

        // For all nonterminals A where A → ε is a production, add ε to FIRST(A)
        for production in epsilonProductions do
            if production.into = [] then
                firstSets[production.from].Add(Epsilon) |> ignore


        // Repeat the following until no changes occur:
        // ● For each production A → α, set FIRST(A) = FIRST(A) ∪ FIRST*(α)
        let firstOfString = firstOfString grammar (fun s -> firstSets[s])

        let nonEpsilonProductions = grammar.productions - epsilonProductions

        let mutable keepGoing = true
        while keepGoing do
            keepGoing <- false
            for production in nonEpsilonProductions do
                let added =
                    firstOfString production.into
                    |> Seq.map(firstSets[production.from].Add)
                    |> Seq.exists id

                keepGoing <- keepGoing || added

        let firstSets = firstSets |> Map.map (fun _ v -> Set v)

        firstSets

    let private createFollowSets
            (eof : 's)
            (grammar : Grammar<AugmentedSymbol<'s>>)
            : Map<AugmentedSymbol<'s>, 's Set> =

        let followSets =
            grammar.nonTerminals
            |> Seq.map (fun s -> (s, HashSet()))
            |> Map.ofSeq

        // Initially, for each nonterminal A, set FOLLOW(A) = { t | B → αAtω is a production }
        for production in grammar.productions do
            production.into
            |> Seq.pairwise
            |> Seq.filter (fun (A, t) -> grammar.nonTerminals.Contains(A) && grammar.terminals.Contains(t))
            |> Seq.iter (fun (A, t) ->
                match t with
                | PlainSymbol t -> followSets[A].Add(t) |> ignore
                | _ -> failwith "A terminal should always be a PlainSymbol")

        // Add $ to FOLLOW(S), where S is the start symbol
        followSets[grammar.startingSymbol].Add(eof) |> ignore

        // Repeat the following until no changes occur:
        // ● If B → αAω is a production, set FOLLOW(A) = FOLLOW(A) ∪ FIRST*(ω) - { ε }.
        // ● If B → αAω is a production and ε ∈ FIRST*(ω), set FOLLOW(A) = FOLLOW(A) ∪ FOLLOW(B).
        let firstSets = createFirstSets grammar

        let firstOfString = firstOfString grammar (fun s -> firstSets[s])

        let mutable keepGoing = true
        while keepGoing do
            keepGoing <- false
            for production in grammar.productions do
                let rec addFollow intoTail =
                    match intoTail with
                    | [] -> ()
                    | t :: w when grammar.terminals.Contains(t) -> addFollow w
                    | A :: w ->
                        let followA = followSets[A]
                        let firstW = firstOfString w
                        let added =
                            firstW
                            |> Seq.choose (function
                                | Epsilon -> None
                                | Symbol (PlainSymbol t) -> Some t
                                | _ -> failwith "First set can not contain non-terminals")
                            |> Seq.map followA.Add
                            |> Seq.exists id

                        keepGoing <- keepGoing || added

                        if firstW |> Set.contains Epsilon then
                            let followB = followSets[production.from]

                            let added =
                                followB
                                |> Seq.map followA.Add
                                |> Seq.exists id

                            keepGoing <- keepGoing || added

                        addFollow w

                addFollow production.into

        let followSets = followSets |> Map.map (fun _ v -> Set v)

        followSets

    let private createLookahead
            (lr0 : LR0.Automaton<'s>)
            (followSets : Map<AugmentedSymbol<'s>, Set<'s>>)
            : Map<LookaheadKey<'s>, Set<'s>> =

        let rec trace startingState inputSymbols : LR0.State<'s> =
            match inputSymbols with
            | [] -> startingState
            | symbol :: inputSymbolsRest ->
                let transitionFromStateOnSymbol =
                    lr0.transitions
                    |> Seq.find (fun tr -> tr.symbol = symbol && tr.sourceState = startingState)

                let state' = transitionFromStateOnSymbol.destinationState

                trace state' inputSymbolsRest

        let mutable lookaheadMap = Map.empty
        for state in lr0.states do
            let startConfigs = state.configurations |> Set.filter (fun c -> c.cursorOffset = 0)
            for config in startConfigs do
                // config: A -> . w
                let A = config.production.from
                let transitionStateOnA =
                    lr0.transitions
                    |> Seq.tryFind (fun tr -> tr.symbol = A && tr.sourceState = state)
                    |> Option.map (fun tr -> tr.destinationState)
                let lookahead =
                    match transitionStateOnA with
                    | Some trState -> followSets[TransitionalSymbol(A, state, trState)]
                    | None -> followSets[PlainSymbol A]

                let endState = trace state config.production.into

                let endConfig = { config with cursorOffset = config.production.into.Length }

                let key = (endState, endConfig)

                lookaheadMap <-
                    match lookaheadMap |> Map.tryFind key with
                    | Some existingLookahead -> lookaheadMap |> Map.add key (existingLookahead + lookahead)
                    | None -> lookaheadMap |> Map.add key lookahead

        lookaheadMap

    let private createLalrTransitions
            (lr0 : LR0.Automaton<'s>)
            (lookaheads : Map<LookaheadKey<'s>, Set<'s>>)
            : Set<StateTransition<'s>> =

        let toLalrState (lr0State : LR0.State<'s>) : State<'s> =
            let configurations =
                lr0State.configurations
                |> Set.map (fun cfg ->
                    let lookahead =
                        match lookaheads.TryGetValue((lr0State, cfg)) with
                        | true, la -> la
                        | _ -> Set.empty
                    {
                        production = cfg.production
                        cursorOffset = cfg.cursorOffset
                        lookahead = lookahead
                    })

            { configurations = configurations }

        let lalrTransitions =
            lr0.transitions
            |> Set.map (fun tr ->
                { sourceState = toLalrState tr.sourceState
                  symbol = tr.symbol
                  destinationState = toLalrState tr.destinationState })

        lalrTransitions

    let create (eof : 's) (grammar : Grammar<'s>) : Automaton<'s> =
        let lr0 = LR0.Automaton.create grammar

        let augmentedGrammar = createAugmentedGrammar lr0 grammar

        let followSets = createFollowSets eof augmentedGrammar

        let lookaheads = createLookahead lr0 followSets

        let lalrTransitions = createLalrTransitions lr0 lookaheads

        let states =
            seq {
                for tr in lalrTransitions do
                    yield tr.sourceState
                    yield tr.destinationState
            } |> Set.ofSeq

        { _transitions = lalrTransitions
          _states = states
          _grammar = grammar }