namespace ParserGenerator

type Grammar<'symbol when 'symbol : comparison> =
    private {
        productions : Production<'symbol> Set
        symbols : 'symbol Set
        terminals : 'symbol Set
        nonTerminals : 'symbol Set
        startingSymbol : 'symbol
    }

module Grammar =
    let fromProductions productions =
        let nonTerminals = productions |> Set.map (fun p -> p.from)
        let producedSymbols = productions |> Seq.collect (fun p -> p.into) |> Set.ofSeq
        let terminals = producedSymbols - nonTerminals
        let startingSymbols = nonTerminals - producedSymbols

        let startingSymbol =
            match startingSymbols |> List.ofSeq with
            | [] -> failwith "Invalid grammar: no starting symbols"
            | [s] -> s
            | _ -> failwith "Ivalid grammar: more than one potential starting symbol"

        {
            productions = productions
            symbols = terminals + nonTerminals
            terminals = terminals
            nonTerminals = nonTerminals
            startingSymbol = startingSymbol
        }