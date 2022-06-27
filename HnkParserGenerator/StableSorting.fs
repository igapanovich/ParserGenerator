module internal HnkParserGenerator.StableSorting

open HnkParserGenerator.LALR
open System

let keyOfSymbol (symbol : 'a when 'a :> IComparable) =
    symbol :> IComparable

let keyOfSymbolString (symbols : _ list) =
    symbols
    |> Seq.map keyOfSymbol
    |> List.ofSeq
    :> IComparable

let keyOfProduction (prod : Production<_>) =
    [
        keyOfSymbol prod.from
        keyOfSymbolString prod.into
    ]
    :> IComparable

let keyOfConfiguration (cfg : Configuration<_>) =
    let lookaheadKey =
        cfg.lookahead
        |> Seq.map keyOfSymbol
        |> Seq.sort
        |> List.ofSeq

    [
        keyOfProduction cfg.production
        cfg.cursorOffset :> IComparable
        lookaheadKey :> IComparable
    ]
    :> IComparable

let keyOfState (state : State<_>) =
    state.configurations
    |> Seq.map keyOfConfiguration
    |> Seq.sort
    |> List.ofSeq
    :> IComparable