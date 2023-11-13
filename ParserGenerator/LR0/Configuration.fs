namespace ParserGenerator.LR0
open ParserGenerator
open System.Diagnostics

[<DebuggerDisplay("{ToString()}")>]
type internal Configuration<'symbol> =
    {
        production : Production<'symbol>
        cursorOffset : int
    }
    override this.ToString() =
        let str = this.production.ToString()

        let subProd = {
            this.production with
                into = this.production.into |> List.take this.cursorOffset
            }
        let subProdLen = subProd.ToString().Length

        str.Insert(subProdLen, "·")

module internal Configuration =
    let tryAheadSymbol configuration =
        if configuration.production.into.Length > configuration.cursorOffset
        then configuration.production.into[configuration.cursorOffset] |> Some
        else None

    let createStart (production : Production<_>) : Configuration<_> =
        { production = production
          cursorOffset = 0 }

    let rec close
        (productions : Production<_> Set)
        (configuration : Configuration<_>)
        : Configuration<_> list =
        [
            // if configuration is A -> α.Bω where B is non-terminal then
            // get all productions of B at start configuration (B -> .γ)
            if configuration.production.into.Length > configuration.cursorOffset then
                let symbolToExpand = configuration.production.into.[configuration.cursorOffset]
                let symbolToExpandProductions = productions |> Set.filter (fun prod -> prod.from = symbolToExpand)
                let unusedProductions = productions - symbolToExpandProductions
                for prod in symbolToExpandProductions do
                    let startConfiguration = createStart prod
                    yield startConfiguration
                    yield! close unusedProductions startConfiguration
        ]