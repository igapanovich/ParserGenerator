namespace ParserGenerator.LALR

open ParserGenerator
open System.Diagnostics

[<DebuggerDisplay("{ToString()}")>]
type internal Configuration<'symbol when 'symbol : comparison> =
    {
        production : Production<'symbol>
        cursorOffset : int
        lookahead : 'symbol Set
    }

    override this.ToString() =
        let str = this.production.ToString()

        let subProd = {
            this.production with
                into = this.production.into |> List.take this.cursorOffset
            }
        let subProdLen = subProd.ToString().Length

        let mainPart = str.Insert(subProdLen, "·")

        let lookahead =
            this.lookahead
            |> Seq.map (fun s -> s.ToString())
            |> String.concat " "

        $"%s{mainPart} [%s{lookahead}]"

module internal Configuration =
    let isStarting cfg = cfg.cursorOffset = 0

    let isFinal cfg = cfg.cursorOffset = cfg.production.into.Length

    let getSymbolAfterCursor cfg =
        if cfg.cursorOffset >= cfg.production.into.Length
        then None
        else Some cfg.production.into[cfg.cursorOffset]