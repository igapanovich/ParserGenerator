namespace ParserGenerator

type internal DefaultingMap<'k , 'v when 'k : comparison> =
    private {
        map : Map<'k, 'v>
        defaultVal : 'v
    }

module internal DefaultingMap =
    let empty defVal = { map = Map.empty; defaultVal = defVal }

    let add k v dmap = { dmap with map = dmap.map |> Map.add k v }

    let find k dmap =
        match dmap.map |> Map.tryFind k with
        | Some v -> v
        | None -> dmap.defaultVal

    let toSeq dmap = dmap.map |> Map.toSeq

    let ofSeq defVal kvs =
        let map = kvs |> Map.ofSeq
        { map = map; defaultVal = defVal }
