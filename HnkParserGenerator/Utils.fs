namespace HnkParserGenerator
open System

module internal List =
    let insert x pos list =
        let rec insert' x pos head tail =
            if pos < 0 then
                raise (IndexOutOfRangeException())
            elif pos = 0 then
                if head |> List.isEmpty
                then x :: tail
                else head @ (x :: tail)
            else
                insert' x (pos - 1) (head @ [ tail.Head ]) tail.Tail

        insert' x pos [] list

module internal Result =
    let fromSeqOfResults (rs : #seq<_>) =
        let fold state el =
            match state, el with
            | Ok state, Ok el -> Ok (el :: state)
            | Error state, Error el -> Error (el :: state)
            | Ok _, Error el -> Error [el]
            | Error state, Ok _ -> Error state

        rs |> Seq.fold fold (Ok [])

[<AutoOpen>]
module internal Utils =
    type OptionBuilder() =
        member __.Bind(opt, f) = Option.bind f opt
        member __.Return(x) = Some x

    let option = OptionBuilder()