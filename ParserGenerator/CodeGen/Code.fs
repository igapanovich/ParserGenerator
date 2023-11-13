namespace ParserGenerator.CodeGen

open System.IO

type internal Code =
    | Zero
    | Line of string
    | Indented of Code
    | Block of Code list

module internal Code =
    type CodeBuilder() =
        member _.Zero () = Zero
        member _.Combine (code1 : Code, code2 : Code) =
            match code1, code2 with
            | Zero, c -> c
            | c, Zero -> c
            | Block lines1, Block lines2 -> Block (lines1 @ lines2)
            | Block lines, c -> Block (lines @ [c])
            | c, Block lines -> Block ([c] @ lines)
            | _ -> Block [code1; code2]
        member _.For (seq : #seq<_>, f) = Block (seq |> Seq.map f |> List.ofSeq)
        member _.Delay f = f()
        member _.Yield (code : Code) = code

    let code = CodeBuilder()

    let write (newLine : string) (writer : TextWriter) code =
        let mkIndentation n = System.String(' ', n * 4)
        let rec write indentation code =
            match code with
            | Zero -> ()
            | Line str ->
                if str <> ""
                then writer.Write($"{mkIndentation indentation}{str}{newLine}")
                else writer.Write(newLine)
            | Indented code -> write (indentation + 1) code
            | Block codeList -> codeList |> List.iter (write indentation)

        write 0 code