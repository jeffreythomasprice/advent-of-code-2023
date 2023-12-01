module Solutions.Day01a

open System

let isDigit c = c >= '0' && c <= '9'

let doIt (input: string) : int =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    let numbers =
        lines
        |> Seq.map (fun line ->
            let lineSeq = seq { 0 .. line.Length - 1 } |> Seq.map (fun i -> line[i])
            let lineSeqRev = seq { line.Length - 1 .. -1 .. 0 } |> Seq.map (fun i -> line[i])
            let first = Seq.tryFind isDigit lineSeq
            let last = Seq.tryFind isDigit lineSeqRev

            match (first, last) with
            | (Some first, Some last) -> sprintf "%c%c" first last
            | _ -> raise (Exception(sprintf "failed to parse line: %s" line)))
        |> Seq.map int

    Seq.sum numbers
