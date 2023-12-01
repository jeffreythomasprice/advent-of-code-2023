module Solutions.Day01b

open System
open System.Collections.Generic

let tryParseDigitAt (line: string) (i: int) : char option =
    if line[i] >= '0' && line[i] <= '9' then
        Some line[i]
    else
        let numbers =
            dict
                [ ('0', "zero")
                  ('1', "one")
                  ('2', "two")
                  ('3', "three")
                  ('4', "four")
                  ('5', "five")
                  ('6', "six")
                  ('7', "seven")
                  ('8', "eight")
                  ('9', "nine") ]

        seq {
            for KeyValue(digit, word) in numbers do
                if line.Substring(i).StartsWith(word) then
                    yield Some digit
        }
        |> Seq.filter (fun x -> x.IsSome)
        |> Seq.tryHead
        |> Option.flatten

let doIt (input: string) : int =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    let numbers =
        lines
        |> Seq.map (fun line ->
            let lineSeq = seq { 0 .. line.Length - 1 }
            let lineSeqRev = seq { line.Length - 1 .. -1 .. 0 }

            let parseDigitFromSeq seq =
                seq
                |> Seq.map (fun i -> tryParseDigitAt line i)
                |> Seq.toArray
                |> Seq.filter (fun x -> x.IsSome)
                |> Seq.tryHead
                |> Option.flatten

            let first = parseDigitFromSeq lineSeq
            let last = parseDigitFromSeq lineSeqRev

            match (first, last) with
            | (Some first, Some last) -> sprintf "%c%c" first last
            | _ -> raise (Exception(sprintf "failed to parse line: %s" line)))
        |> Seq.map int

    Seq.sum numbers
