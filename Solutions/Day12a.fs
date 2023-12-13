module Solutions.Day12a

open System
open System.Text.RegularExpressions
open System.Collections.Generic

let doIt (input: string) : int =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    let input =
        lines
        |> Seq.map (fun line -> Regex("^([.#?]+)\s+([0-9]+(?:,[0-9]+)*)$").Match(line))
        |> Seq.filter (fun m -> m.Success)
        |> Seq.map (fun m ->
            let pattern =
                m.Groups[1].Value
                |> Seq.map (fun c ->
                    match c with
                    | '#' -> Some true
                    | '.' -> Some false
                    | '?' -> None
                    | _ -> failwith (sprintf "unhandled char: %c" c))
                |> Seq.toList

            let numbers = m.Groups[2].Value.Split(',') |> Seq.map int |> Seq.toList
            pattern, numbers)

    let rec findAllPermutations (input: bool option list) : bool list seq =
        match input with
        | Some next :: remaining -> findAllPermutations remaining |> Seq.map (fun remaining -> next :: remaining)
        | None :: remaining ->
            findAllPermutations remaining
            |> Seq.collect (fun remaining -> [ true :: remaining; false :: remaining ])
        | [] -> seq [ [] ]

    let rec countRanges (input: bool list) : int list =
        match input with
        | next :: remaining ->
            // there is some input remaining
            // find the sequence at the beginning that is all either true or false
            let prefix = next :: (remaining |> List.takeWhile (fun x -> x = next))
            // and the rest of the input that must start with a different value or be empty
            let remaining = remaining.GetSlice(Some(prefix.Length - 1), None)
            // if our prefix is a number then we can append that, otherwise just recurse with no new number yet
            if next then
                prefix.Length :: (countRanges remaining)
            else
                countRanges remaining
        | [] ->
            // no more input, can't fit any numbers in here
            []

    input
    |> Seq.map (fun (pattern, numbers) ->
        findAllPermutations pattern
        |> Seq.map countRanges
        |> Seq.filter (fun possible -> possible = numbers)
        |> Seq.length)
    |> Seq.sum
