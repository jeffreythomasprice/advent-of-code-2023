module Solutions.Day12b

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

    let rec checkInput (input: bool option list) (numbers: int list) : int =
        match numbers with
        // at least one number remaining
        | nextNumber :: remainingNumbers ->

            let waysToPutNumberAtBeginning =
                if
                    // true if we have enough space for the number and a gap after it
                    (input.Length >= (nextNumber + 1))
                    // true if the number could fit at the start of the input
                    && (input |> Seq.take nextNumber |> Seq.forall (fun x -> x <> Some false))
                    // true if the one after that is the gap
                    && input[nextNumber] <> Some true
                then
                    // we could put the number at this location in the list, skip the number and the next gap, and recurse
                    let remainingInput = input |> List.skip (nextNumber + 1)
                    checkInput remainingInput remainingNumbers
                else if
                    // true if we have enough space just for the number
                    input.Length = nextNumber
                    // true if the number can fit there
                    && (input |> Seq.forall (fun x -> x <> Some false))
                then
                    // recurse anyway, even though we should be out of input, just to check whether there are more numbers
                    checkInput [] remainingNumbers
                else
                    // the number can't fit here
                    0

            let waysToSkipAGapAtTheBeginning =
                match input with
                // first is definitely not false
                | Some true :: remainingInput -> 0
                // first could be false, skip it and keep trying
                | _ :: remainingInput -> checkInput remainingInput numbers
                // no input remaining
                | [] -> 0

            waysToPutNumberAtBeginning + waysToSkipAGapAtTheBeginning

        // no numbers remaining
        | [] ->
            // if we could have nothing in the input then we could do it this way
            // if there's at least one true in the remaining input then this can't work
            if input |> Seq.forall (fun x -> x <> Some true) then
                1
            else
                0

    input
    // TODO no
    // |> Seq.skip 1
    // |> Seq.take 1
    |> Seq.map (fun (pattern, numbers) ->
        // TODO part b has the bigger inputs

        let pattern =
            pattern
            @ [ None ]
            @ pattern
            @ [ None ]
            @ pattern
            @ [ None ]
            @ pattern
            @ [ None ]
            @ pattern

        let numbers = numbers @ numbers @ numbers @ numbers @ numbers

        printfn "TODO pattern = %A" pattern
        printfn "TODO numbers = %A" numbers
        let result = checkInput pattern numbers
        printfn "TODO result = %d" result

        result)
    |> Seq.sum
