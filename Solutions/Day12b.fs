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
            let s = m.Groups[1].Value

            let pattern =
                s
                |> Seq.map (fun c ->
                    match c with
                    | '#' -> Some true
                    | '.' -> Some false
                    | '?' -> None
                    | _ -> failwith (sprintf "unhandled char: %c" c))
                |> Seq.toList

            let numbers = m.Groups[2].Value.Split(',') |> Seq.map int |> Seq.toList
            s, pattern, numbers)

    let rec checkInput (input: bool option list) (numbers: int list) : int =
        match input, numbers with
        // success, we reached the end of both lists
        | [], [] -> 1

        // fail, we reached the end of the input but have more numbers to place
        | [], _ -> 0

        // we can skip gaps
        | Some false :: remainingInput, _ -> checkInput remainingInput numbers

        // if we have a choice we take both paths and sum the results
        | None :: remainingInput, _ ->
            (checkInput (Some true :: remainingInput) numbers)
            + (checkInput (Some false :: remainingInput) numbers)

        // we must place a number here, but have no numbers remaining
        | Some true :: _, [] -> 0

        // we must place the next number here, and have one to check
        | Some true :: _, nextNumber :: remainingNumbers ->
            // we have to be able to place the number at the start, so the first few elements have to all be either Some true or None
            if
                input.Length >= nextNumber
                && input |> Seq.take nextNumber |> Seq.forall (fun x -> x <> Some false)
            then
                // ok so it goes here, so skip that many
                let remainingInput = input |> List.skip nextNumber
                // if there is at least one remaining input, it has to be Some false or None, because it has to be a gap
                if remainingInput.Length >= 1 then
                    if remainingInput.Head = Some true then
                        // the next one has to be part of a number, so this can't work
                        0
                    else
                        // the next one can be a gap, so skip that too
                        let remainingInput = remainingInput |> List.skip 1
                        // and recurse
                        checkInput remainingInput remainingNumbers
                else
                    // this was actually the entire input, we can just recurse to see if there are more numbers
                    checkInput remainingInput remainingNumbers
            else
                // the number can't fit at the start
                0

    input
    // TODO no
    // |> Seq.skip 1
    // |> Seq.take 1
    |> Seq.indexed
    |> Seq.map (fun (i, (patternString, pattern, numbers)) ->
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

        printfn "(%d/%d) %s, %A" (i + 1) (input |> Seq.length) patternString numbers
        let result = checkInput pattern numbers
        printfn "result = %d" result

        result)
    |> Seq.sum
