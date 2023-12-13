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
        (*
            TODO I have started and deleted so many solutions here I have no idea
        *)
        0

    input
    // TODO no
    // |> Seq.skip 1
    // |> Seq.take 1
    |> Seq.map (fun (pattern, numbers) ->
        // TODO part b has the bigger inputs

        // let pattern =
        //     pattern
        //     @ [ None ]
        //     @ pattern
        //     @ [ None ]
        //     @ pattern
        //     @ [ None ]
        //     @ pattern
        //     @ [ None ]
        //     @ pattern

        // let numbers = numbers @ numbers @ numbers @ numbers @ numbers

        printfn "TODO pattern = %A" pattern
        printfn "TODO numbers = %A" numbers
        let result = checkInput pattern numbers
        printfn "TODO result = %d" result

        result)
    |> Seq.sum
