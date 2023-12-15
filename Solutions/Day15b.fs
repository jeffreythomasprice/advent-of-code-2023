module Solutions.Day15b

open System
open System.Text.RegularExpressions
open System.Collections.Generic

let hash (s: string) : int =
    s
    |> Seq.map (fun c -> c |> int)
    |> Seq.fold (fun result x -> ((result + x) * 17) % 256) 0

type AddOperation = { label: string; number: int }
type SubtractOperation = { label: string }

type Operation =
    | Add of AddOperation
    | Subtract of SubtractOperation

    member this.box =
        match this with
        | Add x -> hash x.label
        | Subtract x -> hash x.label

type Lens = { label: string; number: int }

let doIt (input: string) : int =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    if lines |> Seq.length <> 1 then
        failwith "expected a single line of input"

    let input = lines |> Seq.head
    let input = input.Split(",")

    let addRegex = Regex("^([^=]+)=([0-9]+)$")
    let subRegex = Regex("^([^-]+)-$")

    let instructions =
        input
        |> Seq.map (fun s ->
            let m = addRegex.Match(s)

            if m.Success then
                let label = m.Groups[1].Value
                let number = m.Groups[2].Value |> int
                Add { label = label; number = number }
            else
                let m = subRegex.Match(s)

                if m.Success then
                    let label = m.Groups[1].Value
                    Subtract { label = label }
                else
                    failwith (sprintf "unmatched instruction: %s" s))

    instructions
    |> Seq.fold
        (fun (boxes: Lens list array) instruction ->
            let box = boxes[instruction.box]

            boxes[instruction.box] <-
                match instruction with
                | Add { label = label; number = number } ->
                    // replace existing value with new number if it exists
                    let box =
                        box
                        |> List.map (fun existing ->
                            { label = existing.label
                              number = if existing.label = label then number else existing.number })

                    // if list doesn't contain the label add to back
                    if (box |> List.tryFind (fun existing -> existing.label = label)).IsNone then
                        box @ [ { label = label; number = number } ]
                    else
                        box
                | Subtract { label = label } ->
                    // remove label
                    box |> List.filter (fun existing -> existing.label <> label)

            boxes)
        (Array.init 256 (fun _ -> []))
    |> Array.indexed
    |> Seq.map (fun (i, box) ->
        let boxNum = i + 1

        box
        |> Seq.indexed
        |> Seq.map (fun (i, lens) ->
            let slotNum = i + 1
            boxNum * slotNum * lens.number)
        |> Seq.sum)
    |> Seq.sum
