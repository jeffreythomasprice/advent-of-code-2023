module Solutions.Day13b

open System
open System.Text.RegularExpressions
open System.Collections.Generic

type Shape = { height: int; shape: string list }

let score (shape: Shape) : int =
    let possibleMirrorLocations =
        seq {
            for y in 1 .. (shape.height - 1) do
                let above = y
                let below = shape.height - y
                let range = min above below

                let mutable haveChangedSomething = false

                if
                    seq {
                        for i in 0 .. (range - 1) do
                            let y1 = y + i
                            let y2 = y - 1 - i
                            let line1 = shape.shape[y1]
                            let line2 = shape.shape[y2]

                            let lineToBitmask (line: string) : int =
                                line
                                |> Seq.indexed
                                |> Seq.fold
                                    (fun result (i, c) ->

                                        let bit = if c = '#' then 1 else 0
                                        result ||| (bit <<< i))
                                    0

                            let bitmask1 = lineToBitmask line1
                            let bitmask2 = lineToBitmask line2

                            yield
                                if bitmask1 = bitmask2 then
                                    true
                                else if not haveChangedSomething then
                                    // should be one where they are different
                                    let difference = bitmask1 ^^^ bitmask2
                                    // we want exactly one bit set, so this should just be a power of 2
                                    // https://stackoverflow.com/q/1053582
                                    if not ((difference <> 1) && ((difference &&& (difference - 1)) <> 0)) then
                                        haveChangedSomething <- true
                                        true
                                    else
                                        false
                                else
                                    // not equal and we've already changed something so we can't again
                                    false
                    }
                    |> Seq.forall (fun x -> x)
                    && haveChangedSomething
                then
                    yield y
        }

    match possibleMirrorLocations |> Seq.toList with
    | [] -> 0
    | [ result ] -> result
    | _ -> failwith "multiple possible mirror locations"

let doIt (input: string) : int =
    let lines = input.Split "\n" |> Array.toSeq

    let shapes =
        seq {
            let mutable current = []

            for line in lines do
                if line.Trim().Length = 0 then
                    yield current
                    current <- []
                else
                    current <- current @ [ line ]

            if current.Length > 0 then
                yield current
        }
        |> Seq.map (fun shape ->
            let height = shape.Length
            let width = shape[0].Length

            let rotated =
                seq {
                    for y in 0 .. (width - 1) do
                        yield
                            String.Join(
                                "",
                                seq {
                                    for x in 0 .. (height - 1) do
                                        let originalY = height - 1 - x
                                        let originalX = y
                                        shape[originalY][originalX]
                                }
                                |> Seq.toArray
                            )
                }
                |> Seq.toList

            { height = height; shape = shape }, { height = width; shape = rotated })

    shapes
    |> Seq.map (fun (original, rotated) -> (score original) * 100 + (score rotated))
    |> Seq.sum
