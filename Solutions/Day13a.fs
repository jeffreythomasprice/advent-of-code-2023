module Solutions.Day13a

open System
open System.Text.RegularExpressions
open System.Collections.Generic

type Shape = { height: int; shape: string list }


let score (shape: Shape) : int =
    let sortedLines =
        shape.shape
        |> Seq.indexed
        |> Seq.fold
            (fun (results: Map<string, int list>) (i, line) ->
                match results.TryGetValue line with
                | true, l -> results.Add(line, (i :: l))
                | false, _ -> results.Add(line, [ i ]))
            (Map [])

    let possibleMirrorLocations =
        seq {
            for y in 1 .. (shape.height - 1) do
                let above = y
                let below = shape.height - y
                let range = min above below

                if
                    seq {
                        for i in 0 .. (range - 1) do
                            let y1 = y + i
                            let y2 = y - 1 - i
                            let line1 = shape.shape[y1]
                            let line2 = shape.shape[y2]
                            yield line1 = line2
                    }
                    |> Seq.forall (fun x -> x)
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
