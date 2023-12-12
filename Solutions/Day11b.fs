module Solutions.Day11b

open System
open System.Text.RegularExpressions
open System.Collections.Generic

let doIt (input: string) : int64 =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    let input =
        lines
        |> Seq.map (fun line ->
            line
            |> Seq.map (fun c ->
                match c with
                | '#' -> true
                | '.' -> false
                | _ -> failwith (sprintf "invalid char: %c" c))
            |> Seq.toList)
        |> Seq.toList

    let height = input.Length

    if (input |> Seq.map (fun row -> row.Length) |> Set).Count <> 1 then
        failwith "not all rows the same length"

    let width = input[0].Length

    let input = Array2D.init height width (fun y x -> input[y][x])

    let row y =
        seq {
            for x in 0 .. (width - 1) do
                yield input[y, x]
        }

    let column x =
        seq {
            for y in 0 .. (height - 1) do
                yield input[y, x]
        }

    let emptyColumns =
        seq {
            for x in 0 .. (width - 1) do
                if column x |> Seq.forall (fun value -> not value) then
                    yield x
        }

    let emptyRows =
        seq {
            for y in 0 .. (height - 1) do
                if row y |> Seq.forall (fun value -> not value) then
                    yield y
        }

    let starLocations =
        seq {
            for y in 0 .. (height - 1) do
                yield!
                    seq {
                        for x in 0 .. (width - 1) do
                            if input[y, x] then
                                yield (x |> int64, y |> int64)
                    }
        }

    let updatedStarLocations =
        starLocations
        |> Seq.map (fun (x, y) ->
            let emptyColumnsBeforeHere =
                emptyColumns |> Seq.filter (fun column -> x > column) |> Seq.length |> int64

            let emptyRowsBeforeHere =
                emptyRows |> Seq.filter (fun row -> y > row) |> Seq.length |> int64

            (x + emptyColumnsBeforeHere * 999999L, y + emptyRowsBeforeHere * 999999L))

    let distance (x1, y1) (x2, y2) = (abs (x2 - x1)) + (abs (y2 - y1))

    Seq.allPairs (updatedStarLocations |> Seq.indexed) (updatedStarLocations |> Seq.indexed)
    |> Seq.filter (fun (a, b) ->
        let a, _ = a
        let b, _ = b
        a < b)
    |> Seq.map (fun (a, b) ->
        let _, a = a
        let _, b = b
        distance a b)
    |> Seq.sum
