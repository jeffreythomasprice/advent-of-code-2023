module Solutions.Day10b

open System
open System.Text.RegularExpressions
open System.Collections.Generic

type Coord = { x: int; y: int }

let north = { x = 0; y = -1 }
let south = { x = 0; y = 1 }
let west = { x = -1; y = 0 }
let east = { x = 1; y = 0 }

type Cell =
    | NorthSouth
    | WestEast
    | NorthWest
    | NorthEast
    | SouthWest
    | SouthEast
    | Empty

    member this.connectsTo =
        match this with
        | NorthSouth -> [ north; south ]
        | WestEast -> [ west; east ]
        | NorthWest -> [ north; west ]
        | NorthEast -> [ north; east ]
        | SouthWest -> [ south; west ]
        | SouthEast -> [ south; east ]
        | Empty -> []

    static member fromDeltas(deltas: Coord seq) : Cell =
        let deltas = deltas |> Seq.toList
        let hasNorth = List.contains north deltas
        let hasSouth = List.contains south deltas
        let hasWest = List.contains west deltas
        let hasEast = List.contains east deltas

        match (deltas.Length, hasNorth, hasSouth, hasWest, hasEast) with
        | (0, false, false, false, false) -> Empty
        | (2, true, true, false, false) -> NorthSouth
        | (2, false, false, true, true) -> WestEast
        | (2, true, false, true, false) -> NorthWest
        | (2, true, false, false, true) -> NorthEast
        | (2, false, true, true, false) -> SouthWest
        | (2, false, true, false, true) -> SouthEast
        | _ -> failwith (sprintf "impossible set of neighbor deltas %A" deltas)

// return the coord of all the points reachable by following the pipe at the given location
let neighborsOf (coord: Coord) (input: Cell array2d) =
    input[coord.y, coord.x].connectsTo
    |> Seq.map (fun neighbor ->
        { x = coord.x + neighbor.x
          y = coord.y + neighbor.y })

let doIt (input: string) : int =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    let mutable startCoord: Coord option = None

    let input =
        lines
        |> Seq.indexed
        |> Seq.map (fun (y, line) ->
            line
            |> Seq.indexed
            |> Seq.map (fun (x, c) ->
                match c with
                | 'S' ->
                    if startCoord.IsSome then
                        failwith "multiple start locations"

                    startCoord <- Some { x = x; y = y }
                    // placeholder, we'll fill it in with the right value later
                    Empty
                | '|' -> NorthSouth
                | '-' -> WestEast
                | 'L' -> NorthEast
                | 'J' -> NorthWest
                | '7' -> SouthWest
                | 'F' -> SouthEast
                | '.' -> Empty
                | _ -> failwith (sprintf "unmatched: %c" c))
            |> Seq.toList)
        |> Seq.toList

    if (input |> List.map (fun row -> row.Length) |> set |> Set.count) <> 1 then
        failwith "not all rows are the same length"

    if input.Length = 0 then
        failwith "zero rows"

    if startCoord.IsNone then
        failwith "no start location"

    let startCoord = startCoord.Value

    let width = input[0].Length
    let height = input.Length
    let input = Array2D.init height width (fun y x -> input[y][x])

    let neighborsOfStart =
        [ { x = startCoord.x - 1
            y = startCoord.y }
          { x = startCoord.x + 1
            y = startCoord.y }
          { x = startCoord.x
            y = startCoord.y - 1 }
          { x = startCoord.x
            y = startCoord.y + 1 } ]
        |> Seq.filter (fun coord -> coord.x >= 0 && coord.x < width && coord.y >= 0 && coord.y < height)
        |> Seq.map (fun coord -> coord, input |> neighborsOf coord)
        |> Seq.filter (fun (neighbor, coords) -> coords |> Seq.contains startCoord)
        |> Seq.map (fun (coord, _) -> coord)
        |> Seq.toList

    let startCell =
        Cell.fromDeltas (
            neighborsOfStart
            |> Seq.map (fun neighbor ->
                { x = neighbor.x - startCoord.x
                  y = neighbor.y - startCoord.y })
        )

    let input =
        Array2D.init height width (fun y x ->
            if x = startCoord.x && y = startCoord.y then
                startCell
            else
                input[y, x])

    printfn "size = %d x %d" width height
    printfn "start = %A" startCoord
    printfn "input = %A" input

    let mutable loop: Coord Set = Set.empty

    let rec findLoop (coord: Coord) =
        if not (loop.Contains coord) then
            // we've now visited this one
            loop <- loop |> Set.add coord

            // recurse over all the places we could reach from here
            input |> neighborsOf coord |> Seq.iter findLoop

    findLoop startCoord

    (*
        flood fill
        we're going to treat the whole area as twice as big as it really is
        the actual cells in the input are the even coordinates in the flood fill area
        the pipe will get filled in, and the odd cells in between pipe cells also
        this leaves unfilled regions between neighboring but disconnected pipe cells that flood fill can walk later
        then we flood fill from the edges of the region in
        the only untouched regions should be those interior to the pipe, so we can just iterate over all even cells and count them
    *)

    // assume everything is "potentially outside" to start with, so true
    let floodWidth = width * 2
    let floodHeight = height * 2
    let floodFill = Array2D.init floodHeight floodWidth (fun _ _ -> true)

    // go clear all the pipes, and the connecting between them
    loop
    |> Seq.collect (fun coord ->
        let bigCoord = { x = coord.x * 2; y = coord.y * 2 }

        Seq.concat
            [
              // the average of this cell and it's neighbors, so the odd cells on the bigger grid between them
              (input
               |> neighborsOf coord
               |> Seq.map (fun neighbor ->
                   let bigNeighbor =
                       { x = neighbor.x * 2
                         y = neighbor.y * 2 }

                   { x = (bigCoord.x + bigNeighbor.x) / 2
                     y = (bigCoord.y + bigNeighbor.y) / 2 }))
              // and also this point we're on right now, to clear the middle of the pipe
              seq { { x = bigCoord.x; y = bigCoord.y } } ])
    |> Seq.iter (fun coord -> floodFill[coord.y, coord.x] <- false)

    let rec fill (coord: Coord) =
        // is this a cell that might have been inside the loop?
        if floodFill[coord.y, coord.x] then
            // it's definitely not because we got here
            floodFill[coord.y, coord.x] <- false
            // recurse over the neighbors
            [ { x = coord.x - 1; y = coord.y }
              { x = coord.x + 1; y = coord.y }
              { x = coord.x; y = coord.y - 1 }
              { x = coord.x; y = coord.y + 1 } ]
            |> Seq.filter (fun neighbor ->
                neighbor.x >= 0
                && neighbor.x < floodWidth
                && neighbor.y >= 0
                && neighbor.y < floodHeight)
            |> Seq.iter fill

    // flood fill the edges, remembering to multiple by two because we're on the bigger grid
    Seq.concat
        [ (seq { 0..2 .. (floodWidth - 1) }
           |> Seq.collect (fun x -> [ { x = x; y = 0 }; { x = x; y = floodHeight - 1 } ]))
          (seq { 0..2 .. (floodHeight - 1) }
           |> Seq.collect (fun y -> [ { x = 0; y = y }; { x = floodWidth - 1; y = y } ])) ]
    |> Seq.iter fill

    // count the remaining bits we didn't reach
    seq {
        for y in 0..2 .. (floodHeight - 1) do
            yield!
                seq {
                    for x in 0..2 .. (floodWidth - 1) do
                        if floodFill[y, x] then
                            yield { x = x; y = y }
                }
    }
    |> Seq.length
