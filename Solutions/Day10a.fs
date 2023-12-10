module Solutions.Day10a

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

    let score: int option array2d = Array2D.init height width (fun _ _ -> None)

    let rec findDistances (coord: Coord) (previousScore: int) (visitedNodes: Coord Set) =
        // we've now visited this one
        let visitedNodes = visitedNodes |> Set.add coord

        // the cost to get to this node is one more than the previous one
        let proposedNewScore = previousScore + 1

        // only keep the new score if it's better than the previous attempt to get here, if any
        let newScore =
            match score[coord.y, coord.x] with
            | Some existing -> min existing proposedNewScore
            | None -> proposedNewScore

        score[coord.y, coord.x] <- Some newScore

        // find all the places we can go from here
        let neighbors = input |> neighborsOf coord |> Seq.toList

        // but not any we've already been to
        let unvisitedNeighbors =
            neighbors |> List.filter (fun neighbor -> not (visitedNodes.Contains neighbor))

        for neighbor in unvisitedNeighbors do
            findDistances neighbor newScore visitedNodes

    findDistances startCoord -1 Set.empty

    seq {
        for y in 0 .. (height - 1) do
            yield!
                seq {
                    for x in 0 .. (width - 1) do
                        match score[y, x] with
                        | Some score -> yield score
                        | None -> ()
                }
    }
    |> Seq.max
