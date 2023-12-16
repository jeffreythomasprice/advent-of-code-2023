module Solutions.Day16a

open System
open System.Text.RegularExpressions
open System.Collections.Generic

type Cell =
    | Empty
    | VerticalSplitter
    | HorizontalSplitter
    | WestToNorthMirror
    | WestToSouthMirror

type Map =
    { width: int
      height: int
      data: Cell array2d }

type Vector = { x: int; y: int }

type Direction =
    | West
    | East
    | North
    | South

    member this.vector: Vector =
        match this with
        | West -> { x = -1; y = 0 }
        | East -> { x = 1; y = 0 }
        | North -> { x = 0; y = -1 }
        | South -> { x = 0; y = 1 }

type Ray =
    { origin: Vector; direction: Direction }

type LineSegment = { a: Vector; b: Vector }

let inBounds (map: Map) (point: Vector) : bool =
    point.x >= 0 && point.x < map.width && point.y >= 0 && point.y < map.height

let rec check
    (map: Map)
    (visitedCells: LineSegment list)
    (visitedRays: Ray list)
    (current: Ray)
    : LineSegment list * Ray list =
    let rec traceUntilCollision (location: Vector) (direction: Direction) : Vector =
        let newLocation =
            { x = location.x + direction.vector.x
              y = location.y + direction.vector.y }

        if not (inBounds map newLocation) then
            location
        else
            match map.data[newLocation.y, newLocation.x] with
            // we go through empty space and keep looking
            | Empty -> traceUntilCollision newLocation direction
            // for splitters, depending on the direction we're travelling with either stop here or keep going
            | VerticalSplitter ->
                match direction with
                | West
                | East -> newLocation
                | North
                | South -> traceUntilCollision newLocation direction
            | HorizontalSplitter ->
                match direction with
                | West
                | East -> traceUntilCollision newLocation direction
                | North
                | South -> newLocation
            // for mirrors we stop
            | WestToNorthMirror
            | WestToSouthMirror -> newLocation

    if visitedRays |> List.contains current then
        visitedCells, visitedRays
    else
        let visitedRays = current :: visitedRays
        let finalPoint = traceUntilCollision current.origin current.direction
        let visitedCells = { a = current.origin; b = finalPoint } :: visitedCells

        match map.data[finalPoint.y, finalPoint.x] with
        // if we're at an empty cell we must be hitting the edge of the map, we're done
        | Empty -> visitedCells, visitedRays
        // for splitters, if we stopped here, and we're travelling the right direction, we need to generate new rays
        // otherwise we stop
        | VerticalSplitter ->
            match current.direction with
            | West
            | East ->
                let visitedCells, visitedRays =
                    check
                        map
                        visitedCells
                        visitedRays
                        { origin = finalPoint
                          direction = North }

                let visitedCells, visitedRays =
                    check
                        map
                        visitedCells
                        visitedRays
                        { origin = finalPoint
                          direction = South }

                visitedCells, visitedRays
            | North
            | South -> visitedCells, visitedRays
        | HorizontalSplitter ->
            match current.direction with
            | West
            | East -> visitedCells, visitedRays
            | North
            | South ->
                let visitedCells, visitedRays =
                    check
                        map
                        visitedCells
                        visitedRays
                        { origin = finalPoint
                          direction = West }

                let visitedCells, visitedRays =
                    check
                        map
                        visitedCells
                        visitedRays
                        { origin = finalPoint
                          direction = East }

                visitedCells, visitedRays
        // for mirrors we generate a new ray based on the direction
        | WestToNorthMirror ->
            let newDirection =
                match current.direction with
                | West -> South
                | East -> North
                | North -> East
                | South -> West

            check
                map
                visitedCells
                visitedRays
                { origin = finalPoint
                  direction = newDirection }
        | WestToSouthMirror ->
            let newDirection =
                match current.direction with
                | West -> North
                | East -> South
                | North -> West
                | South -> East

            check
                map
                visitedCells
                visitedRays
                { origin = finalPoint
                  direction = newDirection }

let doIt (input: string) : int =
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
                | '.' -> Empty
                | '|' -> VerticalSplitter
                | '-' -> HorizontalSplitter
                | '/' -> WestToNorthMirror
                | '\\' -> WestToSouthMirror
                | _ -> failwith (sprintf "unrecognized character: %c" c))
            |> Seq.toList)
        |> Seq.toList

    let height = input.Length
    let width = input[0].Length

    if not (input |> Seq.forall (fun row -> row.Length = width)) then
        failwith "not all rows are equal length"

    let map: Map =
        { width = width
          height = height
          data = Array2D.init height width (fun y x -> input[y][x]) }

    let visitedCells, _ =
        check
            map
            []
            []
            { origin = { x = -1; y = 0 }
              direction = East }

    let uniqueLocations =
        visitedCells
        |> Seq.collect (fun line ->
            let dx = line.b.x - line.a.x
            let dy = line.b.y - line.a.y

            if dx <> 0 && dy <> 0 then
                failwith (sprintf "non-axis-aligned line segment %A" line)

            let dx = sign dx
            let dy = sign dy

            printfn "TODO %A" line

            seq {
                let mutable point = line.a

                while point <> line.b do
                    yield point
                    point <- { x = point.x + dx; y = point.y + dy }

                yield point
            })
        // because we started off-grid at (-1, 0)
        |> Seq.filter (inBounds map)
        |> Seq.distinct
        |> Seq.toList

    // TODO no
    let visitedGrid = Array2D.init height width (fun y x -> 0)

    for location in uniqueLocations do
        visitedGrid[location.y, location.x] <- visitedGrid[location.y, location.x] + 1

        if visitedGrid[location.y, location.x] >= 2 then
            failwith (sprintf "duplicate on %A" location)

    for y in 0 .. (height - 1) do
        for x in 0 .. (width - 1) do
            printf "%c" (if visitedGrid[y, x] = 0 then '.' else '#')

        printfn ""

    uniqueLocations |> Seq.length
