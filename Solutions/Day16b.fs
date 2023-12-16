module Solutions.Day16b

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

let inBounds (map: Map) (point: Vector) : bool =
    point.x >= 0 && point.x < map.width && point.y >= 0 && point.y < map.height

let rec check (map: Map) (visitedCells: Vector Set) (visitedRays: Ray list) (current: Ray) : Vector Set * Ray list =
    if visitedRays |> List.contains current then
        visitedCells, visitedRays
    else
        let visitedRays = current :: visitedRays

        let newLocation =
            { x = current.origin.x + current.direction.vector.x
              y = current.origin.y + current.direction.vector.y }

        if not (inBounds map newLocation) then
            visitedCells, visitedRays
        else
            let visitedCells = visitedCells |> Set.add newLocation

            match map.data[newLocation.y, newLocation.x] with
            // if we're at an empty cell we keep going
            | Empty ->
                check
                    map
                    visitedCells
                    visitedRays
                    { origin = newLocation
                      direction = current.direction }
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
                            { origin = newLocation
                              direction = North }

                    let visitedCells, visitedRays =
                        check
                            map
                            visitedCells
                            visitedRays
                            { origin = newLocation
                              direction = South }

                    visitedCells, visitedRays
                | North
                | South ->
                    check
                        map
                        visitedCells
                        visitedRays
                        { origin = newLocation
                          direction = current.direction }
            | HorizontalSplitter ->
                match current.direction with
                | West
                | East ->
                    check
                        map
                        visitedCells
                        visitedRays
                        { origin = newLocation
                          direction = current.direction }
                | North
                | South ->
                    let visitedCells, visitedRays =
                        check
                            map
                            visitedCells
                            visitedRays
                            { origin = newLocation
                              direction = West }

                    let visitedCells, visitedRays =
                        check
                            map
                            visitedCells
                            visitedRays
                            { origin = newLocation
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
                    { origin = newLocation
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
                    { origin = newLocation
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

    // TODO no?
    let solve (ray: Ray) : int =
        let visitedCells, _ = check map Set.empty [] ray

        visitedCells
        // because we started off-grid
        |> Set.filter (inBounds map)
        |> Set.count

    // returns the list of all visited points until the next non-empty, and the last point visited
    let rec walkToNext (ray: Ray) : Vector Set * Vector =
        let next =
            { x = ray.origin.x + ray.direction.vector.x
              y = ray.origin.y + ray.direction.vector.y }

        if not (inBounds map next) then
            set [ ray.origin ], ray.origin
        else
            match map.data[next.y, next.x] with
            | Empty ->
                let results, last =
                    walkToNext
                        { origin = next
                          direction = ray.direction }

                Set.union results (set [ ray.origin ]), last
            | _ -> set [ ray.origin; next ], next

    let mutable cachedResults = Map.empty

    seq {
        for x in 0 .. (width - 1) do
            printfn "TODO start of x = %d (out of width = %d)" x width

            yield
                { origin = { x = x; y = -1 }
                  direction = South }

            yield
                { origin = { x = x; y = height }
                  direction = North }

        for y in 0 .. (height - 1) do
            printfn "TODO start of y = %d (out of height = %d)" y height

            yield
                { origin = { x = -1; y = y }
                  direction = East }

            yield
                { origin = { x = width; y = y }
                  direction = West }
    }
    |> Seq.map (fun ray ->
        let points, last = walkToNext ray

        let startingRay =
            { origin =
                { x = last.x - ray.direction.vector.x
                  y = last.y - ray.direction.vector.y }
              direction = ray.direction }

        match cachedResults.TryFind startingRay with
        | Some cachedPoints -> Set.union points cachedPoints
        | None ->
            let visitedCells, _ = check map Set.empty [] startingRay
            cachedResults <- cachedResults.Add(startingRay, visitedCells)
            Set.union points visitedCells)
    |> Seq.map (fun points -> points |> Set.filter (inBounds map) |> Set.count)
    |> Seq.max
