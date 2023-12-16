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

    let visitedCells, _ =
        check
            map
            Set.empty
            []
            { origin = { x = -1; y = 0 }
              direction = East }

    visitedCells
    // because we started off-grid at (-1, 0)
    |> Set.filter (inBounds map)
    |> Set.count
