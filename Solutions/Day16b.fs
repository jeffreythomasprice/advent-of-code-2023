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

type Vector =
    { x: int
      y: int }

    member this.add(other: Vector) : Vector =
        { x = this.x + other.x
          y = this.y + other.y }

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

type Line =
    { origin: Ray
      endPoint: Vector
      children: Ray list }

    member this.points: Vector seq =
        seq {
            let mutable point = this.origin.origin

            while point <> this.endPoint do
                yield point
                point <- point.add this.origin.direction.vector

            yield point
        }

let lineAt (map: Map) (origin: Ray) : Line =
    let rec scan (current: Ray) : Vector * Ray list =
        let next =
            { origin = current.origin.add current.direction.vector
              direction = current.direction }

        if inBounds map next.origin then
            match map.data[next.origin.y, next.origin.x] with
            | Empty -> scan next
            | VerticalSplitter ->
                match next.direction with
                | West
                | East ->
                    next.origin,
                    [ { origin = next.origin
                        direction = North }
                      { origin = next.origin
                        direction = South } ]
                | North
                | South -> scan next
            | HorizontalSplitter ->
                match next.direction with
                | West
                | East -> scan next
                | North
                | South ->
                    next.origin,
                    [ { origin = next.origin
                        direction = West }
                      { origin = next.origin
                        direction = East } ]
            | WestToNorthMirror ->
                next.origin,
                [ { origin = next.origin
                    direction =
                      (match next.direction with
                       | West -> South
                       | East -> North
                       | North -> East
                       | South -> West) } ]
            | WestToSouthMirror ->
                next.origin,
                [ { origin = next.origin
                    direction =
                      (match next.direction with
                       | West -> North
                       | East -> South
                       | North -> West
                       | South -> East) } ]
        else
            current.origin, []

    let endPoint, children = scan origin

    { origin = origin
      endPoint = endPoint
      children = children }

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

    let rec uncachedSolve (ray: Ray) (visited: Ray Set) (results: Vector Set) : Ray Set * Vector Set =
        if visited.Contains ray then
            visited, results
        else
            let visited = visited |> Set.add ray
            let line = lineAt map ray
            let results = Set.union results (line.points |> Set)

            line.children
            |> Seq.fold
                (fun (visited, results) child ->
                    let visited, results = uncachedSolve child visited results
                    visited, results)
                (visited, results)

    seq {
        for x in 0 .. (width - 1) do
            yield
                { origin = { x = x; y = -1 }
                  direction = South }

            yield
                { origin = { x = x; y = height }
                  direction = North }

        for y in 0 .. (height - 1) do
            yield
                { origin = { x = -1; y = y }
                  direction = East }

            yield
                { origin = { x = width; y = y }
                  direction = West }
    }
    |> Seq.map (fun ray ->
        let _, results = uncachedSolve ray Set.empty Set.empty
        results |> Set.filter (inBounds map) |> Set.count)
    |> Seq.max
