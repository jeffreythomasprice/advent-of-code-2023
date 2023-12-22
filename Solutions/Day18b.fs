module Solutions.Day18b

open System
open System.Text.RegularExpressions
open System.Collections.Generic

type Vector =
    { x: int64
      y: int64 }

    static member (+)(a: Vector, b: Vector) : Vector = { x = a.x + b.x; y = a.y + b.y }

    static member (-)(a: Vector, b: Vector) : Vector = { x = a.x - b.x; y = a.y - b.y }

    static member (*)(a: Vector, b: int64) : Vector = { x = a.x * b; y = a.y * b }

type Direction =
    | Up
    | Down
    | Left
    | Right

    member this.vector =
        match this with
        | Up -> { x = 0; y = -1 }
        | Down -> { x = 0; y = 1 }
        | Left -> { x = -1; y = 0 }
        | Right -> { x = 1; y = 0 }

type Instruction = { direction: Direction; steps: int64 }

type Cell =
    | Empty
    | Initial
    | Colored of string
    | Interior

type Rectangle =
    { origin: Vector
      size: Vector }

    // TODO check various things here, may not need all these helpers

    member this.width = this.size.x

    member this.height = this.size.y

    member this.left = this.origin.x

    member this.right = this.origin.x + this.width

    member this.top = this.origin.y

    member this.bottom = this.origin.y + this.height

    member this.contains(v: Vector) =
        v.x >= this.left && v.x < this.right && v.y >= this.top && v.y < this.bottom

    member this.expand(v: Vector) : Rectangle =
        let left = min v.x this.left
        let right = max (v.x + 1L) this.right
        let top = min v.y this.top
        let bottom = max (v.y + 1L) this.bottom

        { origin = { x = left; y = top }
          size = { x = right - left; y = bottom - top } }

type Horizontal =
    { first: int64
      second: int64
      y: int64 }

type Vertical =
    { first: int64
      second: int64
      x: int64 }

type LineSegment =
    | Horizontal of Horizontal
    | Vertical of Vertical

    member this.first =
        match this with
        | Horizontal line -> { x = line.first; y = line.y }
        | Vertical line -> { x = line.x; y = line.first }

    member this.second =
        match this with
        | Horizontal line -> { x = line.second; y = line.y }
        | Vertical line -> { x = line.x; y = line.second }

let rec everyTwo (input: 't list) =
    match input with
    | [] -> []
    | a :: b :: tail -> (a, b) :: (everyTwo tail)
    | _ -> failwith "odd number of elements"

type Range =
    { first: int64
      last: int64 }

    member this.subtract(others: Range list) : Range seq =
        match others with
        | [] -> [ this ]
        | other :: remainder ->
            if this.first > other.last || this.last < other.first then
                [ this ]
            else if this.first >= other.first && this.last <= other.last then
                []
            else if this.first < other.first && this.last > other.last then
                [ { first = this.first
                    last = other.first - 1L }
                  { first = other.last + 1L
                    last = this.last } ]
            else if this.first < other.first then
                [ { first = this.first
                    last = other.first - 1L } ]
            else
                [ { first = other.last + 1L
                    last = this.last } ]
            |> Seq.collect (fun portion -> portion.subtract remainder)


let doIt (input: string) : int64 =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    // TODO JEFF put the real parser back
    // let input =
    //     lines
    //     |> Seq.map (fun line -> Regex("^([UDLR]) ([0-9]+) \(#([0-9a-f]{6})\)$").Match(line))
    //     |> Seq.map (fun m ->
    //         let direction =
    //             match m.Groups[1].Value with
    //             | "U" -> Up
    //             | "D" -> Down
    //             | "L" -> Left
    //             | "R" -> Right
    //             | _ -> failwith "bad enum"

    //         let steps = m.Groups[2].Value |> int
    //         let color = m.Groups[3].Value

    //         { direction = direction; steps = steps })
    //     |> Seq.toList
    let input =
        lines
        |> Seq.map (fun line -> Regex("^[UDLR] [0-9]+ \(#([0-9a-f]{5})([0-3])\)$").Match(line))
        |> Seq.map (fun m ->
            let direction =
                match m.Groups[2].Value with
                | "0" -> Right
                | "1" -> Down
                | "2" -> Left
                | "3" -> Up
                | _ -> failwith "bad enum"

            let steps = Convert.ToInt64(m.Groups[1].Value, 16)

            { direction = direction; steps = steps })
        |> Seq.toList

    let mutable currentLocation = { x = 0; y = 0 }

    let lines =
        input
        |> Seq.map (fun instruction ->
            let first = currentLocation
            let second = currentLocation + instruction.direction.vector * instruction.steps
            currentLocation <- second

            match instruction.direction with
            | Up
            | Down ->
                Vertical
                    { first = first.y
                      second = second.y
                      x = currentLocation.x }
            | Left
            | Right ->
                Horizontal
                    { first = first.x
                      second = second.x
                      y = currentLocation.y })
        |> Seq.map (fun line ->
            printfn "TODO line = %A" line
            line)
        |> Seq.toList

    let allPoints =
        lines |> Seq.collect (fun line -> [ line.first; line.second ]) |> Seq.toList

    // TODO not needed?
    let totalBounds =
        (allPoints
         |> Seq.fold
             (fun (r: Rectangle option) v ->
                 match r with
                 | Some r -> Some(r.expand v)
                 | None -> Some({ origin = v; size = { x = 1; y = 1 } }))
             None)
            .Value

    printfn "total bounds = %A" totalBounds

    let uniqueY = allPoints |> Seq.map (fun v -> v.y) |> SortedSet |> Seq.toList

    let yAndXs =
        uniqueY
        |> Seq.map (fun y ->
            let allX =
                lines
                |> Seq.map (fun line ->
                    match line with
                    | Horizontal _ -> None
                    | Vertical line ->
                        let lineTop = min line.first line.second
                        let lineBottom = max line.first line.second
                        if lineTop <= y && y < lineBottom then Some line else None)
                |> Seq.filter (fun x -> x.IsSome)
                |> Seq.map (fun x -> x.Value)
                |> Seq.map (fun line -> line.x)
                |> Seq.toList
                |> List.sort

            y, allX)

    yAndXs
    |> Seq.pairwise
    |> Seq.map (fun ((top, topX), (bottom, bottomX)) ->
        let mainAreas =
            topX
            |> everyTwo
            |> Seq.map (fun (left, right) ->
                printfn "TODO top = %d, bottom = %d, left = %d, right = %d" top bottom left right
                (bottom - top) * (right - left + 1L))
            |> Seq.sum

        let horizontalLinesAtTheBottom =
            lines
            |> Seq.map (fun line ->
                match line with
                | Horizontal line -> if line.y = bottom then Some line else None
                | Vertical _ -> None)
            |> Seq.filter (fun x -> x.IsSome)
            |> Seq.map (fun x -> x.Value)
            |> Seq.map (fun line ->
                (*
                    TODO this is the only thing wrong
                    we double count horizontal edges that on the area region
                    need to count all horizontals, but only the portions not covered by pairwise bottomX

                    width += line subtract (union of all horizontal edges formed by pairwise bottomX)
                *)
                { first = min line.first line.second
                  last = max line.first line.second }
                    .subtract (
                        bottomX
                        |> Seq.pairwise
                        |> Seq.map (fun (left, right) -> { first = left; last = right })
                        |> Seq.toList
                    )
                |> Seq.map (fun range ->
                    let result = range.last - range.first + 1L

                    printfn
                        "TODO bottomY = %d, horizontal range = %d, %d, result = %d"
                        bottom
                        range.first
                        range.last
                        result

                    result)
                |> Seq.sum)
            |> Seq.sum

        mainAreas + horizontalLinesAtTheBottom)
    |> Seq.sum
