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

let doIt (input: string) : int64 =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

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

    (*
        TODO not quite

        I think I'm not counting the bottom horizontal lines and I need to be

        e.g. the horizontal line in the middle right wouldn't be counted
        |              |
        |              |
        |      +-------+
        |      |
        |      |
        +------+
    *)

    let everythingButTheBottomRow =
        uniqueY
        |> Seq.pairwise
        |> Seq.map (fun (top, bottom) ->
            let matchingLines =
                lines
                |> Seq.map (fun line ->
                    match line with
                    | Horizontal _ -> None
                    | Vertical line ->
                        let lineTop = min line.first line.second
                        let lineBottom = max line.first line.second

                        if lineTop <= top && top < lineBottom then
                            Some line
                        else
                            None)
                |> Seq.filter (fun x -> x.IsSome)
                |> Seq.map (fun x -> x.Value)

            matchingLines
            |> Seq.map (fun line -> line.x)
            |> Seq.toList
            |> List.sort
            |> everyTwo
            |> Seq.map (fun (left, right) ->
                printfn "TODO top = %d, bottom = %d, left = %d, right = %d" top bottom left right
                (bottom - top) * (right - left + 1L))
            |> Seq.sum)
        |> Seq.sum

    let bottomRow =
        lines
        |> Seq.map (fun line ->
            match line with
            | Horizontal line -> if line.y = (uniqueY |> List.last) then Some line else None
            | Vertical _ -> None)
        |> Seq.filter (fun x -> x.IsSome)
        |> Seq.map (fun x -> x.Value)
        |> Seq.map (fun line ->
            let y = line.y
            let left = min line.first line.second
            let right = max line.first line.second
            printfn "TODO bottom line = %A" line
            right - left + 1L)
        |> Seq.sum

    everythingButTheBottomRow + bottomRow
