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

let flattenOption (input: 't option seq): 't seq =
    input |> Seq.filter _.IsSome |> Seq.map _.Value

type Range =
    { first: int64; second: int64}

    member this.subtract (others: Range list): Range seq =
        match others with
        | [] -> [this]
        | other :: remainder ->
            let thisMin = min this.first this.second
            let thisMax = max this.first this.second
            let otherMin = min other.first other.second
            let otherMax = max other.first other.second
            (
                if thisMax < otherMin || thisMin > otherMax then
                    [this]
                else if thisMin >= otherMin && thisMax <= otherMax then
                    []
                else if thisMin < otherMin && thisMax > otherMax then
                    [ { first = thisMin ; second = otherMin - 1L};
                      { first = otherMax + 1L; second = thisMax} ]
                else if thisMin < otherMin then
                    [ { first = thisMin ; second = otherMin - 1L} ]
                else
                    [ { first = otherMax + 1L; second = thisMax} ]
            )
            |> Seq.collect (fun portion -> portion.subtract remainder)

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
            printfn "line = %A" line
            line)
        |> Seq.toList

    let allPoints =
        lines |> Seq.collect (fun line -> [ line.first; line.second ]) |> Seq.toList

    let uniqueY = allPoints |> Seq.map (fun v -> v.y) |> SortedSet |> Seq.toList

    let horizontalLines = 
        lines |>
        Seq.map (fun line ->
            match line with
            | Horizontal line -> Some line
            | Vertical _ -> None
        )
        |> flattenOption

    let verticalLines = 
        lines |>
        Seq.map (fun line ->
            match line with
            | Horizontal _ -> None
            | Vertical line -> Some line
        )
        |> flattenOption

    let interiorRegions =
        uniqueY
        |> Seq.pairwise
        |> Seq.map (fun (top, bottom) ->
            // height including top and bottom would be (bottom - top + 1L)
            // we want just the top, but not the bottom because that will be the next pair of y coordinates
            let height = bottom - top
            printfn "top = %d, bottom = %d, height = %d" top bottom height

            // the horizontal lines on the top of this region
            let topLines = 
                horizontalLines
                |> Seq.filter (fun line -> line.y = top)

            // left to right, the x coordinates of the ranges to fill in
            let allX = 
                verticalLines
                |> Seq.filter (fun line ->
                    let lineTop = min line.first line.second
                    let topBottom = max line.first line.second
                    not (top >= topBottom || bottom <= lineTop)
                )
                |> Seq.map (fun line ->
                    line.x
                )
                |> Seq.toList
                |> List.sort
                |> everyTwo
                |> Seq.map (fun (first, second) -> { first = first; second = second})
                |> Seq.toList
            
            // remove from the horizontal lines on top all the regions here
            // this lets us keep the portion that represents the bottom of some region above here without double counting any area
            let topLinesWidth = 
                topLines
                |> Seq.collect (fun line ->
                    { first = min line.first line.second; second = max line.first line.second }.subtract allX 
                )
                |> Seq.map (fun range ->
                    let width = range.second - range.first + 1L
                    printfn "top line, left = %d, right = %d, width = %d" range.first range.second width
                    width
                )
                |> Seq.sum

            // the area of in between the vertical lines
            let area = 
                allX
                |> Seq.map (fun range ->
                    let width = range.second - range.first + 1L
                    let result = width * height
                    printfn "left = %d, right = %d, width = %d, result = %d" range.first range.second width result
                    result
                )
                |> Seq.sum
            
            area + topLinesWidth
        )
        |> Seq.sum

    // the only place we haven't counted is the bottom line
    // this can only be horizontal lines and can't possibly be double counted with anything we've already done, so just add those up
    let lastLine =
        horizontalLines
        |> Seq.filter (fun line -> line.y = (uniqueY |> List.last))
        |> Seq.map (fun line ->
            let left = min line.first line.second
            let right = max line.first line.second
            let width = right - left + 1L
            printfn "last row y= %d, left = %d, right = %d, width = %d" line.y left right width
            width
        )
        |> Seq.sum

    interiorRegions + lastLine