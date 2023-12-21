module Solutions.Day18a

open System
open System.Text.RegularExpressions
open System.Collections.Generic

type Vector =
    { x: int
      y: int }

    static member (+)(a: Vector, b: Vector) : Vector = { x = a.x + b.x; y = a.y + b.y }

    static member (-)(a: Vector, b: Vector) : Vector = { x = a.x - b.x; y = a.y - b.y }

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

type Instruction =
    { direction: Direction
      steps: int
      color: string }

type Cell =
    | Empty
    | Initial
    | Colored of string

type Rectangle =
    { origin: Vector
      size: Vector }

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
        let right = max (v.x + 1) this.right
        let top = min v.y this.top
        let bottom = max (v.y + 1) this.bottom

        { origin = { x = left; y = top }
          size = { x = right - left; y = bottom - top } }

type Grid =
    { mutable bounds: Rectangle
      mutable data: Cell array2d }

    static member init(cell: Cell) =
        { bounds =
            { origin = { x = 0; y = 0 }
              size = { x = 1; y = 1 } }
          data = Array2D.init 1 1 (fun _ _ -> cell) }

    member this.Item
        with get (v: Vector) =
            if this.bounds.contains v then
                let v = v - this.bounds.origin
                this.data[v.y, v.x]
            else
                Empty
        and set (v: Vector) (value: Cell) =
            if not (this.bounds.contains v) then
                let newBounds = this.bounds.expand v

                let newData =
                    Array2D.init newBounds.height newBounds.width (fun y x ->
                        let inRealSpace = { x = x; y = y } + newBounds.origin
                        this[inRealSpace])

                this.bounds <- newBounds
                this.data <- newData

            let v = v - this.bounds.origin
            this.data[v.y, v.x] <- value

let doIt (input: string) : int =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    let input =
        lines
        |> Seq.map (fun line -> Regex("^([UDLR]) ([0-9]+) \(#([0-9a-f]{6})\)$").Match(line))
        |> Seq.map (fun m ->
            let direction =
                match m.Groups[1].Value with
                | "U" -> Up
                | "D" -> Down
                | "L" -> Left
                | "R" -> Right
                | _ -> failwith "bad enum"

            let steps = m.Groups[2].Value |> int
            let color = m.Groups[3].Value

            { direction = direction
              steps = steps
              color = color })

    let grid = Grid.init Initial

    let mutable currentLocation = grid.bounds.origin

    input
    |> Seq.iter (fun instruction ->
        (Seq.replicate instruction.steps instruction.direction.vector)
        |> Seq.iter (fun delta ->
            currentLocation <- currentLocation + delta
            grid[currentLocation] <- Colored instruction.color))

    printfn "final grid bounds = %A" grid.bounds

    for y in grid.bounds.top .. (grid.bounds.bottom - 1) do
        for x in grid.bounds.left .. (grid.bounds.right - 1) do
            let v = { x = x; y = y }

            let c =
                match grid[v] with
                | Initial
                | Colored _ -> '#'
                | Empty -> '.'

            printf "%c" c

        printfn ""

    for y in grid.bounds.top .. (grid.bounds.bottom - 1) do
        // TODO doesn't handle horizontal lines correctly
        let edges =
            seq {
                for x in grid.bounds.left .. (grid.bounds.right - 1) do
                    let v = { x = x; y = y }

                    match grid[v] with
                    | Initial
                    | Colored _ -> yield v
                    | Empty -> ()
            }

        printfn "TODO y = %d, edges = %A" y (edges |> Seq.toList)

    0
