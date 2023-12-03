module Solutions.Day03b

open System
open System.Text.RegularExpressions
open System.Collections.Generic

type Kind =
    | Empty
    | Symbol of char
    | Number of int

type Shape =
    { kind: Kind
      left: int
      right: int
      y: int }

    member this.IsAdjacent(other: Shape) =
        // split on y axis, can't touch
        if (abs (other.y - this.y)) >= 2 then
            false
        // on the same y, check for left and right adjacent
        else if other.y = this.y then
            other.right + 1 = this.left || other.left - 1 = this.right
        // exactly one apart, check for overlap on left and right
        else
            not (other.right + 1 < this.left || other.left - 1 > this.right)

type Grid =
    { data: char array2d }

    member this.Width = this.data.GetLength(0)
    member this.Height = this.data.GetLength(1)

    member this.At x y =
        if x < 0 || x >= this.Width || y < 0 || y >= this.Height then
            { Shape.kind = Empty
              left = x
              right = x
              y = y }
        else
            match this.data[x, y] with
            | '.' ->
                { Shape.kind = Empty
                  left = x
                  right = x
                  y = y }
            | c when (Char.IsDigit c) ->
                // find the edge of the number
                let mutable left = x
                let mutable right = x

                while (left - 1) >= 0 && Char.IsDigit this.data[left - 1, y] do
                    left <- left - 1

                while (right + 1) < this.Width && Char.IsDigit this.data[right + 1, y] do
                    right <- right + 1

                let number =
                    seq {
                        for x in seq { left..right } do
                            this.data[x, y]
                    }
                    |> Seq.toArray
                    |> String
                    |> int

                { Shape.kind = (Number number)
                  left = left
                  right = right
                  y = y }
            | c when not (Char.IsLetter c) ->
                { Shape.kind = Symbol c
                  left = x
                  right = x
                  y = y }
            | c -> raise (Exception(sprintf "invalid char: %c" c))

let doIt (input: string) : int =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    if Set(lines |> Seq.map (fun line -> line.Length)).Count <> 1 then
        raise (Exception "uneven line lengths")

    let lines = lines |> Seq.toArray

    let grid: Grid =
        { data = Array2D.init lines[0].Length lines.Length (fun x y -> lines[y][x]) }

    let shapes =
        Set(
            seq { 0 .. grid.Height - 1 }
            |> Seq.collect (fun y -> seq { 0 .. grid.Width - 1 } |> Seq.map (fun x -> grid.At x y))
            |> Seq.filter (fun s -> s.kind <> Empty)
        )

    let gears = List()
    let numbers = List()

    for shape in shapes do
        match shape.kind with
        | Symbol '*' -> gears.Add shape
        | Number _ -> numbers.Add shape
        | _ -> ()

    gears
    |> Seq.map (fun gear ->
        numbers
        |> Seq.filter (fun n -> gear.IsAdjacent n)
        |> Seq.map (fun n ->
            (match n.kind with
             | Number n -> Some n
             | _ -> None)
                .Value)
        |> Seq.toArray)
    |> Seq.filter (fun numbers -> numbers.Length = 2)
    |> Seq.map (fun numbers -> numbers[0] * numbers[1])
    |> Seq.sum
