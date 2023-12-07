module Solutions.Day06b

open System
open System.Text.RegularExpressions
open System.Collections.Generic

let parseLine (prefix: string) (line: string) =
    if not (line.StartsWith prefix) then
        raise (Exception(sprintf "expected prefix %s" prefix))

    String.Join(
        "",
        ((line.Substring prefix.Length).Split(" ")
         |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))
         |> Seq.toArray)
    )
    |> int64

let getDistanceForHoldTime (totalTime: int64) (holdTime: int64) : int64 =
    let remainingTime = totalTime - holdTime
    holdTime * remainingTime

// https://en.wikipedia.org/wiki/Quadratic_equation
// a*x^2 + b*x + c = 0
let quadratic a b c =
    let temp = sqrt (b * b - 4.0 * a * c)
    (-b - temp) / (2.0 * a), (-b + temp) / (2.0 * a)

type Input =
    { time: int64
      distance: int64 }

    member this.getDistanceForHoldTime holdTime =
        getDistanceForHoldTime this.time holdTime

    member this.isWinningDistance holdTime =
        (this.getDistanceForHoldTime holdTime) > this.distance

    member this.solution =
        (*
            targetDistance = (totalTime - holdTime) * holdTime
            targetDistance = totalTime * holdTime - holdTime * holdTime
            holdTime^2 - totalTime * holdTime + targetDistance = 0
        *)
        let a, b = quadratic 1 (-this.time |> float) (this.distance |> float)

        // round to the nearest ints
        let a, b = (ceil a) |> int64, (floor b) |> int64

        // but then sometimes it's off by one, so just iterate up and down until we have a winner
        (seq { a .. this.time } |> Seq.find this.isWinningDistance),
        (seq { b .. -1L .. 0 } |> Seq.find this.isWinningDistance)

let doIt (input: string) : int64 =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    let time, distance =
        match lines |> Seq.toList with
        | [ a; b ] -> (parseLine "Time:" a), (parseLine "Distance:" b)
        | _ -> raise (Exception "expected exactly two lines")

    let input = { time = time; distance = distance }

    let a, b = input.solution
    b - a + 1L
