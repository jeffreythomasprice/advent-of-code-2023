module Solutions.Day06a

open System
open System.Text.RegularExpressions
open System.Collections.Generic

let parseLine (prefix: string) (line: string) =
    if not (line.StartsWith prefix) then
        raise (Exception(sprintf "expected prefix %s" prefix))

    (line.Substring prefix.Length).Split(" ")
    |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))
    |> Seq.map (fun x -> x |> int)
    |> Seq.toList

let getDistanceForHoldTime (totalTime: int) (holdTime: int) : int =
    let remainingTime = totalTime - holdTime
    holdTime * remainingTime

// https://en.wikipedia.org/wiki/Quadratic_equation
// a*x^2 + b*x + c = 0
let quadratic a b c =
    let temp = sqrt (b * b - 4.0 * a * c)
    (-b - temp) / (2.0 * a), (-b + temp) / (2.0 * a)

type Input =
    { time: int
      distance: int }

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
        let a, b = quadratic 1 -this.time this.distance

        // round to the nearest ints
        let a, b = (ceil a) |> int, (floor b) |> int

        // but then sometimes it's off by one, so just iterate up and down until we have a winner
        (seq { a .. this.time } |> Seq.find this.isWinningDistance),
        (seq { b .. -1 .. 0 } |> Seq.find this.isWinningDistance)

let doIt (input: string) : int =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    let times, distances =
        match lines |> Seq.toList with
        | [ a; b ] -> (parseLine "Time:" a), (parseLine "Distance:" b)
        | _ -> raise (Exception "expected exactly two lines")

    let input =
        Seq.zip times distances
        |> Seq.map (fun (time, distance) -> { time = time; distance = distance })

    input
    |> Seq.map (fun i ->
        let a, b = i.solution
        b - a + 1)
    |> Seq.fold (fun a b -> a * b) 1
