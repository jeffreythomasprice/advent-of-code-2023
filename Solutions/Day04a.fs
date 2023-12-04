module Solutions.Day04a

open System
open System.Text.RegularExpressions
open System.Collections.Generic

type Game = int array

type Card = { winningNumbers: Game; choices: Game }

let doIt (input: string) : int =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    let cards =
        lines
        |> Seq.map (fun line ->
            Regex("^Card\s+([0-9]+)\s*:\s*((?:[0-9]+\s*)*)\s*\|\s*((?:[0-9]+\s*)*)\s*$")
                .Match(line))
        |> Seq.filter (fun m -> m.Success)
        |> Seq.map (fun m ->
            let parseNumbers (s: string) =
                s.Split [| '\t'; ' ' |]
                |> Seq.filter (fun x -> not (String.IsNullOrEmpty x))
                |> Seq.map (fun x -> x |> int)
                |> Seq.toArray

            { Card.winningNumbers = parseNumbers m.Groups[2].Value
              choices = parseNumbers m.Groups[3].Value })

    let score (card: Card) =
        let mutable result = 0

        for _ in card.winningNumbers |> Seq.filter (fun x -> card.choices |> Seq.contains x) do
            result <- result + 1

        if result = 0 then 0 else 1 <<< (result - 1)

    cards |> Seq.sumBy score
