module Solutions.Day04b

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Linq

type Game = int array

type Card =
    { id: int
      winningNumbers: Game
      choices: Game }

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

            { Card.id = (m.Groups[1].Value |> int)
              winningNumbers = parseNumbers m.Groups[2].Value
              choices = parseNumbers m.Groups[3].Value })

    let score (card: Card) =
        (card.winningNumbers |> Seq.filter (fun x -> card.choices |> Seq.contains x))
            .Count()

    let cardsById = cards |> Seq.map (fun x -> x.id, x) |> dict

    let rec newCards (card: Card) : Card seq =
        let newStuff =
            seq { card.id + 1 .. card.id + (score card) }
            |> Seq.collect (fun id -> cardsById[id] |> newCards)

        Seq.append (Seq.singleton card) newStuff

    (cards |> Seq.collect newCards).Count()
