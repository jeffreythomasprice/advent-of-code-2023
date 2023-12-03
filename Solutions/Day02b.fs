module Solutions.Day02b

open System
open System.Text.RegularExpressions
open System.Collections.Generic

type Color = { count: int; color: string }

type Draw = Color array

type Game = { id: int; draws: Draw array }

let minimumColorsForGame (game: Game) =
    let results = Dictionary<string, int>()

    for draw in game.draws do
        for color in draw do
            results[color.color] <-
                match results.TryGetValue(color.color) with
                | true, current -> if current < color.count then color.count else current
                | false, _ -> color.count

    results

let gamePower (game: Game) =
    (minimumColorsForGame game).Values |> Seq.reduce (fun x y -> x * y)

let doIt (input: string) : int =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    let games =
        lines
        |> Seq.map (fun line -> Regex("^Game\s*([0-9]+):\s*([a-z0-9,;\s]+)\s*$").Match(line))
        |> Seq.filter (fun m -> m.Success)
        |> Seq.map (fun m ->
            let draws: Draw array =
                m.Groups[2].Value.Split(";")
                |> Array.map (fun draw ->
                    draw.Trim().Split(",")
                    |> Array.toSeq
                    |> Seq.map (fun item -> Regex("([0-9]+)\s+([^\s]+)").Match(item))
                    |> Seq.filter (fun item -> item.Success)
                    |> Seq.map (fun item ->
                        { Color.count = item.Groups[1].Value |> int
                          color = item.Groups[2].Value })
                    |> Seq.toArray)

            { Game.id = m.Groups[1].Value |> int
              draws = draws })

    games |> Seq.map (fun game -> gamePower game) |> Seq.sum
