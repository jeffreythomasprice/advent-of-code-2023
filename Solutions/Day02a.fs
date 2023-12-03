module Solutions.Day02a

open System
open System.Text.RegularExpressions
open System.Collections.Generic

type Color = { count: int; color: string }

type Draw = Color array

type Game = { id: int; draws: Draw array }

let isColorValid (color: Color) (count: int) = color.count <= count

let isDrawValid (draw: Draw) (requiredCounts: IDictionary<string, int>) =
    not (
        draw
        // look for any that are invalid
        |> Array.exists (fun x ->
            let required = requiredCounts[x.color]
            // test the negative case, true when we have an invalid count
            x.count > required)
    )

let isGameValid (game: Game) (requiredCounts: IDictionary<string, int>) =
    not (
        game.draws
        // are there are any invalid ones?
        |> Array.exists (fun x ->
            // looking for invalid ones
            not (isDrawValid x requiredCounts))
    )

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

    let requiredCounts = dict [ ("red", 12); ("green", 13); ("blue", 14) ]

    games
    |> Seq.filter (fun game -> isGameValid game requiredCounts)
    |> Seq.map (fun game -> game.id)
    |> Seq.sum
