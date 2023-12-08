module Solutions.Day07a

open System
open System.Text.RegularExpressions
open System.Collections.Generic

type HandKind =
    | FiveOfAKind = 1
    | FourOfAKind = 2
    | FullHouse = 3
    | ThreeOfAKind = 4
    | TwoPair = 5
    | OnePair = 6
    | HighCard = 7

type Hand =
    { data: char array }

    member this.kind =
        let counts =
            this.data
            |> Seq.fold
                (fun (results: Map<char, int>) c ->
                    match results.TryGetValue c with
                    | true, count -> results.Add(c, count + 1)
                    | false, _ -> results.Add(c, 1))
                Map.empty

        let counts =
            counts.Keys
            |> Seq.map (fun c -> c, counts[c])
            |> Seq.toList
            |> List.sortByDescending (fun (_, count) -> count)

        match counts with
        | [ (_, 5) ] -> HandKind.FiveOfAKind
        | [ (_, 4); (_, 1) ] -> HandKind.FourOfAKind
        | [ (_, 3); (_, 2) ] -> HandKind.FullHouse
        | [ (_, 3); (_, 1); (_, 1) ] -> HandKind.ThreeOfAKind
        | [ (_, 2); (_, 2); (_, 1) ] -> HandKind.TwoPair
        | [ (_, 2); (_, 1); (_, 1); (_, 1) ] -> HandKind.OnePair
        | _ -> HandKind.HighCard

    member this.compareTo(other: Hand) : int =
        if this.kind < other.kind then
            -1
        else if this.kind > other.kind then
            1
        else
            match Array.zip this.data other.data |> Array.tryFind (fun (a, b) -> a <> b) with
            | Some(a, b) ->
                let chars = "AKQJT98765432"
                (chars.IndexOf a) - (chars.IndexOf b)
            | None -> 0

type Input = { hand: Hand; bid: int }

let doIt (input: string) : int =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    lines
    |> Seq.map (fun line -> Regex("([AKQJT98765432]{5})\s+([0-9]+)").Match(line))
    |> Seq.filter (fun m -> m.Success)
    |> Seq.map (fun m ->
        { hand = { data = m.Groups[1].Value.ToCharArray() }
          bid = m.Groups[2].Value |> int })
    |> Seq.toList
    |> List.sortWith (fun a b -> b.hand.compareTo a.hand)
    |> List.indexed
    |> List.map (fun (i, x) -> (i + 1) * x.bid)
    |> List.sum
