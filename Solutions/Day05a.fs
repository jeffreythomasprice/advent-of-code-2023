module Solutions.Day05a

open System
open System.Text.RegularExpressions
open System.Collections.Generic

let parseInt (s: string) : (int64 * string) option =
    let m = Regex("([0-9]+)(.*)").Match(s.Trim())

    if m.Success then
        Some(m.Groups[1].Value |> int64, m.Groups[2].Value)
    else
        None

let parseInts (s: string) : int64 list * string =
    let rec f s =
        match parseInt s with
        | Some(value, remainder) ->
            let otherResults, remainder = f remainder
            value :: otherResults, remainder
        | None -> [], s

    f s

let parseLinesOfInts (lines: string list) : int64 list list * string list =
    let rec f lines =
        match lines with
        | head :: remainder ->
            match parseInts head with
            | [], _ -> [], lines
            | (result: int64 list), _ ->
                let otherResults, remainder = f remainder
                result :: otherResults, remainder
        | [] -> [], lines

    f lines

let parseSeedsList (lines: string list) : (int64 list * string list) option =
    match lines with
    | head :: remainder ->
        if Regex("^seeds:[0-9 ]+$").Match(head).Success then
            let results, _ = parseInts head
            Some(results, remainder)
        else
            None
    | [] -> None

type Range =
    { source: int64
      destination: int64
      count: int64 }

    member this.contains x =
        x >= this.source && x <= this.source + this.count

    member this.apply x =
        if this.contains x then
            Some(x - this.source + this.destination)
        else
            None

type Map =
    { source: string
      destination: string
      data: Range list }

    member this.apply x =
        // TODO actually check all the ranges
        match (this.data |> Seq.tryFind (fun d -> d.contains x)) with
        | Some(r) -> (r.apply x).Value
        | _ -> x

let parseMap (lines: string list) : (Map * string list) option =
    match lines with
    | head :: remainder ->
        let m = Regex("^([^-]+)\-to\-([^-]+) map:$").Match(head.Trim())

        if m.Success then
            let source = m.Groups[1].Value
            let destination = m.Groups[2].Value
            let numbers, remainder = parseLinesOfInts remainder

            let data =
                numbers
                |> Seq.map (fun row ->
                    match row with
                    | [ destination; source; count ] ->
                        { source = source
                          destination = destination
                          count = count }
                    | _ -> raise (Exception(sprintf "expeted lists of length 3, got %A" numbers)))
                |> Seq.toList

            Some(
                { source = source
                  destination = destination
                  data = data },
                remainder
            )
        else
            None
    | [] -> None

let parseMapsList (lines: string list) : Map list * string list =
    let rec f lines =
        match parseMap lines with
        | Some(result, remainder) ->
            let otherResults, remainder = f remainder
            result :: otherResults, remainder
        | None -> [], lines

    f lines

let doIt (input: string) : int64 =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    let seeds, lines = (parseSeedsList (lines |> Seq.toList)).Value
    printfn "seeds = %A" seeds

    let mutable maps, _ = parseMapsList lines

    for m in maps do
        printfn "map = %A" m

    let mutable names =
        maps |> Seq.collect (fun m -> [ m.source; m.destination ]) |> set

    printfn "names = %A" names

    let mutable current = "seed"
    let mutable state = seeds
    printfn "%s = %A" current state

    while current <> "location" do
        if not (names.Contains current) then
            raise (Exception(sprintf "missing name %s" current))
        // get the map we care about
        let map = maps |> List.find (fun m -> m.source = current)
        // remove this key from the list of things we care about
        names <- names |> Set.remove current
        maps <- maps |> List.filter (fun m -> m.source <> current)
        // actually apply this map to each element of our state
        current <- map.destination
        state <- state |> List.map map.apply
        printfn "%s = %A" current state

    state |> Seq.min
