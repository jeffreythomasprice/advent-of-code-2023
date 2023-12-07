module Solutions.Day05b

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

type Range =
    { first: int64
      last: int64 }

    member this.apply(other: Range) : Range option * Range list =
        if other.last < this.first || other.first > this.last then
            None, [ other ]
        else
            let intersection =
                { first = max this.first other.first
                  last = min this.last other.last }

            let outside =
                if other.first < intersection.first && other.last > intersection.last then
                    [ { first = other.first
                        last = intersection.first - 1L }
                      { first = intersection.last + 1L
                        last = other.last } ]
                else if other.first < intersection.first then
                    [ { first = other.first
                        last = intersection.first - 1L } ]
                else if other.last > intersection.last then
                    [ { first = intersection.last + 1L
                        last = other.last } ]
                else
                    []

            Some(intersection), outside

let parseSeedsList (lines: string list) : (Range list * string list) option =
    match lines with
    | head :: remainder ->
        if Regex("^seeds:[0-9 ]+$").Match(head).Success then
            let results, _ = parseInts head

            let results =
                seq { 0..2 .. results.Length - 1 }
                |> Seq.map (fun i ->
                    { first = results[i]
                      last = results[i] + results[i + 1] - 1L })
                |> Seq.toList

            Some(results, remainder)
        else
            None
    | [] -> None

type MapRange =
    { source: Range
      destination: Range }

    member this.apply(r: Range) : Range option * Range list =
        match this.source.apply r with
        | Some(intersection), outside ->
            Some(
                { first = intersection.first - this.source.first + this.destination.first
                  last = intersection.last - this.source.first + this.destination.first }
            ),
            outside
        | None, outside -> None, outside

type Map =
    { source: string
      destination: string
      data: MapRange list }

    member this.apply(r: Range) : Range list =
        let mutable results = []
        let mutable input = [ r ]

        for d in this.data do
            let mutable newInput = []

            for i in input do
                let intersection, outside = d.apply i

                match intersection with
                | Some(intersection) -> results <- intersection :: results
                | None -> ()

                newInput <- newInput @ outside

            input <- newInput

        results @ input

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
                        { source =
                            { first = source
                              last = source + count - 1L }
                          destination =
                            { first = destination
                              last = destination + count - 1L } }
                    | _ -> raise (Exception(sprintf "expected lists of length 3, got %A" numbers)))
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
        state <- state |> List.collect map.apply
        printfn "%s = %A" current state

    state |> Seq.map _.first |> Seq.min
