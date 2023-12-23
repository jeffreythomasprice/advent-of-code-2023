module Solutions.Day19b

open System
open System.Text.RegularExpressions
open System.Collections.Generic

type Range = { first: int; last: int }

type SparseSet = Range list

type Operator =
    | LessThan
    | GreaterThan

type RuleApplication<'t> = { matches: 't; doesntMatch: 't }

type PossibleParts =
    { data: Map<string, SparseSet> }

    member this.union(other: PossibleParts) : PossibleParts =
        let rec subtract (a: Range) (b: Range list) : Range list =
            match b with
            | b :: remainder ->
                let aSubB =
                    if a.last < b.first || a.first > b.last then
                        [ a ]
                    else if a.first >= b.first && a.last <= b.last then
                        []
                    else if a.first < b.first && a.last > b.last then
                        [ { first = a.first; last = b.first - 1 }
                          { first = b.last + 1; last = a.last } ]
                    else if a.first < b.first then
                        [ { first = a.first; last = b.first - 1 } ]
                    else
                        [ { first = b.last + 1; last = a.last } ]

                aSubB |> List.collect (fun r -> subtract a remainder)
            | [] -> [ a ]

        let subtract (a: SparseSet) (b: SparseSet) : SparseSet =
            a |> List.collect (fun a -> subtract a b)

        let union (a: SparseSet) (b: SparseSet) : SparseSet = a @ (subtract b a)

        { data =
            Set.union (this.data.Keys |> Set) (other.data.Keys |> Set)
            |> Seq.map (fun name -> name, (union this.data[name] other.data[name]))
            |> Map }

    member this.count() =
        this.data
        |> Map.toSeq
        |> Seq.map (fun (_, x) -> x |> Seq.map (fun x -> (x.last |> int64) - (x.first |> int64) + 1L) |> Seq.sum)
        |> Seq.fold (fun a b -> a * b) 1L

type Rule =
    { name: string
      op: Operator
      number: int
      target: string }

    member this.applyRange(input: Range) : SparseSet RuleApplication =
        match this.op with
        | LessThan ->
            if input.last < this.number then
                { matches = [ input ]
                  doesntMatch = List.empty }
            else if input.first >= this.number then
                { matches = List.empty
                  doesntMatch = [ input ] }
            else
                { matches =
                    [ { first = input.first
                        last = this.number - 1 } ]
                  doesntMatch =
                    [ { first = this.number
                        last = input.last } ] }
        | GreaterThan ->
            if input.first > this.number then
                { matches = [ input ]
                  doesntMatch = List.empty }
            else if input.last <= this.number then
                { matches = List.empty
                  doesntMatch = [ input ] }
            else
                { matches =
                    [ { first = this.number + 1
                        last = input.last } ]
                  doesntMatch =
                    [ { first = input.first
                        last = this.number } ] }

    member this.applySet(input: SparseSet) : SparseSet RuleApplication =
        input
        |> Seq.fold
            (fun results range ->
                let newResults = this.applyRange range

                { matches = newResults.matches @ results.matches
                  doesntMatch = newResults.doesntMatch @ results.doesntMatch })
            { matches = []; doesntMatch = [] }

    member this.applyParts(input: PossibleParts) : PossibleParts RuleApplication =
        let result = this.applySet input.data[this.name]

        { matches = { data = input.data |> Map.add this.name result.matches }
          doesntMatch = { data = input.data |> Map.add this.name result.doesntMatch } }

let combine (map: Map<string, PossibleParts>) (name: string) (input: PossibleParts) =
    let update =
        match map |> Map.tryFind name with
        | Some existing -> input.union existing
        | None -> input

    map |> Map.add name update

type Workflow =
    { rules: Rule list
      defaultTarget: string }

    // returns the mapping of next workflow names to the set of parts that go there
    member this.eval(part: PossibleParts) : Map<string, PossibleParts> =
        let results, didntMatchAny =
            this.rules
            |> Seq.fold
                (fun (results: (Map<string, PossibleParts> * PossibleParts)) rule ->
                    let results, part = results
                    let ruleResults = rule.applyParts part
                    let results = combine results rule.target ruleResults.matches
                    (results, ruleResults.doesntMatch))
                (Map.empty, part)

        combine results this.defaultTarget didntMatchAny

let doIt (input: string) : int64 =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    let workflows =
        lines
        |> Seq.map (fun line -> Regex("^([a-zA-Z]+){([[a-zA-Z0-9<>:,]+)}$").Match(line))
        |> Seq.map (fun m ->
            let name = m.Groups[1].Value
            let rules = m.Groups[2].Value

            let rules = rules.Split(",") |> Array.toList
            let defaultTarget = rules |> List.last

            let rules =
                (rules |> List.take (rules.Length - 1))
                |> Seq.map (fun s ->
                    let result = Regex("^([a-zA-Z]+)([<>])([0-9]+)+:([a-zA-Z]+)$").Match(s)

                    if not result.Success then
                        failwith (sprintf "expected matching rule list, got %s" s)

                    let name = result.Groups[1].Value
                    let op = result.Groups[2].Value
                    let number = result.Groups[3].Value |> int
                    let target = result.Groups[4].Value

                    let op =
                        match op with
                        | "<" -> LessThan
                        | ">" -> GreaterThan
                        | _ -> failwith (sprintf "invalid op: %s" op)

                    { name = name
                      op = op
                      number = number
                      target = target })
                |> Seq.toList

            name,
            { rules = rules
              defaultTarget = defaultTarget })
        |> Map

    let rec count (workflowName: string) (parts: PossibleParts) : int64 =
        let workflow = workflows[workflowName]
        let results = workflow.eval parts

        (results.TryFind "A"
         |> Option.map (fun parts -> parts.count ())
         |> Option.defaultValue 0L)
        + (results
           |> Map.toSeq
           |> Seq.filter (fun (name, _) -> name <> "A" && name <> "R")
           |> Seq.map (fun (name, child) -> count name child)
           |> Seq.sum)

    // TODO too high, 169280328932458

    count
        "in"
        { data =
            [ ("x", [ { first = 1; last = 4000 } ])
              ("m", [ { first = 1; last = 4000 } ])
              ("a", [ { first = 1; last = 4000 } ])
              ("s", [ { first = 1; last = 4000 } ]) ]
            |> Map }
