module Solutions.Day19a

open System
open System.Text.RegularExpressions
open System.Collections.Generic

type Part = Map<string, int>

type Operator =
    | LessThan
    | GreaterThan

type Rule =
    { name: string
      op: Operator
      number: int
      target: string }

type Workflow =
    { rules: Rule list
      defaultTarget: string }

    member this.eval(part: Part) : string =
        match
            this.rules
            |> Seq.tryFind (fun rule ->
                let partValue = part[rule.name]

                match rule.op with
                | LessThan -> partValue < rule.number
                | GreaterThan -> partValue > rule.number)
        with
        | Some rule -> rule.target
        | None -> this.defaultTarget

let doIt (input: string) : int =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    let workflows =
        lines
        |> Seq.map (fun line -> Regex("^([a-zA-Z]+){([[a-zA-Z0-9<>:,]+)}$").Match(line))
        |> Seq.takeWhile (fun m -> m.Success)
        |> Seq.toList

    let remaining = lines |> Seq.skip (workflows.Length)

    let parts =
        remaining
        |> Seq.map (fun line -> Regex("^{(([a-zA-Z0-9=,]+))}$").Match(line))
        |> Seq.takeWhile (fun m -> m.Success)
        |> Seq.toList

    let remaining = remaining |> Seq.skip (parts.Length)

    if not (remaining |> Seq.isEmpty) then
        failwith (sprintf "unmatched line: %s" (remaining |> Seq.head))

    let workflows =
        workflows
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

    let parts =
        parts
        |> Seq.map (fun m ->
            let s = m.Groups[1].Value

            s.Split(",")
            |> Seq.map (fun s ->
                let m = Regex("^([a-zA-Z]+)=([0-9]+)$").Match(s)

                if not m.Success then
                    failwith (sprintf "invalid part: %s" s)

                let name = m.Groups[1].Value
                let number = m.Groups[2].Value |> int
                name, number)
            |> Map)
        |> Seq.toList

    parts
    |> Seq.map (fun part ->
        let mutable current = "in"

        while current <> "A" && current <> "R" do
            current <- workflows[current].eval part

        part, current)
    |> Seq.filter (fun (_, result) -> result = "A")
    |> Seq.map (fun (part, _) -> part.Values |> Seq.sum)
    |> Seq.sum
