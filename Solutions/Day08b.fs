module Solutions.Day08b

open System
open System.Text.RegularExpressions
open System.Collections.Generic

type Node =
    { name: string
      left: string
      right: string }

// https://en.wikipedia.org/wiki/Greatest_common_divisor
// specifically, euclidean algorithm
let rec gcd (a: int64) (b: int64) =
    match a, b with
    | x, 0L -> x
    | 0L, x -> x
    | a, b -> gcd b (a % b)

// https://en.wikipedia.org/wiki/Least_common_multiple
let lcm (a: int64) (b: int64) = a * (b / (gcd a b))

let doIt (input: string) : int64 =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    let instructions, nodes =
        match lines |> Seq.toList with
        | h :: r -> h, r
        | _ -> raise (Exception "expected multiple lines of input")

    let instructions = Seq.initInfinite (fun i -> instructions[i % instructions.Length])

    let nodes =
        nodes
        |> Seq.map (fun n -> Regex("^([a-zA-Z0-9]+)\s*=\s*\(([a-zA-Z0-9]+)\s*,\s*([a-zA-Z0-9]+)\)$").Match(n))
        |> Seq.filter (fun m -> m.Success)
        |> Seq.map (fun m ->
            let name = m.Groups[1].Value
            let left = m.Groups[2].Value
            let right = m.Groups[3].Value

            name,
            { name = name
              left = left
              right = right })
        |> dict

    let nodeSeq start =
        let mutable current = start

        instructions
        |> Seq.map (fun inst ->
            let next =
                match inst with
                | 'L' -> nodes[current].left
                | 'R' -> nodes[current].right
                | _ -> raise (Exception(sprintf "invalid instruction: %c" inst))

            current <- next
            current)

    let startNodes = nodes.Keys |> Seq.filter (fun node -> node.EndsWith "A")

    let mutable current = startNodes |> Seq.map (fun node -> node, node) |> Map

    startNodes
    |> Seq.map nodeSeq
    |> Seq.map (fun seq ->
        let possibleEndPoints =
            seq |> Seq.indexed |> Seq.filter (fun (_, node) -> node.EndsWith("Z"))

        let endpoints =
            possibleEndPoints |> Seq.take 2 |> Seq.map (fun (i, _) -> i) |> Seq.toList

        let a, b =
            match endpoints with
            | [ a; b ] -> a, b
            | _ -> raise (Exception "expected exactly two")

        let repeatLen = b - a
        repeatLen |> int64)
    |> Seq.fold lcm 1
