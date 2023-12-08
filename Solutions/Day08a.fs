module Solutions.Day08a

open System
open System.Text.RegularExpressions
open System.Collections.Generic

type Node =
    { name: string
      left: string
      right: string }

let doIt (input: string) : int =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    let instructions :: nodes = lines |> Seq.toList

    let instructions = Seq.initInfinite (fun i -> instructions[i % instructions.Length])

    let nodes =
        nodes
        |> Seq.map (fun n -> Regex("^([a-zA-Z]+)\s*=\s*\(([a-zA-Z]+)\s*,\s*([a-zA-Z]+)\)$").Match(n))
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

    let mutable current = "AAA"
    let mutable count = 0

    instructions
    |> Seq.find (fun inst ->
        let next =
            match inst with
            | 'L' -> nodes[current].left
            | 'R' -> nodes[current].right
            | _ -> raise (Exception(sprintf "invalid instruction: %c" inst))

        printfn "i=%d, %A -> %A" count current next

        count <- count + 1
        current <- next

        current = "ZZZ")
    |> ignore

    count
