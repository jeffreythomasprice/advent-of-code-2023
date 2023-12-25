module Solutions.Day20a

open System
open System.Text.RegularExpressions
open System.Collections.Generic

type NodeType =
    | Button
    | Broadcast
    | FlipFlop
    | Conjunction

type Node =
    { typ: NodeType
      name: string
      targets: string list
      inputs: string list }

type Puzzle = { nodes: Map<string, Node> }

type Signal =
    | Low
    | High

type FlipFlopState =
    | Off
    | On

    static member initial = Off

type ConjunctionState =
    { data: Map<string, Signal> }

    static member initial (puzzle: Puzzle) (name: string) =
        { data = puzzle.nodes[name].inputs |> Seq.map (fun name -> name, Low) |> Map }

type Message =
    { signal: Signal
      source: string
      destination: string }

type NodeState =
    | FlipFlopState of FlipFlopState
    | ConjunctionState of ConjunctionState
    | BroadcastState
    | ButtonState

    static member initial (puzzle: Puzzle) (name: string) =
        let node = puzzle.nodes[name]

        match node.typ with
        | FlipFlop -> FlipFlopState(FlipFlopState.initial)
        | Conjunction -> ConjunctionState(ConjunctionState.initial puzzle name)
        | Broadcast -> BroadcastState
        | Button -> ButtonState

    member this.update(message: Message) : NodeState * Signal option =
        match this, message.signal with
        | FlipFlopState Off, Low -> FlipFlopState On, Some High
        | FlipFlopState On, Low -> FlipFlopState Off, Some Low
        | FlipFlopState prev, High -> FlipFlopState prev, None
        | ConjunctionState memory, _ ->
            let updated = { data = memory.data |> Map.add message.source message.signal }

            let outputSignal =
                if updated.data |> Map.toSeq |> Seq.forall (fun (_, signal) -> signal = High) then
                    Low
                else
                    High

            ConjunctionState updated, Some outputSignal
        | BroadcastState, signal -> BroadcastState, Some signal
        | ButtonState, _ -> failwith "buttons should be receiving signals"

type SignalCounts =
    { low: int
      high: int }

    static member zero = { low = 0; high = 0 }

    member this.add(signal: Signal) : SignalCounts =
        match signal with
        | Low -> { low = this.low + 1; high = this.high }
        | High -> { low = this.low; high = this.high + 1 }

    static member (+)(a: SignalCounts, b: SignalCounts) : SignalCounts =
        { low = a.low + b.low
          high = a.high + b.high }

    static member (-)(a: SignalCounts, b: SignalCounts) : SignalCounts =
        { low = a.low - b.low
          high = a.high - b.high }

    static member (*)(a: SignalCounts, b: int) : SignalCounts = { low = a.low * b; high = a.high * b }

type State =
    { data: Map<string, NodeState> }

    static member initial(puzzle: Puzzle) =
        { data =
            puzzle.nodes.Keys
            |> Seq.map (fun name -> name, NodeState.initial puzzle name)
            |> Map }

    member this.update (puzzle: Puzzle) (message: Message) : State * Message list =
        match this.data |> Map.tryFind message.destination with
        | Some currentState ->
            let newState, outputSignal = currentState.update message

            let node = puzzle.nodes[message.destination]

            let outputSignals =
                match outputSignal with
                | Some outputSignal ->
                    node.targets
                    |> Seq.map (fun target ->
                        { signal = outputSignal
                          source = message.destination
                          destination = target })
                    |> Seq.toList
                | None -> []

            { data = this.data |> Map.add message.destination newState }, outputSignals
        | None -> this, []

    member this.pressButton(puzzle: Puzzle) : State * SignalCounts =
        let rec handleQueue (queue: Message list) (state: State) : State * SignalCounts =
            match queue with
            | [] -> state, SignalCounts.zero
            | next :: queue ->
                let updated, messages = state.update puzzle next
                let resultState, count = handleQueue (queue @ messages) updated
                resultState, count.add next.signal

        handleQueue
            [ { signal = Low
                source = "button"
                destination = "broadcaster" } ]
            this

    member this.pressButtonManyTimes (puzzle: Puzzle) (times: int) : State * SignalCounts =
        let rec dumb (state: State) (times: int) : State * SignalCounts =
            let state, count = state.pressButton puzzle
            let times = times - 1
            if times = 0 then state, count else dumb state times

        let rec smart (state: State) (times: int) (history: (State * SignalCounts) list) : State * SignalCounts =
            let updatedState, count = state.pressButton puzzle
            let times = times - 1

            let count =
                match history with
                | (_, prev) :: _ -> count + prev
                | _ -> count

            if times = 0 then
                updatedState, count
            else
                match history |> List.tryFindIndex (fun (state, _) -> state = updatedState) with
                | Some i ->
                    let _, countAtI = history[i]
                    let deltaPerLoop = count - countAtI
                    let loopLength = i + 1
                    let numLoops = times / loopLength
                    let count = count + deltaPerLoop * numLoops
                    let times = times - numLoops * loopLength

                    if times > 0 then
                        let updatedState, updatedCount = dumb updatedState times
                        updatedState, updatedCount + count
                    else
                        updatedState, count
                | None -> smart updatedState times ((updatedState, count) :: history)

        smart this times [ this, SignalCounts.zero ]

let doIt (input: string) : int =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))

    let input =
        lines
        |> Seq.map (fun line ->
            let m =
                Regex("^([%&])?([a-zA-Z]+)\s*->\s*([a-zA-Z]+(:?\s*,\s*[a-zA-Z]+)*)$")
                    .Match(line)

            if not m.Success then
                failwith (sprintf "invalid line: %s" line)

            let typ = m.Groups[1].Value
            let name = m.Groups[2].Value

            let typ =
                match typ, name with
                | "%", _ -> FlipFlop
                | "&", _ -> Conjunction
                | _, "broadcaster" -> Broadcast
                | _, _ -> failwith (sprintf "unhandled node type: %s" line)

            let targets =
                m.Groups[3].Value.Split(",") |> Seq.map (fun s -> s.Trim()) |> Seq.toList

            typ, name, targets)

    let input =
        { nodes =
            input
            |> Seq.map (fun (typ, name, targets) ->
                let inputs =
                    input
                    |> Seq.filter (fun (_, _, targets) -> targets |> List.contains name)
                    |> Seq.map (fun (_, name, _) -> name)
                    |> Seq.toList

                name,
                { typ = typ
                  name = name
                  targets = targets
                  inputs = inputs })
            |> Map }

    let state = State.initial input
    let _, count = state.pressButtonManyTimes input 1000
    count.low * count.high
