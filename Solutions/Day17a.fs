module Solutions.Day17a

open System
open System.Text.RegularExpressions
open System.Collections.Generic

type Vector =
    { x: int
      y: int }

    static member (+)(a: Vector, b: Vector) : Vector = { x = a.x + b.x; y = a.y + b.y }

    static member (-)(a: Vector, b: Vector) : Vector = { x = a.x - b.x; y = a.y - b.y }

    member this.distanceTo(other: Vector) : int =
        let d = other - this
        (abs d.x) + (abs d.y)

type PuzzleInput =
    { width: int
      height: int
      data: int array2d }

    member this.inBounds(location: Vector) =
        location.x >= 0
        && location.x < this.width
        && location.y >= 0
        && location.y < this.height

    member this.Item
        with get (location: Vector) = this.data[location.y, location.x]

type Direction =
    | North
    | South
    | East
    | West

    member this.vector =
        match this with
        | North -> { x = 0; y = -1 }
        | South -> { x = 0; y = 1 }
        | East -> { x = 1; y = 0 }
        | West -> { x = -1; y = 0 }

    member this.left =
        match this with
        | North -> West
        | South -> East
        | East -> North
        | West -> South

    member this.right =
        match this with
        | North -> East
        | South -> West
        | East -> South
        | West -> North

    member this.backwards =
        match this with
        | North -> South
        | South -> North
        | East -> West
        | West -> East

type Agent =
    { location: Vector
      direction: Direction
      stepsTakenInThatDirection: int
      totalScore: int
      history: Vector list }

type Move =
    { agent: Agent
      newDirection: Direction
      newLocation: Vector }

    member this.newScore(puzzle: PuzzleInput) : int =
        this.agent.totalScore + puzzle[this.newLocation]

let takeMove (puzzle: PuzzleInput) (agent: Agent) (move: Move) : Agent =
    { location = move.newLocation
      direction = move.newDirection
      stepsTakenInThatDirection =
        if agent.direction = move.newDirection then
            agent.stepsTakenInThatDirection + 1
        else
            1
      totalScore = agent.totalScore + puzzle[move.newLocation]
      history = move.newLocation :: agent.history }

type ShortestPathNodeWithMove =
    { location: Vector
      direction: Direction
      stepsTakenInThatDirection: int }

type ShortestPathNode =
    | Start of Vector
    | Move of ShortestPathNodeWithMove

let shortestPathSolver (puzzle: PuzzleInput) (start: Vector) : ShortestPathNode -> (ShortestPathNode * int) option =
    // https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm

    // value is the score and the next node
    let mutable state: Map<ShortestPathNode, (int * ShortestPathNode option)> =
        seq {
            for x in 0 .. (puzzle.width - 1) do
                for y in 0 .. (puzzle.height - 1) do
                    let location = { x = x; y = y }

                    if location = start then
                        // there's only one node at the start, we can't have arrived here from anywhere else
                        yield Start location, (0, None)
                    else
                        // for all nother nodes we could enter that location from any direction, and we could have been travelling in that
                        // direction for 1 to 3 steps
                        for dir in [ North; South; East; West ] do
                            for steps in 1..3 do
                                yield
                                    Move
                                        { location = location
                                          direction = dir
                                          stepsTakenInThatDirection = steps },
                                    (Int32.MaxValue, None)
        }
        |> Map

    let mutable remaining = Map.keys state |> Set

    while not remaining.IsEmpty do
        let current =
            remaining
            |> Seq.minBy (fun location ->
                let score, _ = state[location]
                score)

        remaining <- remaining |> Set.remove current

        let moves =
            match current with
            // if we're at the special start node, then the next nodes all the locations we go from here,
            // where we'll now have been moving for 1 step
            | Start location ->
                [ North; South; East; West ]
                |> Seq.map (fun dir ->
                    { location = location + dir.vector
                      direction = dir
                      stepsTakenInThatDirection = 1 })
            // otherwise, we can either turn left or right,
            // and if we haven't been moving straight for too long we can keep going that direction
            | Move move ->
                let sides = [ move.direction.left; move.direction.right ]

                let directions =
                    if move.stepsTakenInThatDirection < 3 then
                        move.direction :: sides
                    else
                        sides

                directions
                |> Seq.map (fun dir ->
                    { location = move.location + dir.vector
                      direction = dir
                      stepsTakenInThatDirection =
                        if dir = move.direction then
                            move.stepsTakenInThatDirection + 1
                        else
                            1 })

        let moves =
            moves
            // filter that by only those new nodes that are actually on the map
            |> Seq.filter (fun move -> puzzle.inBounds move.location)
            // special case, don't ever go back to start it doesn't have the directional kind of nodes
            |> Seq.filter (fun move -> move.location <> start)

        let currentScore, _ = state[current]

        for move in moves do
            let proposedScore = currentScore + puzzle[move.location]
            let neighborScore, _ = state[Move move]

            if proposedScore < neighborScore then
                state <- state |> Map.add (Move move) (proposedScore, Some current)

    fun (node: ShortestPathNode) ->
        match state.TryFind node with
        | Some(score, Some destination) -> Some(destination, score)
        | _ -> None

let doIt (input: string) : int =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))
        |> Seq.toList

    let height = lines.Length
    let width = lines[0].Length

    if not (lines |> List.forall (fun row -> row.Length = width)) then
        failwith "not all row are the same length"

    let puzzle =
        { width = width
          height = height
          data = Array2D.init height width (fun y x -> lines[y][x] |> string |> int) }

    let start = { x = 0; y = 0 }
    let startingDirections = [ East; South ]
    let goal = { x = width - 1; y = height - 1 }

    let shortestPath = shortestPathSolver puzzle start

    [ North; South; East; West ]
    |> Seq.collect (fun dir ->
        seq { 1..3 }
        |> Seq.map (fun steps ->
            Move
                { location = goal
                  direction = dir
                  stepsTakenInThatDirection = steps }))
    |> Seq.iter (fun node ->
        printfn "TODO possible goal node = %A" node
        printfn "TODO shortest path to this? %A" (shortestPath node))

    0

// let nextMove (agent: Agent) : Move =
//     let current =
//         { location = agent.location
//           direction = agent.direction
//           stepsTakenInThatDirection = agent.stepsTakenInThatDirection }

//     let next = shortestPath current

//     { agent = agent
//       newDirection = next.direction
//       newLocation = next.location }

// let rec solve (agent: Agent) : Agent =
//     printfn "TODO %A" agent

//     if agent.location = goal then
//         agent
//     else
//         solve (takeMove puzzle agent (nextMove agent))

// // take that first step automatically because the shortest path system assumes that ever step has taken at least one
// let startAgent (direction: Direction) : Agent =
//     let location = start + direction.vector

//     { location = location
//       direction = direction
//       stepsTakenInThatDirection = 1
//       totalScore = puzzle[location]
//       history = [ location; start ] }

// let solution =
//     startingDirections
//     |> Seq.map startAgent
//     |> Seq.map solve
//     |> Seq.minBy (fun agent -> agent.totalScore)

// // TODO no
// let historyChars =
//     solution.history
//     |> Seq.rev
//     |> Seq.pairwise
//     |> Seq.map (fun (a, b) ->
//         let c =
//             match b - a with
//             | { x = -1; y = 0 } -> '<'
//             | { x = 1; y = 0 } -> '>'
//             | { x = 0; y = -1 } -> '^'
//             | { x = 0; y = 1 } -> 'v'
//             | _ -> failwith (sprintf "impossible jump from %A to %A" a b)

//         a, c)
//     |> Map

// // TODO no
// for y in 0 .. (height - 1) do
//     for x in 0 .. (width - 1) do
//         let location = { x = x; y = y }

//         match historyChars |> Map.tryFind location with
//         | Some c -> printf "%c" c
//         | None -> printf "%d" puzzle[location]

//     printf "\n"

// solution.totalScore
