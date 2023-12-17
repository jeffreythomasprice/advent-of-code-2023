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

let possibleMoves (puzzle: PuzzleInput) (agent: Agent) : Move seq =
    let possibleDirections = [ agent.direction.left; agent.direction.right ]

    let sides =
        if agent.stepsTakenInThatDirection < 3 then
            agent.direction :: possibleDirections
        else
            possibleDirections

    sides
    |> Seq.map (fun direction -> direction, agent.location + direction.vector)
    |> Seq.filter (fun (_, newLocation) ->
        (puzzle.inBounds newLocation)
        && not (agent.history |> List.contains newLocation))
    |> Seq.map (fun (newDirection, newLocation) ->
        { agent = agent
          newDirection = newDirection
          newLocation = newLocation })

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

// ignores the turns and max distance in a straight line mechanics
// just pure shortest path to goal given the puzzle numbers
let shortestPathSolver (puzzle: PuzzleInput) (goal: Vector) : Vector -> Vector * int =
    // https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm

    let mutable state =
        seq {
            for x in 0 .. (puzzle.width - 1) do
                for y in 0 .. (puzzle.height - 1) do
                    let location = { x = x; y = y }
                    yield location
        }
        |> Seq.map (fun location -> location, (Int32.MaxValue, None))
        |> Map

    state <- state |> Map.add goal (puzzle[goal], None)

    let mutable remaining = Map.keys state |> Set

    while not remaining.IsEmpty do
        let next =
            remaining
            |> Seq.minBy (fun location ->
                let score, _ = state[location]
                score)

        remaining <- remaining |> Set.remove next

        let neighbors =
            [ North; South; East; West ]
            |> Seq.map (fun direction -> direction.vector + next)
            |> Seq.filter remaining.Contains

        let distanceNext, nextNext = state[next]

        for neighbor in neighbors do
            let neighborDistance, _ = state[neighbor]
            let newDistance = distanceNext + puzzle[neighbor]

            if newDistance < neighborDistance then
                state <- state |> Map.add neighbor (newDistance, Some next)

    let state =
        state
        |> Map.filter (fun _ (_, next) -> next.IsSome)
        |> Map.map (fun _ (score, next) -> next.Value, score)

    fun location -> state[location]

let rec bestPossibleMove
    (puzzle: PuzzleInput)
    (agent: Agent)
    (shortestPathSolver: Vector -> Vector * int)
    : Move option =
    // find out where we could go from here
    // organize into a lookup table by the location
    let possibleMoves =
        possibleMoves puzzle agent
        |> Seq.map (fun move -> move.newLocation, move)
        |> Map

    // if we have no choices then we're stuck here
    if possibleMoves.IsEmpty then
        None
    else
        // find the best possible move ignoring the turns
        let location, _ = shortestPathSolver agent.location

        match possibleMoves.TryFind location with
        // that's a valid move, just do that
        | Some move -> Some move
        | None ->
            // otherwise, for each possible move we can make, do the shortest path ignoring turns for each of those locations and pick best
            let move, _ =
                possibleMoves
                |> Map.values
                |> Seq.map (fun move -> move, shortestPathSolver move.newLocation)
                |> Seq.minBy (fun (_, (_, score)) -> score)

            Some move

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

    let shortestPath = shortestPathSolver puzzle goal

    let nextMove (agent: Agent) =
        bestPossibleMove puzzle agent shortestPath

    let rec solve (agent: Agent) : Agent option =
        printfn "TODO %A" agent

        if agent.location = goal then
            Some agent
        else
            match nextMove agent with
            | Some move -> solve (takeMove puzzle agent move)
            | None -> None

    let startAgent direction =
        { location = start
          direction = direction
          stepsTakenInThatDirection = 0
          totalScore = 0
          history = [ start ] }

    let solution =
        startingDirections
        |> Seq.map startAgent
        |> Seq.map solve
        |> Seq.filter (fun agent -> agent.IsSome)
        |> Seq.map (fun agent -> agent.Value)
        |> Seq.minBy (fun agent -> agent.totalScore)

    let historyChars =
        solution.history
        |> Seq.pairwise
        |> Seq.map (fun (a, b) ->
            let c =
                match b - a with
                | { x = -1; y = 0 } -> '<'
                | { x = 1; y = 0 } -> '>'
                | { x = 0; y = -1 } -> '^'
                | { x = 0; y = 1 } -> 'v'
                | _ -> failwith (sprintf "impossible jump from %A to %A" a b)

            a, c)
        |> Map

    for y in 0 .. (height - 1) do
        for x in 0 .. (width - 1) do
            let location = { x = x; y = y }

            match historyChars |> Map.tryFind location with
            | Some c -> printf "%c" c
            | None -> printf "%d" puzzle[location]

        printf "\n"

    solution.totalScore
