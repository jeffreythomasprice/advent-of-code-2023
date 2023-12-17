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
      history = agent.location :: agent.history }

let rec bestPossibleMove
    (puzzle: PuzzleInput)
    (agent: Agent)
    (depth: int)
    (comparer: Move -> Move -> int)
    : Move option =
    let moves = possibleMoves puzzle agent

    if moves |> Seq.isEmpty then
        None
    else if depth <= 0 then
        // we're at the end of our tree, just return the best possible move here
        Some(moves |> Seq.sortWith comparer |> Seq.head)
    else
        moves
        // for each possible move here, recurse and figure out the best move to take if we were to take that move first
        // decrement depth because we need a base case for recursion
        |> Seq.map (fun move ->
            let agent = takeMove puzzle agent move
            let finalMove = bestPossibleMove puzzle agent (depth - 1) comparer

            match finalMove with
            | Some finalMove -> Some(move, finalMove)
            | None -> None)
        |> Seq.filter (fun x -> x.IsSome)
        |> Seq.map (fun x -> x.Value)
        // sort by the best possible place we can end up by taking any of those
        |> Seq.sortWith (fun (_, a) (_, b) -> comparer a b)
        // but the actual move we take is that first one
        |> Seq.map (fun (result, _) -> result)
        |> Seq.tryHead

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

    let input =
        { width = width
          height = height
          data = Array2D.init height width (fun y x -> lines[y][x] |> string |> int) }

    let start = { x = 0; y = 0 }
    let goal = { x = width - 1; y = height - 1 }

    // TODO what should these be?
    let depthSearch = 8
    let howMuchDoesDistanceMatter = 100.0
    let howMuchDoesScoreMatter = 1.0

    let distanceToGoal (location: Vector) = location.distanceTo goal

    let compareMoves (a: Move) (b: Move) : int =
        let ad = distanceToGoal a.newLocation
        let bd = distanceToGoal b.newLocation

        if ad = 0 && bd > 0 then
            -1
        else if ad > 0 && bd = 0 then
            1
        else if ad = 0 && bd = 0 then
            (a.newScore input) - (b.newScore input)
        else
            sign (
                ((ad |> float) * howMuchDoesDistanceMatter
                 + ((a.newScore input) |> float) * howMuchDoesScoreMatter)
                - ((bd |> float) * howMuchDoesDistanceMatter
                   + ((b.newScore input) |> float) * howMuchDoesScoreMatter)
            )

    let nextMove (agent: Agent) =
        bestPossibleMove input agent depthSearch compareMoves

    let rec solve (agent: Agent) : int =
        printfn "TODO %A" agent

        if agent.location = goal then
            agent.totalScore
        else
            match nextMove agent with
            | Some move -> solve (takeMove input agent move)
            | None -> Int32.MaxValue

    let startAgent direction =
        { location = start
          direction = direction
          stepsTakenInThatDirection = 0
          totalScore = 0
          history = [ start ] }

    min (solve (startAgent East)) (solve (startAgent South))
