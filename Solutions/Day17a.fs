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

type Score =
    | Start
    // total cost to get here, and the previous node we were at to get here
    | Move of int * ShortestPathNode

    member this.intScore =
        match this with
        | Start -> 0
        | Move(result, _) -> result

let shortestPathSolver (puzzle: PuzzleInput) (start: Vector) (goal: Vector) : ShortestPathNode -> Score option =
    // https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
    // https://en.wikipedia.org/wiki/A*_search_algorithm

    // all possible nodes
    // looking up a node tells you where you must have been to get here
    // so a chain starting from any particular node navigating to the previous node each time eventually will reach the start node
    let mutable state: Map<ShortestPathNode, (Score option)> =
        seq {
            for x in 0 .. (puzzle.width - 1) do
                for y in 0 .. (puzzle.height - 1) do
                    let location = { x = x; y = y }

                    if location = start then
                        // there's only one node at the start, we can't have arrived here from anywhere else
                        yield ShortestPathNode.Start location, Some Start
                    else
                        // for all nother nodes we could enter that location from any direction, and we could have been travelling in that
                        // direction for 1 to 3 steps
                        for dir in [ North; South; East; West ] do
                            // this is only possible if the node we would have come from is on the puzzle grid
                            if puzzle.inBounds (location + dir.backwards.vector) then
                                for steps in 1..3 do
                                    yield
                                        ShortestPathNode.Move
                                            { location = location
                                              direction = dir
                                              stepsTakenInThatDirection = steps },
                                        None
        }
        |> Map

    let estimateScore (location: ShortestPathNode) : Score * int =
        let score = state[location].Value

        let estimate =
            // when picking a next node, also take into account the distance to the goal
            // that is, nodes that are really far away from the goal are deprioritized over possible solutions that are really close
            let currentLocation =
                match location with
                | ShortestPathNode.Start start -> start
                | ShortestPathNode.Move move -> move.location

            let estimateToGoal = currentLocation.distanceTo goal
            score.intScore + estimateToGoal

        score, estimate

    // keep a list of nodes to check, starting with the one we know a score for
    // the first value is the actual score in the state map
    // the second value is the estimate of the total score of a path through here to the goal
    let mutable remaining =
        [ ShortestPathNode.Start start, estimateScore (ShortestPathNode.Start start) ]
        |> Map

    let mutable remainingGoalNodes =
        Map.keys state
        |> Seq.filter (fun node ->
            match node with
            | ShortestPathNode.Move move -> move.location = goal
            | ShortestPathNode.Start _ -> false)
        |> Set

    // TODO no time
    let mutable lastTick = DateTime.Now

    // iterate while we have possible goal nodes remaining
    // we can stop once we have all possible ways to reach the goal node solved
    while not remainingGoalNodes.IsEmpty do
        // TODO no time
        let now = DateTime.Now

        if (now - lastTick).TotalSeconds >= 2 then
            printfn
                "TODO num remaining nodes? %d, num remaining nodes that are goal nodes? %d"
                remaining.Count
                remainingGoalNodes.Count

            lastTick <- now

        let current, currentScore, _ =
            (remaining
             |> Map.fold
                 (fun (result: (ShortestPathNode * Score * int) option) location (score, estimate) ->
                     match result with
                     | Some(existingLocation, existingScore, existingEstimate) ->
                         if existingEstimate < estimate then
                             Some(existingLocation, existingScore, existingEstimate)
                         else
                             Some(location, score, estimate)
                     | None -> Some(location, score, estimate))
                 None)
                .Value

        // remove this node from our list, we are now examining it and don't need to do so again
        remaining <- remaining |> Map.remove current
        remainingGoalNodes <- remainingGoalNodes |> Set.remove current

        // find all the other nodes that could have this node as their previous step, i.e. all the next nodes
        let neighbors =
            (match current with
             // if we're at the special start node, then the next nodes all the locations we go from here,
             // where we'll now have been moving for 1 step
             | ShortestPathNode.Start location ->
                 [ North; South; East; West ]
                 |> Seq.map (fun dir ->
                     { location = location + dir.vector
                       direction = dir
                       stepsTakenInThatDirection = 1 })
             // otherwise, we can either turn left or right,
             // if this node isn't reached by travelling too far in a straight line, we can keep going straight too
             | ShortestPathNode.Move move ->
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
                             1 }))
            // only keep those neighbors that are actually on the puzzle grid
            |> Seq.filter (fun neighbor -> state.ContainsKey(ShortestPathNode.Move neighbor))

        for neighbor in neighbors do
            // the score at the neighbor would be the score at this node plus how much it would cost to get there
            // i.e. the puzzle value at the destination
            let proposedScore = currentScore.intScore + puzzle[neighbor.location]
            // the current score at  that neighbor
            let neighborScore = state[ShortestPathNode.Move neighbor]

            if
                match neighborScore with
                // only replace if this is a better route
                | Some neighborScore -> proposedScore < neighborScore.intScore
                // no current route to the neighbor, so this will always win
                | None -> true
            then
                // going through the current node is better than however we were getting to this neighbor previously
                // remember this new route
                state <-
                    state
                    |> Map.add (ShortestPathNode.Move neighbor) (Some(Move(proposedScore, current)))
                // since we updated this neighbor, we have to check it again
                remaining <-
                    remaining
                    |> Map.add (ShortestPathNode.Move neighbor) (estimateScore (ShortestPathNode.Move neighbor))

    // the actual lookup is getting us the results at each node
    // so this is the next node, working backwards from somewhere to the start node
    // the value at the start will be the special Start score which has no next
    fun (node: ShortestPathNode) -> (state.TryFind node) |> Option.flatten

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
    let goal = { x = width - 1; y = height - 1 }

    let shortestPath = shortestPathSolver puzzle start goal

    [ North; South; East; West ]
    |> Seq.collect (fun dir ->
        seq { 1..3 }
        |> Seq.map (fun steps ->
            ShortestPathNode.Move
                { location = goal
                  direction = dir
                  stepsTakenInThatDirection = steps }))
    |> Seq.map shortestPath
    |> Seq.filter (fun score -> score.IsSome)
    |> Seq.map (fun score -> score.Value)
    |> Seq.map (fun score -> score.intScore)
    |> Seq.min
