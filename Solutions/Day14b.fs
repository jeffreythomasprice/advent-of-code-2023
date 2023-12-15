module Solutions.Day14b

open System
open System.Text.RegularExpressions
open System.Collections.Generic

type Cell =
    | Movable
    | Stationary
    | Empty

type State =
    { width: int
      height: int
      data: Cell array2d }

type Location = { x: int; y: int }

let doIt (input: string) : int =
    let lines =
        input.Split "\n"
        |> Array.toSeq
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> not (String.IsNullOrWhiteSpace x))
        |> Seq.toList

    let height = lines.Length
    let width = lines |> Seq.map (fun line -> line.Length) |> Set

    if width.Count <> 1 then
        failwith "not all rows are the same size"

    let width = width |> Set.toList |> List.head

    let state: State =
        { width = width
          height = height
          data =
            Array2D.init height width (fun y x ->
                let c = lines[y][x]

                match c with
                | 'O' -> Movable
                | '#' -> Stationary
                | '.' -> Empty
                | _ -> failwith (sprintf "unhandled char: %c" c)) }

    // do one update and return the new state and the new list of potentially still mobile cells
    let update (state: State) (locations: Location Set) (delta: Location) : State * Location Set =
        let newState = Array2D.init state.height state.width (fun y x -> state.data[y, x])

        let newLocations =
            seq {
                for l in locations do
                    let newLocation = { x = l.x + delta.x; y = l.y + delta.y }

                    if
                        newLocation.x >= 0
                        && newLocation.x < state.width
                        && newLocation.y >= 0
                        && newLocation.y < state.height
                        && newState[newLocation.y, newLocation.x] = Empty
                    then
                        newState[newLocation.y, newLocation.x] <- Movable
                        newState[l.y, l.x] <- Empty
                        yield newLocation
                    else if locations.Contains newLocation then
                        yield l
            }

        { width = state.width
          height = state.height
          data = newState },
        newLocations |> Set

    let rec updateUntilStable (state: State) (locations: Location Set) (delta: Location) : State =
        let newState, newLocations = update state locations delta

        if newLocations.IsEmpty then
            newState
        else
            updateUntilStable newState newLocations delta

    let findLocations (state: State) : Location Set =
        seq {
            for y in 0 .. (state.height - 1) do
                for x in 0 .. (state.width - 1) do
                    if state.data[y, x] = Movable then
                        yield { x = x; y = y }
        }
        |> Set

    let spinCycle (state: State) : State =
        let state = updateUntilStable state (findLocations state) { x = 0; y = -1 }
        let state = updateUntilStable state (findLocations state) { x = -1; y = 0 }
        let state = updateUntilStable state (findLocations state) { x = 0; y = 1 }
        let state = updateUntilStable state (findLocations state) { x = 1; y = 0 }
        state

    let score (state: State) : int =
        seq {
            for y in 0 .. (height - 1) do
                for x in 0 .. (width - 1) do
                    if state.data[y, x] = Movable then
                        yield height - y
        }
        |> Seq.sum

    // if the list ends with a repeating section, returns the length of the repeating section
    let checkForRepeatAtEnd (history: State list) : int option =
        if history.Length <= 2 then
            None
        else
            let maxLength = history.Length / 2

            seq { maxLength .. -1 .. 1 }
            |> Seq.map (fun len ->
                let skip = history.Length - len * 2
                let first = history |> List.skip skip |> List.take len
                let second = history |> List.skip (skip + len) |> List.take len
                first, second, len)
            |> Seq.tryFind (fun (first, second, len) -> first = second)
            |> Option.map (fun (first, second, len) -> len)

    let goalIteration = 1000000000

    let history, repeatLen =
        seq {
            let mutable state = state
            let mutable history = [ state ]

            for i in 1..goalIteration do
                state <- spinCycle state
                history <- history @ [ state ]

                match checkForRepeatAtEnd history with
                | Some len -> yield history, len
                | None -> ()
        }
        |> Seq.head

    printfn "searched %d states, repeat len = %d" (history.Length) repeatLen

    let nonRepeatingLen = history.Length - repeatLen * 2
    let numRepeats = (goalIteration - nonRepeatingLen) / repeatLen
    let extraIterations = goalIteration - nonRepeatingLen - repeatLen * numRepeats
    printfn "nonRepeatingLen = %d" nonRepeatingLen
    printfn "numRepeats = %d" numRepeats
    printfn "extraIterations = %d" extraIterations
    let indexEquivalentToGoal = nonRepeatingLen + extraIterations
    printfn "indexEquivalentToGoal = %d" indexEquivalentToGoal

    score history[indexEquivalentToGoal]
