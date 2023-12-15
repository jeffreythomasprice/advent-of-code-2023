module Solutions.Day14a

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

    let locations =
        seq {
            for y in 0 .. (height - 1) do
                for x in 0 .. (width - 1) do
                    if state.data[y, x] = Movable then
                        yield { x = x; y = y }
        }
        |> Set

    // do one update and return the new state and the new list of potentially still mobile cells
    let update (state: State) (locations: Location Set) : State * Location Set =
        let newState = Array2D.init state.height state.width (fun y x -> state.data[y, x])

        let newLocations =
            seq {
                for l in locations do
                    let newLocation = { x = l.x; y = l.y - 1 }

                    if newLocation.y >= 0 && newState[newLocation.y, newLocation.x] = Empty then
                        newState[newLocation.y, newLocation.x] <- Movable
                        newState[l.y, l.x] <- Empty
                        yield newLocation
            }

        { width = state.width
          height = state.height
          data = newState },
        newLocations |> Set

    let rec updateUntilStable (state: State) (locations: Location Set) : State =
        let newState, newLocations = update state locations

        if newLocations.IsEmpty then
            newState
        else
            updateUntilStable newState newLocations

    let finalState = updateUntilStable state locations

    seq {
        for y in 0 .. (height - 1) do
            for x in 0 .. (width - 1) do
                if finalState.data[y, x] = Movable then
                    yield height - y
    }
    |> Seq.sum
