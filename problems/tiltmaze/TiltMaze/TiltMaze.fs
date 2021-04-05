// Learn more about F# at http://fsharp.net
module TiltMazeFs
open System.IO

let askForRowsCols (inp : TextReader) =
//    eprintfn "Rows Cols?"
    let (rowsE, colsE) as size = 
        inp.ReadLine().Split([|' '|]) 
            |> Array.filter (fun s -> s.Length <>0) 
            |> Array.map System.Int32.Parse
            |> function
                | [| r; c |] -> (r,c) 
                | x -> sprintf "Wrong number of params: %A" x |> failwith
//    eprintfn "Size: %A" (rowsE, colsE)
    size


type TiltMaze(rows:int, cols:int) =
    let mazeArr = Array.create (rows*cols) false
    let visitCache = Array.create (rows*cols) -1
    let arrIdx y x = y*cols + x

    let mutable start = (0,0)
    let mutable target = (0,0)
    member this.Start with get() = start and set(value) = start <- value
    member this.Target with get() = target and set(value) = target <- value

    member private this.parse (inp: TextReader) =
        //eprintfn "Maze Layout?"
        for y = 1 to rows-2 do
            let line = inp.ReadLine()
            assert (line.Length = cols - 2)
            for x = 1 to cols-2 do
                mazeArr.[arrIdx y x] <- line.[x-1] <> 'X'
                match line.[x-1] with
                | 'A' -> start <- (y, x)
                | 'B' -> target <- (y, x)
                | _  -> ()
        assert (start <> (0,0) && target <> (0,0))
        

    new(inp : TextReader) as this =
        let (rowsE, colsE) = askForRowsCols inp
        TiltMaze(rowsE+2,colsE+2)
        then 
            this.parse inp
    
    member this.Serialize (outp : TextWriter) = 
        assert this.IsPassable this.Start
        assert this.IsPassable this.Target
        fprintfn outp "%d %d" (rows - 2) (cols - 2)
        for y = 1 to rows-2 do
            for x = 1 to cols-2 do
                fprintf outp (
                    match (y,x) with
                    | pos when pos = this.Start -> "A"
                    | pos when pos = this.Target -> "B"
                    | pos when this.IsPassable pos -> "."
                    | _ -> "X"
                    )
            fprintfn outp ""

    member this.IsPassable (y, x) = arrIdx y x |> Array.get mazeArr
    member this.SetPassable (y,x) passable = arrIdx y x |>Array.set mazeArr <| passable
    member this.HasVisited (y, x) = (arrIdx y x |> Array.get visitCache ) <> -1
    member this.VisitedRound (y, x) = arrIdx y x |> Array.get visitCache 
    member this.Visit (y,x) round = arrIdx y x |> Array.set visitCache <| round
    member this.ClearVisitCache = Array.fill visitCache 0 (rows*cols) -1
    member this.Size = (rows, cols)

type Direction=
    | Up = 0
    | Right = 1 
    | Down = 2
    | Left = 3

let directions = [for intdir = 0 to 3 do yield enum<Direction> intdir]

let clockwise dir = (int dir + 1) % 4 |> enum<Direction>

let go dir (y,x) = 
    match dir with
    | Direction.Up -> (y-1, x)
    | Direction.Right -> (y, x+1)
    | Direction.Down -> (y+1, x)
    | Direction.Left -> (y, x-1)
    | _ -> failwith "Illegal Direction"

let tryStep (maze : TiltMaze) dir pos =
    let newpos = go dir pos
    if maze.IsPassable newpos then Some(newpos) else None


let possibleMajorMinorAngles = [ 
    for dir in directions do
        let next = clockwise dir
        yield (dir, next)
        yield (next, dir)
    ]

type MazeMoves = list<TiltMaze -> int * int -> int * int>

let majorMinorTilts : MazeMoves = 
    let rec posAfterMajorMinorTilt angle (maze : TiltMaze) pos  =
        let (majorDir, minorDir) = angle
        match (tryStep maze majorDir pos, tryStep maze minorDir pos) with
        | (Some(newpos), _ ) -> posAfterMajorMinorTilt angle maze newpos 
        | (None, Some(newpos)) -> posAfterMajorMinorTilt angle maze newpos 
        | (None, None) -> pos
    possibleMajorMinorAngles |> List.map posAfterMajorMinorTilt

let simpleTilts : MazeMoves = 
    let rec posAfterSimpleTilt dir (maze : TiltMaze) pos  =
        match tryStep maze dir pos with
        | Some(newpos) -> posAfterSimpleTilt dir maze newpos 
        | None -> pos
    directions |> List.map posAfterSimpleTilt

let singleSteps : MazeMoves = 
    let posAfterSingleStep dir (maze : TiltMaze) pos  =
        match tryStep maze dir pos with
        | Some(newpos) -> newpos
        | None -> pos
    directions |> List.map posAfterSingleStep

let tryTilts (maze : TiltMaze) round pos = 
    if maze.HasVisited pos then
        []
    else
        do maze.Visit pos round
        [for tilt in majorMinorTilts -> tilt maze pos]
    
let rec walkMaze (maze : TiltMaze) stepsDone positionsReached = 
    if List.isEmpty positionsReached then
        None
    else if List.exists (fun pos -> pos = maze.Target) positionsReached then
        Some(stepsDone)
    else
        List.collect (tryTilts maze stepsDone) positionsReached |> walkMaze maze (stepsDone+1) 
    
let solveTiltMaze (maze : TiltMaze) = 
    do maze.ClearVisitCache
    walkMaze maze 0 [maze.Start]

let solveTiltMazeAndReport maze (outp:TextWriter) = 
    let stepsNeeded = solveTiltMaze maze
    match stepsNeeded with
    | Some(numberOfSteps) -> fprintfn outp "%d" numberOfSteps
    | None -> fprintfn outp "no solution"

let parseAndSolveTiltMaze inp outp = 
    solveTiltMazeAndReport (new TiltMaze (inp)) outp


let inline askAndDoRuns (inp:TextReader) = 
    //eprintf "Runs? "
    let runs = inp.ReadLine().Trim() |> int
    for i in 0 .. (runs-1) do
        //eprintfn "Completed run %d of %d " i runs
        parseAndSolveTiltMaze inp System.Console.Out
