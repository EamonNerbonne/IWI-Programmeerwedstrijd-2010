module RandomMaze

open TiltMazeFs

type random = System.Random

let inline randomPoint (r:random) (rows,cols) =
    let p = r.Next(rows*cols)
    (p / cols, p % cols)

let inline randomInternalPoint r (rows,cols) =
    let (rI,cI) = randomPoint r (rows-2,cols-2)
    (rI+1, cI+1)

let randomStep (r:random) (maze:TiltMaze)  (y0,x0) (y1,x1) =
    let (yOffset, xOffset) = (y1 - y0, x1-x0)
    let dx = sign xOffset
    let dy = sign yOffset
    if yOffset <> 0 && xOffset <> 0 && (maze.IsPassable(y0 + dy, x0 + dx) || r.Next 2 = 1) && not (maze.IsPassable(y0 + dy, x0) && maze.IsPassable(y0, x0 + dx) ) then 
        //choose to minimize local clutter
        if maze.IsPassable(y0 + dy, x0) then
            (y0 + dy, x0)
        else
            (y0, x0 + dx)
    else 
        //choose randomly
        let totDist = abs yOffset + abs xOffset
        let randNum = r.Next(totDist)
        if randNum < abs yOffset then //do y step
            (y0 + dy, x0)
        else 
            (y0, x0 + dx)
        
let rec randomPath (r:random)  (maze:TiltMaze)  fromP toP =
    if fromP = toP then
        []
    else
        let nextP = randomStep r maze fromP toP
        nextP::randomPath r maze nextP toP

let makeNicer r (maze:TiltMaze) =
    let canpass p = if maze.IsPassable p then 1 else 0
    let countNeighbors (y,x) =  canpass (y+1,x) + canpass (y-1,x) + canpass (y,x-1) + canpass (y,x+1)
    let countDNeighbors (y,x) =  canpass (y+1,x+1) + canpass (y-1,x+1) + canpass (y+1,x-1) + canpass (y-1,x-1)

    let (rows, cols) = maze.Size
    let size = rows*cols
    for i = 0 to (size/2) do
        let p = randomInternalPoint r maze.Size
        if p <> maze.Start then
            match countNeighbors p with
            | 0  -> maze.SetPassable p false
            | 1 when not (maze.IsPassable p) && (r.Next 2 = 1) -> maze.SetPassable p true
            | 3 | 4 when maze.IsPassable p && countDNeighbors p > 2 -> maze.SetPassable p false
            | _ -> ()
        else
            ()

let rec randomWalk (r:random) (maze:TiltMaze) fromP stepsToGo =
    match stepsToGo with
    | 0 -> ()
    | n -> 
        let rEnd = randomInternalPoint r maze.Size
        for p in randomPath r maze fromP rEnd do
            maze.SetPassable p true
        makeNicer r maze
        randomWalk r maze rEnd (stepsToGo - 1)

let randomMaze size shouldBePossible =
    let (rows, cols) = size
    let allPoints = 
        seq {
            for y = 1 to rows-2 do
                for x = 1 to cols-2 do
                    yield (y,x)
                    }
    let maze = new TiltMaze(rows, cols)
    let r = new random(int System.DateTime.Now.Ticks)
    while maze.Target = (0,0) do
        maze.Start <- randomInternalPoint r size
        maze.SetPassable maze.Start true
        let stepEst = 2*int (1.0 + log 0.5 / log ( 1.0 - float (rows-2+cols-2)/(3.0 * float (rows-2) * float (cols-2))))
        let seqSteps = (8 + r.Next stepEst + r.Next stepEst + r.Next stepEst + r.Next stepEst ) / 6
        randomWalk r maze maze.Start seqSteps
        randomWalk r maze maze.Start seqSteps
        //eprintfn "should be possible: %b" shouldBePossible
        solveTiltMaze maze |>ignore //initializes visited cache - but since not solvable, initializes everything.
        let targets = 
            Seq.filter (fun p-> maze.IsPassable p && maze.HasVisited p = shouldBePossible && p <> maze.Start) allPoints 
                |> Seq.toArray |> Array.sortBy maze.VisitedRound
        let tImin = targets.Length*2/3;
        let tR = targets.Length - tImin;
        if tR >0 then
            maze.Target <- targets.[ tImin + ( (r.Next tR) |> max  (r.Next tR)|> max  (r.Next tR)|> max  (r.Next tR)|> max  (r.Next tR) )  ]
        else
            ()
    maze


open System.IO
let solveAndPrintRandomMazes (inp:TextWriter) (outp: TextWriter) (rowsE,colsE) = 
    fprintfn inp "2"
    for solvable in [true;false] do
        let maze = randomMaze (rowsE+2, colsE+2) solvable
        maze.Serialize inp
        solveTiltMazeAndReport maze outp
