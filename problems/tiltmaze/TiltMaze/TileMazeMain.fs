module TileMazeMain

open TiltMazeFs
open RandomMaze
open System.IO

let generate size inpName outpName =
    use inpS = File.Open(inpName,FileMode.Create)
    use inp = new StreamWriter(inpS)
    use outpS = File.Open(outpName, FileMode.Create)
    use outp = new StreamWriter(outpS)
    solveAndPrintRandomMazes inp outp size

let generatecustom (_ : unit) =
    generate (askForRowsCols System.Console.In) "example.in" "example.out"

let SizeLimit = 1000
let SizeMin = 3
let generatemany n =
    let r = new random(int System.DateTime.Now.Ticks-137)
    for i = 1 to n do
        let rows =SizeMin+( r.Next (SizeLimit-SizeMin) |> min (r.Next (SizeLimit-SizeMin)))
        let cols = SizeMin+ (r.Next (SizeLimit-SizeMin) |> min (r.Next (SizeLimit-SizeMin)))
        let name = sprintf "%dx%d_%d" rows cols i
        generate (rows,cols) (name+".in") (name+".out")


[<EntryPoint>]
let main args =
    //eprintfn "Arguments passed to function : %A" args
    if args.Length = 0 then
        generatemany 100
    else
        use inp = File.OpenText args.[0] 
        askAndDoRuns inp
    0
