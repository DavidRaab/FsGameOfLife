open libGol

module View =
    let asChar state =
        match state with
        | Game.Dead  -> "."
        | Game.Alive -> "X"

    let asString game =
        let sb  = System.Text.StringBuilder()
        Game.iteri 
            (fun x y state -> ignore (sb.Append (asChar state)))
            (fun y -> ignore (sb.Append '\n'))
            game
        sb.ToString()

// App Helper Functions
let printAt col row (text:string) =
    System.Console.SetCursorPosition(col,row)
    System.Console.Write(text)

let sleep (ms:int) =
    System.Threading.Thread.Sleep ms

let readText file =
    System.IO.File.ReadAllText(file)

let onExit () =
    System.Console.CursorVisible <- true

// Main
[<EntryPoint>]
let main argv =
    let sleepTime, init =
        match Array.toList argv with
        | [] -> 
            failwith "Error: Provide a filename"
        | file::[] ->
            let sleepTime = 100
            let game      = Game.fromStr Game.Dead (readText file)
            (sleepTime, game)
        | file::time::[] ->
            let sleepTime = int time
            let game      = Game.fromStr Game.Dead (readText file)
            (sleepTime,game)
        | file::time::state::tail ->
            let sleepTime = int time
            let game =
                match state with
                | "dead"  -> Game.fromStr Game.Dead  (readText file)
                | "alive" -> Game.fromStr Game.Alive (readText file)
                | _       -> Game.fromStr Game.Dead  (readText file)
            (sleepTime, game)

    System.Console.CancelKeyPress.Add (fun _ -> onExit ())
    System.Console.CursorVisible <- false
    System.Console.Clear()

    let sw = System.Diagnostics.Stopwatch.StartNew();

    printAt 0 0 "Phase: 1"
    printAt 0 1 (View.asString init)

    sleep sleepTime

    let rec loop (phase:int) prev current =
        if   prev = current
        then ()
        else
            printAt 0 0 (System.String.Format("Phase: {0}", phase))
            printAt 0 1 (View.asString current)
            sleep sleepTime
            loop (phase+1) current (Game.nextState current)

    loop 2 init (Game.nextState init)

    sw.Stop();
    printfn "Time: %s" (sw.Elapsed.ToString())

    onExit ()
    0
