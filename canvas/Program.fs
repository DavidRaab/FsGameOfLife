open libGol
open Spectre.Console

module Canvas =
    let iteri doX doY (canvas:Canvas) =
        for y=0 to canvas.Height-1 do
            for x=0 to canvas.Width-1 do
                ignore (doX x y canvas)
            ignore (doY y canvas)
    
    let setAll color canvas =
        iteri (fun x y canvas-> canvas.SetPixel(x,y,color)) (fun _ _ -> ()) canvas
    
    let fromGame bgColor aliveColor game =
        let canvas = Canvas (Game.dimension game)
        setAll bgColor canvas
        Game.iteri (fun x y state ->
            let color = 
                match state with
                | Game.Dead  -> bgColor
                | Game.Alive -> aliveColor
            ignore (canvas.SetPixel(x,y,color))
        ) ignore game
        canvas
    
    let fromGame2 game1 game2 =
        let canvas = Canvas (Game.dimension game1)
        Game.iteri2
            (fun x y gs1 gs2 ->
                let color = 
                    match gs1,gs2 with
                    | Game.Dead,Game.Dead   -> Color.White
                    | Game.Dead,Game.Alive  -> Color.Green
                    | Game.Alive,Game.Alive -> Color.Blue
                    | Game.Alive,Game.Dead  -> Color.Red
                canvas.SetPixel(x,y,color) |> ignore)
            ignore
            game1
            game2
        canvas


// App Helper Functions
let position x y =
    System.Console.SetCursorPosition(x,y)

let printText x y (text:string) =
    position x y
    System.Console.Write(text)

let printCanvas x y (canvas:Canvas) =
    position x y
    AnsiConsole.Render(canvas)

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

    printText   0 0 "Phase: 1"
    printCanvas 0 1 (Canvas.fromGame Color.White Color.Blue init)
    
    sleep sleepTime

    let rec loop (phase:int) prev current =
        if   prev = current
        then ()
        else
            printText   0 0 (System.String.Format("Phase: {0}", phase))
            printCanvas 0 1 (Canvas.fromGame2 prev current)
            sleep sleepTime
            loop (phase+1) current (Game.nextState current)

    loop 2 init (Game.nextState init)

    sw.Stop();
    printfn "Time: %s" (sw.Elapsed.ToString())

    onExit ()
    0
