open libGol
open Spectre.Console

module Canvas =
    type Colors = {
        Alive: Color
        Born:  Color
        Dead:  Color
        Died:  Color
    }

    let defaultColors = {
        Alive = Color.Blue
        Born  = Color.Green
        Dead  = Color.White
        Died  = Color.Red
    }

    let iteri doX doY (canvas:Canvas) =
        for y=0 to canvas.Height-1 do
            for x=0 to canvas.Width-1 do
                ignore (doX x y canvas)
            ignore (doY y canvas)
    
    let setAll color canvas =
        iteri (fun x y canvas -> canvas.SetPixel(x,y,color)) (fun _ _ -> ()) canvas
    
    let createCanvas initColor dimension =
        let canvas = Canvas dimension
        setAll initColor canvas
        canvas
    
    let fromGame colors game =
        let canvas = createCanvas colors.Dead (Game.dimension game)
        Game.iteri (fun x y state ->
            let color = 
                match state with
                | Game.Dead  -> colors.Dead
                | Game.Died  -> colors.Died
                | Game.Alive -> colors.Alive
                | Game.Born  -> colors.Born
            ignore (canvas.SetPixel(x,y,color))
        ) ignore game
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
    printCanvas 0 1 (Canvas.fromGame Canvas.defaultColors init)
    
    sleep sleepTime

    let rec loop (phase:int) prev current =
        if   prev = current
        then ()
        else
            printText   0 0 (System.String.Format("Phase: {0}", phase))
            printCanvas 0 1 (Canvas.fromGame Canvas.defaultColors current)
            sleep sleepTime
            loop (phase+1) current (Game.nextState current)

    loop 2 init (Game.nextState init)

    sw.Stop();
    printfn "Time: %s" (sw.Elapsed.ToString())

    onExit ()
    0
