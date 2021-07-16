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
        let height = Game.height game
        let width  = Game.width game
        let canvas = Canvas(width,height)
        setAll bgColor canvas
        Game.iteri (fun x y state ->
            let color = 
                match state with
                | Game.Dead  -> bgColor
                | Game.Alive -> aliveColor
            ignore (canvas.SetPixel(x-1,y-1,color))
        ) ignore game
        canvas

// App Helper Functions
let position x y =
    System.Console.SetCursorPosition(x,y)

let printText x y (text:string) =
    position x y
    System.Console.Write(text)

let printGame x y game =
    position x y
    AnsiConsole.Render(Canvas.fromGame Color.White Color.Blue game)

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

    printText 0 0 "Phase: 1"
    printGame 0 1 init
    
    sleep sleepTime

    let rec loop (phase:int) prev current =
        if   prev = current
        then ()
        else
            printText 0 0 (System.String.Format("Phase: {0}", phase))
            printGame 0 1 current
            sleep sleepTime
            loop (phase+1) current (Game.nextState current)

    loop 2 init (Game.nextState init)

    sw.Stop();
    printfn "Time: %s" (sw.Elapsed.ToString())

    onExit ()
    0
