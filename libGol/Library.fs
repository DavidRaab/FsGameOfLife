namespace libGol

module Game =
    [<Struct>]
    type State =
        | Dead
        | Died
        | Alive
        | Born
    
    let inline (|IsAlive|IsDead|) state =
        match state with
        | Dead  | Died -> IsDead
        | Alive | Born -> IsAlive

    type Field = State[,]

    let createField init width height : Field =
        Array2D.create width height init

    type T = {
        InitState: State
        Field:     Field
        Width:     int
        Height:    int
    }

    let height game    = game.Height
    let width game     = game.Width
    let dimension game = game.Width, game.Height

    let create init width height = {
        InitState = init
        Width     = width
        Height    = height
        Field     = createField init (width+2) (height+2)
    }

    let inline get x y game =
        game.Field.[x+1,y+1]
    
    let inline set x y state game =
        game.Field.[x+1,y+1] <- state
    
    let fromSeq outOfRange seqOfSeq =
        let maxY = Seq.length seqOfSeq
        let maxX = Seq.max (Seq.map Seq.length seqOfSeq)
        let game = create outOfRange maxX maxY
        seqOfSeq |> Seq.iteri (fun y ys ->
            ys |> Seq.iteri (fun x state ->
                set x y state game
            )
        )
        game
    
    let fromStr outOfRange (str:string) =
        str.Split [|'\n'|]
        |> Array.map (fun str -> [|
            for ch in str do
                if   ch = '.' then Dead
                elif ch = 'x' then Alive
                elif ch = 'X' then Alive
        |])
        |> Array.filter (fun xs -> Array.length xs > 0)
        |> fromSeq outOfRange 
    
    let iteri forCell forRow game =
        for y=0 to height game - 1 do
            for x=0 to width game - 1 do
                forCell x y (get x y game)
            forRow y

    let iteri2 forCell forRow game1 game2 =
        if dimension game1 <> dimension game2 then
            invalidArg "game1|game2" "Both games must have same dimension"
        
        let width, height = dimension game1
        for y=0 to height - 1 do
            for x=0 to width - 1 do
                forCell x y (get x y game1) (get x y game2)
            forRow y
    
    let inline stateToNum state =
            match state with
            | IsDead  _ -> 0
            | IsAlive _ -> 1

    let neighboursAlive x y game =
        stateToNum   (get (x-1) (y-1) game)
        + stateToNum (get (x)   (y-1) game)
        + stateToNum (get (x+1) (y-1) game)
        + stateToNum (get (x-1) (y)   game)
        + stateToNum (get (x+1) (y)   game)
        + stateToNum (get (x-1) (y+1) game)
        + stateToNum (get (x)   (y+1) game)
        + stateToNum (get (x+1) (y+1) game)

    let map f game =
        let newGame = create game.InitState game.Width game.Height
        for y=0 to height game - 1 do
            for x=0 to width game - 1 do
                newGame |> set x y (f (get x y game) (neighboursAlive x y game))
        newGame

    let nextState game =
        map (fun state alives ->
            match state,alives with
            | IsDead,3  -> Born
            | IsDead,_  -> Dead
            | IsAlive,2 -> Alive
            | IsAlive,3 -> Alive
            | IsAlive,_ -> Died
        ) game
