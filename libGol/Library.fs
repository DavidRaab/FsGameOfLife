namespace libGol

module Game =
    [<Struct>]
    type State =
        | Dead
        | Alive

    type Field = State[,]

    let createField init width height : Field =
        Array2D.create width height init

    type T = {
        InitState: State
        Field:     Field
        Width:     int
        Height:    int
    }

    let height game = game.Height
    let width game  = game.Width

    let create init width height = {
        InitState = init
        Width     = width
        Height    = height
        Field     = createField init (width+2) (height+2)
    }
    
    let fromSeq outOfRange seqOfSeq =
        let maxY = Seq.length seqOfSeq
        let maxX = Seq.max (Seq.map Seq.length seqOfSeq)
        let game = create outOfRange maxX maxY
        // Copy LoL starting at pos 1,1
        seqOfSeq |> Seq.iteri (fun y ys ->
            ys |> Seq.iteri (fun x state ->
                game.Field.[x+1,y+1] <- state
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
   
    let inline get x y game =
        game.Field.[x,y]
    
    let iteri forCell forRow game =
        for y=1 to height game do
            for x=1 to width game do
                forCell x y (get x y game)
            forRow y

    let neighboursAlive x y game =
        let stateToNum state =
            match state with
            | Dead  -> 0
            | Alive -> 1

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
        for y=1 to height game do
            for x=1 to width game do
                newGame.Field.[x,y] <- f (get x y game) (neighboursAlive x y game)
        newGame

    let nextState game =
        map (fun state alives ->
            match state,alives with
            | Dead, 3 -> Alive
            | Alive,2 -> Alive
            | Alive,3 -> Alive
            | _       -> Dead
        ) game
