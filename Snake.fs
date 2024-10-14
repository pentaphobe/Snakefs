

open System
open System.Threading.Tasks
open System.Threading

module SnakeGame =

    /// 🚏 Directions on our grid:
    type Movement =
        | Left
        | Right
        | Down
        | Up
        | InvalidMove

    /// 🐍 Sort of like a list, but not:
    and Snake<'Position> =
        | Head of Position * Snake<'Position>
        | Belly of Position * Snake<'Position>
        | Tail

    /// 🕹️ Our basic runtime information:
    and Game =
        | GameRunning of Movement * Snake<Position> * Grid * Eaten:int * Food
        | GameOver of Grid * Eaten:int

    /// 🧭 x & y in our plane:
    and Position = int * int

    /// 🍎 The food sprite our snake will eat:
    and Food = Position * FoodType
    ///    food can be of various types (this is just used to change how it's displayed)
    and FoodType = int

    /// Contents of our Grid
    and Cell =
        | SnakeHead
        | SnakeBelly
        | CellFood of FoodType
        | CellEmpty

    /// 🌐 A simple two dimensional plane:
    and Grid = Cell[][]

    /// Making a list of positions from a Snake 🐍
    let snakeUnzip (snake:Snake<Position>) =
        let rec unzip snake carry =
            match snake with
            | Head (p, rest) -> unzip rest <| carry @ [p]
            | Belly (p, rest) -> unzip rest <| carry @ [p]
            | Tail -> carry
        unzip snake []

    /// Making a Snake from a list of positions 🐍
    let snakeZip (positions:list<Position>) (upto:int) =
        let correctLength = (List.take upto positions)
        let rec zip (carry:Snake<Position>) (rest:list<Position>) =
            match rest with
            | head::[] -> zip (Head(head, carry)) []
            | back::front -> zip (Belly (back, carry)) front
            | [] -> carry
        zip Tail (List.rev correctLength)

    /// 🌍 Separate world logic from rendering
    module World =
        let private random = new Random()
        let isFood square =
            match square with
            | CellFood(_) -> true
            | _ -> false
        let isFreeSpace square = square = CellEmpty
        let isOccupied square =
            match square with
            | square when isFreeSpace square -> false
            | square when isFood square -> false
            | _ -> true

        let makeGrid (dimensionsSquared) : Grid =
            let row _ = Array.init dimensionsSquared (fun _ -> CellEmpty)
            Array.init dimensionsSquared row

        // gives a new copy of grid with all cells filled
        let fillGrid (cell:Cell) (grid:Grid) : Grid =
            Array.map (fun row ->
                Array.map (fun _ ->
                    cell
                ) row
            ) grid

        // returns a new grid with all cells cleared
        let clearGrid = fillGrid CellEmpty

        let getFreeSpaces (grid:Grid) : list<Position> =
            let results : Position list ref = ref []
            for i in 0..(grid.Length-1) do
                for j in 0..(grid.Length-1) do
                    if isFreeSpace grid.[i].[j]
                    then results.Value <- results.Value @ [i,j]
                    else ()
                ()
            results.Value

        let getFood (grid:Grid) =
            Console.Beep()
            let freeSpaces =
                getFreeSpaces grid
                |> Array.ofList
            let food :FoodType = random.Next()
            let randomPos = freeSpaces.[random.Next(freeSpaces.Length - 1)]
            randomPos, food

        let dropFood (food:Food)  (grid:Grid) =
            let (x, y), foodType = food
            // TODO: mutation
            grid.[x].[y] <- CellFood(foodType)
            grid

        let slither (snake:Snake<Position>) (grid:Grid) : Grid =
            try
                let rec slithering (body:Snake<Position>) : Grid =
                    match body with
                    | Head(p, s) ->
                        let row, column = p
                        grid.[row].[column] <- SnakeHead
                        slithering s
                    | Belly(p, s) ->
                        let row, column = p
                        grid.[row].[column] <- SnakeBelly
                        slithering s
                    | Tail -> grid
                slithering snake
            with _ -> failwith "ERROR: Could not slither snake!"

    module Graphics =
        let private head = "🤢"
        let private belly = "🟢"
        let private display = "⬜"
        let private errorDisplay = "🟥"

        let private food = [|"🐁";"🐀";"🐥";"🪺";"🐸";"🐛";"🪰";"🐞";"🦗"|]

        type ScreenTile = string
        type ScreenBuffer = array<array<ScreenTile>>

        // transform a logical Cell into a graphical screen tile
        let spriteFor (cell:Cell) : ScreenTile =
            match cell with
                | SnakeHead -> head
                | SnakeBelly -> belly
                | CellFood(idx) -> food.[idx % food.Length]
                | CellEmpty -> display

        // transform a logical grid into a graphical screen buffer for display
        let renderGrid (grid:Grid) : ScreenBuffer =
            Array.map (fun (row:Cell array) ->
                Array.map spriteFor row
            ) grid

        // display a
        let displayBuffer(rows:ScreenBuffer) : unit =
            Console.Clear()
            Array.iteri(fun i row ->
                let line = String.concat "" row
                printfn $"{line}"
            ) rows

        let render (grid:Grid) : unit =
            grid
                |> renderGrid
                |> displayBuffer
            printfn "Snake Game in FSharp by @wiredsister"
            printfn "Controls: ⬅️ ↕️ ➡️"
            printfn "Press Ctrl+C to Quit Game"
            Console.Title <- "FSharp Snake 🐍"

        let renderEndGame (grid:Grid) : unit =
            grid
                |> renderGrid
                |> Array.map (fun (r) -> Array.map (fun (t) -> errorDisplay) r)
                |> displayBuffer
            Console.Beep()


    module GamePlay =
        let move (direction:Movement) (snake:Snake<Position>) (grid:Grid) (eaten:int) (food:Food) : Game =
            match snake with
            | Head (p, rest:Snake<Position>) ->
                let x, y = p
                let shift =
                    match direction with
                        | Up -> ((x-1), y)
                        | Down -> ((x+1), y)
                        | Left -> (x, (y-1))
                        | Right -> (x, (y+1))
                        | _ -> (x, y)
                try
                    match shift with
                    | (row,column) when World.isOccupied grid.[row].[column] ->
                        GameOver (grid, eaten)
                    | (row, column) when World.isFood grid.[row].[column] ->
                        let unzipped = snakeUnzip (Head (shift, (Belly (p, rest))))
                        let newSnake = snakeZip unzipped (eaten+1)
                        let nextFood = World.getFood grid
                        GameRunning (direction, newSnake, grid, eaten+1, nextFood)
                    | pivot ->
                        let unzipped = snakeUnzip (Head (pivot, (Belly (p, rest))))
                        let newSnake = snakeZip unzipped eaten
                        GameRunning (direction, newSnake, grid, eaten, food)
                with _ -> GameOver (grid, eaten)
            | _ -> failwith "ERROR: No head!"


open SnakeGame

[<EntryPoint>]
let main _ =

    /// A gentle slope function for making the snake go faster:
    let tick (eaten:int) = 100./log10(float eaten) |> int

    let getNextMove prev snake grid eaten food : Task<Game> =
        task {
            do! Task.Delay(tick(eaten))
            if not Console.KeyAvailable
            then
                // keep moving in the same direction
                match prev with
                    | InvalidMove -> return GameOver (grid, eaten)
                    | _  -> return GamePlay.move prev snake grid eaten food
            else
                // try changing direction
                match Console.ReadKey().Key with
                | ConsoleKey.UpArrow ->
                    return GamePlay.move Up snake grid eaten food
                | ConsoleKey.DownArrow ->
                    return GamePlay.move Down snake grid eaten food
                | ConsoleKey.RightArrow ->
                    return GamePlay.move Right snake grid eaten food
                | ConsoleKey.LeftArrow ->
                    return GamePlay.move Left snake grid eaten food
                | _ ->
                    match prev with
                    | InvalidMove -> return GameOver (grid, eaten)
                    | _ -> return GamePlay.move prev snake grid eaten food
        }

    let gridDimension = 20
    let segments = [(0,3); (0,2); (0,1); (0,0)]
    let youngSnake : Snake<Position> = snakeZip segments segments.Length
    let startingGrid = World.makeGrid gridDimension
    let startingFood = World.getFood startingGrid
    let start = GamePlay.move Right youngSnake startingGrid segments.Length startingFood

    let rec gameLoop (game:Game) =
        match game with
        | GameRunning (prev, snake, grid, eaten, food) ->
            let grid' = grid |> World.clearGrid |> World.dropFood food |> World.slither snake

            do Graphics.render grid'
            let eitherPlayerOrCursor = getNextMove prev snake grid' eaten food
            do eitherPlayerOrCursor.Wait()
            gameLoop eitherPlayerOrCursor.Result
        | GameOver (grid,eaten) ->
            do Graphics.renderEndGame grid
            printfn $"Game Over! Snake ate {eaten-segments.Length} critters!"
            do Thread.Sleep(1000)
            let rec wantToPlayAgain () =
                match Console.ReadKey().Key with
                | ConsoleKey.Y  -> gameLoop start
                | ConsoleKey.N  -> ()
                | _ -> wantToPlayAgain ()
            printfn $"Restart? Type Y to continue..."
            wantToPlayAgain()

    do gameLoop start
    0
