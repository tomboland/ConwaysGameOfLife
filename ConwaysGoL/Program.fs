module ConwaysGameOfLife =

    [<StructuredFormatDisplay("{AsString}")>]
    type Cell =
        | Unpopulated
        | Populated
        override this.ToString() = 
            match this with
            | Unpopulated -> "  "
            | Populated -> " X "
        member this.AsString = this.ToString
    
    type Grid = {
        sizeX: int
        sizeY: int
        cells: Cell[,]
    }
       
    let testGrid = { sizeX = 4;
                     sizeY = 4;
                     cells = array2D [| [Cell.Unpopulated; Cell.Unpopulated; Cell.Unpopulated; Cell.Populated]
                                        [Cell.Populated; Cell.Populated; Cell.Populated; Cell.Populated]
                                        [Cell.Populated; Cell.Populated; Cell.Populated; Cell.Populated]
                                        [Cell.Unpopulated; Cell.Populated; Cell.Unpopulated; Cell.Unpopulated] |];
                   }

    let CellNeighbours grid x y = 
        let rec removeOriginator cell subList =
            match subList with
            | head::tail when head = cell -> tail
            | head::tail -> head::(removeOriginator cell tail)
            | _ -> failwith "Why is there an empty list here?"

        let minX = if x - 1 < 0 then 0 else x - 1
        let minY = if y - 1 < 0 then 0 else y - 1
        let maxX = if x + 1 >= grid.sizeX then grid.sizeX - 1 else x + 1
        let maxY = if y + 1 >= grid.sizeY then grid.sizeY - 1 else y + 1
        grid.cells.[minX .. maxX, minY .. maxY]
        |> Seq.cast<Cell>
        |> List.ofSeq
        |> removeOriginator grid.cells.[x,y]
        


    let NewCellState (grid : Grid) x y =
        let currentState = grid.cells.[x,y]
        let neighbours = CellNeighbours grid x y
        let populated =
            neighbours
            |> List.filter (fun cell ->
                match cell with
                | Populated -> true
                | Unpopulated -> false)
            |> List.length
        let unpopulated =
            neighbours
            |> List.filter (fun cell ->
                match cell with
                | Unpopulated -> true
                | Populated -> false)
            |> List.length
 
        match (currentState, populated, unpopulated) with
        | (Populated, _, x) when x < 2 -> Unpopulated
        | (Populated, x, _) when x = 2 || x = 3 -> Populated
        | (Populated, x, _) when x > 3 -> Unpopulated
        | (Unpopulated, x, _) when x = 3 -> Populated
        | _ -> currentState



    let GameOfLifeIteration grid =
        let newCells =
            seq { for x in [ 0 .. grid.sizeX - 1 ] do
                      for y in [ 0 .. grid.sizeY - 1 ] do
                          yield NewCellState grid x y }
            |> Seq.chunkBySize grid.sizeX
            |> array2D
        { sizeX = grid.sizeX
          sizeY = grid.sizeY
          cells = newCells }

                
    let PrintGrid grid =
        for x in [ 0 .. grid.sizeX - 1 ] do
            for y in [ 0 .. grid.sizeY - 1 ] do
                printf "%O" grid.cells.[x, y]
            printfn "\n"
        grid

[<EntryPoint>]
let main argv =
    ConwaysGameOfLife.testGrid
    |> ConwaysGameOfLife.PrintGrid 
    |> ConwaysGameOfLife.GameOfLifeIteration
    |> ConwaysGameOfLife.PrintGrid 
    |> ConwaysGameOfLife.GameOfLifeIteration
    |> ConwaysGameOfLife.PrintGrid 
    |> ConwaysGameOfLife.GameOfLifeIteration
    |> ConwaysGameOfLife.PrintGrid 
    |> ConwaysGameOfLife.GameOfLifeIteration
    |> ConwaysGameOfLife.PrintGrid
    |> ignore
    0 // return an integer exit code

