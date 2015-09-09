namespace FSharpKatas

    module GameOfLife =
        type Cell = Alive | Dead 
        
        type X = X of int
        type Y = Y of int
        type Coordinate = X*Y 

        type Neighbours =  Cell seq 
        type Universe = Cell option[,]

        type Seed = Map<Coordinate, Cell>

        let size = 5

        let createX n = 
            if n >= 0 && n < size
                then Some(X n)
                else None

        let createY n = 
            if n >= 0 && n < size
                then Some(Y n)
                else None

        let createCoordinate x y :Coordinate option =
            let xCoordinate = createX x
            let yCoordinate = createY y
            match (xCoordinate, yCoordinate) with
            | Some x, Some y -> Some (xCoordinate.Value, yCoordinate.Value)
            | _, _ -> None 

        let private countAliveNeighbours neighbours =
            neighbours 
            |> Seq.filter (fun n -> n = Alive) 
            |> Seq.length

        let private isCellAlive cell neighbours =
            let aliveNeighbours = countAliveNeighbours neighbours
            match cell with
            | Alive -> aliveNeighbours >= 2 && aliveNeighbours < 4
            | Dead -> aliveNeighbours = 3 

        let private nextGenerationCellStatus cell neighbours =
            match isCellAlive cell neighbours with
            | true -> Alive
            | false -> Dead

        let tickCell (cell:Cell) =
            nextGenerationCellStatus cell

        let tick (universe:Universe) :Universe =
            universe |> Array2D.map (fun c -> if c.IsSome then Some(tickCell c.Value []) else c) 

        let init (seed:Seed) : Universe =
            Array2D.init<Cell option> size size (fun x y -> 
                if seed.ContainsKey((X x, Y y)) 
                then Some(seed.[(X x, Y y)])
                else None)

    module GameOfLifeTests =
        open NUnit.Framework
        open FsUnit
        open GameOfLife

        [<Test>]
        let ``A live cell with fewer than two live neighbours dies, as if caused by under population``() = 
            let cell = Alive 
            let neighbours = [Alive; Dead; Dead]

            neighbours |> tickCell cell |> should equal Dead

        [<Test>]
        let ``A live cell with more than three live neighbours dies, as if by overcrowding``() = 
            let cell = Alive
            let neighbours = [Alive; Alive; Alive; Alive]

            neighbours |> tickCell cell |> should equal Dead; 

        [<Test>]
        let ``A live cell with two or three live neighbour’s lives on to the next generation``() = 
            let cell = Alive
            let neighbours = [Alive; Alive; Alive]

            neighbours |> tickCell cell |> should equal Alive;

        [<Test>]
        let ``A dead cell with exactly three live neighbours becomes a live cell``() = 
            let cell = Dead
            let neighbours = [Alive; Alive; Alive]

            neighbours |> tickCell cell |> should equal Alive

        [<Test>]
        let ``The Universe is created with a specified size``() =
            let universe = init Map.empty<Coordinate, Cell>
            
            universe.Length |> should equal 25

        [<Test>]
        let ``The Universe is created empty``() =
            let universe = init Map.empty<Coordinate, Cell>
            
            Assert.That(universe |> Seq.cast<Cell option> |> Seq.choose id |> Seq.length, Is.EqualTo(0))

        [<Test>]
        let ``The Universe can be seeded``() =
            let coordinate:Coordinate = (createCoordinate 0 0).Value
            let cell:Cell = Alive
            let seed:Seed = [coordinate, cell] |> Map.ofList
            let universe = init seed

            universe.[0,0].Value |> should equal cell

        [<Test>]
        let ``A Universe with a single live cell will bring no cells alive for next generation``() =
            let coordinate:Coordinate = (createCoordinate 1 1).Value
            let cell:Cell = Alive
            let seed:Seed = [coordinate, cell] |> Map.ofList
            let universe = init seed

            let newUniverse = tick universe

            newUniverse.[1,1].Value |> should equal Dead