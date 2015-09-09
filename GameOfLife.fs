namespace FSharpKatas

    module GameOfLife =
        type CellStatus = Alive | Dead 
        
        type Cell = { Status:CellStatus; Neighbours:Cell list }

        type X = X of int
        type Y = Y of int
        type Coordinate = X*Y 

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

        let private countAliveNeighbours cell =
            cell.Neighbours 
            |> Seq.filter (fun n -> n.Status = Alive) 
            |> Seq.length

        let private isCellAlive cell =
            let aliveNeighbours = countAliveNeighbours cell
            match cell.Status with
            | Alive -> aliveNeighbours >= 2 && aliveNeighbours < 4
            | Dead -> aliveNeighbours = 3 

        let private nextGenerationCellStatus cell =
            match isCellAlive cell with
            | true -> Alive
            | false -> Dead

        let tickCell (cell:Cell) =
            {cell with Status = nextGenerationCellStatus cell}

        let tick (universe:Universe) :Universe =
            universe |> Array2D.map (fun c -> if c.IsSome then Some(tickCell c.Value) else c) 

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
            let neighbour1 = { Status=Alive; Neighbours=[] }
            let neighbour2 = { Status=Dead; Neighbours=[] }
            let neighbour3 = { Status=Dead; Neighbours=[] }
            let cell = { Status=Alive; Neighbours=[neighbour1; neighbour2; neighbour3] }

            (tickCell cell) |> should equal { Status=Dead; Neighbours=[neighbour1; neighbour2; neighbour3] }

        [<Test>]
        let ``A live cell with more than three live neighbours dies, as if by overcrowding``() = 
            let neighbour1 = { Status=Alive; Neighbours=[] }
            let neighbour2 = { Status=Alive; Neighbours=[] }
            let neighbour3 = { Status=Alive; Neighbours=[] }
            let neighbour4 = { Status=Alive; Neighbours=[] }
            let cell = { Status=Alive; Neighbours=[neighbour1; neighbour2; neighbour3; neighbour4] }

            tickCell cell |> should equal { Status=Dead; Neighbours=[neighbour1; neighbour2; neighbour3; neighbour4] }

        [<Test>]
        let ``A live cell with two or three live neighbour’s lives on to the next generation``() = 
            let neighbour1 = { Status=Alive; Neighbours=[] }
            let neighbour2 = { Status=Alive; Neighbours=[] }
            let neighbour3 = { Status=Alive; Neighbours=[] }
            let cell = { Status=Dead; Neighbours=[neighbour1; neighbour2; neighbour3] }

            tickCell cell |> should equal { Status=Alive; Neighbours=[neighbour1; neighbour2; neighbour3] }

        [<Test>]
        let ``A dead cell with exactly three live neighbours becomes a live cell``() = 
            let neighbour1 = { Status=Alive; Neighbours=[] }
            let neighbour2 = { Status=Alive; Neighbours=[] }
            let neighbour3 = { Status=Alive; Neighbours=[] }
            let cell = { Status=Dead; Neighbours=[neighbour1; neighbour2; neighbour3]}

            tickCell cell |> should equal { Status=Alive; Neighbours=[neighbour1; neighbour2; neighbour3] }

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
            let cell:Cell = { Status=Alive; Neighbours=[] }
            let seed:Seed = [coordinate, cell] |> Map.ofList
            let universe = init seed

            universe.[0,0].Value |> should equal cell

        [<Test>]
        let ``A Universe with a single live cell will bring no cells alive for next generation``() =
            let coordinate:Coordinate = (createCoordinate 1 1).Value
            let cell:Cell = { Status=Alive; Neighbours=[] }
            let seed:Seed = [coordinate, cell] |> Map.ofList
            let universe = init seed

            let newUniverse = tick universe

            newUniverse.[1,1].Value |> should equal { Status=Dead; Neighbours=[] }