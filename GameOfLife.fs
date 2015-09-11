namespace FSharpKatas

    module GameOfLife =
        type Cell = Alive | Dead 
        
        type X = X of int
        type Y = Y of int
        type Coordinate = X*Y 

        type Neighbours =  Cell seq 
        type Universe = Map<Coordinate, Cell>

        let createCoordinate x y :Coordinate =
            (X x, Y y)

        let increaseX c:Coordinate =
            (X c, Y c)

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

        let tickCell (cell:Cell) neighbours =
            nextGenerationCellStatus cell neighbours

        let tick (universe:Universe) :Universe =
            universe |> Map.map (fun key value -> tickCell value [])

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
        let ``The Universe is created empty``() =
            let universe = Map.empty<Coordinate, Cell>
            
            Assert.That(universe |> Seq.cast<Cell option> |> Seq.choose id |> Seq.length, Is.EqualTo(0))

        [<Test>]
        let ``The Universe can be seeded``() =
            let universe = [createCoordinate 0 0, Alive] |> Map.ofList

            universe.[(X 0, Y 0)] |> should equal Alive

        [<Test>]
        let ``A Universe with a single live cell will bring no cells alive for next generation``() =
            let universe = [createCoordinate 1 1, Alive] |> Map.ofList

            let updatedUniverse  = tick universe

            updatedUniverse.[(X 1, Y 1)] |> should equal Dead

        [<Test>]
        let ``A Universe with a three neighbour live cells will bring all cells alive for next generation``() =
            let universe = [(X 0,Y 0), Alive; (X 0,Y 1), Alive; (X 1,Y 0), Alive;] 
                           |> Map.ofList

            let updatedUniverse  = tick universe

            updatedUniverse.[(X 0, Y 0)] |> should equal Alive 
            updatedUniverse.[(X 0, Y 1)] |> should equal Alive
            updatedUniverse.[(X 1, Y 0)] |> should equal Alive