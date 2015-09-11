namespace FSharpKatas

    module GameOfLife =
        type X = X of int
        type Y = Y of int
        type Coordinate = {X:X; Y:Y} 

        type Cell = Alive | Dead 
        type Neighbours =  Cell seq 
        type Universe = Map<Coordinate, Cell>

        let private valueX (X x) = x

        let private valueY (Y y) = y

        let private increaseX (c:Coordinate) =
            {c with X = X ((valueX c.X) + 1)}

        let private increaseY (c:Coordinate) =
            {c with Y = Y ((valueY c.Y) + 1)}

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

        let coordinate x y :Coordinate =
            {X=X x; Y=Y y}

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
            let universe = [coordinate 0 0, Alive] |> Map.ofList

            universe.[coordinate 0 0] |> should equal Alive

        [<Test>]
        let ``A Universe with a single live cell will bring no cells alive for next generation``() =
            let universe = [coordinate 1 1, Alive] |> Map.ofList

            let updatedUniverse  = tick universe

            updatedUniverse.[coordinate 1 1] |> should equal Dead

        (* Next iteration
        [<Test>]
        let ``A Universe with a three neighbour live cells will bring all cells alive for next generation``() =
            let universe = [coordinate 0 0, Alive; coordinate 0 1, Alive; coordinate 1 0, Alive;] 
                           |> Map.ofList

            let updatedUniverse  = tick universe

            updatedUniverse.[coordinate 0 0] |> should equal Alive 
            updatedUniverse.[coordinate 0 1] |> should equal Alive
            updatedUniverse.[coordinate 1 0] |> should equal Alive
        *)