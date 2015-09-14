namespace FSharpKatas

    module GameOfLife =     

        type Cell = Alive | Dead

        module Cells =
            
            type X = X of int
            type Y = Y of int
            type Coordinate = {X:X; Y:Y}
            type Neighbours =  Cell seq 
                        
            type TickCell = Cell -> Neighbours -> Cell

            let changeX newValue (c:Coordinate) =
                let valueX (X x) = x

                {c with X = X ((valueX c.X) + newValue)}

            let changeY newValue (c:Coordinate) =
                let valueY (Y y) = y

                {c with Y = Y ((valueY c.Y) + newValue)}

            let countAlive cells =
                cells 
                |> Seq.filter (fun n -> n = Alive) 
                |> Seq.length

            let isCellAlive cell neighbours =
                let aliveNeighbours = countAlive neighbours
                match cell with
                | Alive -> aliveNeighbours >= 2 && aliveNeighbours < 4
                | Dead -> aliveNeighbours = 3 

            let tickCell:TickCell = fun cell neighbours ->
                match isCellAlive cell neighbours with
                | true -> Alive
                | false -> Dead

        open Cells 

        type Universe = Map<Coordinate, Cell>
        
        type CreateCoordinate = int -> int -> Coordinate
        type Tick = Universe -> Universe
        
        let private neighbours (coordinate:Coordinate) (universe:Universe) :Neighbours =
            [
            universe |> Map.tryFind (changeX 1 coordinate);
            universe |> Map.tryFind (changeX -1 coordinate);
            universe |> Map.tryFind (changeY 1 coordinate);
            universe |> Map.tryFind (changeY -1 coordinate);
            universe |> Map.tryFind (changeX -1 (changeY -1 coordinate));
            universe |> Map.tryFind (changeX 1 (changeY 1 coordinate));
            universe |> Map.tryFind (changeX -1 (changeY 1 coordinate));
            universe |> Map.tryFind (changeX 1 (changeY -1 coordinate))
            ]
            |> Seq.filter (fun c -> c.IsSome)
            |> Seq.map (fun c -> c.Value)

        let coordinate:CreateCoordinate = fun x y ->  
            {X=X x; Y=Y y}

        let tick:Tick = fun universe -> 
            universe |> Map.map (fun key value -> tickCell value (neighbours key universe))

    module GameOfLifeTests =
        open NUnit.Framework
        open FsUnit
        open GameOfLife
        open GameOfLife.Cells

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
        let ``The Universe can be seeded``() =
            let universe = [coordinate 0 0, Alive] |> Map.ofList

            universe.[coordinate 0 0] |> should equal Alive

        [<Test>]
        let ``A Universe with a single live cell will bring no cells alive for next generation``() =
            let universe = [coordinate 1 1, Alive] |> Map.ofList

            let updatedUniverse  = tick universe

            updatedUniverse.[coordinate 1 1] |> should equal Dead

        [<Test>]
        let ``A Universe with three neighbour live cells will bring all cells alive for next generation``() =
            let universe = [coordinate 0 0, Alive; coordinate 0 1, Alive; coordinate 1 0, Alive;] 
                           |> Map.ofList

            let updatedUniverse  = tick universe

            updatedUniverse.[coordinate 0 0] |> should equal Alive 
            updatedUniverse.[coordinate 0 1] |> should equal Alive
            updatedUniverse.[coordinate 1 0] |> should equal Alive