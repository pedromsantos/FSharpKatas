﻿namespace GameOfLife.FSharpKatas

    module GameOfLife =     

        type Cell = Alive | Dead

        module internal Coordinates =
            type X = X of int
            type Y = Y of int
            type Coordinate = {X:X; Y:Y}

            let changeX newValue (c:Coordinate) =
                let valueX (X x) = x

                {c with X = X ((valueX c.X) + newValue)}

            let changeY newValue (c:Coordinate) =
                let valueY (Y y) = y

                {c with Y = Y ((valueY c.Y) + newValue)}

        module internal Cells =
            type Neighbours =  Cell seq 
                        
            type TickCell = Cell -> Neighbours -> Cell
            type CountAliveCells = Cell seq -> int
            type IsCellAlive = Cell -> Neighbours -> bool 

            let private countAlive:CountAliveCells = fun cells ->
                cells 
                |> Seq.filter (fun n -> n = Alive) 
                |> Seq.length

            let private isCellAlive:IsCellAlive = fun cell neighbours ->
                let aliveNeighbours = countAlive neighbours
                match cell with
                | Alive -> aliveNeighbours >= 2 && aliveNeighbours < 4
                | Dead -> aliveNeighbours = 3 

            let tickCell:TickCell = fun cell neighbours ->
                match isCellAlive cell neighbours with
                | true -> Alive
                | false -> Dead

        open Cells
        open Coordinates

        type Universe = Map<Coordinate, Cell>
        
        type CreateCoordinate = int -> int -> Coordinate
        type Tick = Universe -> Universe
        
        let private neighboursForCoordinate coordinate universe =
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
            universe 
            |> Map.map (fun key value -> universe |> neighboursForCoordinate key |> tickCell value)

    module GameOfLifeTests =
        open NUnit.Framework
        open Swensen.Unquote
        open GameOfLife
        open GameOfLife.Cells

        [<Test>]
        let ``A live cell with fewer than two live neighbours dies, by under population``() = 
            let cell = Alive 
            let neighbours = [Alive; Dead; Dead]

            test <@ neighbours |> tickCell cell = Dead @>

        [<Test>]
        let ``A live cell with more than three live neighbours dies, by overcrowding``() = 
            let cell = Alive
            let neighbours = [Alive; Alive; Alive; Alive]

            test <@ neighbours |> tickCell cell = Dead @>

        [<Test>]
        let ``A live cell with two or three live neighbour’s lives on to the next generation``() = 
            let cell = Alive
            let neighbours = [Alive; Alive; Alive]

            test <@ neighbours |> tickCell cell = Alive @>

        [<Test>]
        let ``A dead cell with exactly three live neighbours becomes a live cell``() = 
            let cell = Dead
            let neighbours = [Alive; Alive; Alive]

            test <@ neighbours |> tickCell cell = Alive @>

        [<Test>]
        let ``The Universe can be seeded``() =
            let universe = [coordinate 0 0, Alive] |> Map.ofList

            test <@ universe.[coordinate 0 0]  = Alive @>

        [<Test>]
        let ``A Universe with a single live cell will bring no cells alive for next generation``() =
            let universe = [coordinate 1 1, Alive] |> Map.ofList

            let updatedUniverse  = tick universe

            test <@ updatedUniverse.[coordinate 1 1] = Dead @>

        [<Test>]
        let ``A Universe with three neighbour live cells will bring all cells alive for next generation``() =
            let universe = [coordinate 0 0, Alive; coordinate 0 1, Alive; coordinate 1 0, Alive;] 
                           |> Map.ofList

            let updatedUniverse  = tick universe

            test <@ updatedUniverse.[coordinate 0 0] = Alive @>
            test <@ updatedUniverse.[coordinate 0 1] = Alive @>
            test <@ updatedUniverse.[coordinate 1 0] = Alive @>