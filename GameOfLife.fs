namespace FSharpKatas

    module GameOfLife =
        type CellStatus = Alive | Dead 
        
        type Cell = { Status:CellStatus; Neighbours:Cell list }

        type World = Cell[,]

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

        let tick cell =
            {Status = nextGenerationCellStatus cell; Neighbours = cell.Neighbours}

        let init size : World=
            Array2D.zeroCreate<Cell> size size;

    module GameOfLifeTests =
        open NUnit.Framework
        open GameOfLife

        [<Test>]
        let ``A live cell with fewer than two live neighbours dies, as if caused by under population``() = 
            let neighbour1 = { Status=Alive; Neighbours=[] }
            let neighbour2 = { Status=Dead; Neighbours=[] }
            let neighbour3 = { Status=Dead; Neighbours=[] }
            let cell = { Status=Alive; Neighbours=[neighbour1; neighbour2; neighbour3] }

            Assert.That((tick cell).Status, Is.EqualTo(CellStatus.Dead))

        [<Test>]
        let ``A live cell with more than three live neighbours dies, as if by overcrowding``() = 
            let neighbour1 = { Status=Alive; Neighbours=[] }
            let neighbour2 = { Status=Alive; Neighbours=[] }
            let neighbour3 = { Status=Alive; Neighbours=[] }
            let neighbour4 = { Status=Alive; Neighbours=[] }
            let cell = { Status=Alive; Neighbours=[neighbour1; neighbour2; neighbour3; neighbour4] }

            Assert.That((tick cell).Status, Is.EqualTo(CellStatus.Dead))

        [<Test>]
        let ``A live cell with two or three live neighbour’s lives on to the next generation``() = 
            let neighbour1 = { Status=Alive; Neighbours=[] }
            let neighbour2 = { Status=Alive; Neighbours=[] }
            let neighbour3 = { Status=Alive; Neighbours=[] }
            let cell = { Status=Dead; Neighbours=[neighbour1; neighbour2; neighbour3] }

            Assert.That((tick cell).Status, Is.EqualTo(CellStatus.Alive))

        [<Test>]
        let ``A dead cell with exactly three live neighbours becomes a live cell``() = 
            let neighbour1 = { Status=Alive; Neighbours=[] }
            let neighbour2 = { Status=Alive; Neighbours=[] }
            let neighbour3 = { Status=Alive; Neighbours=[] }
            let cell = { Status=Dead; Neighbours=[neighbour1; neighbour2; neighbour3]}

            Assert.That((tick cell).Status, Is.EqualTo(CellStatus.Alive))

        [<Test>]
        let ``The world is created with a size``() =
            let world = init 5
            
            Assert.That(world.Length, Is.EqualTo(25))

        [<Test>]
        let ``The world is created with a specified size``() =
            let world = init 5
            
            Assert.That(world.Length, Is.EqualTo(25))
            