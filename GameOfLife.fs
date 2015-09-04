namespace FSharpKatas

    module GameOfLife =
        type CellStatus = Alive | Dead 
        
        type Cell = { Status:CellStatus; Neighbours:Cell list }

        type IsCellAlive = Cell -> bool
        
        let isCellAlive:IsCellAlive = fun cell ->
            match cell.Status with
            | Alive -> cell.Neighbours.Length >= 2 && cell.Neighbours.Length <= 3
            | Dead -> cell.Neighbours.Length = 3 

    module GameOfLifeTests =
        open NUnit.Framework
        open GameOfLife

        [<Test>]
        let ``A live cell with fewer than two live neighbours dies, as if caused by under population``() = 
            Assert.IsFalse(isCellAlive { Status=Alive; Neighbours=[]})

        [<Test>]
        let ``A live cell with more than three live neighbours dies, as if by overcrowding``() = 
            let neighbour1 = { Status=Alive; Neighbours=[]}
            let neighbour2 = { Status=Alive; Neighbours=[]}
            let neighbour3 = { Status=Alive; Neighbours=[]}
            let neighbour4 = { Status=Alive; Neighbours=[]}

            Assert.IsFalse(isCellAlive { Status=Alive; Neighbours=[neighbour1; neighbour2; neighbour3; neighbour4]})

        [<Test>]
        let ``A live cell with two or three live neighbour’s lives on to the next generation``() = 
            let neighbour1 = { Status=Alive; Neighbours=[]}
            let neighbour2 = { Status=Alive; Neighbours=[]}
            let neighbour3 = { Status=Alive; Neighbours=[]}

            Assert.IsTrue(isCellAlive { Status=Alive; Neighbours=[neighbour1; neighbour2; neighbour3]})

        [<Test>]
        let ``A dead cell with exactly three live neighbours becomes a live cell``() = 
            let neighbour1 = { Status=Alive; Neighbours=[]}
            let neighbour2 = { Status=Alive; Neighbours=[]}
            let neighbour3 = { Status=Alive; Neighbours=[]}

            Assert.IsTrue(isCellAlive { Status=Dead; Neighbours=[neighbour1; neighbour2; neighbour3]})