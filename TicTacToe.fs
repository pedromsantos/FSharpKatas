namespace FSharpKatas

    module TicTacToe =

        type Players = | X | O 

        type TurnStatus = | InvalidMove | InProgress | Winner | Draw

        type Rows = | First = 0 | Second = 1 | Third = 2 | None = -1

        type Columns = | First = 0 | Second = 1 | Third = 2 | None = -1

        type Turn = { Player:Players; Row:Rows; Column:Columns }

        type Turns = List<Turn>

        type TurnOutcome = TurnStatus * Turns

        type TicTacToe = Turn -> Turns -> TurnOutcome

        type Init = unit -> Turns

        let private lastTurn (turns:Turns) = 
            turns.Head

        let private lastTurnPlayer turns =
            (lastTurn turns).Player
        
        let private saveTurn turn turns =
            turn :: turns

        let private isPositionEmpty turn turns =
            not (turns |> Seq.exists (fun t -> t.Column = turn.Column && t.Row = turn.Row))

        let private isPlayerTurn turn turns =
            turn.Player <> lastTurnPlayer turns
        
        let private playerPreviousTurns player turns =
            turns |> Seq.filter (fun t -> player = t.Player)

        let private howManySatisfy turns filter = 
            let turn = lastTurn turns
            playerPreviousTurns (lastTurnPlayer turns) turns 
            |> Seq.filter filter 
            |> Seq.length

        let private hasThreeInARow turns = 
            let turn = lastTurn turns
            3 = (howManySatisfy turns (fun t -> turn.Row = t.Row))

        let private hasThreeInAColumn turns = 
            let turn = lastTurn turns
            3 = (howManySatisfy turns (fun t -> turn.Column = t.Column))

        let private hasThreeInLeftToRightDiagonal turns =
            let turn = lastTurn turns
            3 = (howManySatisfy turns (fun t -> int32 t.Row = int32 t.Column))

        let private hasThreeInRightToLeftDiagonal turns = 
            1 = (howManySatisfy turns (fun t -> t.Row = Rows.First && t.Column = Columns.Third)) &&
            1 = (howManySatisfy turns (fun t -> t.Row = Rows.Second && t.Column = Columns.Second)) &&
            1 = (howManySatisfy turns (fun t -> t.Row = Rows.Third && t.Column = Columns.First))

        let private isWinner turns =
            let verify f = f turns

            verify hasThreeInARow
            || verify hasThreeInAColumn 
            || verify hasThreeInLeftToRightDiagonal
            || verify hasThreeInRightToLeftDiagonal

        let private isDraw turns =
            (turns |> Seq.length) > 9

        let private isValidTurn turn turns =
            match (isPlayerTurn turn turns, isPositionEmpty turn turns) with
            | (false, _) -> false 
            | (_, false) -> false 
            | (_, _) -> true

        let ticTacToe:TicTacToe = fun turn turns ->
            let turnsWithNewTurn = saveTurn turn turns
            let verify f = f turnsWithNewTurn
            if not (isValidTurn turn turns) then (TurnStatus.InvalidMove, turns)
            elif verify isWinner then (TurnStatus.Winner, turnsWithNewTurn)
            elif verify isDraw then (TurnStatus.Draw, turnsWithNewTurn)
            else (TurnStatus.InProgress, turnsWithNewTurn)

        let init:Init = fun() ->
            [{ Player = Players.O; Row = Rows.None; Column = Columns.None }]

    module TicTacToeTests =
        open NUnit.Framework
        open TicTacToe

        [<Test>]
        let ``Should enforce player X to play first``()  =
            let turns = init()
            let turn1 = { Player = Players.O; Row = Rows.First; Column = Columns.First }

            let (turnStatus, _) = ticTacToe turn1 turns

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.InvalidMove))
        
        [<Test>]
        let ``Should allow player X to play first``()  =
            let turns = init()
            let turn1 = { Player = Players.X; Row = Rows.First; Column = Columns.First }

            let (turnStatus, _) = ticTacToe turn1 turns

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.InProgress))

        [<Test>]
        let ``Should allow player O to play second``()  =
            let turns = init()
            let turn1 = { Player = Players.X; Row = Rows.First; Column = Columns.First } 
            let turn2 = { Player = Players.O; Row = Rows.Second; Column = Columns.First } 
            let (_, turns) = ticTacToe turn1 turns

            let (turnStatus, _) = ticTacToe turn2 turns

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.InProgress))
        
        [<Test>]
        let ``Should enforce players alternate``()  =
            let turns = init()
            let turn1 = { Player = Players.X; Row = Rows.First; Column = Columns.First } 
            let turn2 = { Player = Players.X; Row = Rows.Second; Column = Columns.First }
            let (_, turns) = ticTacToe turn1 turns

            let (turnStatus, _) = ticTacToe turn2 turns

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.InvalidMove))
        
        [<Test>]
        let ``Should not allow turn with same row and column as last one``()  =
            let turns = init()
            let turn1 = { Player = Players.X; Row = Rows.First; Column = Columns.First }
            let turn2 = { Player = Players.O; Row = Rows.First; Column = Columns.First }
            let (_, turns) = ticTacToe turn1 turns

            let (turnStatus, _) = ticTacToe turn2 turns

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.InvalidMove))

        [<Test>]
        let ``Should not allow to play in any previously played positions``()  =
            let turns = init()
            let turn1 = { Player = Players.X; Row = Rows.First; Column = Columns.First }
            let turn2 = { Player = Players.O; Row = Rows.Second; Column = Columns.First }
            let turn3 = { Player = Players.X; Row = Rows.First; Column = Columns.First }
            let (_, turns) = ticTacToe turn1 turns
            let (_, turns) = ticTacToe turn2 turns

            let (turnStatus, _) = ticTacToe turn3 turns

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.InvalidMove))

        [<TestCase(Rows.First, Rows.Second)>]
        [<TestCase(Rows.Second, Rows.Third)>]
        [<TestCase(Rows.Third, Rows.First)>]
        let ``Should declare player as winner if he has three in any row``(winnerRow, looserRow)  =
            let turns = init()
            let turn1 = { Player = Players.X; Row = winnerRow; Column = Columns.First }
            let turn2 = { Player = Players.O; Row = looserRow; Column = Columns.First }
            let turn3 = { Player = Players.X; Row = winnerRow; Column = Columns.Second }
            let turn4 = { Player = Players.O; Row = looserRow; Column = Columns.Second }
            let turn5 = { Player = Players.X; Row = winnerRow; Column = Columns.Third }

            let (_, turns) = ticTacToe turn1 turns
            let (_, turns) = ticTacToe turn2 turns
            let (_, turns) = ticTacToe turn3 turns
            let (_, turns) = ticTacToe turn4 turns

            let (turnStatus, _) = ticTacToe turn5 turns

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.Winner))

        [<TestCase(Columns.First, Columns.Second)>]
        [<TestCase(Columns.Second, Columns.Third)>]
        [<TestCase(Columns.Third, Columns.First)>]
        let ``Should declare player as winner if he has three in any column``(winnerColumn, looserColumn)  =
            let turns = init()
            let turn1 = { Player = Players.X; Row = Rows.First; Column = winnerColumn }
            let turn2 = { Player = Players.O; Row = Rows.First; Column = looserColumn }
            let turn3 = { Player = Players.X; Row = Rows.Second; Column = winnerColumn }
            let turn4 = { Player = Players.O; Row = Rows.Second; Column = looserColumn }
            let turn5 = { Player = Players.X; Row = Rows.Third; Column = winnerColumn }

            let (_, turns) = ticTacToe turn1 turns
            let (_, turns) = ticTacToe turn2 turns
            let (_, turns) = ticTacToe turn3 turns
            let (_, turns) = ticTacToe turn4 turns

            let (turnStatus, _) = ticTacToe turn5 turns

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.Winner))

        [<Test>]
        let ``Should declare player as winner if he has three in left to rigth diagonal``()  =
            let turns = init()
            let turn1 = { Player = Players.X; Row = Rows.First; Column = Columns.First }
            let turn2 = { Player = Players.O; Row = Rows.First; Column = Columns.Second }
            let turn3 = { Player = Players.X; Row = Rows.Second; Column = Columns.Second }
            let turn4 = { Player = Players.O; Row = Rows.First; Column = Columns.Third }
            let turn5 = { Player = Players.X; Row = Rows.Third; Column = Columns.Third }

            let (_, turns) = ticTacToe turn1 turns
            let (_, turns) = ticTacToe turn2 turns
            let (_, turns) = ticTacToe turn3 turns
            let (_, turns) = ticTacToe turn4 turns

            let (turnStatus, _) = ticTacToe turn5 turns

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.Winner))

        [<Test>]
        let ``Should declare player as winner if he has three in rigth to left diagonal``()  =
            let turns = init()
            let turn1 = { Player = Players.X; Row = Rows.First; Column = Columns.Third }
            let turn2 = { Player = Players.O; Row = Rows.First; Column = Columns.Second }
            let turn3 = { Player = Players.X; Row = Rows.Second; Column = Columns.Second }
            let turn4 = { Player = Players.O; Row = Rows.Second; Column = Columns.Third }
            let turn5 = { Player = Players.X; Row = Rows.Third; Column = Columns.First }

            let (_, turns) = ticTacToe turn1 turns
            let (_, turns) = ticTacToe turn2 turns
            let (_, turns) = ticTacToe turn3 turns
            let (_, turns) = ticTacToe turn4 turns

            let (turnStatus, _) = ticTacToe turn5 turns

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.Winner))

        [<Test>]
        let ``Should declare draw if all 9 positions are filled``()  =
            let turns = init()
            let turn1 = { Player = Players.X; Row = Rows.First; Column = Columns.First }
            let turn2 = { Player = Players.O; Row = Rows.First; Column = Columns.Second }
            let turn3 = { Player = Players.X; Row = Rows.First; Column = Columns.Third }
            let turn4 = { Player = Players.O; Row = Rows.Second; Column = Columns.First }
            let turn5 = { Player = Players.X; Row = Rows.Second; Column = Columns.Second }
            let turn6 = { Player = Players.O; Row = Rows.Third; Column = Columns.Third }
            let turn7 = { Player = Players.X; Row = Rows.Third; Column = Columns.Second }
            let turn8 = { Player = Players.O; Row = Rows.Third; Column = Columns.First }
            let turn9 = { Player = Players.X; Row = Rows.Second; Column = Columns.Third }

            let (_, turns) = ticTacToe turn1 turns
            let (_, turns) = ticTacToe turn2 turns
            let (_, turns) = ticTacToe turn3 turns
            let (_, turns) = ticTacToe turn4 turns
            let (_, turns) = ticTacToe turn5 turns
            let (_, turns) = ticTacToe turn6 turns
            let (_, turns) = ticTacToe turn7 turns
            let (_, turns) = ticTacToe turn8 turns

            let (turnStatus, _) = ticTacToe turn9 turns

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.Draw))