namespace FSharpKatas

    module TicTacToe =

        type Players = | X | O 

        type TurnStatus = | InvalidMove | InProgress | Winner | Draw

        type Rows = | First = 0 | Second = 1 | Third = 2 | None = -1

        type Columns = | First = 0 | Second = 1 | Third = 2 | None = -1

        type Turn = { Player:Players; Row:Rows; Column:Columns }

        type Turns = List<Turn>

        type TurnOutcome = TurnStatus * Turns

        type Init = unit -> Turns

        type TicTacToe = Turn -> Turns -> TurnOutcome 

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

        let init:Init = fun() ->
            [{ Player = Players.O; Row = Rows.None; Column = Columns.None }]

        let ticTacToe:TicTacToe = fun turn turns ->
            let turnsWithNewTurn = saveTurn turn turns
            let verify f = f turnsWithNewTurn
            if not (isValidTurn turn turns) then (TurnStatus.InvalidMove, turns)
            elif verify isWinner then (TurnStatus.Winner, turnsWithNewTurn)
            elif verify isDraw then (TurnStatus.Draw, turnsWithNewTurn)
            else (TurnStatus.InProgress, turnsWithNewTurn)

    module TicTacToeTests =
        open NUnit.Framework
        open TicTacToe

        [<Test>]
        let ``Should enforce player X to play first``()  =
            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.O; Row = Rows.First; Column = Columns.First }
                |> fst

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.InvalidMove))
        
        [<Test>]
        let ``Should allow player X to play first``()  =
            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.X; Row = Rows.First; Column = Columns.First }
                |> fst

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.InProgress))

        [<Test>]
        let ``Should allow player O to play second``()  =
            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.X; Row = Rows.First; Column = Columns.First }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.Second; Column = Columns.First } 
                |> fst

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.InProgress))
        
        [<Test>]
        let ``Should enforce players alternate``()  =
            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.X; Row = Rows.First; Column = Columns.First }
                |> snd
                |> ticTacToe { Player = Players.X; Row = Rows.Second; Column = Columns.First }
                |> fst

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.InvalidMove))
        
        [<Test>]
        let ``Should not allow turn with same row and column as last one``()  =
            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.X; Row = Rows.First; Column = Columns.First }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.First; Column = Columns.First }
                |> fst 

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.InvalidMove))

        [<Test>]
        let ``Should not allow to play in any previously played positions``()  =
            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.X; Row = Rows.First; Column = Columns.First }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.Second; Column = Columns.First }
                |> snd 
                |> ticTacToe { Player = Players.X; Row = Rows.First; Column = Columns.First }
                |> fst

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.InvalidMove))

        [<TestCase(Rows.First, Rows.Second)>]
        [<TestCase(Rows.Second, Rows.Third)>]
        [<TestCase(Rows.Third, Rows.First)>]
        let ``Should declare player as winner if he has three in any row``(winnerRow, looserRow)  =
            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.X; Row = winnerRow; Column = Columns.First }
                |> snd
                |> ticTacToe { Player = Players.O; Row = looserRow; Column = Columns.First }
                |> snd 
                |> ticTacToe { Player = Players.X; Row = winnerRow; Column = Columns.Second }
                |> snd
                |> ticTacToe { Player = Players.O; Row = looserRow; Column = Columns.Second }
                |> snd
                |> ticTacToe { Player = Players.X; Row = winnerRow; Column = Columns.Third }
                |> fst

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.Winner))

        [<TestCase(Columns.First, Columns.Second)>]
        [<TestCase(Columns.Second, Columns.Third)>]
        [<TestCase(Columns.Third, Columns.First)>]
        let ``Should declare player as winner if he has three in any column``(winnerColumn, looserColumn)  =
            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.X; Row = Rows.First; Column = winnerColumn }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.First; Column = looserColumn }
                |> snd 
                |> ticTacToe { Player = Players.X; Row = Rows.Second; Column = winnerColumn }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.Second; Column = looserColumn }
                |> snd
                |> ticTacToe { Player = Players.X; Row = Rows.Third; Column = winnerColumn }
                |> fst

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.Winner))

        [<Test>]
        let ``Should declare player as winner if he has three in left to rigth diagonal``()  =

            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.X; Row = Rows.First; Column = Columns.First }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.First; Column = Columns.Second }
                |> snd 
                |> ticTacToe { Player = Players.X; Row = Rows.Second; Column = Columns.Second }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.First; Column = Columns.Third }
                |> snd
                |> ticTacToe { Player = Players.X; Row = Rows.Third; Column = Columns.Third }
                |> fst

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.Winner))

        [<Test>]
        let ``Should declare player as winner if he has three in rigth to left diagonal``()  =
            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.X; Row = Rows.First; Column = Columns.Third }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.First; Column = Columns.Second }
                |> snd 
                |> ticTacToe { Player = Players.X; Row = Rows.Second; Column = Columns.Second }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.Second; Column = Columns.Third }
                |> snd
                |> ticTacToe { Player = Players.X; Row = Rows.Third; Column = Columns.First }
                |> fst

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.Winner))

        [<Test>]
        let ``Should declare draw if all 9 positions are filled``()  =
            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.X; Row = Rows.First; Column = Columns.First }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.First; Column = Columns.Second }
                |> snd 
                |> ticTacToe { Player = Players.X; Row = Rows.First; Column = Columns.Third }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.Second; Column = Columns.First }
                |> snd
                |> ticTacToe { Player = Players.X; Row = Rows.Second; Column = Columns.Second }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.Third; Column = Columns.Third }
                |> snd
                |> ticTacToe { Player = Players.X; Row = Rows.Third; Column = Columns.Second }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.Third; Column = Columns.First }
                |> snd
                |> ticTacToe { Player = Players.X; Row = Rows.Second; Column = Columns.Third }
                |> fst

            Assert.That(turnStatus, Is.EqualTo(TurnStatus.Draw))