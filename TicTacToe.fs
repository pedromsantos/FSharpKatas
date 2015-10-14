namespace FSharpKatas

    module TicTacToe =

        type Players = | X | O 

        type TurnStatus = | InvalidMove | InProgress | Winner | Draw

        type Rows = | Top = 0 | Middle = 1 | Bottom = 2 | None = -1

        type Columns = | Left = 0 | Middle = 1 | Right = 2 | None = -1

        type Turn = { Player:Players; Row:Rows; Column:Columns }

        type Turns = Turn list 

        type TurnOutcome = TurnStatus * Turns

        type Init = unit -> Turns

        type TicTacToe = Turn -> Turns -> TurnOutcome 

        let private lastTurn (turns:Turns) = 
            turns.Head

        let private lastTurnPlayer turns =
            (lastTurn turns).Player
        
        let private saveTurn turn turns =
            turn :: turns

        let private isPositionOcupied turn turns =
            turns |> Seq.exists (fun t -> t.Column = turn.Column && t.Row = turn.Row)

        let private isPositionEmpty turn turns =
            turns |> isPositionOcupied turn |> not

        let private isPlayerTurn turn turns =
            turn.Player <> lastTurnPlayer turns
        
        let private playerPreviousTurns player turns =
            turns |> Seq.filter (fun t -> player = t.Player)

        let private howManySatisfyForTurnPlayer filter turns = 
            let player = turns |> lastTurnPlayer
            turns
            |> playerPreviousTurns player
            |> Seq.filter filter 
            |> Seq.length

        let private hasThreeInARow turns = 
            let turn = lastTurn turns
            3 = (turns |> howManySatisfyForTurnPlayer (fun t -> turn.Row = t.Row))

        let private hasThreeInAColumn turns = 
            let turn = lastTurn turns
            3 = (turns |> howManySatisfyForTurnPlayer (fun t -> turn.Column = t.Column))

        let private hasThreeInLeftToRightDiagonal turns =
            3 = (turns |> howManySatisfyForTurnPlayer (fun t -> int32 t.Row = int32 t.Column))

        let private hasThreeInRightToLeftDiagonal turns = 
            1 = (turns |> howManySatisfyForTurnPlayer (fun t -> t.Row = Rows.Top && t.Column = Columns.Right)) &&
            1 = (turns |> howManySatisfyForTurnPlayer (fun t -> t.Row = Rows.Middle && t.Column = Columns.Middle)) &&
            1 = (turns |> howManySatisfyForTurnPlayer (fun t -> t.Row = Rows.Bottom && t.Column = Columns.Left))

        let private isWinner turns =
            let verify f = f turns

            verify hasThreeInARow
            || verify hasThreeInAColumn 
            || verify hasThreeInLeftToRightDiagonal
            || verify hasThreeInRightToLeftDiagonal

        let private isDraw turns =
            (turns |> Seq.length) > 9

        let private isValidTurn turn turns =
            match (turns |> isPlayerTurn turn, turns |> isPositionEmpty turn) with
            | (false, _) -> false 
            | (_, false) -> false 
            | _ -> true

        let init:Init = fun() ->
            [{ Player = Players.O; Row = Rows.None; Column = Columns.None }]

        let ticTacToe:TicTacToe = fun turn turns ->
            let turnsWithNewTurn = turns |> saveTurn turn
            let verify f = f turnsWithNewTurn

            if not (turns |> isValidTurn turn) then (TurnStatus.InvalidMove, turns)
            elif verify isWinner then (TurnStatus.Winner, turnsWithNewTurn)
            elif verify isDraw then (TurnStatus.Draw, turnsWithNewTurn)
            else (TurnStatus.InProgress, turnsWithNewTurn)

    module TicTacToeTests =
        open NUnit.Framework
        open FsUnit
        open TicTacToe

        [<Test>]
        let ``Should not allow player O to play first``()  =
            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.O; Row = Rows.Top; Column = Columns.Left }
                |> fst

            turnStatus |> should equal TurnStatus.InvalidMove
        
        [<Test>]
        let ``Should allow player X to play first``()  =
            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.X; Row = Rows.Top; Column = Columns.Left }
                |> fst

            turnStatus |> should equal TurnStatus.InProgress

        [<Test>]
        let ``Should allow player O to play second``()  =
            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.X; Row = Rows.Top; Column = Columns.Left }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.Middle; Column = Columns.Left } 
                |> fst

            turnStatus |> should equal TurnStatus.InProgress
        
        [<Test>]
        let ``Should enforce players alternate``()  =
            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.X; Row = Rows.Top; Column = Columns.Left }
                |> snd
                |> ticTacToe { Player = Players.X; Row = Rows.Middle; Column = Columns.Left }
                |> fst

            turnStatus |> should equal TurnStatus.InvalidMove
        
        [<Test>]
        let ``Should not allow turn with same row and column as last one``()  =
            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.X; Row = Rows.Top; Column = Columns.Left }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.Top; Column = Columns.Left }
                |> fst 

            turnStatus |> should equal TurnStatus.InvalidMove

        [<Test>]
        let ``Should not allow to play in any previously played positions``()  =
            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.X; Row = Rows.Top; Column = Columns.Left }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.Middle; Column = Columns.Left }
                |> snd 
                |> ticTacToe { Player = Players.X; Row = Rows.Top; Column = Columns.Left }
                |> fst

            turnStatus |> should equal TurnStatus.InvalidMove

        [<TestCase(Rows.Top, Rows.Middle)>]
        [<TestCase(Rows.Middle, Rows.Bottom)>]
        [<TestCase(Rows.Bottom, Rows.Top)>]
        let ``Should declare player as winner if he has three in any row``(winnerRow, looserRow)  =
            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.X; Row = winnerRow; Column = Columns.Left }
                |> snd
                |> ticTacToe { Player = Players.O; Row = looserRow; Column = Columns.Left }
                |> snd 
                |> ticTacToe { Player = Players.X; Row = winnerRow; Column = Columns.Middle }
                |> snd
                |> ticTacToe { Player = Players.O; Row = looserRow; Column = Columns.Middle }
                |> snd
                |> ticTacToe { Player = Players.X; Row = winnerRow; Column = Columns.Right }
                |> fst

            turnStatus |> should equal TurnStatus.Winner

        [<TestCase(Columns.Left, Columns.Middle)>]
        [<TestCase(Columns.Middle, Columns.Right)>]
        [<TestCase(Columns.Right, Columns.Left)>]
        let ``Should declare player as winner if he has three in any column``(winnerColumn, looserColumn)  =
            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.X; Row = Rows.Top; Column = winnerColumn }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.Top; Column = looserColumn }
                |> snd 
                |> ticTacToe { Player = Players.X; Row = Rows.Middle; Column = winnerColumn }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.Middle; Column = looserColumn }
                |> snd
                |> ticTacToe { Player = Players.X; Row = Rows.Bottom; Column = winnerColumn }
                |> fst

            turnStatus |> should equal TurnStatus.Winner

        [<Test>]
        let ``Should declare player as winner if he has three in left to rigth diagonal``()  =

            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.X; Row = Rows.Top; Column = Columns.Left }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.Top; Column = Columns.Middle }
                |> snd 
                |> ticTacToe { Player = Players.X; Row = Rows.Middle; Column = Columns.Middle }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.Top; Column = Columns.Right }
                |> snd
                |> ticTacToe { Player = Players.X; Row = Rows.Bottom; Column = Columns.Right }
                |> fst

            turnStatus |> should equal TurnStatus.Winner

        [<Test>]
        let ``Should declare player as winner if he has three in rigth to left diagonal``()  =
            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.X; Row = Rows.Top; Column = Columns.Right }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.Top; Column = Columns.Middle }
                |> snd 
                |> ticTacToe { Player = Players.X; Row = Rows.Middle; Column = Columns.Middle }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.Middle; Column = Columns.Right }
                |> snd
                |> ticTacToe { Player = Players.X; Row = Rows.Bottom; Column = Columns.Left }
                |> fst

            turnStatus |> should equal TurnStatus.Winner

        [<Test>]
        let ``Should declare draw if all 9 positions are filled``()  =
            let turnStatus = 
                init()
                |> ticTacToe { Player = Players.X; Row = Rows.Top; Column = Columns.Left }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.Top; Column = Columns.Middle }
                |> snd 
                |> ticTacToe { Player = Players.X; Row = Rows.Top; Column = Columns.Right }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.Middle; Column = Columns.Left }
                |> snd
                |> ticTacToe { Player = Players.X; Row = Rows.Middle; Column = Columns.Middle }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.Bottom; Column = Columns.Right }
                |> snd
                |> ticTacToe { Player = Players.X; Row = Rows.Bottom; Column = Columns.Middle }
                |> snd
                |> ticTacToe { Player = Players.O; Row = Rows.Bottom; Column = Columns.Left }
                |> snd
                |> ticTacToe { Player = Players.X; Row = Rows.Middle; Column = Columns.Right }
                |> fst

            turnStatus |> should equal TurnStatus.Draw