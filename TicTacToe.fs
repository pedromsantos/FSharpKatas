namespace FSharpKatas

    // The object of TicTacToe is to get three in a row. 
    // You play on a three by three game board. 
    // The first player is known as X and the second is O. 
    // Players alternate placing X's and Os on the game board 
    // until either opponent has three in a row or all nine squares are filled. 
    // X always goes first, and in the event that no one has three in a row, 
    // the stalemate is called a cat game.

    module TicTacToe =

        type Players = | X | O 

        type TurnResults = | InvalidMove | InProgress | Winner | Draw

        type Rows = | First = 0 | Second = 1 | Third = 2 | None = -1

        type Columns = | First = 0 | Second = 1 | Third = 2 | None = -1

        type Turn = { Player:Players; Row:Rows; Column:Columns }

        type Game = List<Turn>

        let mutable private turns:Game = []

        let private lastTurn() = 
            turns.Head
        
        let private saveTurn turn =
            turns <- turn :: turns

        let private isPositionEmpty turn =
            not (turns |> Seq.exists (fun t -> t.Column = turn.Column && t.Row = turn.Row))

        let private isPlayerTurn turn =
            turn.Player <> lastTurn().Player
        
        let private playerTurns player =
            turns |> Seq.filter (fun t -> player = t.Player)

        let private howManySatisfy turn filter = 
            playerTurns turn.Player 
            |> Seq.filter filter 
            |> Seq.length

        let private hasThreeInARow turn = 
            3 = (howManySatisfy turn (fun t -> turn.Row = t.Row))

        let private hasThreeInAColumn turn = 
            3 = (howManySatisfy turn (fun t -> turn.Column = t.Column))

        let private hasThreeInLeftToRightDiagonal turn =
            3 = (howManySatisfy turn (fun t -> int32 t.Row = int32 t.Column))

        let private hasThreeInRightToLeftDiagonal turn = 
            1 = (howManySatisfy turn (fun t -> t.Row = Rows.First && t.Column = Columns.Third)) &&
            1 = (howManySatisfy turn (fun t -> t.Row = Rows.Second && t.Column = Columns.Second)) &&
            1 = (howManySatisfy turn (fun t -> t.Row = Rows.Third && t.Column = Columns.First))

        let private isWinner turn =
            hasThreeInARow turn 
            || hasThreeInAColumn turn 
            || hasThreeInLeftToRightDiagonal turn
            || hasThreeInRightToLeftDiagonal turn

        let private isDraw() =
            (turns |> Seq.length) > 9

        let private isValidTurn turn =
            match (isPlayerTurn turn, isPositionEmpty turn) with
            | (false, _) -> false 
            | (_, false) -> false 
            | (_, _) -> 
                        saveTurn turn
                        true

        let ticTacToe turn =
             if not (isValidTurn turn) then TurnResults.InvalidMove
             else if isWinner turn then TurnResults.Winner
             else if isDraw() then TurnResults.Draw
             else TurnResults.InProgress

        let init() = 
            turns <- [{ Player = Players.O; Row = Rows.None; Column = Columns.None }]

    module TicTacToeTests =
        open NUnit.Framework
        open TicTacToe

        [<Test>]
        let ``Should enforce player X to play first``()  =
            init()
            let turn1 = { Player = Players.O; Row = Rows.First; Column = Columns.First }

            let turnResult = ticTacToe turn1

            Assert.That(turnResult, Is.EqualTo(TurnResults.InvalidMove))
        
        [<Test>]
        let ``Should allow player X to play first``()  =
            init()
            let turn1 = { Player = Players.X; Row = Rows.First; Column = Columns.First }

            let turnResult = ticTacToe turn1

            Assert.That(turnResult, Is.EqualTo(TurnResults.InProgress))

        [<Test>]
        let ``Should allow player O to play second``()  =
            init()
            let turn1 = { Player = Players.X; Row = Rows.First; Column = Columns.First } 
            let turn2 = { Player = Players.O; Row = Rows.Second; Column = Columns.First } 
            ticTacToe turn1 |> ignore

            let turnResult = ticTacToe turn2

            Assert.That(turnResult, Is.EqualTo(TurnResults.InProgress))
        
        [<Test>]
        let ``Should enforce players alternate``()  =
            init()
            let turn1 = { Player = Players.X; Row = Rows.First; Column = Columns.First } 
            let turn2 = { Player = Players.X; Row = Rows.Second; Column = Columns.First }
            ticTacToe turn1 |> ignore

            let turnResult = ticTacToe turn2

            Assert.That(turnResult, Is.EqualTo(TurnResults.InvalidMove))
        
        [<Test>]
        let ``Should not allow turn with same row and column as last one``()  =
            init()
            let turn1 = { Player = Players.X; Row = Rows.First; Column = Columns.First }
            let turn2 = { Player = Players.O; Row = Rows.First; Column = Columns.First }
            ticTacToe turn1 |> ignore

            let turnResult = ticTacToe turn2

            Assert.That(turnResult, Is.EqualTo(TurnResults.InvalidMove))

        [<Test>]
        let ``Should not allow to play in any previously played positions``()  =
            init()
            let turn1 = { Player = Players.X; Row = Rows.First; Column = Columns.First }
            let turn2 = { Player = Players.O; Row = Rows.Second; Column = Columns.First }
            let turn3 = { Player = Players.X; Row = Rows.First; Column = Columns.First }
            ticTacToe turn1 |> ignore
            ticTacToe turn2 |> ignore

            let turnResult = ticTacToe turn3

            Assert.That(turnResult, Is.EqualTo(TurnResults.InvalidMove))

        [<TestCase(Rows.First, Rows.Second)>]
        [<TestCase(Rows.Second, Rows.Third)>]
        [<TestCase(Rows.Third, Rows.First)>]
        let ``Should declare player as winner if he has three in any row``(winnerRow, looserRow)  =
            init()
            let turn1 = { Player = Players.X; Row = winnerRow; Column = Columns.First }
            let turn2 = { Player = Players.O; Row = looserRow; Column = Columns.First }
            let turn3 = { Player = Players.X; Row = winnerRow; Column = Columns.Second }
            let turn4 = { Player = Players.O; Row = looserRow; Column = Columns.Second }
            let turn5 = { Player = Players.X; Row = winnerRow; Column = Columns.Third }

            ticTacToe turn1 |> ignore
            ticTacToe turn2 |> ignore
            ticTacToe turn3 |> ignore
            ticTacToe turn4 |> ignore

            let turnResult = ticTacToe turn5

            Assert.That(turnResult, Is.EqualTo(TurnResults.Winner))

        [<TestCase(Columns.First, Columns.Second)>]
        [<TestCase(Columns.Second, Columns.Third)>]
        [<TestCase(Columns.Third, Columns.First)>]
        let ``Should declare player as winner if he has three in any column``(winnerColumn, looserColumn)  =
            init()
            let turn1 = { Player = Players.X; Row = Rows.First; Column = winnerColumn }
            let turn2 = { Player = Players.O; Row = Rows.First; Column = looserColumn }
            let turn3 = { Player = Players.X; Row = Rows.Second; Column = winnerColumn }
            let turn4 = { Player = Players.O; Row = Rows.Second; Column = looserColumn }
            let turn5 = { Player = Players.X; Row = Rows.Third; Column = winnerColumn }

            ticTacToe turn1 |> ignore
            ticTacToe turn2 |> ignore
            ticTacToe turn3 |> ignore
            ticTacToe turn4 |> ignore

            let turnResult = ticTacToe turn5

            Assert.That(turnResult, Is.EqualTo(TurnResults.Winner))

        [<Test>]
        let ``Should declare player as winner if he has three in left to rigth diagonal``()  =
            init()
            let turn1 = { Player = Players.X; Row = Rows.First; Column = Columns.First }
            let turn2 = { Player = Players.O; Row = Rows.First; Column = Columns.Second }
            let turn3 = { Player = Players.X; Row = Rows.Second; Column = Columns.Second }
            let turn4 = { Player = Players.O; Row = Rows.First; Column = Columns.Third }
            let turn5 = { Player = Players.X; Row = Rows.Third; Column = Columns.Third }

            ticTacToe turn1 |> ignore
            ticTacToe turn2 |> ignore
            ticTacToe turn3 |> ignore
            ticTacToe turn4 |> ignore

            let turnResult = ticTacToe turn5

            Assert.That(turnResult, Is.EqualTo(TurnResults.Winner))

        [<Test>]
        let ``Should declare player as winner if he has three in rigth to left diagonal``()  =
            init()
            let turn1 = { Player = Players.X; Row = Rows.First; Column = Columns.Third }
            let turn2 = { Player = Players.O; Row = Rows.First; Column = Columns.Second }
            let turn3 = { Player = Players.X; Row = Rows.Second; Column = Columns.Second }
            let turn4 = { Player = Players.O; Row = Rows.Second; Column = Columns.Third }
            let turn5 = { Player = Players.X; Row = Rows.Third; Column = Columns.First }

            ticTacToe turn1 |> ignore
            ticTacToe turn2 |> ignore
            ticTacToe turn3 |> ignore
            ticTacToe turn4 |> ignore

            let turnResult = ticTacToe turn5

            Assert.That(turnResult, Is.EqualTo(TurnResults.Winner))

        [<Test>]
        let ``Should declare draw if all 9 positions are filled``()  =
            init()
            let turn1 = { Player = Players.X; Row = Rows.First; Column = Columns.First }
            let turn2 = { Player = Players.O; Row = Rows.First; Column = Columns.Second }
            let turn3 = { Player = Players.X; Row = Rows.First; Column = Columns.Third }
            let turn4 = { Player = Players.O; Row = Rows.Second; Column = Columns.First }
            let turn5 = { Player = Players.X; Row = Rows.Second; Column = Columns.Second }
            let turn6 = { Player = Players.O; Row = Rows.Third; Column = Columns.Third }
            let turn7 = { Player = Players.X; Row = Rows.Third; Column = Columns.Second }
            let turn8 = { Player = Players.O; Row = Rows.Third; Column = Columns.First }
            let turn9 = { Player = Players.X; Row = Rows.Second; Column = Columns.Third }

            ticTacToe turn1 |> ignore
            ticTacToe turn2 |> ignore
            ticTacToe turn3 |> ignore
            ticTacToe turn4 |> ignore
            ticTacToe turn5 |> ignore   
            ticTacToe turn6 |> ignore
            ticTacToe turn7 |> ignore
            ticTacToe turn8 |> ignore

            let turnResult = ticTacToe turn9

            Assert.That(turnResult, Is.EqualTo(TurnResults.Draw))