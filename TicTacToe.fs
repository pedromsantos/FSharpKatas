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

        type TurnResults = | InvalidMove | InProgress

        type Rows = | First | Second | Third | None

        type Columns = | First | Second | Third | None

        type Turn = { Player:Players; Row:Rows; Column:Columns }

        let mutable private lastTurn = {Player = Players.O; Row = Rows.None; Column = Columns.None }

        let init() = 
            lastTurn <- {Player = Players.O; Row = Rows.None; Column = Columns.None }

        let saveTurn turn =
            lastTurn <- turn
        
        let isValidPositionTurn turn =
            match (turn.Row = lastTurn.Row, turn.Column = lastTurn.Column) with
            | (true, true) -> false 
            | (_, _) -> true  

        let isValidPlayerTurn turn =
            turn.Player <> lastTurn.Player

        let isValidTurn turn =
            match (isValidPlayerTurn turn, isValidPositionTurn turn) with
            | (false, _) -> TurnResults.InvalidMove
            | (_, false) -> TurnResults.InvalidMove
            | (_, _) -> 
                        saveTurn turn
                        TurnResults.InProgress

        let ticTacToe turn =
             let result = isValidTurn turn 
             result

    module TicTacToeTests =
        open NUnit.Framework
        open TicTacToe

        [<Test>]
        let ``Should enforce player X to play first``()  =
            init()
            let turnResult = ticTacToe { Player = Players.O; Row = Rows.First; Column = Columns.First }
            Assert.That(turnResult, Is.EqualTo(TurnResults.InvalidMove))
        
        [<Test>]
        let ``Should allow player X to play first``()  =
            init()
            let turnResult = ticTacToe { Player = Players.X; Row = Rows.First; Column = Columns.First }
            Assert.That(turnResult, Is.EqualTo(TurnResults.InProgress))

        [<Test>]
        let ``Should allow player O to play second``()  =
            init()
            ticTacToe { Player = Players.X; Row = Rows.First; Column = Columns.First } |> ignore
            let turnResult = ticTacToe { Player = Players.O; Row = Rows.Second; Column = Columns.First }
            Assert.That(turnResult, Is.EqualTo(TurnResults.InProgress))
        
        [<Test>]
        let ``Should enforce players alternate``()  =
            init()
            ticTacToe { Player = Players.X; Row = Rows.First; Column = Columns.First } |> ignore
            let turnResult = ticTacToe { Player = Players.X; Row = Rows.Second; Column = Columns.First }
            Assert.That(turnResult, Is.EqualTo(TurnResults.InvalidMove))
        
        [<Test>]
        let ``Should not allow turn with same row and column as last one``()  =
            init()
            ticTacToe { Player = Players.X; Row = Rows.First; Column = Columns.First } |> ignore
            let turnResult = ticTacToe { Player = Players.O; Row = Rows.First; Column = Columns.First }
            Assert.That(turnResult, Is.EqualTo(TurnResults.InvalidMove))
