namespace FSharpKatas

    // The object of Tic Tac Toe is to get three in a row. 
    // You play on a three by three game board. 
    // The first player is known as X and the second is O. 
    // Players alternate placing Xs and Os on the game board 
    // until either oppent has three in a row or all nine squares are filled. 
    // X always goes first, and in the event that no one has three in a row, 
    // the stalemate is called a cat game.

    module TicTacToe =

        type Players = | X | O 

        type TurnResults = | InvalidMove | InProgress

        let mutable private lastTurn = Players.O

        let init() = 
            lastTurn <- Players.O

        let ticTacToe player =
            match player = lastTurn with
            | true -> TurnResults.InvalidMove 
            | false -> 
                lastTurn <- player
                TurnResults.InProgress 

    module TicTacToeTests =
        open NUnit.Framework
        open TicTacToe

        [<Test>]
        let ``Should enforce player X to play first``()  =
            init()
            let turnResult = ticTacToe Players.O
            Assert.That(turnResult, Is.EqualTo(TurnResults.InvalidMove))
        
        [<Test>]
        let ``Should allow player X to play first``()  =
            init()
            let turnResult = ticTacToe Players.X
            Assert.That(turnResult, Is.EqualTo(TurnResults.InProgress))

        [<Test>]
        let ``Should allow player O to play second``()  =
            init()
            ticTacToe Players.X |> ignore
            let turnResult = ticTacToe Players.O
            Assert.That(turnResult, Is.EqualTo(TurnResults.InProgress))
