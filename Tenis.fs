namespace Tenis.FSharpKatas

    module Tenis =
        open System

        type Points =
            | Zero
            | Fifteen
            | Thirty
            | Forty
            | Deuce
            | Advantage
            | Win

        type Player = {Name:string; Points:Points}

        type Game = Player*Player 

        let newPlayer name = 
            { Name = name; Points = Points.Zero }

        let updatePlayer name points = 
            { Name = name; Points = points }

        let firstWinsBall (winner, looser) =
            match (winner.Points, looser.Points) with
                | (Deuce, Advantage) -> (updatePlayer winner.Name Points.Deuce, updatePlayer looser.Name Points.Deuce)
                | (Advantage, _) -> (updatePlayer winner.Name Points.Win, updatePlayer looser.Name Points.Deuce)
                | (Forty, Forty) -> (updatePlayer winner.Name Points.Advantage, updatePlayer looser.Name Points.Deuce)
                | (Deuce, Deuce) -> (updatePlayer winner.Name Points.Advantage, looser)
                | (Zero, _) -> (updatePlayer winner.Name Points.Fifteen, looser)
                | (Fifteen, _) -> (updatePlayer winner.Name Points.Thirty, looser)
                | (Thirty, _) -> (updatePlayer winner.Name Points.Forty, looser)
                | (Forty, _) -> (updatePlayer winner.Name Points.Win, looser)
        
        let reverseGamePlayers game =
            (snd game, fst game)

        let secondWinsBall (looser, winner) =
            firstWinsBall (winner, looser) |> reverseGamePlayers

        let represent point =
           match point with
            | Zero -> "0"
            | Fifteen -> "15"
            | Thirty -> "30"
            | Forty -> "40"
            | Deuce -> "Deuce"
            | Advantage -> "Advantage"
            | Win -> "Winner"

        let score game =
            let player1, player2 = game
            match (player1.Points, player2.Points) with
            | (Points.Forty, Points.Forty) -> "Deuce"
            | (Points.Deuce, Points.Deuce) -> "Deuce"
            | (Points.Advantage, Points.Deuce) -> "Advantage " + player1.Name
            | (Points.Deuce, Points.Advantage) -> "Advantage " + player2.Name
            | (Points.Win, _) -> "Winner " + player1.Name
            | (_, Points.Win) -> "Winner " + player2.Name
            | (p1, p2) -> represent p1 + "-" + represent p2
            
    module TenisTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Tenis

        [<Test>]
        let ``Should increase points for ball winner from Zero to Fifteen on first win``() =
            let game = newPlayer "Player1", newPlayer "Player2"

            let palyer1, _ = firstWinsBall (game)

            test <@ palyer1.Points = Points.Fifteen @>
        
        [<Test>]
        let ``Should increase points for ball winner from Zero to Fifteen on first win for second player``() =
            let game = newPlayer "Player1", newPlayer "Player2"

            let _, player2 = secondWinsBall (game)

            test <@ player2.Points = Points.Fifteen @>

        [<Test>]
        let ``Should increase points for ball winner from Fifteen to Thirty on second win``() =
            let player1, _ = (newPlayer "Player1", newPlayer "Player2") |> firstWinsBall |> firstWinsBall

            test <@ player1.Points = Points.Thirty @>

        [<Test>]
        let ``Should increase points for ball winner from Fifteen to Thirty on second win for second player``() =
            let _, player2 = (newPlayer "Player1", newPlayer "Player2") |> secondWinsBall |> secondWinsBall

            test <@ player2.Points = Points.Thirty @>

        [<Test>]
        let ``Should increase points for ball winner from Thirty to Forty on third win``() =
            let player1, _ = (newPlayer "Player1", newPlayer "Player2") |> firstWinsBall |> firstWinsBall |> firstWinsBall

            test <@ player1.Points = Points.Forty @>

        [<Test>]
        let ``Should increase points for ball winner from Thirty to Forty on third win for second player``() =
            let _, player2 = (newPlayer "Player1", newPlayer "Player2") |> secondWinsBall |> secondWinsBall |> secondWinsBall

            test <@ player2.Points = Points.Forty @>

        [<Test>]
        let ``Should calculate score 0 - 0 if no player has won a ball``() =
            let game = (newPlayer "Player1", newPlayer "Player 2")

            test <@ score game = "0-0" @>

        [<Test>]
        let ``Should calculate score 15 - 0 if first player has won a ball``() =
            let game = (newPlayer "Player1", newPlayer "Player 2") |> firstWinsBall

            test <@ score game = "15-0" @>

        [<Test>]
        let ``Should calculate score winner Player1 if first player has won a ball having 40 points``() =
            let game = (newPlayer "Player1", newPlayer "Player 2") |> firstWinsBall |> firstWinsBall |> firstWinsBall |> firstWinsBall

            test <@ score game = "Winner Player1" @> 

        [<Test>]
        let ``Should calculate score Deuce if both playeres have 40 points``() =
            let game = 
                (newPlayer "Player1", newPlayer "Player 2") 
                |> firstWinsBall 
                |> secondWinsBall 
                |> firstWinsBall 
                |> secondWinsBall
                |> firstWinsBall 
                |> secondWinsBall

            test <@ score game = "Deuce" @>

        [<Test>]
        let ``Should calculate score Advantage if playeres are deuce and player 1 wins ball``() =
            let game = 
                (newPlayer "Player 1", newPlayer "Player 2") 
                |> firstWinsBall 
                |> secondWinsBall 
                |> firstWinsBall 
                |> secondWinsBall
                |> firstWinsBall 
                |> secondWinsBall
                |> firstWinsBall 
            
            test <@ score game = "Advantage Player 1" @>

        [<Test>]
        let ``Should calculate score winner Player1 if playeres are deuce and player 1 wins ball being in advantage``() =
            let game = 
                (newPlayer "Player 1", newPlayer "Player 2") 
                |> firstWinsBall 
                |> secondWinsBall 
                |> firstWinsBall 
                |> secondWinsBall
                |> firstWinsBall 
                |> secondWinsBall
                |> firstWinsBall 
                |> firstWinsBall 
            
            test <@ score game  = "Winner Player 1" @>

        [<Test>]
        let ``Should calculate score Deuce if player that has advantage looses ball``() =
            let game = 
                (newPlayer "Player 1", newPlayer "Player 2") 
                |> firstWinsBall 
                |> secondWinsBall 
                |> firstWinsBall 
                |> secondWinsBall
                |> firstWinsBall 
                |> secondWinsBall
                |> firstWinsBall 
                |> secondWinsBall 
            
            test <@ score game = "Deuce" @>