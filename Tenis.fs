namespace FSharpKatas

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
            | Win -> "Winner"

        let score game =
            let player1, player2 = game
            match (player1.Points, player2.Points) with
            | (Points.Forty, Points.Forty) -> "Deuce"
            | (Points.Advantage, Points.Deuce) -> "Advantage " + player1.Name
            | (Points.Win, _) -> "Winner " + player1.Name
            | (_, Points.Win) -> "Winner " + player2.Name
            | (p1, p2) -> represent p1 + "-" + represent p2
            

    module TenisTests =
        open NUnit.Framework
        open Tenis

        [<Test>]
        let ``Should increase points for ball winner from Zero to Fifteen on first win``() =
            let game = newPlayer "Player1", newPlayer "Player2"

            let game = firstWinsBall (game)

            Assert.That((fst game).Points, Is.EqualTo(Points.Fifteen)) 
        
        [<Test>]
        let ``Should increase points for ball winner from Zero to Fifteen on first win for second player``() =
            let game = newPlayer "Player1", newPlayer "Player2"

            let game = secondWinsBall (game)

            Assert.That((snd game).Points, Is.EqualTo(Points.Fifteen)) 
            
        [<Test>]
        let ``Should increase points for ball winner from Fifteen to Thirty on second win``() =
            let game = (newPlayer "Player1", newPlayer "Player2") |> firstWinsBall |> firstWinsBall

            Assert.That((fst game).Points, Is.EqualTo(Points.Thirty))

        [<Test>]
        let ``Should increase points for ball winner from Fifteen to Thirty on second win for second player``() =
            let game = (newPlayer "Player1", newPlayer "Player2") |> secondWinsBall |> secondWinsBall

            Assert.That((snd game).Points, Is.EqualTo(Points.Thirty))

        [<Test>]
        let ``Should increase points for ball winner from Thirty to Forty on third win``() =
            let game = (newPlayer "Player1", newPlayer "Player2") |> firstWinsBall |> firstWinsBall |> firstWinsBall

            Assert.That((fst game).Points, Is.EqualTo(Points.Forty))

        [<Test>]
        let ``Should increase points for ball winner from Thirty to Forty on third win for second player``() =
            let game = (newPlayer "Player1", newPlayer "Player2") |> secondWinsBall |> secondWinsBall |> secondWinsBall

            Assert.That((snd game).Points, Is.EqualTo(Points.Forty))

        [<Test>]
        let ``Should calculate score 0 - 0 if no player has won a ball``() =
            let game = (newPlayer "Player1", newPlayer "Player 2")

            Assert.That(score game, Is.EqualTo("0-0")) 

        [<Test>]
        let ``Should calculate score 15 - 0 if first player has won a ball``() =
            let game = (newPlayer "Player1", newPlayer "Player 2") |> firstWinsBall

            Assert.That(score game, Is.EqualTo("15-0"))

        [<Test>]
        let ``Should calculate score winner Player1 if first player has won a ball having 40 points``() =
            let game = (newPlayer "Player1", newPlayer "Player 2") |> firstWinsBall |> firstWinsBall |> firstWinsBall |> firstWinsBall

            Assert.That(score game, Is.EqualTo("Winner Player1")) 

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

            Assert.That(score game, Is.EqualTo("Deuce"))

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
            
            Assert.That(score game, Is.EqualTo("Advantage Player 1"))

        

        