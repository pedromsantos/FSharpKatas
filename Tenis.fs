namespace FSharpKatas

    module Tenis =
        open System

        type Points = | Zero = 0 | Fifteen = 15 | Thirty = 30 | Forty = 40 
        type Player = {Name:string; Points:Points}
        type Score = 
            | Points of string*string 
            | Advantage of string

        let allPoints = 
            Enum.GetValues(typeof<Points>) 
            |> Seq.cast<Points>  

        let nextPoint player = 
                allPoints
                |> Seq.skipWhile (fun p -> p <> player.Points) 
                |> Seq.skip 1 
                |> Seq.head

        
        let newPlayer playerName =
           { Name = playerName; Points = Points.Zero }

        let winball player =
            { Name = player.Name; Points = nextPoint player }

        let score player1 player2 =
            match (player1.Points, player2.Points) with
            | (Points.Forty, Points.Forty) -> "Deuce"
            | (p1, p2) -> "" + (int p1).ToString() + "-" + (int p2).ToString()
            

    module TenisTests =
        open NUnit.Framework
        open Tenis

        [<Test>]
        let ``Should increase points for ball winner from Zero to Fifteen on first win``() =
            let player = newPlayer "Player1" |> winball

            Assert.That(player.Points, Is.EqualTo(Points.Fifteen)) 
            
        [<Test>]
        let ``Should increase points for ball winner from Fifteen to Thirty on second win``() =
            let player = newPlayer "Player1" |> winball |> winball
            
            Assert.That(player.Points, Is.EqualTo(Points.Thirty))

        [<Test>]
        let ``Should increase points for ball winner from Thirty to Forty on third win``() =
            let player = newPlayer "Player1" |> winball |> winball |> winball

            Assert.That(player.Points, Is.EqualTo(Points.Forty))

        [<Test>]
        let ``Should calculate score Zero - Zero if no player has won a ball``() =
            let player1 = newPlayer "Player1"
            let player2 = newPlayer "Player2"

            Assert.That(score player1 player2, Is.EqualTo("0-0"))
        
        [<Test>]
        let ``Should calculate score Deuce if both palyers have 40 points``() =
            let player1 = newPlayer "Player1"
            let player2 = newPlayer "Player2"

            let player1 = winball player1 
            let player2 = winball player2          

            let player1 = winball player1 
            let player2 = winball player2          
            
            let player1 = winball player1 
            let player2 = winball player2
            
            Assert.That(score player1 player2, Is.EqualTo("Deuce"))

        