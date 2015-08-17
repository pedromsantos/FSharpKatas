namespace FSharpKatas

    module Tenis =
        type Points = | Zero = 0 | Fifteen = 15 | Thirty = 30 | Forty = 40 
        type Player = {Name:string; Points:Points}
        
        let newPlayer playerName =
           { Name = playerName; Points = Points.Zero} 

        let winball player =
            { Name = player.Name; Points = Points.Fifteen}

    module TenisTests =
        open NUnit.Framework
        open Tenis

        [<Test>]
        let ``Should increase points for ball winner from Zero to Fifteen on first win``() =
            let player = winball(newPlayer "Player1")
            Assert.That(player.Points, Is.EqualTo(Points.Fifteen))