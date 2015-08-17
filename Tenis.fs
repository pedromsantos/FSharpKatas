﻿namespace FSharpKatas

    module Tenis =
        open System

        type Points = | Zero = 0 | Fifteen = 15 | Thirty = 30 | Forty = 40 
        type Player = {Name:string; Points:Points}
        
        let newPlayer playerName =
           { Name = playerName; Points = Points.Zero }

        let winball player =
            let allPoints = Enum.GetValues(typeof<Points>)
            let nextPoint = allPoints |> Seq.cast<Points> |> Seq.skipWhile (fun p -> p <> player.Points) |> Seq.skip 1 |> Seq.head
            { Name = player.Name; Points = nextPoint }

    module TenisTests =
        open NUnit.Framework
        open Tenis

        [<Test>]
        let ``Should increase points for ball winner from Zero to Fifteen on first win``() =
            let player = winball(newPlayer "Player1")
            Assert.That(player.Points, Is.EqualTo(Points.Fifteen)) 
            
        [<Test>]
        let ``Should increase points for ball winner from Fifteen to Thirty on second win``() =
            let player = winball(newPlayer "Player1")
            let player = winball(player)
            Assert.That(player.Points, Is.EqualTo(Points.Thirty))