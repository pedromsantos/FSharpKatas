namespace FSharpKatas

    module Bowling = 
        
        let isSpare firstRoll secondRoll =
            firstRoll + secondRoll = 10

        let scoreSpare firstRoll secondRoll bonusRoll =
            firstRoll + secondRoll + bonusRoll
        
        let rec score frames =
            match frames with
            | [] -> 0
            | r1::r2::rest -> if isSpare r1 r2 then (scoreSpare r1 r2 (List.head rest)) + score rest else r1 + r2 + score rest
            | [r] -> r

    module BowlingTests =
        open NUnit.Framework
        open FsUnit
        open Bowling 

        let rollSame howMany whatValue =
           [for a in 1..howMany -> whatValue]

        let rollSpare = 
            rollSame 2 5

        [<Test>]
        let ``Score should be 0 if all rolls in all frames are 0``() =
            let result = score (rollSame 20 0)
            result |> should equal 0

        [<Test>]
        let ``Score should be 20 if all rolls in all frames are 1``() =
            let result = score (rollSame 20 1) 
            result |> should equal 20

        [<Test>]
        let ``Score should be 16 for frames: 5 5 3 and all zeros afterwards, (5 5) is a spare``() =
            let frames = 3::(rollSame 17 0) |> List.append rollSpare
            let result = score frames 
            result |> should equal 16 