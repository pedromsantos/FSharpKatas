﻿namespace FSharpKatas

    module Bowling = 

        let private isStrike firstRoll =
            firstRoll = 10

        let private scoreStrike firstRoll firstBonusRoll secondBonusRoll =
            firstRoll + firstBonusRoll + secondBonusRoll

        let private isSpare firstRoll secondRoll =
            firstRoll + secondRoll = 10

        let private scoreSpare firstRoll secondRoll bonusRoll =
            firstRoll + secondRoll + bonusRoll
        
        let private scoreFrame firstRoll secondRoll bonusRolls =
            if isStrike firstRoll && bonusRolls |> List.length >= 2 then
               scoreStrike firstRoll (bonusRolls |> List.head) (bonusRolls |> List.tail |> List.head)
            elif isSpare firstRoll secondRoll then
                scoreSpare firstRoll secondRoll (bonusRolls |> List.head)
            else
                firstRoll + secondRoll 

        let rec private score frames =
            match frames with
            | [] -> 0
            | r1::r2::rest -> (scoreFrame r1 r2 rest) + score rest
            | [r] -> r

        let scoreGame frames =
            let gameScore = score frames
            if gameScore > 300 then
                300
            else
                gameScore

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
            let result = scoreGame (rollSame 20 0)
            result |> should equal 0

        [<Test>]
        let ``Score should be 20 if all rolls in all frames are 1``() =
            let result = scoreGame (rollSame 20 1) 
            result |> should equal 20

        [<Test>]
        let ``Score should be 16 for example single spare in first frame``() =
            let frames = 3::(rollSame 17 0) |> List.append rollSpare
            let result = scoreGame frames 
            result |> should equal 16 

        [<Test>]
        let ``Score should be 24 for example single strike in first frame``() =
            let frames = [10;0;4;3] 
            let result = scoreGame frames 
            result |> should equal 24

        [<Test>]
        let ``Score should be 300 for perfect game``() =
            let result = scoreGame (rollSame 22 10)  
            result |> should equal 300