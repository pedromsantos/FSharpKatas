namespace BowlingV2.FSharpKatas

    module Bowling = 
        open System
        
        type RollType = Strike | Spare | Ball
        type Roll = RollType * int
        
        let maxStrikes = 10
        let maxBalls = 20
            
        let parse roll =
            match roll with
            | '-' -> Ball, 0
            | '/' -> Spare, 10
            | 'X' -> Strike, 10
            | r -> Ball, Int32.Parse(r.ToString())
        
        let scoreRoll index rolls =
            let rollList = rolls |> List.ofSeq
            
            let previousRollValue = fun () -> snd rollList.[index - 1]
            let firstBonusBall = fun () -> snd rollList.[index + 1]
            let secondBonusBall = fun () -> snd rollList.[index + 2]
            
            match rollList.[index] with
                | (Strike, _) when index >= maxStrikes -> 0
                | (Ball, _) when index >= maxBalls -> 0
                | (Spare, value) -> value - previousRollValue() + firstBonusBall()
                | (Strike, value) -> value + firstBonusBall() + secondBonusBall()
                | (Ball, value) -> value
        
        let scoreGame rolls =
            let parsedRolls = rolls |> Seq.map parse
            
            parsedRolls
            |> Seq.mapi (fun index _ -> scoreRoll index parsedRolls)
            |> Seq.sum

    module BowlingTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Bowling

        [<Test>]
        let ``Score should be 0 for input "--" ``() =
            test <@ scoreGame "--" = 0 @>
        
        [<Test>]
        let ``Score should be 2 for input "11" ``() =
            test <@ scoreGame "11" = 2 @>
            
        [<Test>]
        let ``Score should be 18 for input "5/4" ``() =
            test <@ scoreGame "5/4" = 18 @>
            
        [<Test>]
        let ``Score should be 28 for input "X54" ``() =
            test <@ scoreGame "X54" = 28 @>
            
        [<Test>]
        let ``Score should be 90 for input "9-9-9-9-9-9-9-9-9-9-" ``() =
            test <@ scoreGame "9-9-9-9-9-9-9-9-9-9-" = 90 @>
            
        [<Test>]
        let ``Score should be 150 for input "5/5/5/5/5/5/5/5/5/5/5" ``() =
            test <@ scoreGame "5/5/5/5/5/5/5/5/5/5/5" = 150 @>
            
        [<Test>]
        let ``Score should be 300 for input "XXXXXXXXXXXX" ``() =
            test <@ scoreGame "XXXXXXXXXXXX" = 300 @>