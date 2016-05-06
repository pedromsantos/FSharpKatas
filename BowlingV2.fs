namespace BowlingV2.FSharpKatas

    module Bowling = 
        open System
        
        let parse roll =
            match roll with
            | '-' -> 0
            | '/' -> 10
            | r -> Int32.Parse(r.ToString())
        
        let scoreRoll index roll (rolls:int list) =
            match roll with
                | 10 -> 10 - rolls.[index - 1] + rolls.[index + 1]
                | _ -> roll
        
        let scoreRolls (rolls:int list) =
            rolls |> Seq.mapi (fun index roll -> scoreRoll index roll rolls) 
        
        let scoreGame (rolls:string) =
            rolls
            |> Seq.map parse
            |> List.ofSeq
            |> scoreRolls
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