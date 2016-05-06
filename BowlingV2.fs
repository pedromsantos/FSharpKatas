namespace BowlingV2.FSharpKatas

    module Bowling = 
        open System
        
        let scoreGame (rolls:string) =
            rolls.Replace('-', '0')
            |> Seq.map (fun r -> Int32.Parse(r.ToString()))
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