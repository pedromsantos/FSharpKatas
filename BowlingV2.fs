namespace BowlingV2.FSharpKatas

    module Bowling = 
        let scoreGame rols =
            0

    module BowlingTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Bowling

        [<Test>]
        let ``Score should be 0 for input "--" ``() =
            test <@ scoreGame "--" = 0 @>
