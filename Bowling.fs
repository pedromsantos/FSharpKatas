namespace FSharpKatas

    module Bowling = 
        let score frames =
            0

    module BowlingTests =
        open NUnit.Framework
        open FsUnit
        open Bowling 

        [<Test>]
        let ``Score shoud be 0 if all rools ian all frames are 0``() =
            let result = score [for a in 1..20 -> 0]
            result |> should equal 0