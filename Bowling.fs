namespace FSharpKatas

    module Bowling = 
        let score frames =
            frames |> Seq.sum 

    module BowlingTests =
        open NUnit.Framework
        open FsUnit
        open Bowling 

        [<Test>]
        let ``Score should be 0 if all rolls in all frames are 0``() =
            let result = score [for a in 1..20 -> 0]
            result |> should equal 0

        [<Test>]
        let ``Score should be 20 if all rolls in all frames are 1``() =
            let result = score [for a in 1..20 -> 1]
            result |> should equal 20


