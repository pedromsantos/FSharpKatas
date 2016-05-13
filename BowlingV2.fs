namespace BowlingV2.FSharpKatas

    module Bowling = 
        open System

        type RollType = Strike | Spare | Ball
        type Roll = RollType * int

        let maxBalls = 20

        let parse roll index (rolls:String) =
            let previousBall = fun () -> rolls |> Seq.item (index - 1)
            match roll with
            | '-' -> Ball, 0
            | '/' -> Spare, 10 - Int32.Parse(previousBall().ToString())
            | 'X' -> Strike, 10
            | r -> Ball, Int32.Parse(r.ToString())

        let scoreRoll index rolls =
            let rollValueForIndexPlus pad = 
                if index + pad < Seq.length rolls then snd (Seq.item (index + pad) rolls) else 0        

            let firstBonusBall = fun () -> rollValueForIndexPlus 1
            let secondBonusBall = fun () -> rollValueForIndexPlus 2

            let exceedsMaxBalls = fun() ->
                rolls 
                |> Seq.take index
                |> Seq.map (fun r -> match r with | (Strike, _) -> 2 | _ -> 1)
                |> Seq.sum >= maxBalls

            match Seq.item index rolls with
                | (_, _) when exceedsMaxBalls() -> 0
                | (Spare, value) -> value + firstBonusBall()
                | (Strike, value) -> value + firstBonusBall() + secondBonusBall()
                | (Ball, value) -> value

        let scoreGame rolls =
            let parsedRolls = rolls |> Seq.mapi (fun index roll -> parse roll index rolls)

            parsedRolls
            |> Seq.mapi (fun index _ -> scoreRoll index parsedRolls)
            |> Seq.sum
                     
    module BowlingTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Bowling

        [<Test>]
        let ``calculate scores with no strikes or spares``() =
            test <@ scoreGame "--" = 0 @> 
            test <@ scoreGame "1" = 1 @>
            test <@ scoreGame "13" = 4 @>
            test <@ scoreGame "13521" = 12 @>
            
        [<Test>]
        let ``calculate scores containing a miss``() =
            test <@ scoreGame "1-5-" = 6 @>
            test <@ scoreGame "9-9-9-9-9-9-9-9-9-9-" = 90 @>
            
        [<Test>]
        let ``calculate scores containing spares``() =
            test <@ scoreGame "1/" = 10 @>
            test <@ scoreGame "1/--" = 10 @>
            test <@ scoreGame "1/-5" = 15 @>
            test <@ scoreGame "1/35-" = 21 @>
            test <@ scoreGame "1/3/23" = 30 @>
            test <@ scoreGame "5/5/5/5/5/5/5/5/5/5/5" = 150 @>
        
        [<Test>]
        let ``calculate scores containing strikes``() =
            test <@ scoreGame "X" = 10 @>
            test <@ scoreGame "X--" = 10 @>
            test <@ scoreGame "X--51" = 16 @>
            test <@ scoreGame "X51" = 22 @>
            test <@ scoreGame "XXXXXXXXXXXX" = 300 @>
            test <@ scoreGame "XXXXXXXXXX12" = 274 @>
            test <@ scoreGame "1/35XXX45" = 103 @>
            test <@ scoreGame "1/35XXX458/X35" = 149 @>
            test <@ scoreGame "1/35XXX458/X3/" = 153 @>
            test <@ scoreGame "1/35XXX458/X3/23" = 160 @>
            test <@ scoreGame "1/35XXX458/X3/X" = 173 @>
            test <@ scoreGame "1/35XXX458/X3/XX6" = 189 @>