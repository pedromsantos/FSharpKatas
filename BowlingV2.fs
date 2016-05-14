namespace BowlingV2.FSharpKatas

    module Bowling = 
        open System

        type RollType = Strike | Spare | Roll
        type Pins = Pins of int
        type Roll = RollType * Pins
        
        let maxRolls = 20
        let maxPins = 10
        let noPins = 0
        
        let pinsDown roll =
            let (Pins pins) = snd roll
            pins
            
        let pinsFromRoll roll =
            Pins (Int32.Parse(roll.ToString()))
            
        let sparePinsFromRoll roll = 
            Pins (maxPins - Int32.Parse(roll.ToString()))

        let parse roll index rolls =
            let previousRoll = fun () -> Seq.item (index - 1) rolls
            match roll with
            | '-' -> Roll, Pins noPins
            | '/' -> Spare, sparePinsFromRoll(previousRoll())
            | 'X' -> Strike, Pins maxPins
            | r -> Roll, pinsFromRoll r

        let scoreRoll index rolls =
            let bonusRoll = fun(roll) ->  
                if index + roll < Seq.length rolls 
                then pinsDown (Seq.item (index + roll) rolls) 
                else noPins       

            let exceedsMaxRolls = fun() ->
                rolls 
                |> Seq.take index
                |> Seq.map (fun r -> match r with | (Strike, _) -> 2 | _ -> 1)
                |> Seq.sum >= maxRolls

            match Seq.item index rolls with
                | (_, _) when exceedsMaxRolls() -> noPins
                | (Spare, Pins pins) -> pins + bonusRoll 1
                | (Strike, Pins pins) -> pins + bonusRoll 1 + bonusRoll 2
                | (Roll, Pins pins) -> pins

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