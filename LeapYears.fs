namespace LeapYear.FSharpKatas

    module LeapYear  =

        let (|DivisibleBy|_|) by x = if x % by = 0 then Some DivisibleBy else None

        let leapYear year =
            match year with
            | DivisibleBy 400 -> true
            | DivisibleBy 100 -> false
            | DivisibleBy 4   -> true
            | _               -> false

    module LeapYearTests =
        open NUnit.Framework
        open Swensen.Unquote
        open FsCheck
        open LeapYear

        [<TestCase(1, false)>]
        [<TestCase(4, true)>]
        [<TestCase(100, false)>]
        [<TestCase(200, false)>]
        [<TestCase(400, true)>]
        [<TestCase(1996, true)>]
        [<TestCase(2000, true)>]

        let ``Should determine if year is a leap year`` year shouldBeLeapYear =
            test <@ leapYear year = shouldBeLeapYear @>

        let yearsMultipleOf multiple years = 
            years |> Gen.suchThat (fun y -> y % multiple = 0)

        let yearsNotMultipleOf multiple years = 
            years |> Gen.suchThat (fun y -> y % multiple <> 0)

        let years = gen { return! Gen.choose (1, 2266) }

        let multiplesOfFourButNoOuneHundred = 
            Arb.fromGen (years |> yearsMultipleOf 4 |> yearsNotMultipleOf 100)

        let multiplesOfOneHundredButNotFourHundred = 
            Arb.fromGen (years |> yearsMultipleOf 100 |> yearsNotMultipleOf 400)

        let multiplesOfFourAndFourHundred = 
            Arb.fromGen (years |> yearsMultipleOf 4 |> yearsMultipleOf 400)

        let verifyNotLeatYear year =
            not (leapYear year)

        [<Test>]
        let ``Should be a leap year when divisible by 4 but not by 100``() =
            Prop.forAll multiplesOfFourButNoOuneHundred leapYear
            |> Check.VerboseThrowOnFailure

        [<Test>]
        let ``Should not be a leap year when divisible by 100 but not 400``() =
            Prop.forAll multiplesOfOneHundredButNotFourHundred verifyNotLeatYear
            |> Check.VerboseThrowOnFailure

        [<Test>]
        let ``Should be a leap year when divisible by 4 and by 400``() =
            Prop.forAll multiplesOfFourAndFourHundred leapYear
            |> Check.VerboseThrowOnFailure
