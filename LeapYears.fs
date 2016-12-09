namespace LeapYear.FSharpKatas

    module LeapYear  =
        let leapYear year =
            match ((year % 4 = 0), (year % 100 = 0), (year % 400 = 0)) with
            | (true, false, _) -> true
            | (true, true, true) -> true
            | _ -> false

    module LeapYearTests =
        open NUnit.Framework
        open Swensen.Unquote
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

    module LeapYearPropertyTests =
        open FsCheck
        open FsCheck.NUnit
        open NUnit.Framework
        open Swensen.Unquote
        open LeapYear

        let years = gen { 
            let! i = Gen.choose (0, 2100) 
            return i 
        }

        let isMultipleOfFour number =
            number % 4 = 0

        let isNotMultipleOfOneHundred number =
            number % 100 <> 0

        let isMultipleOfFourHundred number =
            number % 400 = 0

        let multiplesOfFourButNoOuneHundred = gen { 
            return! (years |> Gen.suchThat (fun y -> isMultipleOfFour y && (isNotMultipleOfOneHundred y)))
        }

        let multiplesOfFourAndFourHundred = gen { 
            return! (years |> Gen.suchThat (fun y -> isMultipleOfFour y && (isMultipleOfFourHundred y)))
        }

        [<Test>]
        let ``Should be a leap year when divisible by 4 but not by 100``() =
            Prop.forAll (Arb.fromGen multiplesOfFourButNoOuneHundred) (fun year -> LeapYear.leapYear year = true)
            |> Check.VerboseThrowOnFailure

        [<Test>]
        let ``Should be a leap year when divisible by 4 and by 400``() =
            Prop.forAll (Arb.fromGen multiplesOfFourAndFourHundred) (fun year -> LeapYear.leapYear year = true)
            |> Check.VerboseThrowOnFailure
