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
        open NUnit.Framework
        open LeapYear

        let years = gen { 
            let! year = Gen.choose (1, 2100) 
            return year 
        }

        let isMultipleOf divisor number =
            number % divisor = 0

        let isNotMultipleOfOneHundred number =
            number % 100 <> 0

        let multiplesOfFourButNoOuneHundred = Arb.fromGen (gen { 
            return! (years |> Gen.suchThat (fun y -> 
                isMultipleOf 4 y && (isNotMultipleOfOneHundred y)))
            })

        let multiplesOfFourAndFourHundred = Arb.fromGen ( gen { 
            return! (years |> Gen.suchThat (fun y -> 
                isMultipleOf 4 y && (isMultipleOf 400 y)))
        })

        [<Test>]
        let ``Should be a leap year when divisible by 4 but not by 100``() =
            Prop.forAll multiplesOfFourButNoOuneHundred leapYear
            |> Check.VerboseThrowOnFailure

        [<Test>]
        let ``Should be a leap year when divisible by 4 and by 400``() =
            Prop.forAll multiplesOfFourAndFourHundred leapYear
            |> Check.VerboseThrowOnFailure
