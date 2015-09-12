namespace FSharpKatas

    module LeapYear  =
        let leapYear year =
            match ((year % 4 = 0), (year % 100 = 0), (year % 400 = 0)) with
            | (true, false, _) -> true
            | (true, true, true) -> true
            | _ -> false

    module LeapYearTests =
        open NUnit.Framework
        open FsUnit
        open LeapYear

        [<TestCase(1, false)>]
        [<TestCase(4, true)>]
        [<TestCase(100, false)>]
        [<TestCase(200, false)>]
        [<TestCase(400, true)>]
        [<TestCase(1996, true)>]
        [<TestCase(2000, true)>]
        let ``Should determine if year is a leap year`` year shouldBeLeapYear =
            let isLeapYear = leapYear year 
            isLeapYear |> should equal shouldBeLeapYear
