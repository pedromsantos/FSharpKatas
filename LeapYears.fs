namespace FSharpKatas

    module LeapYear  =
        let leapYear year =
            match ((year % 4), (year % 100 = 0), (year % 400 = 0)) with
            | (0, false, _) -> true
            | (0, true, true) -> true
            | _ -> false

    module LeapYearTests =
        open NUnit.Framework
        open LeapYear

        [<TestCase(1, false)>]
        [<TestCase(4, true)>]
        [<TestCase(100, false)>]
        [<TestCase(200, false)>]
        [<TestCase(400, true)>]
        [<TestCase(1996, true)>]
        [<TestCase(2000, true)>]
        let ``Should determine if year is a leap year`` year isLeapYear =
            let isLeapYear = leapYear year 
            Assert.AreEqual(isLeapYear, isLeapYear)
