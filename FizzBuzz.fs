namespace FSharpKatas

    module FizzBuzz =
        let fizzBuzz number =
            match (number % 3, number % 5) with
                | (0, 0) -> "FizzBuzz"
                | (0, _) -> "Fizz"
                | (_, 0) -> "Buzz"
                | (_, _) -> number.ToString()

    module FizzBuzzTests =
        open NUnit.Framework
        open FizzBuzz

        [<TestCase(1, "1")>]
        [<TestCase(2, "2")>]
        [<TestCase(3, "Fizz")>]
        [<TestCase(4, "4")>]
        [<TestCase(5, "Buzz")>]
        [<TestCase(6, "Fizz")>]
        [<TestCase(9, "Fizz")>]
        [<TestCase(10, "Buzz")>]
        [<TestCase(15, "FizzBuzz")>]
        let ``Should fizz buzz number``(number:int, expectedFizzBuzz:string) =
            let fizzBuzzed = fizzBuzz number
            Assert.AreEqual(expectedFizzBuzz, fizzBuzzed)
