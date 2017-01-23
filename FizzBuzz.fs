namespace FizzBuzz.FSharpKatas

    module FizzBuzz =

        let (|DivisibleBy|_|) by x = if x % by = 0 then Some DivisibleBy else None

        let fizzBuzz number =
            match number with
                | DivisibleBy 5 & DivisibleBy 3 -> "FizzBuzz"
                | DivisibleBy 3 -> "Fizz"
                | DivisibleBy 5 -> "Buzz"
                | _ -> number.ToString()

    module FizzBuzzTests =
        open NUnit.Framework
        open Swensen.Unquote
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
        let ``Should fizz buzz number``number expectedFizzBuzz =
            test <@ fizzBuzz number = expectedFizzBuzz @>
