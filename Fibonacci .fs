namespace FSharpKatas

    module Fibonacci =
        let rec fibonacci number =
            match number with
            | 0 -> 0
            | 1 -> 1
            | n -> fibonacci(n - 1) + fibonacci(n - 2)

    module FibonacciTests =
        open NUnit.Framework
        open Fibonacci

        [<TestCase(0, 0)>]
        [<TestCase(1, 1)>]
        [<TestCase(2, 1)>]
        [<TestCase(2, 1)>]
        [<TestCase(3, 2)>]
        [<TestCase(4, 3)>]
        [<TestCase(5, 5)>]
        [<TestCase(6, 8)>]
        [<TestCase(7, 13)>]
        [<TestCase(12, 144)>]
        let ``Should determine fibonacci number for number``(number:int, expectedFibonacciNumber:int) =
            let fibonacciNumber = fibonacci number 
            Assert.AreEqual(expectedFibonacciNumber, fibonacciNumber)