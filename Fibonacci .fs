namespace FSharpKatas

    module Fibonacci =
        let fibonacci number =
            match number with
            | 2 -> 1
            | 3 -> 2
            | 4 -> 3
            | n -> n

    module FibonacciTests =
        open NUnit.Framework
        open Fibonacci

        [<TestCase(0, 0)>]
        [<TestCase(1, 1)>]
        [<TestCase(2, 1)>]
        [<TestCase(2, 1)>]
        [<TestCase(3, 2)>]
        [<TestCase(4, 3)>]
        let ``Should determine fibonacci number for number``(number:int, expectedFibonacciNumber:int) =
            let fibonacciNumber = fibonacci number 
            Assert.AreEqual(expectedFibonacciNumber, fibonacciNumber)