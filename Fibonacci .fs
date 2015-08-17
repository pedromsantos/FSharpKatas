namespace FSharpKatas

    module Fibonacci =
        let fibonacci number =
           1

    module FibonacciTests =
        open NUnit.Framework
        open Fibonacci

        [<TestCase(1, 1)>]
        let ``Should determine fibonacci number for number``(number:int, expectedFibonacciNumber:int) =
            let fibonacciNumber = fibonacci number 
            Assert.AreEqual(expectedFibonacciNumber, fibonacciNumber)