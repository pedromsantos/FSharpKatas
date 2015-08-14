namespace FSharpKatas

    module RomanNumerals =
        let numbersToRomans = [1, "I"; 4, "IV"; 5, "V"; 9, "IX"; 10, "X"; 40, "XL"; 50, "L"; 90, "XC"; 100, "C"; 400, "CD"; 500, "D"; 900, "CM"; 1000, "M"] 
                              |> List.rev

        let conversionFor number =
            numbersToRomans |> List.tryFind (fun x -> number >= fst x)

        let rec numberToRomanNumeral number =
            match (number, conversionFor number) with
            | (0, _) | (_, None) -> ""
            | (n, c) -> snd(c.Value) + numberToRomanNumeral(n - fst(c.Value))

    module RomanNumeralsTests =
        open NUnit.Framework
        open RomanNumerals

        [<TestCase(1, "I")>]
        [<TestCase(2, "II")>]
        [<TestCase(3, "III")>]
        [<TestCase(4, "IV")>]
        [<TestCase(5, "V")>]
        [<TestCase(6, "VI")>]
        [<TestCase(7, "VII")>]
        [<TestCase(8, "VIII")>]
        [<TestCase(9, "IX")>]
        [<TestCase(10, "X")>]
        [<TestCase(11, "XI")>]
        [<TestCase(39, "XXXIX")>]
        [<TestCase(40, "XL")>]
        [<TestCase(44, "XLIV")>]
        [<TestCase(50, "L")>]
        [<TestCase(60, "LX")>]
        [<TestCase(70, "LXX")>]
        [<TestCase(89, "LXXXIX")>]
        [<TestCase(90, "XC")>]
        [<TestCase(100, "C")>]
        [<TestCase(400, "CD")>]
        [<TestCase(500, "D")>]
        [<TestCase(900, "CM")>]
        [<TestCase(1000, "M")>]
        [<TestCase(846, "DCCCXLVI")>]
        [<TestCase(1999, "MCMXCIX")>]
        [<TestCase(2008, "MMVIII")>]
        let ``Should convert number to roman numeral``(number:int, expectedRomanNumeral:string) = 
            let romanNumeral = numberToRomanNumeral number
            Assert.AreEqual(expectedRomanNumeral, romanNumeral)