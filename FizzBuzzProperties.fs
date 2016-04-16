module FizzBuzzProperties.FSharpKatas 
(*
    module FizzBuzzProperties =
        let fizzBuzz (number:int) =
            match (number % 3, number % 5) with
            | (0, 0) -> "FizzBuzz"
            | (0, _) -> "Fizz"
            | (_, 0) -> "Buzz"
            | _ -> number.ToString()

    module FizzBuzzPropertiesTests =
        open FsCheck
        open FsCheck.NUnit
        open FizzBuzzProperties

        let is_numeric text = fst (System.Double.TryParse(text))

        type FizzBuzzGenerator = 
            static member PositiveNumbers () =
                Arb.Default.Int32()
                |> Arb.filter (fun t -> t > 0)

        [<Property(Arbitrary=[| typeof<FizzBuzzGenerator> |] )>]
        let ``Every result is 'Fizz', 'Buzz', 'FizzBuzz' or a decimal string``(number:int) =
            let result = fizzBuzz number
            match result with
            | "Fizz" -> true
            | "Buzz" -> true
            | "FizzBuzz" -> true
            | s when is_numeric s -> true
            | _ -> false

        [<Property(Arbitrary=[| typeof<FizzBuzzGenerator> |] )>]
        let ``Every decimal result is the same as the input``(number:int) =
            let result = fizzBuzz number
            match (System.Int32.TryParse result) with
            | (true, n) when n = number -> true
            | (false, _) -> true
            | (_, _) -> false

        [<Property(Arbitrary=[| typeof<FizzBuzzGenerator> |] )>]
        let ``For Every input multiple of 3 result contains 'Fizz'``(number:int) =
            let result = fizzBuzz number
            match (number % 3, result) with
            | (0, r) when r.Contains("Fizz") -> true
            | (n, r) when n <> 0 && r <> "Fizz" -> true 
            | (_, _) -> false

        [<Property(Arbitrary=[| typeof<FizzBuzzGenerator> |] )>]
        let ``For Every input multiple of 5 result contains 'Buzz'``(number:int) =
            let result = fizzBuzz number
            match (number % 5, result) with
            | (0, r) when r.Contains("Buzz") -> true
            | (n, r) when n <> 0 && r <> "Buzz" -> true 
            | (_, _) -> false

        [<Property(Arbitrary=[| typeof<FizzBuzzGenerator> |] )>]
        let ``For Every input multiple of 3 and 5 result is 'FizzBuzz'``(number:int) =
            let result = fizzBuzz number
            match (number % 15, result) with
            | (0, "FizzBuzz") -> true
            | (n, r) when n <> 0 && r <> "FizzBuzz" -> true
            | (_, _) -> false
*)