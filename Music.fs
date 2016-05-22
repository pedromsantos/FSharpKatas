namespace Music.FSharpKatas

    module Notes =
        type Note = | C | CSharp | DFlat | D | DSharp | EFlat | E | F | FSharp 
                    | GFlat | G | GSharp | AFlat | A | ASharp | BFlat | B
                    override self.ToString() =
                        match self with
                        | C -> "C" | CSharp -> "C#" | DFlat -> "Db" | D -> "D"
                        | DSharp -> "D#" | EFlat -> "Eb" | E -> "E" | F ->  "F"
                        | FSharp -> "F#" | GFlat -> "Gb" | G -> "G" | GSharp -> "G#"
                        | AFlat -> "Ab" | A -> "A" | ASharp -> "A#" | BFlat -> "Bb"
                        | B -> "B"
                    member self.sharp =
                        match self with
                        | C -> CSharp | CSharp -> D | DFlat -> D | D -> DSharp
                        | DSharp -> E | EFlat -> E | E -> F | F -> FSharp 
                        | FSharp -> G | GFlat -> G | G -> GSharp | GSharp -> A 
                        | AFlat -> A | A -> ASharp | ASharp -> B | BFlat -> B 
                        | B -> C
                    member self.flat =
                        match self with
                        | C -> B | CSharp -> C | DFlat -> C | D -> DFlat
                        | DSharp -> D | EFlat -> D | E -> EFlat | F -> E
                        | FSharp -> F | GFlat -> F | G -> GFlat | GSharp -> G
                        | AFlat -> G | A -> AFlat | ASharp -> A | BFlat -> A
                        | B -> BFlat
                        
        type Interval = | Unisson | MinorSecond | MajorSecond | MinorThird
                        | MajorThird | PerfectForth | DiminishedFifth
                        | PerfectFifth | AugmentedFifth | MajorSixth
                        | MinorSeventh | MajorSeventh | PerfectOctave
                        override self.ToString() =
                            match self with
                            | Unisson -> "Unisson" | MinorSecond -> "MinorSecond" 
                            | MajorSecond -> "MajorSecond" 
                            | MinorThird -> "MinorThird"
                            | MajorThird -> "MajorThird" 
                            | PerfectForth -> "PerfectForth"
                            | DiminishedFifth -> "DiminishedFifth" 
                            | PerfectFifth -> "PerfectFifth"
                            | AugmentedFifth -> "AugmentedFifth" 
                            | MajorSixth -> "MajorSixth" 
                            | MinorSeventh -> "MinorSeventh"
                            | MajorSeventh -> "MajorSeventh" 
                            | PerfectOctave -> "PerfectOctave"
                         static member fromDistance distance =
                                    match distance with
                                    | 0 -> Unisson
                                    | 1 -> MinorSecond
                                    | 2 -> MajorSecond
                                    | 3 -> MinorThird
                                    | 4 -> MajorThird
                                    | 5 -> PerfectForth
                                    | 6 -> DiminishedFifth
                                    | 7 -> PerfectFifth
                                    | 8 -> AugmentedFifth
                                    | 9 -> MajorSixth
                                    | 10 -> MinorSeventh
                                    | 11 -> MajorSeventh
                                    | 12 -> PerfectOctave
                        
    module NotesTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Notes

        [<Test>]
        let ``Should relate note with its name``() =
            test <@ Note.C.ToString() = "C" @>
            test <@ Note.CSharp.ToString() = "C#" @>
            test <@ Note.DFlat.ToString() = "Db" @>
            test <@ Note.D.ToString() = "D" @>
            test <@ Note.DSharp.ToString() = "D#" @>
            test <@ Note.EFlat.ToString() = "Eb" @>
            test <@ Note.E.ToString() = "E" @>
            test <@ Note.F.ToString() = "F" @>
            test <@ Note.FSharp.ToString() = "F#" @>
            test <@ Note.GFlat.ToString() = "Gb" @>
            test <@ Note.G.ToString() = "G" @>
            test <@ Note.GSharp.ToString() = "G#" @>
            test <@ Note.AFlat.ToString() = "Ab" @>
            test <@ Note.A.ToString() = "A" @>
            test <@ Note.ASharp.ToString() = "A#" @>
            test <@ Note.BFlat.ToString() = "Bb" @>
            test <@ Note.B.ToString() = "B" @>
            
        [<Test>]
        let ``Should sharp note``() =
            test <@ Note.C.sharp = Note.CSharp @>
            test <@ Note.CSharp.sharp = Note.D @>
            test <@ Note.DFlat.sharp = Note.D @>
            test <@ Note.D.sharp = Note.DSharp @>
            test <@ Note.DSharp.sharp = Note.E @>
            test <@ Note.EFlat.sharp = Note.E @>
            test <@ Note.E.sharp = Note.F @>
            test <@ Note.F.sharp = Note.FSharp @>
            test <@ Note.FSharp.sharp = Note.G @>
            test <@ Note.GFlat.sharp = Note.G @>
            test <@ Note.G.sharp = Note.GSharp @>
            test <@ Note.GSharp.sharp = Note.A @>
            test <@ Note.AFlat.sharp = Note.A @>
            test <@ Note.A.sharp = Note.ASharp @>
            test <@ Note.ASharp.sharp = Note.B @>
            test <@ Note.BFlat.sharp = Note.B @>
            test <@ Note.B.sharp = Note.C @>
            
        [<Test>]
        let ``Should flat note``() =
            test <@ Note.C.flat = Note.B @>
            test <@ Note.CSharp.flat = Note.C @>
            test <@ Note.DFlat.flat = Note.C @>
            test <@ Note.D.flat = Note.DFlat @>
            test <@ Note.DSharp.flat = Note.D @>
            test <@ Note.EFlat.flat = Note.D @>
            test <@ Note.E.flat = Note.EFlat @>
            test <@ Note.F.flat = Note.E @>
            test <@ Note.FSharp.flat = Note.F @>
            test <@ Note.GFlat.flat = Note.F @>
            test <@ Note.G.flat = Note.GFlat @>
            test <@ Note.GSharp.flat = Note.G @>
            test <@ Note.AFlat.flat = Note.G @>
            test <@ Note.A.flat = Note.AFlat @>
            test <@ Note.ASharp.flat = Note.A @>
            test <@ Note.BFlat.flat = Note.A @>
            test <@ Note.B.flat = Note.BFlat @>
            
        [<Test>]
        let ``Should relate interval with its name``() =
            test <@ Interval.Unisson.ToString() = "Unisson" @>
            test <@ Interval.MinorSecond.ToString() = "MinorSecond" @>
            test <@ Interval.MajorSecond.ToString() = "MajorSecond" @>
            test <@ Interval.MinorThird.ToString() = "MinorThird" @>
            test <@ Interval.MajorThird.ToString() = "MajorThird" @>
            test <@ Interval.PerfectForth.ToString() = "PerfectForth" @>
            test <@ Interval.DiminishedFifth.ToString() = "DiminishedFifth" @>
            test <@ Interval.PerfectFifth.ToString() = "PerfectFifth" @>
            test <@ Interval.AugmentedFifth.ToString() = "AugmentedFifth" @>
            test <@ Interval.MajorSixth.ToString() = "MajorSixth" @>
            test <@ Interval.MinorSeventh.ToString() = "MinorSeventh" @>
            test <@ Interval.MajorSeventh.ToString() = "MajorSeventh" @>
            test <@ Interval.PerfectOctave.ToString() = "PerfectOctave" @>
            
        [<Test>]
        let ``Should relate interval with distances``() =
            test <@ Interval.fromDistance 0 = Unisson @>
            test <@ Interval.fromDistance 1 = MinorSecond @>
            test <@ Interval.fromDistance 2 = MajorSecond @>
            test <@ Interval.fromDistance 3 = MinorThird @>
            test <@ Interval.fromDistance 4 = MajorThird @>
            test <@ Interval.fromDistance 5 = PerfectForth @>
            test <@ Interval.fromDistance 6 = DiminishedFifth @>
            test <@ Interval.fromDistance 7 = PerfectFifth @>
            test <@ Interval.fromDistance 8 = AugmentedFifth @>
            test <@ Interval.fromDistance 9 = MajorSixth @>
            test <@ Interval.fromDistance 10 = MinorSeventh @>
            test <@ Interval.fromDistance 11 = MajorSeventh @>
            test <@ Interval.fromDistance 12 = PerfectOctave @>
