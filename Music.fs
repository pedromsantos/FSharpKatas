namespace Music.FSharpKatas

    module Notes =
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
                                    | _ -> Unisson
                                    
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
                    member self.pitch =
                        match self with
                        | C -> 0 | CSharp -> 1 | DFlat -> 1 | D -> 2
                        | DSharp -> 3 | EFlat -> 3 | E -> 4 | F -> 5
                        | FSharp -> 6 | GFlat -> 6 | G -> 7 | GSharp -> 8
                        | AFlat -> 8 | A -> 9 | ASharp -> 10 | BFlat -> 10
                        | B -> 11
                    member self.measureAbsoluteSemitones (other:Note) =
                        let octave = 12
                        let unisson = 0
            
                        let distance = other.pitch - self.pitch
                        if distance < unisson 
                        then octave - distance * -1 
                        else distance
                    
                    member self.IntervalWith other =
                        Interval.fromDistance(self.measureAbsoluteSemitones other)
                        
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
        let ``Should measure semitones distance``() =
            test <@ Note.C.measureAbsoluteSemitones Note.C = 0 @>
            test <@ Note.C.measureAbsoluteSemitones Note.CSharp = 1 @>
            test <@ Note.C.measureAbsoluteSemitones Note.DFlat = 1 @>
            test <@ Note.C.measureAbsoluteSemitones Note.D = 2 @>
            test <@ Note.C.measureAbsoluteSemitones Note.DSharp = 3 @>
            test <@ Note.C.measureAbsoluteSemitones Note.EFlat = 3 @>
            test <@ Note.C.measureAbsoluteSemitones Note.E = 4 @>
            test <@ Note.C.measureAbsoluteSemitones Note.F = 5 @>
            test <@ Note.C.measureAbsoluteSemitones Note.FSharp = 6 @>
            test <@ Note.C.measureAbsoluteSemitones Note.GFlat = 6 @>
            test <@ Note.C.measureAbsoluteSemitones Note.G = 7 @>
            test <@ Note.C.measureAbsoluteSemitones Note.GSharp = 8 @>
            test <@ Note.C.measureAbsoluteSemitones Note.AFlat = 8 @>
            test <@ Note.C.measureAbsoluteSemitones Note.A = 9 @>
            test <@ Note.C.measureAbsoluteSemitones Note.ASharp = 10 @>
            test <@ Note.C.measureAbsoluteSemitones Note.BFlat = 10 @>
            test <@ Note.C.measureAbsoluteSemitones Note.B = 11 @>
            
        [<Test>]
        let ``Should create interval from distance``() =
            test <@ Note.C.IntervalWith Note.C = Unisson @>
            test <@ Note.C.IntervalWith Note.CSharp = MinorSecond @>
            test <@ Note.C.IntervalWith Note.DFlat = MinorSecond @>
            test <@ Note.C.IntervalWith Note.D = MajorSecond @>
            test <@ Note.C.IntervalWith Note.DSharp = MinorThird @>
            test <@ Note.C.IntervalWith Note.EFlat = MinorThird @>
            test <@ Note.C.IntervalWith Note.E = MajorThird @>
            test <@ Note.C.IntervalWith Note.F = PerfectForth @>
            test <@ Note.C.IntervalWith Note.FSharp = DiminishedFifth @>
            test <@ Note.C.IntervalWith Note.GFlat = DiminishedFifth @>
            test <@ Note.C.IntervalWith Note.G = PerfectFifth @>
            test <@ Note.C.IntervalWith Note.GSharp = AugmentedFifth @>
            test <@ Note.C.IntervalWith Note.AFlat = AugmentedFifth @>
            test <@ Note.C.IntervalWith Note.A = MajorSixth @>
            test <@ Note.C.IntervalWith Note.ASharp = MinorSeventh @>
            test <@ Note.C.IntervalWith Note.BFlat = MinorSeventh @>
            test <@ Note.C.IntervalWith Note.B = MajorSeventh @>
            
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
