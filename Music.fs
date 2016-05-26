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
                        
                    member self.Transpose (transposingInterval:Interval) =
                        let rec loop (interval:Interval) note =
                            let newNote = transposingInterval.TransposeNote note
                            let newInterval = self.IntervalWith newNote
                            
                            if newInterval = transposingInterval then
                                newNote
                            else
                                loop newInterval newNote
                            
                        loop transposingInterval self
                                
        and Interval = | Unisson | MinorSecond | MajorSecond | MinorThird
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
                                
                        member self.TransposeNote (note:Note) =
                            match self with
                            | Unisson -> note | MinorSecond  -> note.flat 
                            | MajorSecond -> note.sharp | MinorThird -> note.flat 
                            | MajorThird -> note.sharp | PerfectForth -> note.sharp 
                            | DiminishedFifth -> note.flat | PerfectFifth -> note.sharp 
                            | AugmentedFifth -> note.sharp | MajorSixth -> note.sharp 
                            | MinorSeventh -> note.flat | MajorSeventh  -> note.flat 
                            | PerfectOctave -> note.sharp
                                
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

    module Scales =
        open Notes

        type Scale = 
                | AMajor | AFlatMajor | BMajor | BFlatMajor | CMajor
                | CSharpMajor | DMajor | DFlatMajor | EMajor | EFlatMajor
                | FMajor | FSharpMajor | GMajor | GFlatMajor | AMinor
                | AFlatMinor | ASharpMinor | BMinor | BFlatMinor
                | CMinor | CSharpMinor | DMinor | DSharpMinor
                | EMinor | FMinor | FSharpMinor | GMinor 
                | GSharpMinor | EFlatMinor
            
                static member Fifths = [F; C; G; D; A; E; B;] 

                member private self.accidentals =
                    match self with
                    | AMajor -> 3 | AFlatMajor -> -4 | BMajor -> 5 
                    | BFlatMajor -> -2 | CMajor -> 0 | CSharpMajor -> 7
                    | DMajor -> 2 | DFlatMajor -> -5 | EMajor -> 4 
                    | EFlatMajor -> -3 | FMajor -> -1 | FSharpMajor -> 6 
                    | GMajor -> 1 | GFlatMajor -> -6 | AMinor -> 0
                    | AFlatMinor -> -7 | ASharpMinor -> 7 | BMinor -> 2 
                    | BFlatMinor -> -5 | CMinor -> -3 | CSharpMinor -> 4 
                    | DMinor -> -1 | DSharpMinor -> 6 | EMinor -> 1
                    | FMinor -> -4 | FSharpMinor -> 3 | GMinor -> -2 
                    | GSharpMinor -> 5 | EFlatMinor -> -6
                
                member private self.root =
                    match self with
                    | AMajor -> A | AFlatMajor -> AFlat | BMajor -> B 
                    | BFlatMajor -> BFlat | CMajor -> C | CSharpMajor -> CSharp
                    | DMajor -> D | DFlatMajor -> DFlat | EMajor -> E 
                    | EFlatMajor -> EFlat | FMajor -> F | FSharpMajor -> FSharp 
                    | GMajor -> G | GFlatMajor -> GFlat | AMinor -> A
                    | AFlatMinor -> AFlat | ASharpMinor -> ASharp | BMinor -> B 
                    | BFlatMinor -> BFlat | CMinor -> C | CSharpMinor -> CSharp 
                    | DMinor -> D | DSharpMinor -> DSharp | EMinor -> E
                    | FMinor -> F | FSharpMinor -> FSharp | GMinor -> G 
                    | GSharpMinor -> GSharp | EFlatMinor -> EFlat
                
                member self.rawNotes = 
                    if self.accidentals = 0 then Scale.Fifths
                    else
                        if self.accidentals < 0 then
                            (Scale.Fifths |> List.rev |> List.skip -self.accidentals) 
                            @ 
                            (Scale.Fifths
                            |> List.rev
                            |> List.take(-self.accidentals)
                            |> List.map( fun n -> n.flat))
                        else
                            ((Scale.Fifths |> List.skip self.accidentals) )
                            @ 
                            (Scale.Fifths
                            |> List.take(self.accidentals)
                            |> List.map( fun n -> n.sharp))

                member self.notes = 
                    (self.rawNotes
                    |> List.sortBy (fun n -> n.pitch)
                    |> List.skipWhile (fun n -> n <> self.root))
                    @
                    (self.rawNotes
                    |> List.sortBy (fun n -> n.pitch)
                    |> List.takeWhile (fun n -> n <> self.root))


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
        let ``Should transpose note using interval``() =
            test <@ Note.C.Transpose Unisson = Note.C @>
            test <@ Note.C.Transpose MinorSecond = Note.DFlat @>
            test <@ Note.C.Transpose MajorSecond = Note.D @>
            test <@ Note.C.Transpose MinorThird = Note.EFlat @>
            test <@ Note.C.Transpose MajorThird = Note.E @>
            test <@ Note.C.Transpose PerfectForth = Note.F @>
            test <@ Note.C.Transpose DiminishedFifth = Note.GFlat @>
            test <@ Note.C.Transpose PerfectFifth = Note.G @>
            test <@ Note.C.Transpose AugmentedFifth = Note.GSharp @>
            test <@ Note.C.Transpose MajorSixth = Note.A @>
            test <@ Note.C.Transpose MinorSeventh = Note.BFlat @>
            test <@ Note.C.Transpose MajorSeventh = Note.B @>
            
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

     module ScalesTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Scales
        open Notes

        [<Test>]
        let ``Should have notes for scale``() =
            test <@ CMajor.notes = [ C; D; E; F; G; A; B ] @>
            test <@ AMinor.notes = [ A; B; C; D; E; F; G ] @>
            test <@ AFlatMajor.notes = [ AFlat; BFlat; C; DFlat; EFlat; F; G ] @>
            test <@ GFlatMajor.notes = [ GFlat; AFlat; BFlat; B; DFlat; EFlat; F ] @>
            test <@ EFlatMinor.notes = [ EFlat; F; GFlat; AFlat; BFlat; B; DFlat ] @>
            test <@ CSharpMajor.notes = [ CSharp; DSharp; F; FSharp; GSharp; ASharp; C ] @>
            test <@ ASharpMinor.notes = [ ASharp; C; CSharp; DSharp; F; FSharp; GSharp ] @>