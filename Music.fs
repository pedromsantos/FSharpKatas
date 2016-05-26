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
                            | Unisson -> note 
                            | MajorSecond | PerfectFifth | MajorThird | PerfectForth 
                            | AugmentedFifth | MajorSixth | PerfectOctave -> note.sharp
                            | MinorSecond | DiminishedFifth | MinorThird | MinorSeventh 
                            | MajorSeventh  -> note.flat 
                            
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
                | DMajor | DFlatMajor | EMajor | EFlatMajor
                | FMajor | FSharpMajor | GMajor | GFlatMajor | AMinor
                | BMinor | BFlatMinor | CMinor | CSharpMinor | DMinor
                | EMinor | FMinor | FSharpMinor | GMinor 
                | GSharpMinor | EFlatMinor
            
                static member Fifths = [F; C; G; D; A; E; B;] 

                member private self.accidentals =
                    match self with
                    | AMajor -> 3 | AFlatMajor -> -4 | BMajor -> 5 
                    | BFlatMajor -> -2 | CMajor -> 0
                    | DMajor -> 2 | DFlatMajor -> -5 | EMajor -> 4 
                    | EFlatMajor -> -3 | FMajor -> -1 | FSharpMajor -> 6 
                    | GMajor -> 1 | GFlatMajor -> -6 | AMinor -> 0
                    | BMinor -> 2 | BFlatMinor -> -5 | CMinor -> -3 
                    | CSharpMinor -> 4 | DMinor -> -1 | EMinor -> 1
                    | FMinor -> -4 | FSharpMinor -> 3 | GMinor -> -2 
                    | GSharpMinor -> 5 | EFlatMinor -> -6
                
                member private self.root =
                    match self with
                    | AMajor -> A | AFlatMajor -> AFlat | BMajor -> B 
                    | BFlatMajor -> BFlat | CMajor -> C
                    | DMajor -> D | DFlatMajor -> DFlat | EMajor -> E 
                    | EFlatMajor -> EFlat | FMajor -> F | FSharpMajor -> FSharp 
                    | GMajor -> G | GFlatMajor -> GFlat | AMinor -> A
                    | BMinor -> B | BFlatMinor -> BFlat | CMinor -> C 
                    | CSharpMinor -> CSharp | DMinor -> D | EMinor -> E
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
            test <@ C.ToString() = "C" @>
            test <@ CSharp.ToString() = "C#" @>
            test <@ DFlat.ToString() = "Db" @>
            test <@ D.ToString() = "D" @>
            test <@ DSharp.ToString() = "D#" @>
            test <@ EFlat.ToString() = "Eb" @>
            test <@ E.ToString() = "E" @>
            test <@ F.ToString() = "F" @>
            test <@ FSharp.ToString() = "F#" @>
            test <@ GFlat.ToString() = "Gb" @>
            test <@ G.ToString() = "G" @>
            test <@ GSharp.ToString() = "G#" @>
            test <@ AFlat.ToString() = "Ab" @>
            test <@ A.ToString() = "A" @>
            test <@ ASharp.ToString() = "A#" @>
            test <@ BFlat.ToString() = "Bb" @>
            test <@ B.ToString() = "B" @>
            
        [<Test>]
        let ``Should sharp note``() =
            test <@ C.sharp = CSharp @>
            test <@ CSharp.sharp = D @>
            test <@ DFlat.sharp = D @>
            test <@ D.sharp = DSharp @>
            test <@ DSharp.sharp = E @>
            test <@ EFlat.sharp = E @>
            test <@ E.sharp = F @>
            test <@ F.sharp = FSharp @>
            test <@ FSharp.sharp = G @>
            test <@ GFlat.sharp = G @>
            test <@ G.sharp = GSharp @>
            test <@ GSharp.sharp = A @>
            test <@ AFlat.sharp = A @>
            test <@ A.sharp = ASharp @>
            test <@ ASharp.sharp = B @>
            test <@ BFlat.sharp = B @>
            test <@ B.sharp = C @>
            
        [<Test>]
        let ``Should flat note``() =
            test <@ C.flat = B @>
            test <@ CSharp.flat = C @>
            test <@ DFlat.flat = C @>
            test <@ D.flat = DFlat @>
            test <@ DSharp.flat = D @>
            test <@ EFlat.flat = D @>
            test <@ E.flat = EFlat @>
            test <@ F.flat = E @>
            test <@ FSharp.flat = F @>
            test <@ GFlat.flat = F @>
            test <@ G.flat = GFlat @>
            test <@ GSharp.flat = G @>
            test <@ AFlat.flat = G @>
            test <@ A.flat = AFlat @>
            test <@ ASharp.flat = A @>
            test <@ BFlat.flat = A @>
            test <@ B.flat = BFlat @>
            
        [<Test>]
        let ``Should measure semitones distance``() =
            test <@ C.measureAbsoluteSemitones C = 0 @>
            test <@ C.measureAbsoluteSemitones CSharp = 1 @>
            test <@ C.measureAbsoluteSemitones DFlat = 1 @>
            test <@ C.measureAbsoluteSemitones D = 2 @>
            test <@ C.measureAbsoluteSemitones DSharp = 3 @>
            test <@ C.measureAbsoluteSemitones EFlat = 3 @>
            test <@ C.measureAbsoluteSemitones E = 4 @>
            test <@ C.measureAbsoluteSemitones F = 5 @>
            test <@ C.measureAbsoluteSemitones FSharp = 6 @>
            test <@ C.measureAbsoluteSemitones GFlat = 6 @>
            test <@ C.measureAbsoluteSemitones G = 7 @>
            test <@ C.measureAbsoluteSemitones GSharp = 8 @>
            test <@ C.measureAbsoluteSemitones AFlat = 8 @>
            test <@ C.measureAbsoluteSemitones A = 9 @>
            test <@ C.measureAbsoluteSemitones ASharp = 10 @>
            test <@ C.measureAbsoluteSemitones BFlat = 10 @>
            test <@ C.measureAbsoluteSemitones B = 11 @>
            
        [<Test>]
        let ``Should create interval from distance``() =
            test <@ C.IntervalWith C = Unisson @>
            test <@ C.IntervalWith CSharp = MinorSecond @>
            test <@ C.IntervalWith DFlat = MinorSecond @>
            test <@ C.IntervalWith D = MajorSecond @>
            test <@ C.IntervalWith DSharp = MinorThird @>
            test <@ C.IntervalWith EFlat = MinorThird @>
            test <@ C.IntervalWith E = MajorThird @>
            test <@ C.IntervalWith F = PerfectForth @>
            test <@ C.IntervalWith FSharp = DiminishedFifth @>
            test <@ C.IntervalWith GFlat = DiminishedFifth @>
            test <@ C.IntervalWith G = PerfectFifth @>
            test <@ C.IntervalWith GSharp = AugmentedFifth @>
            test <@ C.IntervalWith AFlat = AugmentedFifth @>
            test <@ C.IntervalWith A = MajorSixth @>
            test <@ C.IntervalWith ASharp = MinorSeventh @>
            test <@ C.IntervalWith BFlat = MinorSeventh @>
            test <@ C.IntervalWith B = MajorSeventh @>
            
        [<Test>]
        let ``Should transpose note using interval``() =
            test <@ C.Transpose Unisson = C @>
            test <@ C.Transpose MinorSecond = DFlat @>
            test <@ C.Transpose MajorSecond = D @>
            test <@ C.Transpose MinorThird = EFlat @>
            test <@ C.Transpose MajorThird = E @>
            test <@ C.Transpose PerfectForth = F @>
            test <@ C.Transpose DiminishedFifth = GFlat @>
            test <@ C.Transpose PerfectFifth = G @>
            test <@ C.Transpose AugmentedFifth = GSharp @>
            test <@ C.Transpose MajorSixth = A @>
            test <@ C.Transpose MinorSeventh = BFlat @>
            test <@ C.Transpose MajorSeventh = B @>
            
        [<Test>]
        let ``Should relate interval with its name``() =
            test <@ Unisson.ToString() = "Unisson" @>
            test <@ MinorSecond.ToString() = "MinorSecond" @>
            test <@ MajorSecond.ToString() = "MajorSecond" @>
            test <@ MinorThird.ToString() = "MinorThird" @>
            test <@ MajorThird.ToString() = "MajorThird" @>
            test <@ PerfectForth.ToString() = "PerfectForth" @>
            test <@ DiminishedFifth.ToString() = "DiminishedFifth" @>
            test <@ PerfectFifth.ToString() = "PerfectFifth" @>
            test <@ AugmentedFifth.ToString() = "AugmentedFifth" @>
            test <@ MajorSixth.ToString() = "MajorSixth" @>
            test <@ MinorSeventh.ToString() = "MinorSeventh" @>
            test <@ MajorSeventh.ToString() = "MajorSeventh" @>
            test <@ PerfectOctave.ToString() = "PerfectOctave" @>
            
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
            test <@ GMajor.notes = [ G; A; B; C; D; E; FSharp ] @>
            test <@ DMajor.notes = [ D; E; FSharp; G; A; B; CSharp ] @>
            test <@ AMajor.notes = [ A; B; CSharp; D; E; FSharp; GSharp ] @>
            test <@ EMajor.notes = [ E; FSharp; GSharp; A; B; CSharp; DSharp ] @>
            test <@ BMajor.notes = [ B; CSharp; DSharp; E; FSharp; GSharp; ASharp ] @>
            test <@ FSharpMajor.notes = [ FSharp; GSharp; ASharp; B; CSharp; DSharp; F ] @>
            test <@ DFlatMajor.notes = [ DFlat; EFlat; F; GFlat; AFlat; BFlat; C ] @>
            test <@ AFlatMajor.notes = [ AFlat; BFlat; C; DFlat; EFlat; F; G ] @>
            test <@ GFlatMajor.notes = [ GFlat; AFlat; BFlat; B; DFlat; EFlat; F ] @>
            test <@ EFlatMajor.notes = [ EFlat; F; G; AFlat; BFlat; C; D ] @>
            test <@ BFlatMajor.notes = [ BFlat; C; D; EFlat; F; G; A ] @>
            test <@ FMajor.notes = [ F; G; A; BFlat; C; D; E ] @>

            test <@ AMinor.notes = [ A; B; C; D; E; F; G ] @>
            test <@ EMinor.notes = [ E; FSharp; G; A; B; C; D ] @>
            test <@ BMinor.notes = [ B; CSharp; D; E; FSharp; G; A ] @>
            test <@ FSharpMinor.notes = [ FSharp; GSharp; A; B; CSharp; D; E ] @>
            test <@ CSharpMinor.notes = [ CSharp; DSharp; E; FSharp; GSharp; A; B ] @>
            test <@ GSharpMinor.notes = [ GSharp; ASharp; B; CSharp; DSharp; E; FSharp ] @>
            test <@ EFlatMinor.notes = [ EFlat; F; GFlat; AFlat; BFlat; B; DFlat ] @>
            test <@ BFlatMinor.notes = [ BFlat; C; DFlat; EFlat; F; GFlat; AFlat ] @>
            test <@ FMinor.notes = [ F; G; AFlat; BFlat; C; DFlat; EFlat ] @>
            test <@ CMinor.notes = [ C; D; EFlat; F; G; AFlat; BFlat ] @>
            test <@ GMinor.notes = [ G; A; BFlat; C; D; EFlat; F ] @>
            test <@ DMinor.notes = [ D; E; F; G; A; BFlat; C ] @>
            
            