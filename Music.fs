namespace Music.FSharpKatas

    module Notes =
        type Note = | C | CSharp | DFlat | D | DSharp | EFlat | E | F | FSharp 
                    | GFlat | G | GSharp | AFlat | A | ASharp | BFlat | B
                    
        type Interval = | Unisson | MinorSecond | MajorSecond | MinorThird
                        | MajorThird | PerfectForth | DiminishedFifth
                        | PerfectFifth | AugmentedFifth | MajorSixth
                        | MinorSeventh | MajorSeventh | PerfectOctave
                    
        let noteName note =
            match note with
            | C -> "C" | CSharp -> "C#" | DFlat -> "Db" | D -> "D"
            | DSharp -> "D#" | EFlat -> "Eb" | E -> "E" | F ->  "F"
            | FSharp -> "F#" | GFlat -> "Gb" | G -> "G" | GSharp -> "G#"
            | AFlat -> "Ab" | A -> "A" | ASharp -> "A#" | BFlat -> "Bb"
            | B -> "B"
                
        let sharp note =
            match note with
            | C -> CSharp | CSharp -> D | DFlat -> D | D -> DSharp
            | DSharp -> E | EFlat -> E | E -> F | F -> FSharp 
            | FSharp -> G | GFlat -> G | G -> GSharp | GSharp -> A 
            | AFlat -> A | A -> ASharp | ASharp -> B | BFlat -> B 
            | B -> C
            
        let flat note =
            match note with
            | C -> B | CSharp -> C | DFlat -> C | D -> DFlat
            | DSharp -> D | EFlat -> D | E -> EFlat | F -> E
            | FSharp -> F | GFlat -> F | G -> GFlat | GSharp -> G
            | AFlat -> G | A -> AFlat | ASharp -> A | BFlat -> A
            | B -> BFlat
            
        let pitch note =
            match note with
            | C -> 0 | CSharp -> 1 | DFlat -> 1 | D -> 2
            | DSharp -> 3 | EFlat -> 3 | E -> 4 | F -> 5
            | FSharp -> 6 | GFlat -> 6 | G -> 7 | GSharp -> 8
            | AFlat -> 8 | A -> 9 | ASharp -> 10 | BFlat -> 10
            | B -> 11
                             
        let intervalName interval =
            match interval with
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
        
        let distance interval =
            match interval with
            | Unisson -> 0 | MinorSecond -> 1 | MajorSecond -> 2
            | MinorThird -> 3 | MajorThird -> 4 | PerfectForth -> 5
            | DiminishedFifth -> 6 | PerfectFifth -> 7 | AugmentedFifth -> 8
            | MajorSixth -> 9 | MinorSeventh -> 10 | MajorSeventh -> 11
            | PerfectOctave -> 12
            
        let fromDistance distance =
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
        
        let measureAbsoluteSemitones note other =
            let octave = 12
            let unisson = 0

            let distance = (pitch other) - (pitch note)
            if distance < unisson 
            then octave - distance * -1 
            else distance    
                
        let transposeStep note interval =
            match interval with
            | Unisson -> note 
            
            | MajorSecond | PerfectFifth | MajorThird | PerfectForth
            | AugmentedFifth | MajorSixth | PerfectOctave -> sharp note
            
            | MinorSecond | DiminishedFifth | MinorThird
            | MinorSeventh | MajorSeventh  -> flat note
            
        let intervalBetween note other =
            fromDistance (measureAbsoluteSemitones note other)
            
        let transpose noteToTranspose transposingInterval =
            let rec loop note interval =
                let newNote = transposeStep note transposingInterval
                let newInterval = intervalBetween noteToTranspose newNote
                
                if newInterval = transposingInterval then
                    newNote
                else
                    loop newNote newInterval
                
            loop noteToTranspose transposingInterval

    module Scales =
        open Notes

        type Scale = 
            | AMajor | AFlatMajor | BMajor | BFlatMajor | CMajor
            | DMajor | DFlatMajor | EMajor | EFlatMajor
            | FMajor | FSharpMajor | GMajor | GFlatMajor | AMinor
            | BMinor | BFlatMinor | CMinor | CSharpMinor | DMinor
            | EMinor | FMinor | FSharpMinor | GMinor 
            | GSharpMinor | EFlatMinor

        let accidentals scale =
            match scale with
            | AMajor -> 3 | AFlatMajor -> -4 | BMajor -> 5 
            | BFlatMajor -> -2 | CMajor -> 0
            | DMajor -> 2 | DFlatMajor -> -5 | EMajor -> 4 
            | EFlatMajor -> -3 | FMajor -> -1 | FSharpMajor -> 6 
            | GMajor -> 1 | GFlatMajor -> -6 | AMinor -> 0
            | BMinor -> 2 | BFlatMinor -> -5 | CMinor -> -3 
            | CSharpMinor -> 4 | DMinor -> -1 | EMinor -> 1
            | FMinor -> -4 | FSharpMinor -> 3 | GMinor -> -2 
            | GSharpMinor -> 5 | EFlatMinor -> -6
        
        let root scale =
            match scale with
            | AMajor -> A | AFlatMajor -> AFlat | BMajor -> B 
            | BFlatMajor -> BFlat | CMajor -> C
            | DMajor -> D | DFlatMajor -> DFlat | EMajor -> E 
            | EFlatMajor -> EFlat | FMajor -> F | FSharpMajor -> FSharp 
            | GMajor -> G | GFlatMajor -> GFlat | AMinor -> A
            | BMinor -> B | BFlatMinor -> BFlat | CMinor -> C 
            | CSharpMinor -> CSharp | DMinor -> D | EMinor -> E
            | FMinor -> F | FSharpMinor -> FSharp | GMinor -> G 
            | GSharpMinor -> GSharp | EFlatMinor -> EFlat
        
        let flatedNotesScale fifths scaleAccidents =
            (fifths |> List.rev |> List.skip -scaleAccidents) 
            @ (fifths
            |> List.rev
            |> List.take(-scaleAccidents)
            |> List.map( fun n -> flat n))
        
        let sharpedNotesScale fifths scaleAccidents =
            ((fifths |> List.skip scaleAccidents) )
            @ (fifths
            |> List.take(scaleAccidents)
            |> List.map( fun n -> sharp n))
        
        let rawNotes scale = 
            let fifths = [F; C; G; D; A; E; B;]
            let scaleAccidents = accidentals scale
            
            if scaleAccidents = 0 then
                fifths
            else 
                if scaleAccidents < 0 then 
                    flatedNotesScale fifths scaleAccidents
                else
                    sharpedNotesScale fifths scaleAccidents

        let notes scale = 
            (rawNotes scale
            |> List.sortBy (fun n -> pitch n)
            |> List.skipWhile (fun n -> n <> root scale))
            @
            (rawNotes scale
            |> List.sortBy (fun n -> pitch n)
            |> List.takeWhile (fun n -> n <> root scale))

    module Chords =
        open Notes

        type ChordFunction = 
            | Major | Augmented | Minor | Diminished
            | Major7 | Augmented7 | Minor7 | Diminished7 
            | Dominant7 | Minor7b5 | MinorMaj7
            | Sus2 | Sus2Diminished | Sus2Augmented
            | Sus4 | Sus4Diminished | Sus4Augmented
            
        type ChordNoteFunction = | Root | Third | Fifth | Seventh | Ninth | Eleventh | Thirteenth
        type ChordType = | Open | Closed | Drop2 | Drop3
        type ChordNote = Note * ChordNoteFunction

        type Chord = {notes:ChordNote list; chordType:ChordType;}
        
        let functionForInterval interval =
            match interval with
            | Unisson -> Root
            | MajorThird | MinorThird -> Third 
            | PerfectFifth | DiminishedFifth | AugmentedFifth  -> Fifth
            | MajorSeventh | MinorSeventh | MajorSixth -> Seventh
            | _ -> Root
            
        let intervalsForFunction chordFunction =
            match chordFunction with
            | Major -> [MajorThird; PerfectFifth]
            | Augmented -> [MajorThird; AugmentedFifth]
            | Minor -> [MinorThird; PerfectFifth]
            | Diminished -> [MinorThird; DiminishedFifth]
            | Major7 -> [MajorThird; PerfectFifth; MajorSeventh]
            | Augmented7 -> [MajorThird; AugmentedFifth; MajorSeventh]
            | Minor7 -> [MinorThird; PerfectFifth; MinorSeventh]
            | Diminished7 -> [MinorThird; DiminishedFifth; MajorSixth]
            | Dominant7 -> [MajorThird; PerfectFifth; MinorSeventh]
            | Minor7b5 -> [MinorThird; DiminishedFifth; MinorSeventh]
            | MinorMaj7 -> [MinorThird; PerfectFifth; MajorSeventh]
            | Sus2 -> [MajorSecond; PerfectFifth]
            | Sus2Diminished -> [MajorSecond; DiminishedFifth]
            | Sus2Augmented -> [MajorSecond; AugmentedFifth]
            | Sus4 -> [PerfectForth; PerfectFifth]
            | Sus4Diminished -> [PerfectForth; DiminishedFifth]
            | Sus4Augmented -> [PerfectForth; AugmentedFifth]

        let functionForIntervals intervals =
            match intervals with
            | [MajorThird; PerfectFifth] -> Major
            | [MajorThird; AugmentedFifth] -> Augmented
            | [MinorThird; PerfectFifth] -> Minor
            | [MinorThird; DiminishedFifth] -> Diminished
            | [MajorThird; PerfectFifth; MajorSeventh] -> Major7
            | [MajorThird; AugmentedFifth; MajorSeventh] -> Augmented7
            | [MinorThird; PerfectFifth; MinorSeventh] -> Minor7
            | [MinorThird; DiminishedFifth; MajorSixth] -> Diminished7
            | [MajorThird; PerfectFifth; MinorSeventh] -> Dominant7
            | [MinorThird; DiminishedFifth; MinorSeventh] -> Minor7b5
            | [MinorThird; PerfectFifth; MajorSeventh] -> MinorMaj7
            | [MajorSecond; PerfectFifth] -> Sus2
            | [MajorSecond; DiminishedFifth] -> Sus2Diminished 
            | [MajorSecond; AugmentedFifth] -> Sus2Augmented
            | [PerfectForth; PerfectFifth] -> Sus4
            | [PerfectForth; DiminishedFifth] -> Sus4Diminished
            | [PerfectForth; AugmentedFifth] -> Sus4Augmented
            | _ -> Major

        let abrevitedName chordFunction =
            match chordFunction with
            | Major -> "Maj" | Augmented -> "Aug" | Minor -> "Min" 
            | Diminished -> "Dim" | Major7 -> "Maj7" 
            | Augmented7 -> "Aug7" | Minor7 -> "Min7" 
            | Diminished7 -> "Dim7" | Dominant7 -> "Dom7" 
            | Minor7b5 -> "Min7b5" | MinorMaj7 -> "MinMaj7"
            | Sus2 -> "Sus2" | Sus2Diminished -> "Sus2Dim" 
            | Sus2Augmented -> "Sus2Aug"
            | Sus4 -> "Sus4" | Sus4Diminished -> "SusDim" 
            | Sus4Augmented -> "Sus4Aug"

        let noteForFunction chord chordNoteFunction =
            fst (chord.notes |> List.find (fun n -> snd n = chordNoteFunction))
        
        let bass chord =
            fst (chord.notes |> List.head)
        
        let lead chord =
            fst (chord.notes |> List.last)
        
        let intervalsForChord chord =
            let root = noteForFunction chord Root
            chord.notes
            |> List.map (fun n -> intervalBetween root (fst n))
            |> List.sortBy(fun i -> distance i)
            |> List.skip 1
            
        let name chord =
            noteName (noteForFunction chord Root) 
            + abrevitedName (functionForIntervals(intervalsForChord chord))
            
        let noteNames chord =
            chord.notes |> List.map (fun n -> noteName (fst n))
            
        let chordFromRootAndFunction root chordFunction =
            {notes=
                [(root, Root)]@
                (intervalsForFunction chordFunction
                |> List.map (fun i -> ((transpose root i), functionForInterval i)));
             chordType=Closed}

        let rotate list =
            (list |> List.skip 1) @ (list |> List.take 1)

        let invert chord =
            let invertedChordNotes = rotate chord.notes
            {notes= invertedChordNotes; chordType=Closed}
                        
    module NotesTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Notes

        [<Test>]
        let ``Should relate note with its name``() =
            test <@ noteName C = "C" @>
            test <@ noteName CSharp = "C#" @>
            test <@ noteName DFlat = "Db" @>
            test <@ noteName D = "D" @>
            test <@ noteName DSharp = "D#" @>
            test <@ noteName EFlat = "Eb" @>
            test <@ noteName E = "E" @>
            test <@ noteName F = "F" @>
            test <@ noteName FSharp = "F#" @>
            test <@ noteName GFlat = "Gb" @>
            test <@ noteName G = "G" @>
            test <@ noteName GSharp = "G#" @>
            test <@ noteName AFlat = "Ab" @>
            test <@ noteName A = "A" @>
            test <@ noteName ASharp = "A#" @>
            test <@ noteName BFlat = "Bb" @>
            test <@ noteName B = "B" @>
            
        [<Test>]
        let ``Should sharp note``() =
            test <@ sharp C = CSharp @>
            test <@ sharp CSharp = D @>
            test <@ sharp DFlat = D @>
            test <@ sharp D = DSharp @>
            test <@ sharp DSharp = E @>
            test <@ sharp EFlat = E @>
            test <@ sharp E = F @>
            test <@ sharp F = FSharp @>
            test <@ sharp FSharp = G @>
            test <@ sharp GFlat = G @>
            test <@ sharp G = GSharp @>
            test <@ sharp GSharp = A @>
            test <@ sharp AFlat = A @>
            test <@ sharp A = ASharp @>
            test <@ sharp ASharp = B @>
            test <@ sharp BFlat = B @>
            test <@ sharp B = C @>
            
        [<Test>]
        let ``Should flat note``() =
            test <@ flat C = B @>
            test <@ flat CSharp = C @>
            test <@ flat DFlat = C @>
            test <@ flat D = DFlat @>
            test <@ flat DSharp = D @>
            test <@ flat EFlat = D @>
            test <@ flat E = EFlat @>
            test <@ flat F = E @>
            test <@ flat FSharp = F @>
            test <@ flat GFlat = F @>
            test <@ flat G = GFlat @>
            test <@ flat GSharp = G @>
            test <@ flat AFlat = G @>
            test <@ flat A = AFlat @>
            test <@ flat ASharp = A @>
            test <@ flat BFlat = A @>
            test <@ flat B = BFlat @>
            
        [<Test>]
        let ``Should measure semitones distance``() =
            test <@ measureAbsoluteSemitones C C = 0 @>
            test <@ measureAbsoluteSemitones C CSharp = 1 @>
            test <@ measureAbsoluteSemitones C DFlat = 1 @>
            test <@ measureAbsoluteSemitones C D = 2 @>
            test <@ measureAbsoluteSemitones C DSharp = 3 @>
            test <@ measureAbsoluteSemitones C EFlat = 3 @>
            test <@ measureAbsoluteSemitones C E = 4 @>
            test <@ measureAbsoluteSemitones C F = 5 @>
            test <@ measureAbsoluteSemitones C FSharp = 6 @>
            test <@ measureAbsoluteSemitones C GFlat = 6 @>
            test <@ measureAbsoluteSemitones C G = 7 @>
            test <@ measureAbsoluteSemitones C GSharp = 8 @>
            test <@ measureAbsoluteSemitones C AFlat = 8 @>
            test <@ measureAbsoluteSemitones C A = 9 @>
            test <@ measureAbsoluteSemitones C ASharp = 10 @>
            test <@ measureAbsoluteSemitones C BFlat = 10 @>
            test <@ measureAbsoluteSemitones C B = 11 @>
            
        [<Test>]
        let ``Should create interval from distance``() =
            test <@ intervalBetween C C = Unisson @>
            test <@ intervalBetween C CSharp = MinorSecond @>
            test <@ intervalBetween C DFlat = MinorSecond @>
            test <@ intervalBetween C D = MajorSecond @>
            test <@ intervalBetween C DSharp = MinorThird @>
            test <@ intervalBetween C EFlat = MinorThird @>
            test <@ intervalBetween C E = MajorThird @>
            test <@ intervalBetween C F = PerfectForth @>
            test <@ intervalBetween C FSharp = DiminishedFifth @>
            test <@ intervalBetween C GFlat = DiminishedFifth @>
            test <@ intervalBetween C G = PerfectFifth @>
            test <@ intervalBetween C GSharp = AugmentedFifth @>
            test <@ intervalBetween C AFlat = AugmentedFifth @>
            test <@ intervalBetween C A = MajorSixth @>
            test <@ intervalBetween C ASharp = MinorSeventh @>
            test <@ intervalBetween C BFlat = MinorSeventh @>
            test <@ intervalBetween C B = MajorSeventh @>
            
        [<Test>]
        let ``Should transpose note using interval``() =
            test <@ transpose C Unisson = C @>
            test <@ transpose C MinorSecond = DFlat @>
            test <@ transpose C MajorSecond = D @>
            test <@ transpose C MinorThird = EFlat @>
            test <@ transpose C MajorThird = E @>
            test <@ transpose C PerfectForth = F @>
            test <@ transpose C DiminishedFifth = GFlat @>
            test <@ transpose C PerfectFifth = G @>
            test <@ transpose C AugmentedFifth = GSharp @>
            test <@ transpose C MajorSixth = A @>
            test <@ transpose C MinorSeventh = BFlat @>
            test <@ transpose C MajorSeventh = B @>
            
        [<Test>]
        let ``Should relate interval with its name``() =
            test <@ intervalName Unisson = "Unisson" @>
            test <@ intervalName MinorSecond = "MinorSecond" @>
            test <@ intervalName MajorSecond = "MajorSecond" @>
            test <@ intervalName MinorThird = "MinorThird" @>
            test <@ intervalName MajorThird = "MajorThird" @>
            test <@ intervalName PerfectForth = "PerfectForth" @>
            test <@ intervalName DiminishedFifth = "DiminishedFifth" @>
            test <@ intervalName PerfectFifth = "PerfectFifth" @>
            test <@ intervalName AugmentedFifth = "AugmentedFifth" @>
            test <@ intervalName MajorSixth = "MajorSixth" @>
            test <@ intervalName MinorSeventh = "MinorSeventh" @>
            test <@ intervalName MajorSeventh = "MajorSeventh" @>
            test <@ intervalName PerfectOctave = "PerfectOctave" @>
            
        [<Test>]
        let ``Should relate interval with distances``() =
            test <@ fromDistance 0 = Unisson @>
            test <@ fromDistance 1 = MinorSecond @>
            test <@ fromDistance 2 = MajorSecond @>
            test <@ fromDistance 3 = MinorThird @>
            test <@ fromDistance 4 = MajorThird @>
            test <@ fromDistance 5 = PerfectForth @>
            test <@ fromDistance 6 = DiminishedFifth @>
            test <@ fromDistance 7 = PerfectFifth @>
            test <@ fromDistance 8 = AugmentedFifth @>
            test <@ fromDistance 9 = MajorSixth @>
            test <@ fromDistance 10 = MinorSeventh @>
            test <@ fromDistance 11 = MajorSeventh @>
            test <@ fromDistance 12 = PerfectOctave @>

     module ScalesTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Scales
        open Notes

        [<Test>]
        let ``Should have notes for scale``() =
            test <@ notes CMajor = [ C; D; E; F; G; A; B ] @>
            test <@ notes GMajor = [ G; A; B; C; D; E; FSharp ] @>
            test <@ notes DMajor = [ D; E; FSharp; G; A; B; CSharp ] @>
            test <@ notes AMajor = [ A; B; CSharp; D; E; FSharp; GSharp ] @>
            test <@ notes EMajor = [ E; FSharp; GSharp; A; B; CSharp; DSharp ] @>
            test <@ notes BMajor = [ B; CSharp; DSharp; E; FSharp; GSharp; ASharp ] @>
            test <@ notes FSharpMajor = [ FSharp; GSharp; ASharp; B; CSharp; DSharp; F ] @>
            test <@ notes DFlatMajor = [ DFlat; EFlat; F; GFlat; AFlat; BFlat; C ] @>
            test <@ notes AFlatMajor = [ AFlat; BFlat; C; DFlat; EFlat; F; G ] @>
            test <@ notes GFlatMajor = [ GFlat; AFlat; BFlat; B; DFlat; EFlat; F ] @>
            test <@ notes EFlatMajor = [ EFlat; F; G; AFlat; BFlat; C; D ] @>
            test <@ notes BFlatMajor = [ BFlat; C; D; EFlat; F; G; A ] @>
            test <@ notes FMajor = [ F; G; A; BFlat; C; D; E ] @>

            test <@ notes AMinor = [ A; B; C; D; E; F; G ] @>
            test <@ notes EMinor = [ E; FSharp; G; A; B; C; D ] @>
            test <@ notes BMinor = [ B; CSharp; D; E; FSharp; G; A ] @>
            test <@ notes FSharpMinor = [ FSharp; GSharp; A; B; CSharp; D; E ] @>
            test <@ notes CSharpMinor = [ CSharp; DSharp; E; FSharp; GSharp; A; B ] @>
            test <@ notes GSharpMinor = [ GSharp; ASharp; B; CSharp; DSharp; E; FSharp ] @>
            test <@ notes EFlatMinor = [ EFlat; F; GFlat; AFlat; BFlat; B; DFlat ] @>
            test <@ notes BFlatMinor = [ BFlat; C; DFlat; EFlat; F; GFlat; AFlat ] @>
            test <@ notes FMinor = [ F; G; AFlat; BFlat; C; DFlat; EFlat ] @>
            test <@ notes CMinor = [ C; D; EFlat; F; G; AFlat; BFlat ] @>
            test <@ notes GMinor = [ G; A; BFlat; C; D; EFlat; F ] @>
            test <@ notes DMinor = [ D; E; F; G; A; BFlat; C ] @>

    module ChordsTests =
        open NUnit.Framework
        open Swensen.Unquote
        open Scales
        open Notes
        open Chords

        let cMaj = {notes= [(C, Root); (E, Third); (G, Fifth)]; chordType=Closed}
        let cAug = {notes= [(C, Root); (E, Third); (GSharp, Fifth)]; chordType=Closed}
        let cMin = {notes= [(C, Root); (EFlat, Third); (G, Fifth)]; chordType=Closed}
        let cDim = {notes= [(C, Root); (EFlat, Third); (GFlat, Fifth)]; chordType=Closed}
        let cMaj7 = {notes= [(C, Root); (E, Third); (G, Fifth); (B, Seventh)]; chordType=Closed}
        let cAug7 = {notes= [(C, Root); (E, Third); (GSharp, Fifth); (B, Seventh)];chordType=Closed}
        let cMin7 = {notes= [(C, Root); (EFlat, Third); (G, Fifth); (BFlat, Seventh)]; chordType=Closed}
        let cDim7 = {notes= [(C, Root); (EFlat, Third); (GFlat, Fifth); (A, Seventh)]; chordType=Closed}
        let cMin7b5 = {notes= [(C, Root); (EFlat, Third); (GFlat, Fifth); (BFlat, Seventh)]; chordType=Closed}
                
        [<Test>]
        let ``Chord should have notes for function``() =
            test <@ noteForFunction cMaj7 Root = C @>
            test <@ noteForFunction cMaj7 Third = E @>
            test <@ noteForFunction cMaj7 Fifth = G @>
            test <@ noteForFunction cMaj7 Seventh = B @>
            
        [<Test>]
        let ``Chord should return note names``() =
            test <@ noteNames cMaj7 = ["C"; "E"; "G"; "B"] @>

        [<Test>]
        let ``Chord should return lowest note for bass``() =
            test <@ bass cMaj7 = C @>

        [<Test>]
        let ``Chord should return highest note for lead``() =
            test <@ lead cMaj7 = B @>

        [<Test>]
        let ``Chord should be named after the root``() =
            test <@ (name cMaj7).StartsWith("C") @>

        [<Test>]
        let ``Chord should be named after the function``() = 
            test <@ (name cMaj).StartsWith("CMaj") @>
            test <@ (name cAug).StartsWith("CAug") @>
            test <@ (name cMin).StartsWith("CMin") @>
            test <@ (name cDim).StartsWith("CDim") @>
            test <@ (name cMaj7).StartsWith("CMaj7") @>
            test <@ (name cAug7).StartsWith("CAug7") @>
            test <@ (name cMin7).StartsWith("CMin7") @>
            test <@ (name cDim7).StartsWith("CDim7") @>
            
        [<Test>]
        let ``Should create chord from root and function``() =
            test <@ chordFromRootAndFunction C Major = cMaj @>
            test <@ chordFromRootAndFunction C Augmented = cAug @>
            test <@ chordFromRootAndFunction C Minor = cMin @>
            test <@ chordFromRootAndFunction C Diminished = cDim @>
            test <@ chordFromRootAndFunction C Major7 = cMaj7 @>
            test <@ chordFromRootAndFunction C Augmented7 = cAug7 @>
            test <@ chordFromRootAndFunction C Minor7 = cMin7 @>
            test <@ chordFromRootAndFunction C Diminished7 = cDim7 @>
            test <@ chordFromRootAndFunction C Minor7b5 = cMin7b5 @>

        [<Test>]
        let ``Should invert chord for first inversion``() =
            test <@ (invert cMaj).notes = [(E, Third); (G, Fifth); (C, Root)]  @>
            test <@ (invert cAug).notes = [(E, Third); (GSharp, Fifth); (C, Root)]  @>
            test <@ (invert cMin).notes = [(EFlat, Third); (G, Fifth); (C, Root)]  @>
            test <@ (invert cDim).notes = [(EFlat, Third); (GFlat, Fifth); (C, Root)]  @>
            test <@ (invert cMaj7).notes = [(E, Third); (G, Fifth); (B, Seventh); (C, Root)]  @>
            test <@ (invert cAug7).notes = [(E, Third); (GSharp, Fifth); (B, Seventh); (C, Root)]  @>
            test <@ (invert cMin7).notes = [(EFlat, Third); (G, Fifth); (BFlat, Seventh); (C, Root)]  @>
            test <@ (invert cDim7).notes = [(EFlat, Third); (GFlat, Fifth); (A, Seventh); (C, Root)]  @>
            test <@ (invert cMin7b5).notes = [(EFlat, Third); (GFlat, Fifth); (BFlat, Seventh); (C, Root)]  @>

        [<Test>]
        let ``Should invert chord for second inversion``() =
            test <@ (cMaj |> invert |> invert).notes = [(G, Fifth); (C, Root); (E, Third)]  @>
            test <@ (cAug |> invert |> invert).notes = [(GSharp, Fifth); (C, Root); (E, Third)]  @>
            test <@ (cMin |> invert |> invert).notes = [(G, Fifth); (C, Root); (EFlat, Third)]  @>
            test <@ (cDim |> invert |> invert).notes = [(GFlat, Fifth); (C, Root); (EFlat, Third)]  @>
            test <@ (cMaj7 |> invert |> invert).notes = [(G, Fifth); (B, Seventh); (C, Root); (E, Third)]  @>
            test <@ (cAug7 |> invert |> invert).notes = [(GSharp, Fifth); (B, Seventh); (C, Root); (E, Third)]  @>
            test <@ (cMin7 |> invert |> invert).notes = [(G, Fifth); (BFlat, Seventh); (C, Root); (EFlat, Third)]  @>
            test <@ (cDim7 |> invert |> invert).notes = [(GFlat, Fifth); (A, Seventh); (C, Root); (EFlat, Third)]  @>
            test <@ (cMin7b5 |> invert |> invert).notes = [(GFlat, Fifth); (BFlat, Seventh); (C, Root); (EFlat, Third)]  @>

        [<Test>]
        let ``Should invert chord for third inversion``() =
            test <@ (cMaj7 |> invert |> invert |> invert).notes = [(B, Seventh); (C, Root); (E, Third); (G, Fifth)]  @>
            test <@ (cAug7 |> invert |> invert |> invert).notes = [(B, Seventh); (C, Root); (E, Third); (GSharp, Fifth)]  @>
            test <@ (cMin7 |> invert |> invert |> invert).notes = [(BFlat, Seventh); (C, Root); (EFlat, Third); (G, Fifth)]  @>
            test <@ (cDim7 |> invert |> invert |> invert).notes = [(A, Seventh); (C, Root); (EFlat, Third); (GFlat, Fifth)]  @>
            test <@ (cMin7b5 |> invert |> invert |> invert).notes = [(BFlat, Seventh); (C, Root); (EFlat, Third); (GFlat, Fifth)]  @>