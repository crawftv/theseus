module ParseLine exposing (Accent(..), Duration(..), Key(..), Sound, SoundVowelLength(..), StaveNote, SyllableLength(..), SyllableSound, VowelSound, accentFinder, accentReturn, chompContinuant, consonant, getSounds, longShortVowels, lookAhead, makeStaveNote, mapLookAhead, parseString, parseString_, statements, stringToSyllableSounds,soundParse)

import Array
import Json.Encode
import Parser exposing (Parser, chompIf, loop, succeed, oneOf, Step, getChompedString, andThen, problem, (|=), (|.), backtrackable, token, run )
import String exposing (contains)
import Tuple


type alias Sound =
    { consonant : String
    , vowel : String
    }


type Accent
    = Circumflex
    | Grave
    | Acute
    | NoAccent



-- Parsers


statements : Parser (List Sound)
statements =
    loop [] statementsHelp


statementsHelp : List Sound -> Parser (Step (List Sound) (List Sound))
statementsHelp revStmts =
    oneOf
        [ succeed (\stmt -> Parser.Loop (stmt :: revStmts))
            |= soundParse
        , succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse revStmts))
        ]


soundParse : Parser Sound
soundParse =
    andThen
        (\c ->
            andThen
                (\v ->
                    if String.isEmpty c && String.isEmpty v then
                        problem "no sound"

                    else
                        succeed (Sound c v)
                )
                vowel
        )
        consonant


stopConsonants : List String
stopConsonants =
    removeSpaces "τ Τ θ Θ π Π δ Δ φ Φ γ Γ ξ Ξ κ Κ ζ Ζ χ Χ ψ Ψ β Β"


doubleConsonant : List String
doubleConsonant =
    removeSpaces "ψ Ψ ζ Ζ ξ Ξ"


nasalConsonants : List String
nasalConsonants =
    removeSpaces "ν Ν μ Μ"


fricativeConsonant : List String
fricativeConsonant =
    removeSpaces "σ Σ ς"


liquidConsonant : List String
liquidConsonant =
    removeSpaces "ρ Ρ ῥ Ῥ ῤ λ Λ"


continuantConsonants : List String
continuantConsonants =
    List.concat [ nasalConsonants, fricativeConsonant, liquidConsonant ]


chompContinuant : Parser ()
chompContinuant =
    oneOf
        [ backtrackable (succeed () |. oneOf (chompLetter fricativeConsonant) |. oneOf (chompLetter stopConsonants))
        , succeed () |. oneOf (chompLetter continuantConsonants)
        ]


chompStopConsonant : Parser ()
chompStopConsonant =
    oneOf
        [ backtrackable (succeed () |. oneOf (chompLetter stopConsonants) |. oneOf (chompLetter liquidConsonant))
        , succeed () |. oneOf (chompLetter stopConsonants)
        ]


consonant : Parser String
consonant =
    getChompedString <|
        oneOf
            [ succeed () |. oneOf (chompLetter [ " " ]) |. chompConsonant
            , chompConsonant
            ]


chompConsonant : Parser ()
chompConsonant =
    oneOf
        [ chompContinuant
        , chompStopConsonant
        , token ""
        ]


vowel : Parser String
vowel =
    getChompedString <|
        oneOf
            [ succeed () |. oneOf (chompLetter [ " ", ",", "’", "᾽", ":" ]) |. chompVowel
            , chompVowel
            ]


chompVowel : Parser ()
chompVowel =
    oneOf
        [ chompDipthong
        , succeed () |. oneOf (chompLetter long_vowels)
        , succeed () |. oneOf (chompLetter accented_vowels)
        , succeed () |. oneOf (chompLetter (List.concat [ a, e, o, u, i, umlat ]))
        , token ""
        ]


chompDipthong_ : String -> List String -> Parser ()
chompDipthong_ openLetter closedLetter =
    backtrackable <| succeed () |. oneOf (chompLetter <| removeSpaces openLetter) |. oneOf (chompLetter closedLetter)


chompAiDipthong : Parser ()
chompAiDipthong =
    chompDipthong_ a_noAccent i


chompAuDiphtong : Parser ()
chompAuDiphtong =
    chompDipthong_ a_noAccent u


chompEuDipthong : Parser ()
chompEuDipthong =
    chompDipthong_ e_noAccent u


chompEiDipthong : Parser ()
chompEiDipthong =
    chompDipthong_ e_noAccent i


chompHuDipthong : Parser ()
chompHuDipthong =
    chompDipthong_ h_noAccent u


chompOiDipthong : Parser ()
chompOiDipthong =
    chompDipthong_ o_noAccent i


chompOuDipthong : Parser ()
chompOuDipthong =
    chompDipthong_ o_noAccent u


chompUiDipthong : Parser ()
chompUiDipthong =
    chompDipthong_ u_noAccent i


chompWuDipthong : Parser ()
chompWuDipthong =
    chompDipthong_ w_noAccent u


chompDipthong : Parser ()
chompDipthong =
    oneOf
        [ chompAiDipthong
        , chompAuDiphtong
        , chompEuDipthong
        , chompEiDipthong
        , chompHuDipthong
        , chompOiDipthong
        , chompOuDipthong
        , chompUiDipthong
        , chompWuDipthong
        ]



-- sound lists


long_vowels : List String
long_vowels =
    w ++ h


removeSpaces : String -> List String
removeSpaces x =
    String.split "" (String.replace " " "" x)


a_noAccent : String
a_noAccent =
    "Α α ἀ Ἀ ἁ ᾰ ᾱ ᾳ ᾀ ᾁ Ᾰ Ᾱ ᾼ ᾈ ᾉ"


a_accent_short : String
a_accent_short =
    " ἂ ἃ ἄ ἅ ὰ ά ᾲ ᾴ ᾂ ᾃ ᾄ ᾅ Ἀ Ἁ Ἂ Ἃ Ἄ Ἅ  Ὰ Ά ᾊ ᾋ ᾌ ᾍ ά"


a_accent_long : String
a_accent_long =
    "ᾶ ἆ ἇ ᾆ ᾇ ᾷ Ἆ Ἇ ᾎ ᾏ"


a : List String
a =
    removeSpaces (a_noAccent ++ a_accent_short ++ a_accent_long)


e_noAccent : String
e_noAccent =
    "Ε ε ἐ ἑ Ἐ Ἑ"


e_accent : String
e_accent =
    "ἒ ἓ ἔ ἕ ὲ έ έ Ἒ Ἓ Ἔ Ἕ Ὲ Έ"


e : List String
e =
    removeSpaces (e_noAccent ++ e_accent)


h_noAccent : String
h_noAccent =
    "Η η ἠ ἡ ῃ ᾐ ᾑ Ἠ Ἡ ῌ ᾘ ᾙ"


h_accent : String
h_accent =
    "ἢ ἣ ἤ ἥ ἦ ἧ ὴ ή ῆ ῂ ῄ ᾒ ᾓ ᾔ ᾕ ᾖ ᾗ ῇ ή Ἢ Ἣ Ἤ Ἥ Ἦ Ἧ Ὴ Ή ᾚ ᾛ ᾜ ᾝ ᾞ ᾟ"


h : List String
h =
    removeSpaces (h_noAccent ++ h_accent)


i_noAccent : String
i_noAccent =
    " ι ἰ ἱ ῐ ῑ Ι Ἰ Ἱ Ῐ Ῑ "


i_accent_short : String
i_accent_short =
    "ἲ ἳ ἴ ἵ ὶ ί  ί Ἲ Ἳ Ἴ Ἵ Ἶ Ἷ Ὶ Ί"


i_accent_long : String
i_accent_long =
    "ἶ ἷ ῖ"


iUmlat : String
iUmlat =
    "ῒ ΐ ΐ ῗ ϊ"


i : List String
i =
    removeSpaces (i_noAccent ++ i_accent_short ++ i_accent_long)


o_noAccent : String
o_noAccent =
    "ο ὀ ὁ Ὀ Ὁ Ο "


o_accent : String
o_accent =
    "ὂ ὃ ὄ ὅ ὸ ό ό  Ὂ Ὃ Ὄ Ὅ Ὸ Ό "


o : List String
o =
    removeSpaces (o_accent ++ o_noAccent)


w_noAccent : String
w_noAccent =
    "ω ὠ ὡ ῳ ᾠ ᾡ  Ὠ  Ὡ ῼ ᾨ ᾩ Ω "


w_accent : String
w_accent =
    "ὢ ὣ ὤ ὥ ὦ ὧ ὼ ώ ῶ ᾢ ᾣ ᾤ ᾥ ᾦ ᾧ ῷ ώ  Ὢ Ὣ Ὤ Ὥ Ὦ Ὧ Ὼ Ώ ᾪ ᾫ ᾬ ᾭ ᾮ ᾯ ῲ ῲ ῴ"


w : List String
w =
    removeSpaces (w_accent ++ w_noAccent)


u_noAccent : String
u_noAccent =
    "υ ὑ  ῠ ῡ ὐ Ὑ  Ῠ Ῡ Υ"


u_accent_short : String
u_accent_short =
    "ὓ ὕ ὺ ύ ύ ὒ ὔ  Ὓ  Ὕ  Ὺ  Ύ"


u_accent_long : String
u_accent_long =
    " ὖ ῦ ὗ  Ὗ "


uUmlat : String
uUmlat =
    " ῧ ῢ ΰ"


circumflex : String
circumflex =
    String.replace " " "" "ῧ ὖ ῦ ὗ Ὗ ῶ ῆ ᾦ ᾧ ῷ Ὦ Ὧ  ᾮ ᾯ ἶ ἷ ῖ ἦ ἧ  ᾞ ᾟ ᾖ ᾗ ῇ ᾶ ἆ ἇ ᾆ ᾇ ᾷ Ἆ Ἇ ᾎ ᾏ"


grave : String
grave =
    String.replace " " "" " ῢ ῢ ὺ ὓ  Ὓ ὢ ὣ ὼ  Ὢ Ὣ Ὼ ᾪ ᾫ ὂ ὃ  Ὂ Ὃ Ὸ ὸ ῒ ἲ ἳ ὶ  Ἲ Ἳ Ὶ ἢ ἣ ὴ ῂ Ἢ Ἣ Ὴ ᾚ ᾛ ἒ ἓ ὲ Ὲ Ἒ Ἓ ἂ ἃ ὰ ᾲ Ἂ Ἃ Ὰ  ᾊ ᾋ"


acute : String
acute =
    String.replace " " "" "ᾌ ᾍ ά Ἄ Ἅ ᾄ ᾅ ᾴ ἄ ἅ ά Ἔ Ἕ Έ ἔ ἕ έ έ ᾜ ᾝ Ή Ἤ Ἥ ή ΰ ύ ύ  Ύ ὔ ὕ ὤ ὥ ώ ᾤ ᾥ ώ Ὤ Ὥ ᾬ ᾭ ῴ ὄ ὅ ό ό Ὄ Ὅ Ό ΐ ΐ ἴ ἵ ί Ἴ Ἵ ί Ί ἤ ἥ ή ῄ ᾔ ᾕ"


u : List String
u =
    removeSpaces (u_accent_short ++ u_accent_long ++ u_noAccent)


accented_vowels : List String
accented_vowels =
    removeSpaces
        (String.concat
            [ a_accent_short
            , a_accent_long
            , e_accent
            , h_accent
            , i_accent_short
            , i_accent_long
            , o_accent
            , w_accent
            , u_accent_long
            , u_accent_short
            ]
        )


umlat : List String
umlat =
    removeSpaces (String.concat [ iUmlat, uUmlat ])


chompLetter : List String -> List (Parser ())
chompLetter letters =
    List.map chompLetterLambda (List.concat [ letters, [ "," ] ])


chompLetterLambda : String -> Parser ()
chompLetterLambda letter =
    chompIf (\c -> String.fromChar c == letter)


getSounds : String -> List Sound
getSounds string =
    case run statements string of
        Ok soundList ->
            soundList

        _ ->
            [ Sound "" "" ]


type SoundVowelLength
    = LongVowel
    | ShortVowel
    | NoVowel


type alias VowelSound =
    { consonant : String, vowel : String, vowelLength : SoundVowelLength }


longShortVowels : Sound -> VowelSound
longShortVowels sound =
    if List.member sound.vowel long_vowels then
        VowelSound sound.consonant sound.vowel LongVowel

    else if String.length (removeNonLetterChars sound.vowel) > 1 then
        VowelSound sound.consonant sound.vowel LongVowel

    else if removeNonLetterChars sound.vowel == "" then
        VowelSound sound.consonant sound.vowel NoVowel

    else
        VowelSound sound.consonant sound.vowel ShortVowel


indexedSounds : List VowelSound -> List ( Int, VowelSound )
indexedSounds vowelSounds =
    List.indexedMap Tuple.pair vowelSounds


type SyllableLength
    = LongSyllable
    | ShortSyllable
    | NoSyllable


type alias SyllableSound =
    { consonant : String, vowel : String, syllableLength : SyllableLength }


lookAhead : Array.Array VowelSound -> ( Int, VowelSound ) -> SyllableSound
lookAhead array element =
    let
        sound : VowelSound
        sound =
            Tuple.second element
    in
    case .vowelLength sound of
        LongVowel ->
            SyllableSound sound.consonant sound.vowel LongSyllable

        NoVowel ->
            SyllableSound sound.consonant sound.vowel NoSyllable

        ShortVowel ->
            let
                index: Int
                index =
                    Tuple.first element
            in
            if Array.length array > index then
                case Array.get (index + 1) array of
                    Just vowelSound ->
                        let
                            nextConsonant : String
                            nextConsonant =
                                .consonant vowelSound
                        in
                        if List.member nextConsonant doubleConsonant || String.length (removeNonLetterChars nextConsonant) > 1 then
                            SyllableSound sound.consonant sound.vowel LongSyllable

                        else if vowelSound.vowelLength == NoVowel then
                            case Array.get (index + 2) array of
                                Just vowelSound2 ->
                                    let
                                        nextConsonant2 : String
                                        nextConsonant2 =
                                            .consonant vowelSound2
                                    in
                                    if String.length (removeNonLetterChars nextConsonant2) > 0 then
                                        SyllableSound sound.consonant sound.vowel LongSyllable

                                    else
                                        SyllableSound sound.consonant sound.vowel ShortSyllable

                                Nothing ->
                                    SyllableSound sound.consonant sound.vowel ShortSyllable

                        else
                            SyllableSound sound.consonant sound.vowel ShortSyllable

                    Nothing ->
                        SyllableSound sound.consonant sound.vowel ShortSyllable

            else
                SyllableSound sound.consonant sound.vowel ShortSyllable


mapLookAhead : List Sound -> List SyllableSound
mapLookAhead sounds =
    let
        vowelSounds : List VowelSound
        vowelSounds =
            List.map longShortVowels sounds
    in
    List.map
        (lookAhead (Array.fromList vowelSounds))
        (indexedSounds vowelSounds)


stringToSyllableSounds : String -> List SyllableSound
stringToSyllableSounds string =
    case run statements string of
        Ok listSounds ->
            mapLookAhead listSounds

        _ ->
            []


removeNonLetterChars : String -> String
removeNonLetterChars string =
    string
        |> String.replace "’" ""
        |> String.replace " " ""
        |> String.replace "," ""
        |> String.replace "᾽" ""
        |> String.replace ":" ""


accentReturn : SyllableSound -> Accent
accentReturn data =
    let
        result : Maybe Accent
        result =
            List.head (List.filter accentTruth (List.map accentFinder (List.map String.fromChar (String.toList data.vowel))))
    in
    case result of
        Nothing ->
            NoAccent

        Just Grave ->
            Grave

        Just Acute ->
            Acute

        Just Circumflex ->
            Circumflex

        Just NoAccent ->
            -- I don't expect this would be called.
            NoAccent


accentTruth : Accent -> Bool
accentTruth data =
    case data of
        Acute ->
            True

        Grave ->
            True

        Circumflex ->
            True

        NoAccent ->
            False


accentFinder : String -> Accent
accentFinder vowel_ =
    if contains vowel_ grave then
        Grave

    else if contains vowel_ circumflex then
        Circumflex

    else if contains vowel_ acute then
        Acute

    else
        NoAccent


type Key
    = E4
    | C4
    | G4
    | NoNote


type Duration
    = Half
    | Quarter
    | Eighth
    | NoDuration


type alias StaveNote =
    { key : Key
    , duration : Duration
    , annotation : String
    , accent : Accent
    }


makeStaveNote : SyllableSound -> List StaveNote
makeStaveNote syllableSound =
    let
        accent : Accent
        accent =
            accentReturn syllableSound
    in
    if syllableSound.syllableLength == NoSyllable then
        [ StaveNote NoNote NoDuration (syllableSound.consonant ++ syllableSound.vowel) accent ]

    else if accent == Grave then
        [ StaveNote E4 (determineDuration syllableSound.syllableLength) (syllableSound.consonant ++ syllableSound.vowel) accent ]

    else if accent == Acute then
        [ StaveNote G4 (determineDuration syllableSound.syllableLength) (syllableSound.consonant ++ syllableSound.vowel) accent ]

    else if accent == Circumflex then
        [ StaveNote E4 Eighth (syllableSound.consonant ++ syllableSound.vowel) accent
        , StaveNote G4 Quarter "" accent
        , StaveNote E4 Eighth "" accent
        ]

    else if accent == NoAccent then
        [ StaveNote C4 (determineDuration syllableSound.syllableLength) (syllableSound.consonant ++ syllableSound.vowel) accent ]

    else
        []


determineDuration : SyllableLength -> Duration
determineDuration length =
    if length == LongSyllable then
        Half

    else
        Quarter


parseString_ : String -> List StaveNote
parseString_ string =
    List.concatMap makeStaveNote (stringToSyllableSounds string)


parseString : String -> Json.Encode.Value
parseString stavenotes =
    parseString_ stavenotes
        |> Json.Encode.list encodeStaveNote


encodeStaveNote : StaveNote -> Json.Encode.Value
encodeStaveNote staveNote =
    Json.Encode.object
        [ ( "key", encodeKey staveNote.key )
        , ( "duration", encodeDuration staveNote.duration )
        , ( "annotation", Json.Encode.string staveNote.annotation )
        , ( "accent", encodeAccent staveNote.accent )
        , ( "length", encodeLength staveNote.duration )
        , ( "note", encodeNote staveNote.key )
        ]


encodeDuration : Duration -> Json.Encode.Value
encodeDuration duration =
    case duration of
        Half ->
            Json.Encode.string "h"

        Quarter ->
            Json.Encode.string "q"

        Eighth ->
            Json.Encode.string "8"

        NoDuration ->
            Json.Encode.string ""


encodeLength : Duration -> Json.Encode.Value
encodeLength duration =
    case duration of
        Half ->
            Json.Encode.string "2n"

        Quarter ->
            Json.Encode.string "4n"

        Eighth ->
            Json.Encode.string "8n"

        NoDuration ->
            Json.Encode.string ""


encodeNote : Key -> Json.Encode.Value
encodeNote key =
    case key of
        E4 ->
            Json.Encode.string "E4"

        C4 ->
            Json.Encode.string "C4"

        G4 ->
            Json.Encode.string "G4"

        NoNote ->
            Json.Encode.string ""


encodeKey : Key -> Json.Encode.Value
encodeKey key =
    case key of
        E4 ->
            Json.Encode.string "e/4"

        C4 ->
            Json.Encode.string "c/4"

        G4 ->
            Json.Encode.string "g/4"

        NoNote ->
            Json.Encode.string ""


encodeAccent : Accent -> Json.Encode.Value
encodeAccent accent =
    case accent of
        Acute ->
            Json.Encode.string "acute"

        Circumflex ->
            Json.Encode.string "circumflex"

        Grave ->
            Json.Encode.string "grave"

        NoAccent ->
            Json.Encode.string "noAccent"
