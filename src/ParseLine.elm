module ParseLine exposing (..)

import Array exposing (..)
import Parser exposing (..)
import Tuple exposing (..)


type alias Sound =
    { consonant : String
    , vowel : String
    }



-- Parsers


statements : Parser (List Sound)
statements =
    loop [] statementsHelp


statementsHelp : List Sound -> Parser (Step (List Sound) (List Sound))
statementsHelp revStmts =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts))
            |= soundParse
        , succeed ()
            |> Parser.map (\_ -> Done (List.reverse revStmts))
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

consonants : List String
consonants = continuantConsonants ++ doubleConsonant ++ stopConsonants
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
            [ succeed () |. oneOf (chompLetter [ " ", ",", "’" ]) |. chompVowel
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


vowels : List String
vowels =
    List.concat [ a, e, h, i, o, w, u, umlat ]


long_vowels : List String
long_vowels =
    w ++ h


short_vowels : List String
short_vowels =
    e ++ o


removeSpaces : String -> List String
removeSpaces x =
    String.split "" (String.replace " " "" x)


a_noAccent : String
a_noAccent =
    "Α α ἀ ἁ ᾰ ᾱ ᾳ ᾀ ᾁ Ᾰ Ᾱ ᾼ ᾈ ᾉ"


a_accent_short : String
a_accent_short =
    " ἂ ἃ ἄ ἅ  ὰ ά ᾲ ᾴ ᾂ ᾃ ᾄ ᾅ Ἀ Ἁ Ἂ Ἃ Ἄ Ἅ  Ὰ Ά ᾊ ᾋ ᾌ ᾍ ά"


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
    "ἢ ἣ ἤ ἥ ἦ ἧ ὴ ή ῆ ῂ ῄ ᾒ ᾓ ᾔ ᾕ ᾖ ᾗ ῇ ή Ἢ Ἣ Ἤ Ἥ Ἦ Ἧ Ὴ Ή ᾚ ᾛ ᾜ  ᾝ ᾞ ᾟ"


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
    "ω ὠ ὡ  ῳ ῲ ῴ ᾠ ᾡ  Ὠ  Ὡ ῼ ᾨ ᾩ Ω "


w_accent : String
w_accent =
    "ὢ ὣ ὤ ὥ ὦ ὧ ὼ ώ ῶ ᾢ ᾣ ᾤ ᾥ ᾦ ᾧ ῷ ώ  Ὢ Ὣ Ὤ Ὥ Ὦ Ὧ Ὼ Ώ ᾪ ᾫ ᾬ ᾭ ᾮ ᾯ"


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


u : List String
u =
    removeSpaces (u_accent_short ++ u_accent_long ++ u_noAccent)


unaccented_open_vowels : List String
unaccented_open_vowels =
    removeSpaces (a_noAccent ++ e_noAccent ++ h_noAccent ++ o_noAccent ++ w_noAccent)


closed_vowels : List String
closed_vowels =
    List.concat [ i, u ]


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


lookAhead : Array VowelSound -> ( Int, VowelSound ) -> SyllableSound
lookAhead array element =
    let
        sound =
            Tuple.second element

        index =
            Tuple.first element
    in
    case .vowelLength sound of
        LongVowel ->
            SyllableSound sound.consonant sound.vowel LongSyllable

        NoVowel ->
            SyllableSound sound.consonant sound.vowel NoSyllable

        ShortVowel ->
            if Array.length array > index then
                case Array.get (index + 1) array of
                    Just vowelSound ->
                        let
                            nextConsonant =
                                .consonant vowelSound
                        in
                        if List.member nextConsonant doubleConsonant || String.length (removeNonLetterChars nextConsonant) > 1 then
                            SyllableSound sound.consonant sound.vowel LongSyllable

                        else if vowelSound.vowelLength == NoVowel then
                            case Array.get (index + 2) array of
                                Just vowelSound2 ->
                                    let
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
        vowelSounds =
            List.map longShortVowels sounds
    in
    List.map
        (lookAhead (Array.fromList vowelSounds))
        (indexedSounds vowelSounds)


parseString : String -> List SyllableSound
parseString string =
    case run statements string of
        Ok listSounds ->
            mapLookAhead listSounds

        _ ->
            []


removeNonLetterChars : String -> String
removeNonLetterChars string =
    String.replace " " "" (String.replace "," "" (String.replace "’" "" string))
