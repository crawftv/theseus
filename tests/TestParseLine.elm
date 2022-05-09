module TestParseLine exposing (..)

import Array exposing (..)
import Expect exposing (Expectation)
import ParseLine exposing (..)
import Parser exposing (getChompedString, run)
import Test exposing (..)


testSoundParse : Test
testSoundParse =
    describe "testing sound Parser"
        [ test "οπ" <|
            \_ ->
                run soundParse "οπ"
                    |> Expect.equal (Ok (Sound "" "ο"))
        , test "ο" <|
            \_ ->
                run soundParse "ο"
                    |> Expect.equal (Ok (Sound "" "ο"))
        , test "test μοι" <|
            \_ ->
                run soundParse "μοι"
                    |> Expect.equal (Ok (Sound "μ" "οι"))
        , test "ἄνδρα" <|
            \_ ->
                run soundParse "ἄ"
                    |> Expect.equal (Ok (Sound "" "ἄ"))
        , test "δρα" <|
            \_ ->
                run soundParse "δρα"
                    |> Expect.equal (Ok (Sound "δρ" "α"))
        , test "δδρα" <|
            \_ ->
                run soundParse "δδρα"
                    |> Expect.equal (Ok (Sound "δ" ""))
        , test "νδρα" <|
            \_ ->
                run soundParse "νδρα"
                    |> Expect.equal (Ok (Sound "ν" ""))
        , test "σκό" <|
            \_ ->
                run soundParse "σκό"
                    |> Expect.equal (Ok (Sound "σκ" "ό"))
        , test "σκ ό" <|
            \_ ->
                run soundParse "σκ ό"
                    |> Expect.equal (Ok (Sound "σκ" " ό"))
        ]


test_consonants : Test
test_consonants =
    describe "testing consonant Parser"
        [ test "simple consonant" <|
            \_ ->
                run consonant "π"
                    |> Expect.equal (Ok "π")
        , test "2 consonant" <|
            \_ ->
                run consonant "πΠ"
                    |> Expect.equal (Ok "π")
        , test "ππ" <|
            \_ ->
                run consonant "ππ"
                    |> Expect.equal (Ok "π")
        , test "λλ" <|
            \_ ->
                run consonant "λλ"
                    |> Expect.equal (Ok "λ")
        , test "πΠο" <|
            \_ ->
                run consonant "πΠο"
                    |> Expect.equal (Ok "π")
        , test "οπΠ" <|
            \_ ->
                run consonant "οπΠ"
                    |> Expect.equal (Ok "")
        , test "νδρα" <|
            \_ ->
                run consonant "νδρα"
                    |> Expect.equal (Ok "ν")
        , test "δρα" <|
            \_ ->
                run consonant "δρα"
                    |> Expect.equal (Ok "δρ")
        , test "δδρα" <|
            \_ ->
                run consonant "δδρα"
                    |> Expect.equal (Ok "δ")
        , test "α" <|
            \_ ->
                run consonant "α"
                    |> Expect.equal (Ok "")
        ]


test_chompContinuant : Test
test_chompContinuant =
    describe "test chompContinuant"
        [ test "νδρα" <|
            \_ ->
                run (getChompedString <| chompContinuant) "ν"
                    |> Expect.equal (Ok "ν")
        , test "ννδρα" <|
            \_ ->
                run (getChompedString <| chompContinuant) "ν"
                    |> Expect.equal (Ok "ν")
        ]


testStatements : Test
testStatements =
    describe "testing statements"
        [ test "ἄνδρα" <|
            \_ ->
                run statements "ἄνδρα"
                    |> Expect.equal (Ok [ Sound "" "ἄ", Sound "ν" "", Sound "δρ" "α" ])
        , test "ἄ" <|
            \_ ->
                run statements "ἄ"
                    |> Expect.equal (Ok [ Sound "" "ἄ" ])
        , test "ν" <|
            \_ ->
                run statements "ν"
                    |> Expect.equal (Ok [ Sound "ν" "" ])
        , test "ἄν" <|
            \_ ->
                run statements "ἄν"
                    |> Expect.equal (Ok [ Sound "" "ἄ", Sound "ν" "" ])
        , test "δρα" <|
            \_ ->
                run statements "δρα"
                    |> Expect.equal (Ok [ Sound "δρ" "α" ])
        , test "ἔ" <|
            \_ ->
                run statements "ἔ"
                    |> Expect.equal (Ok [ Sound "" "ἔ" ])
        , test "ἔν" <|
            \_ ->
                run statements "ἔν"
                    |> Expect.equal (Ok [ Sound "" "ἔ", Sound "ν" "" ])
        , test "ἔννε" <|
            \_ ->
                run statements "ἔννε"
                    |> Expect.equal (Ok [ Sound "" "ἔ", Sound "ν" "", Sound "ν" "ε" ])
        , test "ἔννεπε" <|
            \_ ->
                run statements "ἔννεπε"
                    |> Expect.equal (Ok [ Sound "" "ἔ", Sound "ν" "", Sound "ν" "ε", Sound "π" "ε" ])
        , test "μοῦσα" <|
            \_ ->
                run statements "μοῦσα"
                    |> Expect.equal (Ok [ Sound "μ" "οῦ", Sound "σ" "α" ])
        , test "πολύτροπον" <|
            \_ ->
                run statements "πολύτροπον"
                    |> Expect.equal (Ok [ Sound "π" "ο", Sound "λ" "ύ", Sound "τρ" "ο", Sound "π" "ο", Sound "ν" "" ])
        , test "ὃς" <|
            \_ ->
                run statements "ὃς"
                    |> Expect.equal (Ok [ Sound "" "ὃ", Sound "ς" "" ])
        , test "μάλα" <|
            \_ ->
                run statements "μάλα"
                    |> Expect.equal (Ok [ Sound "μ" "ά", Sound "λ" "α" ])
        , test "πολλὰ" <|
            \_ ->
                run statements "πολλὰ"
                    |> Expect.equal (Ok [ Sound "π" "ο", Sound "λ" "", Sound "λ" "ὰ" ])
        , test "ἄνδρα μοι ἔννεπε, μοῦσα, πολύτροπον, ὃς μάλα πολλὰ" <|
            \_ ->
                run statements "ἄνδρα μοι ἔννεπε, μοῦσα, πολύτροπον, ὃς μάλα πολλὰ"
                    |> Expect.equal
                        (Ok
                            (List.concat
                                [ [ Sound "" "ἄ", Sound "ν" "", Sound "δρ" "α" ]
                                , [ Sound " μ" "οι" ]
                                , [ Sound " " "ἔ", Sound "ν" "", Sound "ν" "ε", Sound "π" "ε," ]
                                , [ Sound " μ" "οῦ", Sound "σ" "α," ]
                                , [ Sound " π" "ο", Sound "λ" "ύ", Sound "τρ" "ο", Sound "π" "ο", Sound "ν" "," ]
                                , [ Sound " " "ὃ", Sound "ς" " " ]
                                , [ Sound "μ" "ά", Sound "λ" "α" ]
                                , [ Sound " π" "ο", Sound "λ" "", Sound "λ" "ὰ" ]
                                ]
                            )
                        )
        , test "σκότος ἔμὸν φαός" <|
            \_ ->
                run statements "σκότος ἔμὸν φαός"
                    |> Expect.equal
                        (Ok
                            (List.concat
                                [ [ Sound "σκ" "ό", Sound "τ" "ο", Sound "ς" " ἔ" ]
                                , [ Sound "μ" "ὸ", Sound "ν" " " ]
                                , [ Sound "φ" "α", Sound "" "ό", Sound "ς" "" ]
                                ]
                            )
                        )
        , test "ἔρεβος ὦ φαεννότατον ὡς ἐμοί" <|
            \_ ->
                run statements "ἔρεβος ὦ φαεννότατον ὡς ἐμοί"
                    |> Expect.equal
                        (Ok
                            (List.concat
                                [ [ Sound "" "ἔ", Sound "ρ" "ε", Sound "β" "ο", Sound "ς" " ὦ" ]
                                , [ Sound " φ" "α", Sound "" "ε", Sound "ν" "", Sound "ν" "ό", Sound "τ" "α", Sound "τ" "ο" ]
                                , [ Sound "ν" " ὡ" ]
                                , [ Sound "ς" " ἐ" ]
                                , [ Sound "μ" "οί" ]
                                ]
                            )
                        )
        , test "μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος" <|
            \_ ->
                run statements "μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος"
                    |> Expect.equal
                        (Ok
                            (List.concat
                                [ [ Sound "μ" "ῆ", Sound "ν" "ι", Sound "ν" " ἄ" ]
                                , [ Sound "" "ει", Sound "δ" "ε" ]
                                , [ Sound " θ" "ε", Sound "" "ὰ" ]
                                , [ Sound " Π" "η", Sound "λ" "η", Sound "" "ϊ", Sound "" "ά", Sound "δ" "ε", Sound "" "ω" ]
                                , [ Sound " " "Ἀ", Sound "χ" "ι", Sound "λ" "ῆ", Sound "" "ο", Sound "ς" "" ]
                                ]
                            )
                        )
        ]


test_getSounds : Test
test_getSounds =
    describe "testing getSounds"
        [ test "ἄνδρα μοι ἔννεπε, μοῦσα, πολύτροπον, ὃς μάλα πολλὰ" <|
            \_ ->
                getSounds "ἄνδρα μοι ἔννεπε, μοῦσα, πολύτροπον, ὃς μάλα πολλὰ"
                    |> Expect.equal
                        (List.concat
                            [ [ Sound "" "ἄ", Sound "ν" "", Sound "δρ" "α" ]
                            , [ Sound " μ" "οι" ]
                            , [ Sound " " "ἔ", Sound "ν" "", Sound "ν" "ε", Sound "π" "ε," ]
                            , [ Sound " μ" "οῦ", Sound "σ" "α," ]
                            , [ Sound " π" "ο", Sound "λ" "ύ", Sound "τρ" "ο", Sound "π" "ο", Sound "ν" "," ]
                            , [ Sound " " "ὃ", Sound "ς" " " ]
                            , [ Sound "μ" "ά", Sound "λ" "α" ]
                            , [ Sound " π" "ο", Sound "λ" "", Sound "λ" "ὰ" ]
                            ]
                        )
        ]


test_longShortVowels : Test
test_longShortVowels =
    describe "longShortVowels"
        [ test "{consonant = \"\", vowel= \"ἄ\"}" <|
            \_ ->
                longShortVowels { consonant = "", vowel = "ἄ" }
                    |> Expect.equal
                        { consonant = "", vowel = "ἄ", vowelLength = ShortVowel }
        , test "{consonant = \" μ\", vowel= \"οι\"}" <|
            \_ ->
                longShortVowels { consonant = " μ", vowel = "οι" }
                    |> Expect.equal
                        { consonant = " μ", vowel = "οι", vowelLength = LongVowel }
        , test "{consonant = \"\", vowel= \"ω\"}" <|
            \_ ->
                longShortVowels { consonant = "", vowel = "ω" }
                    |> Expect.equal
                        { consonant = "", vowel = "ω", vowelLength = LongVowel }
        , test "{consonant = \"λ\", vowel= \"\"}" <|
            \_ ->
                longShortVowels { consonant = "λ", vowel = "" }
                    |> Expect.equal
                        { consonant = "λ", vowel = "", vowelLength = NoVowel }
        , test "δ’ὅ" <|
            \_ ->
                longShortVowels { consonant = "δ", vowel = "’ὅ" }
                    |> Expect.equal
                        { consonant = "δ", vowel = "’ὅ", vowelLength = ShortVowel }
        ]


odyssey_1_1 =
    List.concat
        [ [ Sound "" "ἄ", Sound "ν" "", Sound "δρ" "α" ]
        , [ Sound " μ" "οι" ]
        , [ Sound " " "ἔ", Sound "ν" "", Sound "ν" "ε", Sound "π" "ε," ]
        , [ Sound " μ" "οῦ", Sound "σ" "α," ]
        , [ Sound " π" "ο", Sound "λ" "ύ", Sound "τρ" "ο", Sound "π" "ο", Sound "ν" "," ]
        , [ Sound " " "ὃ", Sound "ς" " " ]
        , [ Sound "μ" "ά", Sound "λ" "α" ]
        , [ Sound " π" "ο", Sound "λ" "", Sound "λ" "ὰ" ]
        ]


test_mapLongShortVowels : Test
test_mapLongShortVowels =
    describe "test_mapLongShortVowels"
        [ test "Odyssey 1.1" <|
            \_ ->
                List.map longShortVowels odyssey_1_1
                    |> Expect.equal
                        [ VowelSound "" "ἄ" ShortVowel
                        , VowelSound "ν" "" NoVowel
                        , VowelSound "δρ" "α" ShortVowel
                        , VowelSound " μ" "οι" LongVowel
                        , VowelSound " " "ἔ" ShortVowel
                        , VowelSound "ν" "" NoVowel
                        , VowelSound "ν" "ε" ShortVowel
                        , VowelSound "π" "ε," ShortVowel
                        , VowelSound " μ" "οῦ" LongVowel
                        , VowelSound "σ" "α," ShortVowel
                        , VowelSound " π" "ο" ShortVowel
                        , VowelSound "λ" "ύ" ShortVowel
                        , VowelSound "τρ" "ο" ShortVowel
                        , VowelSound "π" "ο" ShortVowel
                        , VowelSound "ν" "," NoVowel
                        , VowelSound " " "ὃ" ShortVowel
                        , VowelSound "ς" " " NoVowel
                        , VowelSound "μ" "ά" ShortVowel
                        , VowelSound "λ" "α" ShortVowel
                        , VowelSound " π" "ο" ShortVowel
                        , VowelSound "λ" "" NoVowel
                        , VowelSound "λ" "ὰ" ShortVowel
                        ]
        ]


test_lookAhead : Test
test_lookAhead =
    describe "test_lookAhead"
        [ test "μοῦσα" <|
            \_ ->
                lookAhead (Array.fromList [ VowelSound " μ" "οῦ" LongVowel, VowelSound "σ" "α," ShortVowel ]) ( 0, VowelSound " μ" "οῦ" LongVowel )
                    |> Expect.equal
                        { consonant = " μ", vowel = "οῦ", syllableLength = LongSyllable }
        , test "μοῦσα 2" <|
            \_ ->
                lookAhead (Array.fromList [ VowelSound " μ" "οῦ" LongVowel ]) ( 0, VowelSound " μ" "οῦ" LongVowel )
                    |> Expect.equal
                        { consonant = " μ", vowel = "οῦ", syllableLength = LongSyllable }
        , test "μοῦσα3" <|
            \_ ->
                lookAhead (Array.fromList [ VowelSound " μ" "οῦ" LongVowel, VowelSound "σ" "α," ShortVowel ]) ( 1, VowelSound " μ" "οῦ" LongVowel )
                    |> Expect.equal
                        { consonant = " μ", vowel = "οῦ", syllableLength = LongSyllable }
        , test "λύτρο" <|
            \_ ->
                lookAhead (Array.fromList [ VowelSound "λ" "ύ" ShortVowel, VowelSound "τρ" "ο" ShortVowel ]) ( 0, VowelSound "λ" "ύ" ShortVowel )
                    |> Expect.equal
                        { consonant = "λ", vowel = "ύ", syllableLength = LongSyllable }
        ]


test_mapLookAhead : Test
test_mapLookAhead =
    describe "test_mapLookAhead"
        [ test " πολύτροπον," <|
            \_ ->
                mapLookAhead [ Sound " π" "ο", Sound "λ" "ύ", Sound "τρ" "ο", Sound "π" "ο", Sound "ν" "," ]
                    |> Expect.equal
                        [ { consonant = " π", syllableLength = ShortSyllable, vowel = "ο" }
                        , { consonant = "λ", syllableLength = LongSyllable, vowel = "ύ" }
                        , { consonant = "τρ", syllableLength = ShortSyllable, vowel = "ο" }
                        , { consonant = "π", syllableLength = ShortSyllable, vowel = "ο" }
                        , { consonant = "ν", syllableLength = NoSyllable, vowel = "," }
                        ]
        , test "andra" <|
            \_ ->
                mapLookAhead [ Sound "" "ἄ", Sound "ν" "", Sound "δρ" "α" ]
                    |> Expect.equal
                        [ SyllableSound "" "ἄ" LongSyllable, SyllableSound "ν" "" NoSyllable, SyllableSound "δρ" "α" ShortSyllable ]
        , test "πολλὰ δ’ὅ γ’ἐν πόντῳ πάθεν ἄλγεα ὃν κατὰ θυμόν," <|
            \_ ->
                mapLookAhead [ Sound "π" "ο", Sound "λ" "", Sound "λ" "ὰ", Sound " δ’" "ὅ", Sound " γ’" "’ἐ", Sound "ν " "", Sound "π" "ό", Sound "ν" "", Sound "τ" "ῳ" ]
                    |> Expect.equal
                        [ SyllableSound "π" "ο" LongSyllable
                        , SyllableSound "λ" "" NoSyllable
                        , SyllableSound "λ" "ὰ" ShortSyllable
                        , SyllableSound " δ’" "ὅ" ShortSyllable
                        , SyllableSound " γ’" "’ἐ" LongSyllable
                        , SyllableSound "ν " "" NoSyllable
                        , SyllableSound "π" "ό" LongSyllable
                        , SyllableSound "ν" "" NoSyllable
                        , SyllableSound "τ" "ῳ" LongSyllable
                        ]
        ]


test_parseString : Test
test_parseString =
    describe "test_parseString"
        [ test "πολλὰ δ’ὅ γ’ἐν πόντῳ πάθεν ἄλγεα ὃν κατὰ θυμόν," <|
            \_ ->
                parseString "πολλὰ δ’ὅ γ’ἐν πόντῳ πάθεν ἄλγεα ὃν κατὰ θυμόν,"
                    |> Expect.equal
                        [ SyllableSound "π" "ο" LongSyllable
                        , SyllableSound "λ" "" NoSyllable
                        , SyllableSound "λ" "ὰ" ShortSyllable
                        , SyllableSound " δ" "’ὅ" ShortSyllable
                        , SyllableSound " γ" "’ἐ" LongSyllable
                        , SyllableSound "ν" " " NoSyllable
                        , SyllableSound "π" "ό" LongSyllable
                        , SyllableSound "ν" "" NoSyllable
                        , SyllableSound "τ" "ῳ" LongSyllable
                        , SyllableSound " π" "ά" ShortSyllable
                        , SyllableSound "θ" "ε" ShortSyllable
                        , SyllableSound "ν" " ἄ" LongSyllable
                        , SyllableSound "λ" "" NoSyllable
                        , SyllableSound "γ" "ε" ShortSyllable
                        , SyllableSound "" "α" ShortSyllable
                        , SyllableSound " " "ὃ" LongSyllable
                        , SyllableSound "ν" " " NoSyllable
                        , SyllableSound "κ" "α" ShortSyllable
                        , SyllableSound "τ" "ὰ" ShortSyllable
                        , SyllableSound " θ" "υ" ShortSyllable -- rules/algo say long, chamberlain says long
                        , SyllableSound "μ" "ό" ShortSyllable -- rules/algo say long, chamberlain says long
                        , SyllableSound "ν" "," NoSyllable
                        ]
        , test "ἄνδρα μοι ἔννεπε, μοῦσα, πολύτροπον, ὃς μάλα πολλὰ" <|
            \_ ->
                parseString "ἄνδρα μοι ἔννεπε, μοῦσα, πολύτροπον, ὃς μάλα πολλὰ"
                    |> Expect.equal
                        [ { consonant = "", syllableLength = LongSyllable, vowel = "ἄ" }
                        , { consonant = "ν", syllableLength = NoSyllable, vowel = "" }
                        , { consonant = "δρ", syllableLength = ShortSyllable, vowel = "α" }
                        , { consonant = " μ", syllableLength = LongSyllable, vowel = "οι" } --TODO solve this, chamberlain says short, dipthong rule says long.
                        , { consonant = " ", syllableLength = LongSyllable, vowel = "ἔ" }
                        , { consonant = "ν", syllableLength = NoSyllable, vowel = "" }
                        , { consonant = "ν", syllableLength = ShortSyllable, vowel = "ε" }
                        , { consonant = "π", syllableLength = ShortSyllable, vowel = "ε," }
                        , { consonant = " μ", syllableLength = LongSyllable, vowel = "οῦ" }
                        , { consonant = "σ", syllableLength = ShortSyllable, vowel = "α," }
                        , { consonant = " π", syllableLength = ShortSyllable, vowel = "ο" }
                        ,{ consonant = "λ", syllableLength = LongSyllable, vowel = "ύ" }
                        ,{ consonant = "τρ", syllableLength = ShortSyllable, vowel = "ο" }
                        ,{ consonant = "π", syllableLength = ShortSyllable, vowel = "ο" }
                        ,{ consonant = "ν", syllableLength = NoSyllable, vowel = "," }
                        ,{ consonant = " ", syllableLength = LongSyllable, vowel = "ὃ" }
                        ,{ consonant = "ς", syllableLength = NoSyllable, vowel = " " }
                        ,{ consonant = "μ", syllableLength = ShortSyllable, vowel = "ά" }
                        ,{ consonant = "λ", syllableLength = ShortSyllable, vowel = "α" }
                        ,{ consonant = " π", syllableLength = LongSyllable, vowel = "ο" }
                        ,{ consonant = "λ", syllableLength = NoSyllable, vowel = "" }
                        ,{ consonant = "λ", syllableLength = ShortSyllable, vowel = "ὰ" }]
        , test "πλάγχθη, ἐπεὶ Τροίης ἱερὸν πτολίεθρον ἔπερσεν·" <|
            \_ ->
                parseString "πλάγχθη, ἐπεὶ Τροίης ἱερὸν πτολίεθρον ἔπερσεν·"
                    |> Expect.equal
                    [ { consonant = "πλ", syllableLength = LongSyllable, vowel = "ά" }
                    , { consonant = "γ", syllableLength = NoSyllable, vowel = "" }
                    , { consonant = "χ", syllableLength = NoSyllable, vowel = "" }
                    , { consonant = "θ", syllableLength = ShortSyllable, vowel = "η," }
                    , { consonant = " ", syllableLength = ShortSyllable, vowel = "ἐ" }
                    , { consonant = "π", syllableLength = LongSyllable, vowel = "εὶ" }
                    , { consonant = " Τρ", syllableLength = LongSyllable, vowel = "οί" }
                    , { consonant = "", syllableLength = LongSyllable, vowel = "η" }
                    , { consonant = "ς", syllableLength = ShortSyllable, vowel = " ἱ" }
                    , { consonant = "", syllableLength = ShortSyllable, vowel = "ε" }
                    , { consonant = "ρ", syllableLength = LongSyllable, vowel = "ὸ" }
                    , { consonant = "ν", syllableLength = NoSyllable, vowel = " " }
                    , { consonant = "π", syllableLength = NoSyllable, vowel = "" }
                    , { consonant = "τ", syllableLength = ShortSyllable, vowel = "ο" }
                    , { consonant = "λ", syllableLength = ShortSyllable, vowel = "ί" }
                    , { consonant = "", syllableLength = LongSyllable, vowel = "ε" }
                    , { consonant = "θρ", syllableLength = ShortSyllable, vowel = "ο" }
                    , { consonant = "ν", syllableLength = ShortSyllable, vowel = " ἔ" }
                    , { consonant = "π", syllableLength = LongSyllable, vowel = "ε" }
                    , { consonant = "ρ", syllableLength = NoSyllable, vowel = "" }
                    , { consonant = "σ", syllableLength = ShortSyllable, vowel = "ε" }
                    , { consonant = "ν", syllableLength = NoSyllable, vowel = "" }
                    ]
        , test "πολλῶν δ’ἀνθρώπων ἴδεν ἄστεα καὶ νόον ἔγνω," <|
            \_ ->
                parseString "πολλῶν δ’ἀνθρώπων ἴδεν ἄστεα καὶ νόον ἔγνω,"
                    |> Expect.equal
                     [ { consonant = "π", syllableLength = LongSyllable, vowel = "ο" }
                     , { consonant = "λ", syllableLength = NoSyllable, vowel = "" }
                     , { consonant = "λ", syllableLength = LongSyllable, vowel = "ῶ" }
                     , { consonant = "ν", syllableLength = NoSyllable, vowel = " " }
                     , { consonant = "δ", syllableLength = LongSyllable, vowel = "’ἀ" }
                     , { consonant = "ν", syllableLength = NoSyllable, vowel = "" }
                     , { consonant = "θρ", syllableLength = LongSyllable, vowel = "ώ" }
                     , { consonant = "π", syllableLength = LongSyllable, vowel = "ω" }
                     , { consonant = "ν", syllableLength = ShortSyllable, vowel = " ἴ" }
                     , { consonant = "δ", syllableLength = ShortSyllable, vowel = "ε" }
                     , { consonant = "ν", syllableLength = LongSyllable, vowel = " ἄ" }
                     , { consonant = "στ", syllableLength = ShortSyllable, vowel = "ε" }
                     , { consonant = "", syllableLength = ShortSyllable, vowel = "α" }
                     , { consonant = " κ", syllableLength = LongSyllable, vowel = "αὶ" }
                     , { consonant = " ν", syllableLength = ShortSyllable, vowel = "ό" }
                     , { consonant = "", syllableLength = ShortSyllable, vowel = "ο" }
                     , { consonant = "ν", syllableLength = LongSyllable, vowel = " ἔ" }
                     , { consonant = "γ", syllableLength = NoSyllable, vowel = "" }
                     , { consonant = "ν", syllableLength = ShortSyllable, vowel = "ω," }
                     ]

        ]
