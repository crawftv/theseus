module TestParseLsj exposing (..)

import Expect exposing (Expectation)
import ParseLsj exposing (..)
import Parser exposing (getChompedString, run)
import Test exposing (..)


testSoundParse : Test
testSoundParse =
    describe "testing sound Parser"
        [ test "οπ" <|
            \_ ->
                run chompAuthor "θᾰλασσό-πλαγκτος, ον, ( πλάζω) `A` **made to wander o'er the sea, seatost**, of ships, A. *Pr.* 467; of a corpse, E. *Hec.* 782 :—also θᾰλασσο-πλάνητος [ πλᾰ], ον, Sch. Opp. *H.* 4.582. "
                    |> Expect.equal (Ok (Sound "" "ο"))
                    ]
