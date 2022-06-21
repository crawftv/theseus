module ParseLsj exposing (..)
import Array exposing (..)
import Parser exposing (..)
import ParseLine exposing (consonants,vowels)
import Tuple exposing (..)

type alias Tragic =
    { author : Author
    , work : Work
    , location : Int
    }

type Author =  Euripdes | Aeschylus

type Work = Hecuba | Persae


chompAuthor : Parser Author
chompAuthor = oneOf
    [ succeed Euripdes |. keyword "E."
    , succeed Aeschylus |. keyword "A."
    ]

-- chompLine  =
--     succeed ()
--     |. chompWhile (\c -> Char.isAlphaNum c == False)
--     |= getChompedString chompAuthor



