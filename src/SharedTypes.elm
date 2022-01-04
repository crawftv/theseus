module SharedTypes exposing (..)


type alias BookPage =
    { title : String
    , linesOfText : List Line
    , chapter : Int
    , allBooks : List Book
    }


type alias BookCommentaryPage =
    { title : String
    , linesOfText : List Line
    , chapter : Int
    , allBooks : List Book
    , commentary : List Commentary
    }


type alias Commentary =
    { text : String
    , author : String
    , source : String
    }


type alias Book =
    { bookTitle : String
    , bookId : Int
    }


type alias Line =
    { text : String
    , lineNumber : Int
    }
