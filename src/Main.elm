module Main exposing (main)

import Array
import Browser
import Dict
import Element exposing (alignLeft, alignRight, alignTop, padding, px, spaceEvenly, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events exposing (onClick)
import Element.Region as Region
import Html
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string, oneOf)
import Json.Decode.Pipeline exposing (custom, hardcoded, required)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Url.Builder exposing (crossOrigin)


type Model
    = BookPage_ (WebData BookPage)
    | BookPageCommentary_ (WebData BookPageCommentary)


type alias ZipListLine = { p1 : List Line, p2 : List Line, p3 : List Line}
type alias BookPage =
    { title : String
    , linesOfText : ZipListLine
    , chapter : Int
    , allBooks : List Book
    }


type alias BookPageCommentary =
    { title : String
    , linesOfText : ZipListLine
    , chapter : Int
    , allBooks : List Book
    , commentary : List Commentary
    , lineNumber : Int
    }


type alias Commentary =
    { text : String
    , commentaryAuthorId : String
    , source : String
    }


type alias Book =
    { bookTitle : String
    , bookId : Int
    , numberOfChapters : Int
    }


type alias Line =
    { text : String
    , lineNumber : Int
    }


type Msg
    = HandleBookPageResponse (WebData BookPage)
    | FetchBookPage String Int
    | FetchBookPageCommentary String Int Int
    | HandleBookPageCommentaryResponse (WebData BookPageCommentary)


init : () -> ( Model, Cmd Msg )
init _ =
    ( BookPage_ Loading
    , fetchBookPageCmd "iliad" 1
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


bookPageCommentaryDecoder : Decoder BookPageCommentary
bookPageCommentaryDecoder =
    Decode.succeed BookPageCommentary
        |> required "bpcTitle" string
        |> required "bpcLinesOfText" zipListLineDecoder
        |> required "bpcChapter" int
        |> required "bpcAllBooks" (list bookDecoder)
        |> required "bpcCommentary" (list commentaryDecoder)
        |> required "bpcLineNumber" int

zipListLineDecoder : Decoder ZipListLine
zipListLineDecoder =
    Decode.succeed ZipListLine
        |> required "p1" (list lineDecoder)
        |> required "p2" (list lineDecoder)
        |> required "p3" (list lineDecoder)

commentaryDecoder : Decoder Commentary
commentaryDecoder =
    Decode.succeed Commentary
        |> required "commentaryText" string
        |> required "commentaryAuthorId" string
        |> required "commentarySource" string


bookPageDecoder : Decoder BookPage
bookPageDecoder =
    Decode.succeed BookPage
        |> required "title" string
        |> required "linesOfText" zipListLineDecoder
        |> required "chapter" int
        |> required "allBooks" (list bookDecoder)


lineDecoder : Decoder Line
lineDecoder =
    Decode.succeed Line
        |> required "lineText" string
        |> required "lineLineNumber" int


bookDecoder : Decoder Book
bookDecoder =
    Decode.succeed Book
        |> required "bookTitle" string
        |> required "bookId" int
        |> required "numberOfChapters" int


decodeError : Model -> Http.Error -> Browser.Document Msg
decodeError model error =
    case error of
        Http.BadUrl string ->
            { title = "Bad Url"
            , body = [ Element.layout [] (Element.text ("Error: " ++ string)) ]
            }

        Http.Timeout ->
            { title = "Timeout"
            , body = [ Element.layout [] (Element.text "timeout") ]
            }

        Http.NetworkError ->
            { title = "Network Error"
            , body = [ Element.layout [] (Element.text "network error") ]
            }

        Http.BadStatus int ->
            { title = "Bad Status"
            , body = [ Element.layout [] (Element.text ("Error: " ++ String.fromInt int)) ]
            }

        Http.BadBody string ->
            { title = "Bad Body"
            , body = [ Element.layout [] (Element.text ("Error: " ++ string)) ]
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchBookPage title chapter ->
            ( model, fetchBookPageCmd title chapter )

        FetchBookPageCommentary title chapter lineNumber ->
            ( model, fetchBookPageCommentaryCmd title chapter lineNumber )

        HandleBookPageResponse data ->
            ( BookPage_ data, Cmd.none )

        HandleBookPageCommentaryResponse data ->
            ( BookPageCommentary_ data, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    case model of
        BookPage_ data ->
            case data of
                NotAsked ->
                    { title = "Not Asked"
                    , body = [ Element.layout [] (Element.text "Not Asked") ]
                    }

                Failure error ->
                    decodeError model error

                Loading ->
                    { title = "Loading"
                    , body = [ Element.layout [] (Element.text "Loading") ]
                    }

                Success bookPage ->
                    { title =
                        String.join " "
                            [ bookPage.title
                            , String.append "Chapter " (String.fromInt bookPage.chapter)
                            ]
                    , body =
                        [ viewPageComposition bookPage ]
                    }

        BookPageCommentary_ data ->
            case data of
                NotAsked ->
                    { title = "Not Asked"
                    , body = [ Element.layout [] (Element.text "Not Asked") ]
                    }

                Failure error ->
                    decodeError model error

                Loading ->
                    { title = "Loading"
                    , body = [ Element.layout [] (Element.text "Loading") ]
                    }

                Success page ->
                    { title =
                        String.join " "
                            [ page.title
                            , String.append "Chapter " (String.fromInt page.chapter)
                            ]
                    , body =
                        [ viewPageComposition page
                        ]
                    }

viewPageComposition page =  Element.layout [ spaceEvenly, Element.inFront viewTopNavigation , Element.width Element.fill] <|
                            Element.column [ padding 20 ]
                                [viewNavigationOffSet,
                                 Element.row [ padding 10, spacing 20, Element.explain Debug.todo ]
                                    ([ viewAllBooks page.allBooks
                                    , Element.textColumn [ spacing 5  ]
                                        [ Element.text
                                            (String.join " "
                                                [ page.title
                                                , String.append "Chapter " (String.fromInt page.chapter)
                                                ]
                                            )
                                        , viewAllLines page.title page.chapter page.linesOfText page.commentary
                                        ]
                                    ] )
                                ]

viewBookPageCommentary bookPageCommentary = [Element.column [ spacing 5, alignTop ]
                                        [ Element.el [Region.heading 1] (Element.text "Word Analysis")
                                        , viewWordAnalysis (Array.get (bookPageCommentary.lineNumber-1)  (Array.fromList bookPageCommentary.linesOfText))
                                        , Element.el [ Region.heading 1 ] (Element.text "Commentary")
                                        , Element.column [] (List.map viewCommentary bookPageCommentary.commentary)
                                        ]]

viewWordAnalysis : Maybe Line -> Element.Element Msg
viewWordAnalysis maybe =
    case maybe of
        Just line ->
            Element.column [] (List.map viewWord (String.split " " (line.text)) )
        Nothing ->
            Element.text "Error: No text could be found"

viewWord word =
    Element.el [] (Element.text word)

viewHomePage_ =
    Element.text "Home Page"


viewAllBooks books =
    Element.column [ spacing 5, alignTop ] (List.map viewBook books)


viewBook : Book -> Element.Element Msg
viewBook book =
    Element.column []
        [ Element.text book.bookTitle
        , Element.wrappedRow [] (List.map (viewChapter book) (List.range 1 book.numberOfChapters))
        ]

viewChapter : Book -> Int -> Element.Element Msg
viewChapter book chapter =
    Element.el [ padding 2, onClick (FetchBookPage book.bookTitle chapter) ] (Element.text (String.fromInt chapter))


viewLine : String -> Int -> Line -> Element.Element Msg
viewLine title chapter line =
    Element.row []
        [ Element.el [ alignLeft, width (px 50) ] (Element.text (String.fromInt line.lineNumber))
        , Element.el [ padding 15, onClick (FetchBookPageCommentary title chapter line.lineNumber) ] (Element.text line.text)
        ]



viewCommentaryLine : String -> Int ->List Line -> Commentary -> Element.Element Msg
viewCommentaryLine title chapter listLine commentary =
    let res = List.head listLine
    in
        case res of
            Just line ->
                Element.row []
                    [ Element.el [ alignLeft, width (px 50), Element.onRight (viewCommentary commentary )] (Element.text (String.fromInt line.lineNumber))
                    , Element.el [ padding 15, onClick (FetchBookPageCommentary title chapter line.lineNumber) ] (Element.text line.text)
                    ]
            Nothing ->
                Element.none

viewAllLines : String -> Int -> ZipListLine -> Commentary -> Element.Element Msg
viewAllLines title chapter zipListLine commentary=
    Element.textColumn [] (List.concat [
         (List.map (viewLine title chapter) zipListLine.p1)
        , [viewCommentaryLine title chapter zipListLine.p2 commentary]
        , (List.map (viewLine title chapter) zipListLine.p3)
        ])

navigationOffSet = {top=25,bottom=0,right=0,left=0}
viewNavigationOffSet=
         Element.el [ Element.width Element.fill, Region.navigation, alignTop, Element.paddingEach navigationOffSet,spacing 20] Element.none
       
viewTopNavigation =
    Element.row [ Background.color navColorWhite, Element.width Element.fill, Region.navigation, Border.widthEach navBorders, alignTop, padding 10 , Element.height (Element.px 25) ]
        [ Element.el [ alignLeft ] (Element.text "Logo")
        , Element.el [ alignRight ] (Element.text "Settings")
        ]


navBorders =
    { bottom = 1
    , left = 0
    , right = 0
    , top = 0
    }
navColorWhite = Element.fromRgb255 {
    red = 255,
    green= 255,
    blue= 255,
    alpha= 255
                }

viewCommentary : Commentary -> Element.Element Msg
viewCommentary commentary =
    Element.paragraph [ padding 5, spacing 5 ]
        [ Element.el [] (Element.text commentary.text)
        , Element.el [] (Element.text commentary.source)
        , Element.el [] (Element.text commentary.commentaryAuthorId)
        ]



main =
    Browser.document
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


fetchBookPageCmd : String -> Int -> Cmd Msg
fetchBookPageCmd title chapter =
    RemoteData.Http.getWithConfig RemoteData.Http.defaultConfig (crossOrigin "http://localhost:8080/books" [ title, String.fromInt chapter ] []) HandleBookPageResponse bookPageDecoder


fetchBookPageCommentaryCmd : String -> Int -> Int -> Cmd Msg
fetchBookPageCommentaryCmd title chapter lineNumber =
    RemoteData.Http.getWithConfig RemoteData.Http.defaultConfig (crossOrigin "http://localhost:8080/books" [ title, String.fromInt chapter, String.fromInt lineNumber ] []) HandleBookPageCommentaryResponse bookPageCommentaryDecoder
