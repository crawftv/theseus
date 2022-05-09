module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict
import Element exposing (alignLeft, alignRight, alignTop, padding, px, spaceEvenly, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events exposing (onClick)
import Element.Region as Region
import Html
import Http
import Json.Decode as Decode exposing (Decoder, array, int, list, oneOf, string)
import Json.Decode.Pipeline exposing (custom, hardcoded, required)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Url.Builder exposing (crossOrigin)


type Model
    = BookPage_ (WebData BookPage)
    | BookPageCommentary_ (WebData BookPageCommentary)


type alias Title =
    String


type alias Text =
    String


type alias Chapter =
    Int


type alias LineNumber =
    Int


type alias BookId =
    Int


type alias NumberOfChapters =
    Int


type alias CommentaryAuthorId =
    String


type alias CommentaryNumber =
    Int


type alias Source =
    String


type alias ZipListLine =
    { p1 : List Line, p2 : List Line, p3 : List Line }


type alias BookPage =
    { title : Title
    , linesOfText : ZipListLine
    , chapter : Chapter
    , allBooks : List Book
    }


type alias BookPageCommentary =
    { title : Title
    , linesOfText : ZipListLine
    , chapter : Chapter
    , allBooks : List Book
    , commentary : Array Commentary
    , lineNumber : LineNumber
    , commentaryNumber : CommentaryNumber
    }


type alias Commentary =
    { text : Text
    , commentaryAuthorId : CommentaryAuthorId
    , source : Source
    }


type alias Book =
    { bookTitle : Title
    , bookId : BookId
    , numberOfChapters : NumberOfChapters
    }


type alias Line =
    { text : Text
    , lineNumber : LineNumber
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
        |> required "bpcCommentary" (array commentaryDecoder)
        |> required "bpcLineNumber" int
        |> hardcoded 0


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

                Success page ->
                    { title =
                        String.join " "
                            [ page.title
                            , String.append "Chapter " (String.fromInt page.chapter)
                            ]
                    , body =
                        [ viewPageComposition page ]
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
                        [ viewBookPageCommentary page
                        ]
                    }


viewPageComposition page =
    Element.layout [ spaceEvenly, Element.inFront viewTopNavigation, Element.width Element.fill ] <|
        Element.column [ padding 20 ]
            [ viewNavigationOffSet
            , Element.row [ padding 10, spacing 20 ]
                (viewCommonToAll page)
            ]


viewCommonToAll page =
    [ viewAllBooks page.allBooks
    , Element.textColumn [ spacing 5 ]
        [ Element.text
            (String.join " "
                [ page.title
                , String.append "Chapter " (String.fromInt page.chapter)
                ]
            )
        , viewAllLines page.title page.chapter page.linesOfText
        ]
    ]


viewBookPageCommentary : BookPageCommentary -> Html.Html Msg
viewBookPageCommentary page =
    Element.layout [ spaceEvenly, Element.inFront viewTopNavigation, Element.width Element.fill ] <|
        Element.column [ padding 20 ]
            [ viewNavigationOffSet
            , Element.row [ padding 10, spacing 20 ]
                [ viewAllBooks page.allBooks
                , Element.textColumn [ spacing 5 ]
                    [ Element.text
                        (String.join " "
                            [ page.title
                            , String.append "Chapter " (String.fromInt page.chapter)
                            ]
                        )
                    , viewAllLinesCommentary page.title page.chapter page.linesOfText page.commentary page.commentaryNumber
                    ]
                ]
            ]


viewAllLinesCommentary : Title -> Chapter -> ZipListLine -> Array Commentary -> CommentaryNumber -> Element.Element Msg
viewAllLinesCommentary title chapter zipListLine commentary commentaryNumber =
    Element.textColumn []
        (List.concat
            [ List.map (viewLine title chapter) zipListLine.p1
            , [ viewCommentaryLine title chapter zipListLine.p2 commentary commentaryNumber ]
            , List.map (viewLine title chapter) zipListLine.p3
            ]
        )


viewCommentaryLine : Title -> Chapter -> List Line -> Array Commentary -> CommentaryNumber -> Element.Element Msg
viewCommentaryLine title chapter listLine arrayCommentary commentaryNumber =
    let
        res =
            List.head listLine
    in
    case res of
        Just line ->
            Element.row [ Element.onRight (viewCommentary commentaryNumber arrayCommentary listLine) ]
                [ Element.el [ alignLeft, width (px 50) ] (Element.text (String.fromInt line.lineNumber))
                , Element.el
                    [ padding 15
                    , onClick (FetchBookPageCommentary title chapter line.lineNumber)
                    ]
                    (Element.text line.text)
                ]

        Nothing ->
            Element.none


viewCommentary : CommentaryNumber -> Array Commentary -> List Line -> Element.Element Msg
viewCommentary commentaryNumber arrayCommentary line =
    let
        res =
            Array.get commentaryNumber arrayCommentary
    in
    case res of
        Just commentary ->
            Element.textColumn [ Element.paddingEach commentaryPadding, spacing 5, Element.above (viewWordAnalysis (List.head line)), Border.width 2 ]
                [ viewCommentaryElement commentary.text
                , viewCommentaryElement commentary.source
                , viewCommentaryElement commentary.commentaryAuthorId
                ]

        Nothing ->
            Element.el [ Element.onRight (viewWordAnalysis (List.head line)), Element.width Element.fill ] Element.none


viewWordAnalysis : Maybe Line -> Element.Element Msg
viewWordAnalysis maybe =
    case maybe of
        Just line ->
            Element.column [ Element.paddingEach commentaryPadding, Border.width 2 ] (List.map viewWord (String.split " " line.text))

        Nothing ->
            Element.text "Error: No text could be found"


viewCommentaryElement a =
    Element.paragraph [] [ Element.el [] (Element.text a) ]


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


viewChapter : Book -> Chapter -> Element.Element Msg
viewChapter book chapter =
    Element.el [ padding 2, onClick (FetchBookPage book.bookTitle chapter) ] (Element.text (String.fromInt chapter))


viewLine : String -> Chapter -> Line -> Element.Element Msg
viewLine title chapter line =
    Element.row []
        [ Element.el [ alignLeft, width (px 50) ] (Element.text (String.fromInt line.lineNumber))
        , Element.el [ padding 15, onClick (FetchBookPageCommentary title chapter line.lineNumber) ] (Element.text line.text)
        ]


viewAllLines : Title -> Chapter -> ZipListLine -> Element.Element Msg
viewAllLines title chapter zipListLine =
    Element.textColumn []
        (List.concat
            [ List.map (viewLine title chapter) zipListLine.p1
            , List.map (viewLine title chapter) zipListLine.p2
            , List.map (viewLine title chapter) zipListLine.p3
            ]
        )



-- this offsets the page for the navbar, so it doesnt cover text.
-- viewNavigationOffSet =Element.el [ Element.width Element.fill, Region.navigation, alignTop, Element.paddingEach navigationOffSet, spacing 20 ] Element.none


viewNavigationOffSet =
    Element.none


viewTopNavigation =
    Element.row [ Background.color navColorWhite, Element.width Element.fill, Region.navigation, Border.widthEach navBorders, Element.paddingXY 10 0, Element.height (Element.maximum 25 Element.fill) ]
        [ Element.el [] (Element.text "Logo")
        , Element.el [ alignRight ] (Element.text "Settings")
        ]


navBorders =
    { bottom = 1
    , left = 0
    , right = 0
    , top = 0
    }


navigationOffSet =
    { top = 25, bottom = 0, right = 0, left = 0 }


commentaryPadding =
    { bottom = 5
    , left = 5
    , right = 5
    , top = 5
    }


navColorWhite =
    Element.fromRgb255
        { red = 255
        , green = 255
        , blue = 255
        , alpha = 255
        }


main =
    Browser.document
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


fetchBookPageCmd : Title -> Chapter -> Cmd Msg
fetchBookPageCmd title chapter =
    RemoteData.Http.getWithConfig RemoteData.Http.defaultConfig (crossOrigin "http://localhost:8080/books" [ title, String.fromInt chapter ] []) HandleBookPageResponse bookPageDecoder


fetchBookPageCommentaryCmd : Title -> Chapter -> LineNumber -> Cmd Msg
fetchBookPageCommentaryCmd title chapter lineNumber =
    RemoteData.Http.getWithConfig RemoteData.Http.defaultConfig (crossOrigin "http://localhost:8080/books" [ title, String.fromInt chapter, String.fromInt lineNumber ] []) HandleBookPageCommentaryResponse bookPageCommentaryDecoder
