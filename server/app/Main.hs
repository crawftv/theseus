{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Database.SQLite.Simple
import Network.Wai.Handler.Warp
import Servant
import Network.Wai.Middleware.Cors
import GHC.Generics
import Data.Aeson
import Data.Text

data BookPage = BookPage {
                title :: Text
                , linesOfText :: ZipListLine
                , chapter :: Int
                , allBooks :: [Book]
                } deriving ( Generic)

instance ToJSON BookPage

data BookPageCommentary = BookPageCommentary {
  bpcTitle :: Text,
  bpcLinesOfText :: ZipListLine,
  bpcChapter :: Int,
  bpcAllBooks :: [Book],
  bpcCommentary :: [Commentary],
  bpcLineNumber :: Int
  } deriving (Generic)

instance ToJSON BookPageCommentary

data Line = Line {
     lineText :: Text
    ,   lineLineNumber :: Int
    } deriving (Show, Generic)
instance FromRow Line where
  fromRow = Line <$> field <*> field
instance ToJSON Line

data ZipListLine = ZipListLine {
 p1 :: [Line]
 , p2 :: [Line]
 , p3 :: [Line]
 } deriving (Show,Generic)
instance ToJSON ZipListLine

data Book = Book{
  bookTitle :: Text
  , bookId :: Int
  , numberOfChapters :: Int
  } deriving (Show,Generic)

instance FromRow Book where
  fromRow = Book <$> field <*> field <*> field

instance ToJSON Book

data Commentary = Commentary {
  commentaryText :: Text
  ,commentaryAuthorId :: Text
  , commentarySource :: Text
    } deriving (Generic)
instance ToJSON Commentary
instance FromRow Commentary where
  fromRow = Commentary <$> field <*>field<*>field -- <*>field<*>field<*>field


type API =  Get '[JSON] [Book]
            :<|> "books" :>  Capture "title" Text :> Capture "chapter" Int :> Get '[JSON] BookPage
            :<|> "books" :> Capture "title" Text :> Capture "chapter" Int :> Capture "lineNumber" Int :> Get '[JSON] BookPageCommentary

allBooksQuery :: Query
allBooksQuery = "SELECT title, bookId, Count(DISTINCT chapter) FROM fullLines Inner JOIN books on books.id=fullLines.bookId GROUP BY bookId"
queryAllBooks :: FilePath -> IO [Book]
queryAllBooks dbfile = withConnection dbfile $ \conn -> query_ conn allBooksQuery

bookChapterCommentaryQuery :: Query
bookChapterCommentaryQuery = "SELECT text, commentaryAuthorId, source FROM commentary INNER JOIN books ON books.id = commentary.bookId WHERE lineNumber = :lineNumber AND books.title = :title AND chapter = :chapter"
queryBookChapterCommentary :: Text -> Int-> Int ->FilePath-> IO [Commentary]
queryBookChapterCommentary title chapter lineNumber dbfile = withConnection dbfile $ \conn -> queryNamed conn bookChapterCommentaryQuery [":title" := title, ":chapter" := chapter, ":lineNumber" := lineNumber]

zipListLineQuery1 :: Query
zipListLineQuery1 = "Select line,lineNumber FROM fullLines INNER JOIN books ON fullLines.bookId = books.id WHERE books.title = :title AND fullLines.chapter = :chapter AND lineNumber < :lineNumber ORDER BY lineNumber ASC"
zipListLineQuery2 :: Query
zipListLineQuery2 = "Select line,lineNumber FROM fullLines INNER JOIN books ON fullLines.bookId = books.id WHERE books.title = :title AND fullLines.chapter = :chapter AND lineNumber = :lineNumber ORDER BY lineNumber ASC"
zipListLineQuery3 :: Query
zipListLineQuery3 = "Select line,lineNumber FROM fullLines INNER JOIN books ON fullLines.bookId = books.id WHERE books.title = :title AND fullLines.chapter = :chapter AND lineNumber > :lineNumber ORDER BY lineNumber ASC"
zipListLineQuery :: Text -> Int-> Int -> FilePath -> IO ZipListLine
zipListLineQuery title chapter lineNumber dbfile = do
  p1 <- withConnection dbfile $ \conn -> queryNamed conn zipListLineQuery1 [":title" := title, ":chapter" := chapter, ":lineNumber" := lineNumber]
  p2 <- withConnection dbfile $ \conn -> queryNamed conn zipListLineQuery2 [":title" := title, ":chapter" := chapter, ":lineNumber" := lineNumber]
  p3 <- withConnection dbfile $ \conn -> queryNamed conn zipListLineQuery3 [":title" := title, ":chapter" := chapter, ":lineNumber" := lineNumber]
  return ZipListLine {p1=p1,p2=p2,p3=p3}

bookChapterQuery :: Query
bookChapterQuery = "Select line,lineNumber FROM fullLines INNER JOIN books ON fullLines.bookId = books.id WHERE books.title = :book AND fullLines.chapter = :chapter ORDER BY lineNumber ASC"
queryLines :: Text-> Int -> FilePath ->IO [Line]
queryLines title chapter dbfile= withConnection dbfile $ \conn -> queryNamed conn bookChapterQuery [":book" :=title,":chapter":=chapter ]

api :: Proxy API
api = Proxy

server dbfile = listAllBooks
   :<|> getBookPageWithChapter
   :<|> getBookPageWithChapterWithCommentary

 where
       listAllBooks :: Handler [Book]
       listAllBooks = liftIO (queryAllBooks dbfile)

       getBookPageWithChapter :: Text -> Int -> Handler BookPage
       getBookPageWithChapter title chapter = do
         lines <- liftIO (zipListLineQuery title chapter 1 dbfile)
         allBooks <- liftIO (queryAllBooks dbfile)
         return BookPage {title=title,chapter=chapter,linesOfText=lines, allBooks= allBooks }

       getBookPageWithChapterWithCommentary :: Text -> Int -> Int -> Handler BookPageCommentary
       getBookPageWithChapterWithCommentary title chapter lineNumber = do
         lines <- liftIO (zipListLineQuery title chapter lineNumber dbfile)
         allBooks <- liftIO (queryAllBooks dbfile)
         commentary <- liftIO (queryBookChapterCommentary title chapter lineNumber dbfile)
         return BookPageCommentary {bpcTitle=title, bpcChapter=chapter, bpcLinesOfText=lines,bpcAllBooks=allBooks,bpcCommentary =commentary,bpcLineNumber=lineNumber}




runApp :: FilePath -> IO ()
runApp dbfile = run 3000  (simpleCors $ (serve api $ server dbfile))


main :: IO ()
main = do
  -- you could read this from some configuration file,
  -- environment variable or somewhere else instead.
  let dbfile = "../db.sqlite3"
  runApp dbfile
