import os

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from typing import List, Optional
import sqlite3
from dataclasses import dataclass
import contextlib
import libsql_experimental as libsql

from starlette.responses import HTMLResponse
from starlette.staticfiles import StaticFiles


# Pydantic models for request/response
class Healthz(BaseModel):
    success: bool


class Line(BaseModel):
    lineText: str
    lineLineNumber: int


class ZipListLine(BaseModel):
    p1: List[Line]
    p2: List[Line]
    p3: List[Line]


class Book(BaseModel):
    bookTitle: str
    bookId: int
    numberOfChapters: int


class Commentary(BaseModel):
    commentaryText: str
    commentaryAuthorId: str
    commentarySource: str


class BookPage(BaseModel):
    title: str
    linesOfText: ZipListLine
    chapter: int
    allBooks: List[Book]


class BookPageCommentary(BaseModel):
    bpcTitle: str
    bpcLinesOfText: ZipListLine
    bpcChapter: int
    bpcAllBooks: List[Book]
    bpcCommentary: List[Commentary]
    bpcLineNumber: int


# Database helper
@contextlib.contextmanager
def get_db_connection():
    conn = libsql.connect(
        "theseus",
        sync_url=os.getenv("TURSO_THESEUS_URL"),
        auth_token=os.getenv("TURSO_THESEUS_TOKEN")
    )
    conn.sync()
    conn.row_factory = sqlite3.Row
    try:
        yield conn
    finally:
        conn.close()


# Database queries
def query_all_books(db_path: str) -> List[Book]:
    with get_db_connection() as conn:
        cursor = conn.execute("""
            SELECT title, bookId, Count(DISTINCT chapter) 
            FROM fullLines 
            Inner JOIN books on books.id=fullLines.bookId 
            GROUP BY bookId
        """)
        return [Book(bookTitle=row['title'],
                     bookId=row['bookId'],
                     numberOfChapters=row['Count(DISTINCT chapter)'])
                for row in cursor.fetchall()]


def query_book_chapter_commentary(title: str, chapter: int, line_number: int, db_path: str) -> List[Commentary]:
    with get_db_connection() as conn:
        cursor = conn.execute("""
            SELECT text, commentaryAuthorId, source 
            FROM commentary 
            INNER JOIN books ON books.id = commentary.bookId 
            WHERE lineNumber = ? AND books.title = ? AND chapter = ?
        """, (line_number, title, chapter))
        return [Commentary(commentaryText=row['text'],
                           commentaryAuthorId=row['commentaryAuthorId'],
                           commentarySource=row['source'])
                for row in cursor.fetchall()]


def query_zip_list_line(title: str, chapter: int, line_number: int, db_path: str) -> ZipListLine:
    with get_db_connection() as conn:
        # Previous lines
        cursor = conn.execute("""
            Select line, lineNumber 
            FROM fullLines 
            INNER JOIN books ON fullLines.bookId = books.id 
            WHERE books.title = ? AND fullLines.chapter = ? AND lineNumber < ? 
            ORDER BY lineNumber ASC
        """, (title, chapter, line_number))
        p1 = [Line(lineText=row['line'], lineLineNumber=row['lineNumber'])
              for row in cursor.fetchall()]

        # Current line
        cursor = conn.execute("""
            Select line, lineNumber 
            FROM fullLines 
            INNER JOIN books ON fullLines.bookId = books.id 
            WHERE books.title = ? AND fullLines.chapter = ? AND lineNumber = ? 
            ORDER BY lineNumber ASC
        """, (title, chapter, line_number))
        p2 = [Line(lineText=row['line'], lineLineNumber=row['lineNumber'])
              for row in cursor.fetchall()]

        # Next lines
        cursor = conn.execute("""
            Select line, lineNumber 
            FROM fullLines 
            INNER JOIN books ON fullLines.bookId = books.id 
            WHERE books.title = ? AND fullLines.chapter = ? AND lineNumber > ? 
            ORDER BY lineNumber ASC
        """, (title, chapter, line_number))
        p3 = [Line(lineText=row['line'], lineLineNumber=row['lineNumber'])
              for row in cursor.fetchall()]

        return ZipListLine(p1=p1, p2=p2, p3=p3)


# FastAPI application
app = FastAPI()

# Database path configuration
DB_PATH = "../db.sqlite3"

# Add CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Mount the static directory
app.mount("/static", StaticFiles(directory="static"), name="static")


# Modified root route to serve HTML
@app.get("/", response_class=HTMLResponse)
async def root():
    try:
        with open("static/index.html", "r") as f:
            return HTMLResponse(content=f.read())
    except FileNotFoundError as e:
        raise HTTPException(status_code=404, detail="HTML file not found") from e


@app.get("/books/{title}/{chapter}", response_model=BookPage)
async def get_book_page_with_chapter(title: str, chapter: int):
    lines = query_zip_list_line(title, chapter, 1, DB_PATH)
    all_books = query_all_books(DB_PATH)
    return BookPage(
        title=title,
        chapter=chapter,
        linesOfText=lines,
        allBooks=all_books
    )


@app.get("/books/{title}/{chapter}/{line_number}", response_model=BookPageCommentary)
async def get_book_page_with_chapter_with_commentary(title: str, chapter: int, line_number: int):
    lines = query_zip_list_line(title, chapter, line_number, DB_PATH)
    all_books = query_all_books(DB_PATH)
    commentary = query_book_chapter_commentary(title, chapter, line_number, DB_PATH)
    return BookPageCommentary(
        bpcTitle=title,
        bpcChapter=chapter,
        bpcLinesOfText=lines,
        bpcAllBooks=all_books,
        bpcCommentary=commentary,
        bpcLineNumber=line_number
    )


@app.get("/healthz", response_model=Healthz)
async def healthz():
    return Healthz(success=True)


if __name__ == "__main__":
    import uvicorn

    uvicorn.run(app, host="0.0.0.0", port=3000)
