import glob
import json
import re
import sqlite3


def create_scrubbed_lines():
    sql = sqlite3.connect("db.sqlite3")
    try:
        sql.execute("DROP TABLE scrubbedLines")
    except:
        pass
    sql.execute(
        """CREATE VIRTUAL TABLE scrubbedLines USING fts5(
        line, lineNumber UNINDEXED, bookId UNINDEXED, chapter UNINDEXED, subsection UNINDEXED,
        );""")
    sql.close()
    return 1


def add_scrubbed_lines_to_db(chapter, book, subsection):
    file = f"fulltexts/{book}/{book}{chapter:02d}.txt"
    with open(file) as f:
        text = re.sub(pattern=r"(Ἀ|Ἁ|Ἂ|Ἃ|Ἄ|Ἅ|Ἆ|Ἇ|Ὰ|Ά|Ᾰ|Ᾱ|ᾼ|ᾈ|ᾉ|ᾊ|ᾋ|ᾌ|ᾍ|ᾎ|ᾏ)", repl="Α", string=f.read(), count=100_000)
        text = re.sub(pattern=r"(Ἐ|Ἑ|Ἒ|Ἓ|Ἔ|Ἕ|Ὲ|Έ)", repl="Ε", string=text, count=100_000)
        text = re.sub(pattern=r"(Ἠ|Ἡ|Ἢ|Ἣ|Ἤ|Ἥ|Ἦ|Ἧ|Ὴ|Ή|ῌ|ᾘ|ᾙ|ᾚ|ᾛ|ᾜ|ᾝ|ᾞ|ᾟ)", repl="Η", string=text, count=100_000)
        text = re.sub(pattern=r"(Ἰ|Ἱ|Ἲ|Ἳ|Ἴ|Ἵ|Ἶ|Ἷ|Ὶ|Ί|Ῐ|Ῑ)", repl="Ι", string=text, count=100_000)
        text = re.sub(pattern=r"(Ὑ|Ὓ|Ὕ|Ὗ|Ὺ|Ύ|Ῠ|Ῡ)", repl="Υ", string=text, count=100_000)
        text = re.sub(pattern=r"(ἀ|ἁ|ἂ|ἃ|ἄ|ἅ|ἆ|ἇ|ὰ|ά|ᾰ|ᾱ|ᾶ|ᾳ|ᾲ|ᾴ|ᾀ|ᾁ|ᾂ|ᾃ|ᾄ|ᾅ|ᾆ|ᾇ|ᾷ|ά)", repl="α", string=text,
                      count=100_000)
        text = re.sub(pattern=r"(ἐ|ἑ|ἒ|ἓ|ἔ|ἕ|ὲ|έ|έ)", repl="ε", string=text, count=100_000)
        text = re.sub(pattern=r"(ἠ|ἡ|ἢ|ἣ|ἤ|ἥ|ἦ|ἧ|ὴ|ή|ῆ|ῃ|ῂ|ῄ|ᾐ|ᾑ|ᾒ|ᾓ|ᾔ|ᾕ|ᾖ|ᾗ|ῇ|ή)", repl="η", string=text,
                      count=100_000)
        text = re.sub(pattern=r"(ἰ|ἱ|ἲ|ἳ|ἴ|ἵ|ἶ|ἷ|ὶ|ί|ῐ|ῑ|ῖ|ῒ|ΐ|ῗ|ί|ΐ)", repl="ι", string=text, count=100_000)
        text = re.sub(pattern=r"(ὀ|ὁ|ὂ|ὃ|ὄ|ὅ|ὸ|ό|ό)", repl="ο", string=text, count=100_000)
        text = re.sub(pattern=r"(ὑ|ὓ|ὕ|ὗ|ὺ|ύ|ῠ|ῡ|ὐ|ὒ|ὔ|ὖ|ῦ|ῢ|ΰ|ῧ|ύ)", repl="υ", string=text, count=100_000)
        text = re.sub(pattern=r"(ὠ|ὡ|ὢ|ὣ|ὤ|ὥ|ὦ|ὧ|ὼ|ώ|ῶ|ῳ|ῲ|ῴ|ᾠ|ᾡ|ᾢ|ᾣ|ᾤ|ᾥ|ᾦ|ᾧ|ῷ|ώ)", repl="ω", string=text,
                      count=100_000)
        #ὨὩὪὫὬὭὮὯῺΏῼᾨᾩᾪᾫᾬᾭᾮᾯ
        #ὈὉὊὋὌὍῸΌ
        #Ῥ
        text = re.sub(pattern=r'\n’', repl='’\n', string=text, count=100_000)
        text = text.split("\n")
        text = [t.strip("0123456789") for t in text]
        text = [i for i in text if i not in ["\n", "", '’', '’ ’', "’’", "‘"]]

    for index, line in enumerate(text, start=1):
        with sqlite3.connect("db.sqlite3") as conn:
            book_id = conn.execute("SELECT id FROM books WHERE title=?", (book,)).fetchone()[0]
            conn.execute(
                "INSERT INTO scrubbedLines VALUES (?,?,?,?,?)", (line, index, book_id, chapter, subsection))
    return 1


def create_full_lines():
    sql = sqlite3.connect("db.sqlite3")
    try:
        sql.execute("DROP TABLE fullLines")
    except:
        pass
    sql.execute("""
            CREATE TABLE fullLines(
            line TEXT NOT NULL,
            lineNumber INT NOT NULL,
            bookId INT NOT NULL,
            chapter INT NOT NULL,
            subsection INT,
            FOREIGN KEY (bookId) REFERENCES books(id)
            );""")
    sql.execute("CREATE UNIQUE INDEX textLocation on fullLines(bookId,chapter,lineNumber)")
    sql.close()
    return 1


def add_full_lines_to_db(chapter, book, subsection):
    file = f"fulltexts/{book}/{book}{chapter:02d}.txt"
    with open(file) as f:
        text = re.sub(pattern=r'\n’', repl='’\n', string=f.read(), count=100_000)
        text = text.split("\n")
        text = [t.strip("0123456789") for t in text]
        text = [i for i in text if i not in ["\n", "", '’', '’ ’', "’’", "‘"]]

    for index, line in enumerate(text, start=1):
        with sqlite3.connect("db.sqlite3") as conn:
            book_id = conn.execute("SELECT id FROM books WHERE title=?", (book,)).fetchone()[0]
            res = conn.execute(
                "INSERT INTO fullLines VALUES (?,?,?,?,?)", (line, index, book_id, chapter, subsection)
            )
    return 1


def create_authors():
    sql = sqlite3.connect("db.sqlite3")
    try:
        sql.execute("DROP TABLE authors")
    except:
        pass
    sql.execute("CREATE TABLE authors (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT UNIQUE NOT NULL);")
    sql.close()
    return 1


def create_books():
    sql = sqlite3.connect("db.sqlite3")
    try:
        sql.execute("DROP TABLE books")
    except:
        pass
    sql.execute(
        "CREATE TABLE books(id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT, authorId INTEGER, FOREIGN KEY (authorId) REFERENCES author(id));")
    sql.close()
    return 1

def create_commentary():
    with sqlite3.connect("db.sqlite3") as sql:
        try:
            sql.execute("DROP TABLE commentary")
        except:
            pass
        sql.execute(
            """CREATE TABLE commentary(
            text TEXT,
            commentaryAuthorId INT,
            source TEXT,
            bookId INT,
            chapter INT,
            lineNumber INT,
            FOREIGN KEY (bookId) REFERENCES books(id)
            );""")
        from fulltexts.commentary.commentary import commentary
        for i in commentary:
            for line in i["lineNumber"]:
                confirm = sql.execute("""INSERT INTO commentary(text,commentaryAuthorId,source,bookId,chapter,lineNumber)
                VALUES(?,?,?,?,?,?)""",
                (i["text"],i["commentaryAuthorId"],i["source"],i["bookId"],i["chapter"],line)).lastrowid
                if confirm is None:
                    print(f"Error adding commentary {i['source']}, {i['bookId']}, {i['chapter']}, {line}")

def main(metadata):
    create_authors()
    create_books()
    create_full_lines()
    create_scrubbed_lines()
    create_commentary()
    for i in metadata:
        # Insert Author
        try:
            with sqlite3.connect("db.sqlite3") as conn:
                conn.execute("INSERT INTO authors(name) VALUES (?)", (i["author"],))
        except sqlite3.IntegrityError:
            pass
        # Insert Lines
        with sqlite3.connect("db.sqlite3") as conn:
            _, author_id = conn.execute("SELECT name,id FROM authors WHERE name=?", (i["author"],)).fetchone()
            conn.execute("INSERT INTO books(title,authorId) VALUES (?,?)", (i["book"], author_id))
        for j in glob.glob(f'{i["path"]}*'):
            print(j)
            chapter = int(j.replace(i["path"], "").replace(i["book"], "").replace(".txt", ""))
            add_scrubbed_lines_to_db(chapter, i["book"], None)
            add_full_lines_to_db(chapter, i["book"], None)

def create_dioris(force:bool = False):
    """create the database to hold dioris info"""
    with sqlite3.connect("db.sqlite3") as sql:
        if force:
            try:
                sql.execute("DROP TABLE dioris")
            except:
                pass
        sql.execute(
            """CREATE TABLE dioris_works(
            title TEXT,
            author TEXT,
            tlgAuthor TEXT,
            tlgId TEXT,
            funder TEXT,
            );""")
        sql.execute("""
        CREATE TABLE dioris_lemmas 
        
        """)

def create_tlg(force:bool = False):
    """create the database to hold tlg lines"""
    with sqlite3.connect("db.sqlite3") as sql:
        if force:
            try:
                sql.execute("DROP TABLE tlg_lines")
            except:
                pass
        sql.execute(
            """CREATE TABLE tlg_lines(
            speaker TEXT,
            text TEXT,
            line_number INT,
            bracketed INT,
            sub_textpart TEXT
            textpart TEXT
            tag TEXT)
            """)

if __name__ == "__main__":
    # metadata = [
    #      {"author": "homer", "book": "iliad", "path": "fulltexts/iliad/"},
    #      {"author": "homer", "book": "odyssey", "path": "fulltexts/odyssey/"}
    #  ]
    # main(metadata)
    create_tlg()
