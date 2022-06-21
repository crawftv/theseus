import re
import sqlite3


def parse(string):
    # parse out boilerplate
    pattern = r"(?P<a>\d+)(?P<b>#urn:cite2:hmt:lsj\.markdown:n)(?P<c>\d+)#(?P<d>.*)(#|#\*)\s(?P<e>.*)"
    match = re.match(pattern, string).groupdict()
    leftover = match["e"]
    main_entry = match["d"]

    entry_parsers = [
        ("standard_entry", "(?P<entry>[^a-zA-Z`*=]{2,})(?P<extra>.*)"),
        ("semicolon_entry", "(?P<entry>^[^a-zA-Z]+:[^a-zA-Z]+)(?P<extra>.*)"),
        ("dashed_entry", "(?P<entry>(^[^a-zA-Z]+-)(, | +))(?P<extra>.*)"),
        ("multiple_entry", "(?P<entry>^[^a-zA-Z]{2,})\([a-zA-Z\s.]+\),([^a-zA-Z`(]+)(?P<extra>.*)"),
        # multiple entries are (A) (B)
        ("complex_paren", "(?P<entry>^[^a-zA-Z`]{2,})(?P<extra>\([\W\w *., \d]+\)(.*))"),
        ("unmatched_paren", "(?P<entry>^[^a-zA-Z`]{2,})(?P<extra>\(.*)"),
    ]

    for name, expression in entry_parsers:
        if match := re.match(expression, leftover):
            match = match.groupdict()
            entry = match["entry"]
            extra = match["extra"]
            break
    if not match:
        entry = main_entry
        extra = leftover
    # definition
    definition = "(?P<definition>\*\*[a-zA-Z ,\:\.\-\'!]+\*\*)"
    if match := re.findall(definition, extra):
        definitions = ", ".join([i.replace("**", "") for i in match])
    else:
        definitions = None
    # part of speech
    patterns = [
        ("noun", "((.*), (((εως)|(εος)|(.δος)|(ου)|(ης)|(ατος)), (((ὁ)|(ἡ)|(τό)))))"),
        ("verb", "((^[^a-zA-Z,]+ω,))|((^[^a-zA-Z,]+σθαι)|(^[^a-zA-Z,]+ομαι))")
    ]
    part_of_speech = None
    for name, expression in patterns:
        if match := re.match(expression, entry):
            part_of_speech = name

    return main_entry, entry, definitions, leftover, part_of_speech


def clean_text(text):
    text = re.sub(pattern=r"(Ἀ|Ἁ|Ἂ|Ἃ|Ἄ|Ἅ|Ἆ|Ἇ|Ὰ|Ά|Ᾰ|Ᾱ|ᾼ|ᾈ|ᾉ|ᾊ|ᾋ|ᾌ|ᾍ|ᾎ|ᾏ)", repl="Α", string=text, count=100_000)
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
    text = text.strip("0123456789")
    return text


if __name__ == "__main__":
    with sqlite3.connect("db.sqlite3") as conn:
        conn.execute(
            "DROP TABLE simpleLsj "
        )
    with sqlite3.connect("db.sqlite3") as conn:
        conn.execute("CREATE TABLE simpleLsj ("
                     "mainEntry TEXT NOT NULL,"
                     "secondEntry TEXT NOT NULL,"
                     "definitions TEXT, leftover TEXT NOT NULL,"
                     "scrubbedEntry TEXT NOT NULL, "
                     "partOfSpeech TEXT"
                     ")"
                     )
    with open("lsj_perseus.cex", "r") as f:
        lines = f.readlines()
    with sqlite3.connect("db.sqlite3") as conn:
        for line in lines:
            main_entry, entry, definitions, leftover, part_of_speech = parse(line)
            conn.execute(
                "INSERT INTO simpleLSJ VALUES (?,?,?,?,?,?)",
                (main_entry,
                 entry,
                 definitions,
                 leftover,
                 clean_text(main_entry),
                 part_of_speech,
                 )
            )
