from bs4 import BeautifulSoup
import lxml
import pathlib
from beta_code import beta_code_to_greek
import glob
import re
tlg_author = "0085"
tlg_work = "003"
canonical = f"canonical-greekLit/data/tlg{tlg_author}/tlg{tlg_work}/tlg{tlg_author}.tlg{tlg_work}.perseus-grc2.xml"
diorisis = [i for i in glob.glob("diorisis/*") if re.search(f"diorisis\/.*{tlg_author}.*{tlg_work}.*xml",i)][0]


canonical = pathlib.Path(canonical).read_text()
diorisis = pathlib.Path(diorisis).read_text()
canonical = BeautifulSoup(canonical,"lxml")
diorisis = BeautifulSoup(diorisis,"lxml")

diorisis_sentences = diorisis.find_all("sentence")
diorisis_sentence = diorisis_sentences[0]
diorisis_words = []
for diorisis_sentence in diorisis_sentences:
    words_in_diorisis_sentence = diorisis_sentence.find_all("word")
    for word in words_in_diorisis_sentence:
        w = beta_code_to_greek(word.attrs["form"])
        diorisis_words.append(w)

canonical_words = []
for line_index, canonical_line in enumerate(canonical.find_all("l"),start=1):
    for word_index, canonical_word in enumerate(canonical_line.text.split(" ")):
        if canonical_word == '':
            pass
        else:
            canonical_words.append(canonical_word)

print(len(diorisis_words),len(canonical_words))

canonical_index = 0
for index,_value in enumerate(diorisis_words):
    if _value == "ἔσκενἐπεὶ":
        breakpoint()
    _canonical_word = canonical_words[canonical_index]
    value = _value.strip("'ʼ,.·,;\n—")
    canonical_word = _canonical_word.strip("'ʼ,.·,;\n—")
    if "-" in canonical_word:
        split_word = canonical_word
        canonical_index+=1
        canonical_word = split_word.replace("-","")+ canonical_words[canonical_index].strip("'ʼ.·,;\n—")
    print(value, canonical_word)
    canonical_index+=1
    if value != canonical_word:
        print(value, canonical_word)
        break
