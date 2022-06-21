import xml.etree.ElementTree as ET
import re
from typing import Union, Literal
import time
import sqlite3

class DiorisisText:
    """Class for holding TLg Greek Text metadata and text"""

    def parse_tag(self, tag, tag_name) -> bool:
        return tag.tag == "{http://www.tei-c.org/ns/1.0}"+ f"{tag_name}"
    def __init__(self, xml_path: str):
        self.lines = []
        with open(xml_path) as file:
            et = ET.parse(file)
            root = et.getroot()
            info = list(list(list(root)[0])[0])[0]
            info = { i.tag: i.text for i in info.getchildren() }
            with sqlite3.connect("db.sqlite3") as conn:
                conn.execute("")
            body = list(list(list(root)[1])[0])
            for sentence in body:
                line_number = sentence.get("id")
                words = list(sentence)
                for word in words:
                    if word == "word":
                        word_id =  word.get("id")
                        word_form = word.get("form")
                        lemma = list(word)[0]
                        lemma_id = lemma.get("id")
                        lemma_entry = lemma.get("entry")
                        lemma_POS = lemma.get("POS")
                        lemma_disambiguated = lemma.get("disambiguated")
                        lemma_TreeTagger= lemma.get("TreeTagger")
                        analysis = lemma.getchildren()[0]
                        analysis_morph = analysis.get("morph")



x = DiorisisText(xml_path="diorisis/Euripides (0006) - Heracles (009).xml")
for i in x.lines:
    print(i)
