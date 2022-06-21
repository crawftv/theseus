import xml.etree.ElementTree as ET
import re
from typing import Union, Literal
import time
import tqdm



class Line:
    # TODO gap reason
    def __init__(self, *, speaker, text, line_number, bracketed=False, textpart=None, sub_textpart=None, tag):
        self.speaker :str = speaker
        self.text :str = text
        self.bracketed :bool = bracketed
        self.textpart: Literal["Choral", "Episode"] = textpart  # episode, choral.
        self.sub_textpart :str = sub_textpart  # strophe/antistrophe, etc.
        try:
            self.line_number :int = int(line_number)
        except (ValueError, TypeError):
            self.alternate_marker :str = next(iter(re.findall(r"[a-z]", line_number or "")))
            self.line_number :int = int(next(iter(re.split(r"[a-z]", line_number or ""))))

    def __repr__(self) -> str:
        return f"{self.line_number:>5}: " + f"{'T' if self.bracketed else ' '}: " + f"{self.speaker:>20}: {self.text:>55}: " + f"{self.textpart:>20}: {self.sub_textpart}"


class TLGGreekText:
    """Class for holding TLg Greek Text metadata and text"""

    def parse_tag(self, tag, tag_name) -> bool:
        return tag.tag == "{http://www.tei-c.org/ns/1.0}"+ f"{tag_name}"
    def __init__(self, xml_path: str):
        self.lines = []
        textpart = None
        sub_textpart = ""
        speaker = None
        with open(xml_path) as file:
            et = ET.parse(file).iter()

        for tag in et:
            if self.parse_tag(tag, "title"):
                self.title = tag.text
            elif self.parse_tag(tag, "author"):
                self.author = tag.text
            elif self.parse_tag(tag, "editor"):
                self.editor = tag.text
            elif self.parse_tag(tag, "div"):
                t = tag.get(key="subtype",default="")
                if t == "choral":
                    textpart = "Choral"
                    sub_textpart = ""
                elif t == "episode":
                    sub_textpart = ""
                    textpart = "Episode"
                else:
                    sub_textpart = t
            elif self.parse_tag(tag, "speaker"):
                speaker = tag.text
            elif self.parse_tag(tag, "l"):
                line_number = tag.get("n")
                if tag.text is not None:
                    self.lines.append(
                        Line(
                            speaker=speaker,
                            text=tag.text,
                            line_number=line_number,
                            sub_textpart=sub_textpart,
                            textpart=textpart,
                            tag = tag
                        )
                    )
            elif self.parse_tag(tag, "del"):
                self.lines.append(
                    Line(
                        speaker= speaker,
                        text=tag.text,
                        line_number=line_number,
                        bracketed=True,
                        sub_textpart=sub_textpart,
                        textpart=textpart,
                        tag = tag
                    ))


import glob
files = sorted([ i for i in glob.glob("canonical-greekLit/data/*/*/*.xml")
          if not i.endswith("__cts__.xml")
          and  "grc" in i])
for file in tqdm.tqdm(files):
    print(file)
    x = TLGGreekText(xml_path=file)
    x.lines
