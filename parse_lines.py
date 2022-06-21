import re
from typing import List

single_vowels = [i for i in ("αἀἁἂἃἄἅἆἇὰάᾰᾱᾶᾳᾲᾴᾀᾁᾂᾃᾄᾅᾆᾇᾷ"
                             "ἈἉἊἋἌἍἎἏᾺΆᾸᾹᾼᾈᾉᾊᾋᾌᾍᾎᾏ"
                             "οὀὁὂὃὄὅὸόό"
                             "ὈὉὊὋὌὍῸΌ"
                             "εἐἑἒἓἔἕὲέέ"
                             "ἘἙἚἛἜἝῈΈ"
                             "ηἠἡἢἣἤἥἦἧὴήῆῃῂῄᾐᾑᾒᾓᾔᾕᾖᾗῇή"
                             "ἨἩἪἫἬἭἮἯῊΉῌᾘᾙᾚᾛᾜᾝᾞᾟ"
                             "ιἰἱἲἳἴἵἶἷὶίῐῑῖῒΐῗίΐ"
                             "ἸἹἺἻἼἽἾἿῚΊῘῙ"
                             "ωὠὡὢὣὤὥὦὧὼώῶῳῲῴᾠᾡᾢᾣᾤᾥᾦᾧῷώ"
                             "ὨὩὪὫὬὭὮὯῺΏῼᾨᾩᾪᾫᾬᾭᾮᾯ"
                             "υὑὓὕὗὺύῠῡὐὒὔὖῦῢΰῧύ"
                             "ὙὛὝὟῪΎῨῩ"
                             )]
dipthongs = ["οῦ"]
vowels = "".join(dipthongs + single_vowels)
consonants = "ςΣρΡτΤθΘπΠσδΔφΦγΓξΞκΚλΛζΖχΧψΨβΒνΝμΜῥῬῤ-"
stop_consonants = ""
continuant_consonants = ""


def seperate_duplicate_sounds(input: str):
    """in greek each letter gets its own sound. i.e. Mis-sis-sip-pi not Miss-is-ipp-i"""
    new_line = [input[0]]
    for char in input[1:]:
        if char == new_line[-1]:
            new_line += "- "
        new_line += char
    x = "".join(new_line).split(" ")
    print(x)
    return x


def split_syllables(input: List[str]):
    new_syllables = ""
    for split in input:
        new_syllables += f" {split[0]}"
        #breakpoint()
        for index, value in enumerate(split[1:], start=1):
            if value in consonants:
                if split[index - 1] in vowels: # and split[index + 1] != "-":
                    new_syllables += f" {value}"
                else:
                    new_syllables += value
            else:
                new_syllables += value
            print(new_syllables)
    return new_syllables


if __name__ == "__main__":
    # input = "ἄγετ’, ὦ παῖδες, τὴν γραῦν πρὸ δόμων,"
    input = "ἄνδρα μοι ἔννεπε, μοῦσα, πολύτροπον, ὃς μάλα πολλὰ"
    final = seperate_duplicate_sounds(input)
    split_syllables(final)
