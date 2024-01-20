import sys
import regex as re

fb = ""

with open(sys.argv[1], "r") as f:
    fb = f.read()

def lookahead(l):
    for i, x in enumerate(l):
        if x == "}":
            return i + 1


fl = fb.split("\n")
for i, line in enumerate(fl):
    if "(void)" in line and "{" in line:
        # print()
        # print("FUNC")
        # print("\n".join(fl[i:i + lookahead(fl[i:])]))
        # print(re.findall("o", "\n".join(fl[i:i + lookahead(fl[i:])])))
        if re.search("o", "\n".join(fl[i:i + lookahead(fl[i:])])):
            print(line.replace("(void)", "(struct Object *o)"))
            continue

    print(line)

