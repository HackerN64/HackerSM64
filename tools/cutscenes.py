import sys, os

cdict = {}

import re

funcdict = {}

def cname(sym):
    words = re.findall('[A-Z][^A-Z]*', sym)[1:]
    nm = "_".join([i.lower() for i in words])
    return nm

with open(sys.argv[1]) as f:
    buf = ""
    inFunc = False
    nm = ""
    for line in f:
        if "/**" in line[:-1]:
            inFunc = True
            buf += line
        elif inFunc:
            buf += line
            if "struct Cutscene " in line:
                t = line.replace("[]", "").split()[2]
                nm = cname(t)
            if line == "};\n":
                with open(f"src/game/cutscenes/{nm}.c", 'w+') as f:
                    f.write(buf)
                nm = ""
                buf = ""
                inFunc = False

import glob
files = glob.glob("src/game/cutscenes/*")




