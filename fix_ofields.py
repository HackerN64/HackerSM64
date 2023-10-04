import sys, os, glob

flist = [i.replace("src/game/behaviors/", "").replace(".c","") for i in glob.glob("src/game/behaviors/*.c")]

# print(flist)

fl = []
with open("include/object_constants.h") as f:
    fl = f.readlines()

field_dic = {}
field_dic[""] = []

curr_key = ""
for l in fl:
    if "/* " in l:
        fname = l[:-1].replace("*", "")\
                      .replace("/","")\
                      .replace(" ","_")\
                      .replace("'","")\
                      .lower()[1:-1]
        if fname in flist:
            curr_key = fname
            field_dic[curr_key] = []
            print(fname)
        else:
            curr_key = ""
    elif "define" in l:
        field_dic[curr_key].append(l)

del field_dic[""]

preamble = """
#include <ultra64.h>
#include "global_object_constants.h"
"""

left_files = [i for i in flist if i not in field_dic]

for i in field_dic.keys():
    flines = ""
    with open("src/game/behaviors/%s.c" % i) as f:
        flines = f.readlines()[1:]

    with open("src/game/behaviors/%s.c" % i, "w+") as f:
        f.write(preamble)
        for l in field_dic[i]:
            f.write(l)
        f.write(''.join(fc))
#     print(i)


# print files left
print(left_files)


# for i in left_files:
#     os.system("mv src/game/behaviors/%s.inc.c src/game/behaviors/_%s.c" % (i,i))
