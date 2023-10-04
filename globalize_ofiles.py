import sys, os, glob

preamble = """#include <ultra64.h>
#include "global_object_fields.h"
#include "object_helpers.h"

"""
os.system("mv src/game/behaviors/_%s.c src/game/behaviors/%s.c" % (
    sys.argv[1], sys.argv[1]))