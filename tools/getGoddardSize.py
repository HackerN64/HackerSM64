import os
import sys

preliminaryMapPath: str = sys.argv[1]
buildFolder: str = sys.argv[2]

with open(preliminaryMapPath) as mapFile:
    line: str
    for line in mapFile:
        if "GODDARD_SIZE" in line:
            tokens: list[str] = line.split()
            with open(f"{buildFolder}/goddard.txt", "w+") as outputFile:
                sz: int = int(tokens[0], 16)
                sz += 16
                sz &= 0xFFFFFFF0
                outputFile.write(f"GODDARD_SIZE = 0x{sz:X};\n")
                sys.exit(0)
