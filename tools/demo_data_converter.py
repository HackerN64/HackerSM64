#!/usr/bin/env python3
import sys
import os
import glob

def usage():
    print(f"Usage: {sys.argv[0]} path/to/demo/folder/")

def main():
    if len(sys.argv) != 2:
        usage()
        sys.exit(1)

    demo_folder = sys.argv[1]
    demo_files = glob.glob(f"{demo_folder}/*.s")
    available_levels = [os.path.basename(i).split(".")[0] for i in demo_files]
    
    # Get available levels
    level_list = []
    stub_counter = 0
    with open("levels/level_defines.h") as levelfile:
        for line in levelfile:
            if line.startswith("DEFINE_LEVEL("):
                level_list.append(line.split(",")[3].strip())
            elif line.startswith("STUB_LEVEL("):
                level_list.append(f"stub_{stub_counter}")
                stub_counter += 1

    # Check that demo files actually correspond to a level
    for level, filename in zip(available_levels, demo_files):
        if level not in level_list:
            print(f"Unknown Demo at {filename} - '{level}' is not a level name", file=sys.stderr)
            sys.exit(1)

    print('#include <PR/os_cont.h>')
    print('#include "macros.inc"')
    print('#include "demo_macros.inc"')
    print()

    print(".section .data")
    print("glabel demoFile")

    for level in level_list:
        if level in available_levels:
            print(f".word demo_{level}_start, demo_{level}_end")
        else:
            print(f".word 0, 0")

    print("glabel demoFileEnd")
    print()

    # start actual data counting
    for file, name in zip(demo_files, available_levels):
        print(f"glabel demo_{name}_start")
        print(f'#include "{file}"')
        print(f"glabel demo_{name}_end")



if __name__ == "__main__":
    main()
