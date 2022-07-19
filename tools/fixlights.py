#!/usr/bin/python3

import sys, subprocess

if len(sys.argv) != 2:
    print(f"Usage: {sys.argv[0]} [folder to search]")
    sys.exit()

lightFiles = set()

# The original command that was run across the entire repo to convert all assets
# cmds = ("grep -Rl \"gsSPSetLight\" levels actors bin","grep -Rl \"gsSPLight\" levels actors bin","grep -Rl \"Lights1\" levels actors bin")

# Operate on the folder passed as an argument to this program
cmds = [f"grep -Rl \"gsSPSetLight\" {sys.argv[1]}",f"grep -Rl \"gsSPLight\" {sys.argv[1]}",f"grep -Rl \"Lights1\" {sys.argv[1]}"]

for cmd in cmds:
    try:
        output = subprocess.check_output(cmd, shell=True).decode('UTF-8').split("\n")[:-1] 
        lightFiles = set.union(lightFiles,set(output))
    except subprocess.CalledProcessError:
        continue

lightStructs = {}
# FIRST PASS: COLLECT LIGHT STRUCT INFO, REMOVE LIGHT STRUCTS   
for file in lightFiles:
    changed = False
    with open(file, "r") as f:
        fileLines = f.readlines()
        index = 0
        while index < len(fileLines):
            curLine = fileLines[index].strip()
            
            if "const Lights1" in curLine and "gdSPDefLights1" in curLine:
                structName = curLine.split("const Lights1")[1].strip().split(" ")[0] # Get string between Lights 1 and following space
                del fileLines[index] # Remove beginning
                args = ""
                while not ")" in fileLines[index]:
                    args += fileLines[index].strip()
                    del fileLines[index]
                del fileLines[index] # Remove ending line
                args = args.strip(");")
                args = [int(arg.strip()[2:],16) for arg in args.split(",")]
                lightStructs[structName] = args
                changed = True
            elif "Lights1" in curLine and "gdSPDefLights1" in curLine:
                structName = curLine.split("Lights1")[1].strip().split(" ")[0] # Get string between Lights 1 and following space
                del fileLines[index] # Remove beginning
                args = ""
                while True:
                    args += fileLines[index].strip()
                    if ")" in fileLines[index]:
                        break
                    del fileLines[index]
                del fileLines[index] # Remove ending line
                args = args.strip(");")
                args = [int(arg.strip()[2:],16) for arg in args.split(",")]
                lightStructs[structName] = args
                changed = True

            index += 1
    # Don't write to the file if the contents haven't been changed, as this will trigger a rebuild by Make
    if changed:
        with open(file, "w") as f:
            f.writelines(fileLines)
    
# SECOND PASS - CHANGE LIGHT COMMANDS
for file in lightFiles:
    changed = False
    with open(file, "r") as f:
        fileLines = f.readlines()
        index = 0
        while index < len(fileLines):
            curLine = fileLines[index].strip()

            if curLine.startswith("gsSPSetLights1("):
                structName = curLine.split("(")[1].split(")")[0] # Get string between ( and )
                if structName in lightStructs.keys():
                    changed = True
                    args = lightStructs[structName]
                    light1Args = args[3]*0x1000000 + args[4]*0x10000 + args[5]*0x100 + 0xFF
                    light2Args = args[0]*0x1000000 + args[1]*0x10000 + args[2]*0x100 + 0xFF
                    fileLines[index] = "    gsSPLightColor(LIGHT_1, 0x%x),\n" % light1Args
                    fileLines.insert(index+1,"    gsSPLightColor(LIGHT_2, 0x%x),\n" % light2Args)
                    index += 1
                else:
                    print("Missing light definition %s in file %s. Skipping..." % (structName, file))
                
            elif curLine.startswith("gsSPLight("):
                if not "&" in curLine or not "." in curLine:
                    print("Malformed gsSPLight command in file %s, line %d. Skipping..." % (file, index+1))
                    index += 1
                    continue
                structName = curLine.split("&")[1].split(".")[0] # Get string between & and . (&lightname.l or &lightname.a)
                if structName in lightStructs.keys():
                    changed = True
                    args = lightStructs[structName]
                    lightNum = int(curLine.split(",")[1].strip()[0]) # , 1),
                    argIndex = (3 if lightNum == 1 else 0)
                    lightArgs = args[argIndex]*0x1000000 + args[argIndex+1]*0x10000 + args[argIndex+2]*0x100 + 0xFF
                    fileLines[index] = "    gsSPLightColor(LIGHT_%d, 0x%x),\n" % (lightNum, lightArgs)
                else:
                    print("Missing light definition %s in file %s, line %d. Skipping..." % (structName, file, index+1))
                
            index += 1
    # Don't write to the file if the contents haven't been changed, as this will trigger a rebuild by Make
    if changed:
        with open(file, "w") as f:
            f.writelines(fileLines)

