import subprocess, glob

flist = glob.glob("src/game/behaviors/*.c")


goodfiles = []
badfiles = []
warnfiles = []
for i in flist:
    cmd = ["mips64-elf-gcc", "-c", "-I.", "-Isrc", "-Iinclude", "-Iinclude/n64", "-o/tmp/out.c", i]
    p = subprocess.Popen(cmd, stdout=subprocess.PIPE)
    pp = p.communicate()[0].decode('ascii')
    rc = p.returncode
    # print(rc)

    if rc == 0:
        goodfiles.append(i)
    else:
        badfiles.append(i)
    if len(pp) != 0:
        warnfiles.append(i)


print(f"num good files: {len(goodfiles)}")
print(f"num bad files: {len(badfiles)}")
print("list of bad files:")
print(badfiles)

print(f"Files with warnings: {len(warnfiles)}")
print(warnfiles)
