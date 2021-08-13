import sys, os

with open(sys.argv[1]) as f:
	for line in f:
		if "GODDARD_SIZE" in line:
			tokens=line.split()
			with open("build/us/goddard.txt", "w+") as f:
				sz = int(tokens[0], 16)
				sz += 16
				sz &= 0xFFFFFFF0
				f.write("GODDARD_SIZE = 0x%X;" % sz)
				