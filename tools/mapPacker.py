import sys, struct

class MapEntry():
	def __init__(self, nm, addr):
		self.name = nm
		self.addr = addr
		self.strlen = (len(nm) + 4) & (~3)
	def __str__(self):
		return "%s %s %d" % (self.addr, self.name, self.strlen)
	def __repr__(self):
		return "%s %s %d" % (self.addr, self.name, self.strlen)


structDef = ">LLLL"

symNames = []

with open(sys.argv[1]) as f:
	for line in f:
		if "0x000000008" in line and "=" not in line and "." not in line and "*" not in line and "load address" not in line:
			tokens = line.split()
			symNames.append(MapEntry(tokens[1], int(tokens[0], 16)))



f1 = open(sys.argv[2], "wb+")
f2 = open(sys.argv[3], "wb+")

symNames.sort(key=lambda x: x.addr)

off = 0
for x in symNames:
	f1.write(struct.pack(structDef, x.addr, off, len(x.name), 0))
	f2.write(struct.pack(">%ds" % x.strlen, bytes(x.name, encoding="ascii")))
	off += x.strlen


f1.close()
f2.close()

# print('\n'.join([str(hex(x.addr)) + " " + x.name for x in symNames]))

