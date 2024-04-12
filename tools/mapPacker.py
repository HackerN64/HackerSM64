import sys, struct, subprocess

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


proc = subprocess.Popen(["nm", "-S", sys.argv[1]], stdout=subprocess.PIPE)

symbols = proc.communicate()[0].decode('ascii').split("\n")
for line in symbols:
	# format:
	# 80153210 000000f8 T global_sym
	# 80153210 t static_sym
	tokens = line.split()
	if len(tokens) >= 3 and len(tokens[-2]) == 1:
		addr = int(tokens[0], 16)
		if addr & 0x80000000 and tokens[-2].lower() == "t":
			symNames.append(MapEntry(tokens[-1], addr))



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

