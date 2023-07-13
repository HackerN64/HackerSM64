import sys, struct, subprocess

class MapEntry():
	def __init__(self, addr, size, type, name, errc):
		self.addr = addr
		self.size = size
		self.errc = errc
		self.type = type
		self.name = name
		self.strlen = (len(name) + 4) & (~3)
	def __str__(self):
		return "%s %d %s %hi %c %c" % (self.addr, self.size, self.name, self.strlen, self.errc, self.type)
	def __repr__(self):
		return "%s %d %s %hi %c %c" % (self.addr, self.size, self.name, self.strlen, self.errc, self.type)


structDef = ">LLLHBB"

symNames = []

proc = subprocess.Popen(["nm", "-S", "-v", sys.argv[1]], stdout=subprocess.PIPE)

symbols = proc.communicate()[0].decode('ascii').split("\n")
for line in symbols:
	# format:
	# 80153210 000000f8 T global_sym
	# 80153210 T static_sym
	tokens = line.split()
	if len(tokens) >= 3 and len(tokens[-2]) == 1:
		addr = int(tokens[0], 16)
		errc = ord('\0')
		if symNames:
			prevEntry = symNames[-1]
			if prevEntry.size == 0 and addr > prevEntry.addr:
				newPrevSize = addr - prevEntry.addr
				if newPrevSize < 0xFFFFF:
					prevEntry.size = newPrevSize
				else:
					errc = ord('S')
		if addr & 0x80000000:
			name = tokens[-1]
			type = ord(tokens[-2])
			if len(tokens) < 4:
				size = 0
			else:
				size = int(tokens[-3], 16)
			symNames.append(MapEntry(addr, size, type, name, errc))


f1 = open(sys.argv[2], "wb+") # addr
f2 = open(sys.argv[3], "wb+") # name

symNames.sort(key=lambda x: x.addr)

off = 0
for x in symNames:
	f1.write(struct.pack(structDef, x.addr, x.size, off, len(x.name), x.errc, x.type))
	f2.write(struct.pack(">%ds" % x.strlen, bytes(x.name, encoding="ascii")))
	off += x.strlen


f1.close()
f2.close()

# print('\n'.join([str(hex(x.addr)) + " " + x.name for x in symNames]))

