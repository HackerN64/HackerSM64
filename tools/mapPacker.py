import sys, struct, subprocess

class MapSymbol():
	def __init__(self, addr, size, name, type, errc):
		self.addr = addr # Symbol address (32 bits).
		self.size = size # Symbol size (32 bits).
		self.name = name # Symbol name offset (32 bits).
		self.strlen = ((len(name) + 4) & (~3)) # Symbol name length (16 bits).
		self.type = type # Symbol type (8 bits).
		self.errc = errc # Error char (8 bits).
	def __str__(self):
		return "%s %d %s %hi %c %c" % (self.addr, self.size, self.name, self.strlen, self.type, self.errc)
	def __repr__(self):
		return "%s %d %s %hi %c %c" % (self.addr, self.size, self.name, self.strlen, self.type, self.errc)


# See struct MapSymbol in map_parser.h.
structDef = ">LLLHBB"

symNames = []

proc = subprocess.Popen(["nm", "--print-size", "--numeric-sort", sys.argv[1]], stdout=subprocess.PIPE)

symbols = proc.communicate()[0].decode('ascii').split("\n")

for line in symbols:
	# Format:
	# [address] [size]   [type] [name]
	# 80153210  000000f8 T      global_sym
	# OR:
	# [address] [type] [name]
	# 80153210  t      static_sym
	tokens = line.split()
	if (len(tokens) >= 3) and (len(tokens[-2]) == 1):
		addr = int(tokens[0], 16)
		# Error char.
		errc = ord('\0')
		# Skip non-RDRAM addresses.
		if (addr & 0x80000000):
			# If this is not the first entry...
			if symNames:
				# Get the previous entry.
				prevEntry = symNames[-1]
				# If the previous entry does not have a defined size...
				if (prevEntry.size == 0) and (addr > prevEntry.addr):
					# Get the size between the current entry and the previous entry.
					sizeToLastEntry = (addr - prevEntry.addr)
					# If the distance to the previous entry is not unreasonably large, use it as the symbol's size.
					if (sizeToLastEntry < 0xFFFFF):
						prevEntry.size = sizeToLastEntry
					else:
						# Size too large, set error char 'S'.
						errc = ord('S')
			# Last token is the name.
			name = tokens[-1]
			# Second to last token is the type char.
			type = ord(tokens[-2])
			# Check for size data.
			if (len(tokens) < 4):
				# No size data, so assume 0. It may be set by the next entry.
				size = 0
			else:
				# Get the size data.
				size = int(tokens[-3], 16)
			# Append the symbol to the list.
			symNames.append(MapSymbol(addr, size, name, type, errc))


f1 = open(sys.argv[2], "wb+") # addr.bin
f2 = open(sys.argv[3], "wb+") # name.bin

symNames.sort(key=lambda x: x.addr)

off = 0
for x in symNames:
	f1.write(struct.pack(structDef, x.addr, x.size, off, len(x.name), x.type, x.errc))
	f2.write(struct.pack(">%ds" % x.strlen, bytes(x.name, encoding="ascii")))
	off += x.strlen


f1.close()
f2.close()

# print('\n'.join([str(hex(x.addr)) + " " + x.name for x in symNames]))

