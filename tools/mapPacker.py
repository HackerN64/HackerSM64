import sys, struct, subprocess

class MapEntry():
	def __init__(self, name, size, addr):
		self.name = name
		self.size = size
		self.addr = addr
		self.strlen = (len(name) + 4) & (~3)
	def __str__(self):
		return "%s %d %s %d" % (self.addr, self.size, self.name, self.strlen)
	def __repr__(self):
		return "%s %d %s %d" % (self.addr, self.size, self.name, self.strlen)


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
			symNames.append(MapEntry(tokens[-1], int(tokens[-3], 16), addr))

# def remove_prefix(text, prefix):
#     return text[text.startswith(prefix) and len(prefix):]

# with open(sys.argv[1]) as f: # sm64_prelim.map
# 	for line in f:
# 		if "0x000000008" in line and "=" not in line and "*" not in line and "load address" not in line:
# 			if ".o" in line:
# 				if "build/" in line or "lib/" in line:
# 					# object file line:
# 					tokens = line.split()
# 					if ".bss" in line or ".text" in line or ".data" in line or ".rodata" in line:
# 						filestartaddr = int(tokens[1], 16)
# 						filesize = int(remove_prefix(tokens[2], "0x"), 16)
# 					else:
# 						filestartaddr = int(tokens[0], 16)
# 						filesize = int(remove_prefix(tokens[1], "0x"), 16)
# 			if "." not in line:
# 				# address entry line:
# 				tokens = line.split()
# 				addr = int(tokens[0], 16)
# 				# position of current entry in the current file
# 				offsetinfile = (addr - filestartaddr)
# 				# the size between here and the end of the file
# 				size = (filesize - offsetinfile)
# 				# if we're past the first entry:
# 				if symNames:
# 					prevEntry = symNames[-1]
# 					prevAddr = prevEntry.addr
# 					# if the previous entry is in the same file...
# 					if (prevAddr >= filestartaddr):
# 						# modify its size so it doesn't overlap the current entry.
# 						prevEntry.size = (addr - prevAddr)
# 				name = tokens[1]
# 				prevEntry = symNames.append(MapEntry(name, size, addr))


f1 = open(sys.argv[2], "wb+") # addr
f2 = open(sys.argv[3], "wb+") # name

symNames.sort(key=lambda x: x.addr)

off = 0
for x in symNames:
	f1.write(struct.pack(structDef, x.addr, x.size, off, len(x.name)))
	f2.write(struct.pack(">%ds" % x.strlen, bytes(x.name, encoding="ascii")))
	off += x.strlen


f1.close()
f2.close()

# print('\n'.join([str(hex(x.addr)) + " " + x.name for x in symNames]))

