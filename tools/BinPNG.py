#!/usr/bin/env python3
import struct
import png
import math
from bitstring import *
import sys
#convert png to bin

def MakeCI(file,Bpp):
    r = png.Reader(file)
    re = r.read()
    Pal = re[3]['palette']
    Pbin = []
    shifts = [3,3,3,7]
    for p in Pal:
        b = [a>>s for a,s in zip(p,shifts)]
        if len(p)==4:
            b = pack('3*uint:5,uint:1',*b)
        else:
            b = pack('3*uint:5,uint:1',*b,1)
        Pbin.append(b.bytes)
    bin = []
    for p in re[2]:
        for w in range(0,re[0],(8//Bpp)):
            b = p[w:w+(8//Bpp)]
            b = pack('%d*uint:%d'%((8//Bpp),Bpp),*b)
            bin.append(b.bytes)
    return [bin,Pbin]

if __name__=='__main__':
	[texture,palette] = MakeCI(sys.argv[1],int(sys.argv[3]))
	pname = sys.argv[2].split('.')
	pname.insert(2,'pal')
	pname = pname[:3]
	pname = '.'.join(pname)
	p = open(pname,'w')
	t = open(sys.argv[2],'w')
	[t.write('0x{:02X},'.format(tex[0])) for tex in texture]
	[p.write('0x{:02X},0x{:02X},'.format(pal[0],pal[1])) for pal in palette]
	t.close()
	p.close()