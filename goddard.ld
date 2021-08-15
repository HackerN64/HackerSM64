OUTPUT_ARCH (mips)

#define BEGIN_SEG(name, addr) \
    _##name##SegmentStart = ADDR(.name); \
    _##name##SegmentRomStart = __romPos; \
    .name addr : AT(__romPos)

#define END_SEG(name) \
    _##name##SegmentEnd = ADDR(.name) + SIZEOF(.name); \
    _##name##SegmentRomEnd = __romPos + SIZEOF(.name); \
    __romPos += SIZEOF(.name);

#define BEGIN_NOLOAD(name) \
    _##name##SegmentBssStart = ADDR(.name.noload); \
    .name.noload (NOLOAD) :

#define END_NOLOAD(name) \
    _##name##SegmentBssEnd = ADDR(.name.noload) + SIZEOF(.name.noload); \
    _##name##SegmentBssSize = SIZEOF(.name.noload);

SECTIONS {
      __romPos = 0;
      SEG_GODDARD = 0x80345670;
      BEGIN_SEG(goddard, SEG_GODDARD)
      {
            #define LOAD
                  #include "goddard.ld.inc"
            #undef LOAD
            . = ALIGN(16);
      }
      END_SEG(goddard)
      BEGIN_NOLOAD(goddard) {
            #define NOLOAD
                  #include "goddard.ld.inc"
            #undef NOLOAD
            . = ALIGN(16);
      }
      END_NOLOAD(goddard)

      GODDARD_SIZE = SIZEOF(.goddard) + SIZEOF(.goddard.noload);

      /DISCARD/ :
      {
            *(*);
      }
}
