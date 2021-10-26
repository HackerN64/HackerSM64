#ifndef __FARCALL_H__
#define __FARCALL_H__

#ifdef __GNUC__
#define farcall(x) x
#define far __attribute__(( long_call ))
#pragma GCC diagnostic ignored "-Wattributes" // Workaround for a bug that produces errant warnings in gcc
#define near __attribute(( near ))
#else
#define farcall(x) (*(&(x)))
#define far
#define near
#endif

#endif

/*
How to use:

Simply place any code you want in any groupX_geo.c or any level's script.c (see the first note for a potential gotcha when using code in level script segments).

To call segment code from normal code or vice versa (or segment code from other segmented code)
 - Add `#include "farcall.h"` to the given file
 - Declare any functions that are not in that file like so (with far)
    `far void print_text(s32 x, s32 y, const char *str);`
 - Call any functions that are not in the file like so:
    `farcall(print_text)(10, 10 "test");`

If you forget any of the above 3 steps, you will get an error during linking your ROM that looks something like:
`script.c:(.text+0x24): relocation truncated to fit: R_MIPS_26 against ...`
So if you see that error, make sure you've followed the above three steps.

If you're using gcc, you don't need to use farcall to call far functions, as declaring a function as far is enough.

Additionally if using gcc, you don't need to declare any functions as far in segmented code due to a change in the makefile.
  Instead, you can declare local functions as `near` to optimize local calls, but this is not required.

If you have code in a groupX_geo.c file, you must load it in level scripts by doing LOAD_RAW_WITH_CODE instead of the normal LOAD_RAW.
  This has two extra arguments, which should be passed as `_groupX_geoSegmentNoloadStart` and `_groupX_geoSegmentNoloadEnd`.
  For example, if you added code to group3's geo segment:

LOAD_RAW_WITH_CODE(SEGMENT_GROUPA_GEO, _group3_geoSegmentRomStart,  _group3_geoSegmentRomEnd, _group3_geoSegmentNoloadStart, _group3_geoSegmentNoloadEnd),

For convenience, included is a new header: src/game/obj_behaviors_2_nonstatic.h. In addition to effectively removing
  the static keyword from object_behaviors_2.c, this header defines prototypes for all functions in that file. This
  allows you to reference those functions elsewhere, which is convenient for segmented behavior callbacks.

Notes:
 - As of right now, Fast64 will not retain LOAD_RAW_WITH_CODE commands in the level script.
     This means that you will have to readd those on each export, but this may change in a future Fast64 version.
     The same is true for any code in the level file, so I suggest making a code.inc.c file in the level's folder
       and adding an `#include "levels/x/code.inc.c"` into the level script file.
     That include will also be wiped on each export, but is easy enough to add after an export.
 - Variables in segments will get reset to their defaults every time a segment is loaded.
     Use variables declared in normal code and extern them if this isn't desirable.
 - Because of the limited number of TLB entries, at most 256kB of segmented memory can be mapped with this patch.
     If you want to be sure that everything is being mapped, use the TLB entries view in PJ64 2.4's debugger.
 - TLB mapping requires at least 4kB alignment, so memory allocations for segments with code are automatically padded to meet this alignment.
     This means that potentially up to 4kB of RAM can be wasted when a given segment with code is loaded.
     This should be far less than the potential savings from using this patch, but is something that should be kept in mind.
*/
