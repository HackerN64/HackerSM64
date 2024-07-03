#pragma once

#include <config.h>

/*
 * Sprite structure
 */

typedef struct {
  void *SourceImagePointer;
  void *TlutPointer;
  short Stride;
  short SubImageWidth;
  short SubImageHeight;
  char  SourceImageType;
  char  SourceImageBitSize;
  short SourceImageOffsetS;
  short SourceImageOffsetT;
  /* 20 bytes for above */

  /* padding to bring structure size to 64 bit allignment */
  char dummy[4]; 

} uSprite_t;

typedef union { 
  uSprite_t  s;

  /* Need to make sure this is 64 bit aligned */   
  long long int         force_structure_allignment[3];
} uSprite;

#if F3DEX_VERSION > 2
    #include "gbi_f3dex3.h"
#else
    #include "gbi_sgi.h"
#endif
