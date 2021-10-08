#ifndef _GBI_POSLIGHT_H_
#define _GBI_POSLIGHT_H_

/* version 990705 */
#define G_LIGHTING_POSITIONAL 0x00400000
#ifdef _LANGUAGE_ASSEMBLY
#define G_LIGHTING_POSITIONAL_H (G_LIGHTING_POSITIONAL/0x10000)
#endif

typedef struct {
  unsigned char col[3];         /* diffuse light value (rgba) */
  unsigned char kc;             /* positional lighting enable flag & constant attenuation Kc */
  unsigned char colc[3];        /* copy of diffuse light value (rgba) */
  unsigned char kl;             /* linear attenuation Kl */
  short pos[3];                 /* light position x, y, z */
  unsigned char kq;             /* quadratic attenuation Kq */
  char reserved1;
} PosLight_t;

typedef union {
  PosLight_t p;
  Light_t    l;
  long long int force_structure_alignment[2];
} PosLight;

typedef struct {
    Ambient     a;
    PosLight    l[7];
} PosLightsn;

typedef struct {
    Ambient     a;
    PosLight    l[1];
} PosLights0;

typedef struct {
    Ambient     a;
    PosLight    l[1];
} PosLights1;

typedef struct {
    Ambient     a;
    PosLight    l[2];
} PosLights2;

typedef struct {
    Ambient     a;
    PosLight    l[3];
} PosLights3;

typedef struct {
    Ambient     a;
    PosLight    l[4];
} PosLights4;

typedef struct {
    Ambient     a;
    PosLight    l[5];
} PosLights5;

typedef struct {
    Ambient     a;
    PosLight    l[6];
} PosLights6;

typedef struct {
    Ambient     a;
    PosLight    l[7];
} PosLights7;

#define _gSPLightColor2(pkt, n, col1, col2) \
{\
  gMoveWd(pkt, G_MW_LIGHTCOL, G_MWO_a##n, col1);\
  gMoveWd(pkt, G_MW_LIGHTCOL, G_MWO_b##n, col2);\
}
#define _gsSPLightColor2(n, col1, col2) \
  gsMoveWd(G_MW_LIGHTCOL, G_MWO_a##n, col1),\
  gsMoveWd(G_MW_LIGHTCOL, G_MWO_b##n, col2)

#define _gdSPDefAmbient(ar,ag,ab)  {{ {ar,ag,ab},0,{ar,ag,ab},0}}
#define _gdSPDefPosLight(r,g,b,x,y,z,c,l,q) {{ {r,g,b},c,{r,g,b},l,{x,y,z},q,0 }}
#define _gdSPDefInfLight(r,g,b,x,y,z)       {{ {r,g,b},0,{r,g,b},0,{((x)<<8)|((y)&0xff),(z)<<8,0},0,0}}
#define gdSPDefPosLights0(ar,ag,ab) \
  { _gdSPDefAmbient(ar,ag,ab), \
    { _gdSPDefPosLight(0,0,0,0,0,0,0,0,0)} }
#define gdSPDefPosLights1(ar,ag,ab,r1,g1,b1,x1,y1,z1,c1,l1,q1) \
  { _gdSPDefAmbient(ar,ag,ab), \
    { _gdSPDefPosLight(r1,g1,b1,x1,y1,z1,c1,l1,q1)} }
#define gdSPDefPosLights2(ar,ag,ab,r1,g1,b1,x1,y1,z1,c1,l1,q1,r2,g2,b2,x2,y2,z2,c2,l2,q2) \
  { _gdSPDefAmbient(ar,ag,ab), \
    { _gdSPDefPosLight(r1,g1,b1,x1,y1,z1,c1,l1,q1), \
      _gdSPDefPosLight(r2,g2,b2,x2,y2,z2,c2,l2,q2)} }
#define gdSPDefPosLights3(ar,ag,ab,r1,g1,b1,x1,y1,z1,c1,l1,q1,r2,g2,b2,x2,y2,z2,c2,l2,q2,r3,g3,b3,x3,y3,z3,c3,l3,q3) \
  { _gdSPDefAmbient(ar,ag,ab), \
    { _gdSPDefPosLight(r1,g1,b1,x1,y1,z1,c1,l1,q1), \
      _gdSPDefPosLight(r2,g2,b2,x2,y2,z2,c2,l2,q2), \
      _gdSPDefPosLight(r3,g3,b3,x3,y3,z3,c3,l3,q3)} }
#define gdSPDefPosLights4(ar,ag,ab,r1,g1,b1,x1,y1,z1,c1,l1,q1,r2,g2,b2,x2,y2,z2,c2,l2,q2,r3,g3,b3,x3,y3,z3,c3,l3,q3,r4,g4,b4,x4,y4,z4,c4,l4,q4) \
  { _gdSPDefAmbient(ar,ag,ab), \
    { _gdSPDefPosLight(r1,g1,b1,x1,y1,z1,c1,l1,q1), \
      _gdSPDefPosLight(r2,g2,b2,x2,y2,z2,c2,l2,q2), \
      _gdSPDefPosLight(r3,g3,b3,x3,y3,z3,c3,l3,q3), \
      _gdSPDefPosLight(r4,g4,b4,x4,y4,z4,c4,l4,q4)} }
#define gdSPDefPosLights5(ar,ag,ab,r1,g1,b1,x1,y1,z1,c1,l1,q1,r2,g2,b2,x2,y2,z2,c2,l2,q2,r3,g3,b3,x3,y3,z3,c3,l3,q3,r4,g4,b4,x4,y4,z4,c4,l4,q4,r5,g5,b5,x5,y5,z5,c5,l5,q5) \
  { _gdSPDefAmbient(ar,ag,ab), \
    { _gdSPDefPosLight(r1,g1,b1,x1,y1,z1,c1,l1,q1), \
      _gdSPDefPosLight(r2,g2,b2,x2,y2,z2,c2,l2,q2), \
      _gdSPDefPosLight(r3,g3,b3,x3,y3,z3,c3,l3,q3), \
      _gdSPDefPosLight(r4,g4,b4,x4,y4,z4,c4,l4,q4), \
      _gdSPDefPosLight(r5,g5,b5,x5,y5,z5,c5,l5,q5)} }
#define gdSPDefPosLights6(ar,ag,ab,r1,g1,b1,x1,y1,z1,c1,l1,q1,r2,g2,b2,x2,y2,z2,c2,l2,q2,r3,g3,b3,x3,y3,z3,c3,l3,q3,r4,g4,b4,x4,y4,z4,c4,l4,q4,r5,g5,b5,x5,y5,z5,c5,l5,q5,r6,g6,b6,x6,y6,z6,c6,l6,q6) \
  { _gdSPDefAmbient(ar,ag,ab), \
    { _gdSPDefPosLight(r1,g1,b1,x1,y1,z1,c1,l1,q1), \
      _gdSPDefPosLight(r2,g2,b2,x2,y2,z2,c2,l2,q2), \
      _gdSPDefPosLight(r3,g3,b3,x3,y3,z3,c3,l3,q3), \
      _gdSPDefPosLight(r4,g4,b4,x4,y4,z4,c4,l4,q4), \
      _gdSPDefPosLight(r5,g5,b5,x5,y5,z5,c5,l5,q5), \
      _gdSPDefPosLight(r6,g6,b6,x6,y6,z6,c6,l6,q6)} }
#define gdSPDefPosLights7(ar,ag,ab,r1,g1,b1,x1,y1,z1,c1,l1,q1,r2,g2,b2,x2,y2,z2,c2,l2,q2,r3,g3,b3,x3,y3,z3,c3,l3,q3,r4,g4,b4,x4,y4,z4,c4,l4,q4,r5,g5,b5,x5,y5,z5,c5,l5,q5,r6,g6,b6,x6,y6,z6,c6,l6,q6,r7,g7,b7,x7,y7,z7,c7,l7,q7) \
  { _gdSPDefAmbient(ar,ag,ab), \
    { _gdSPDefPosLight(r1,g1,b1,x1,y1,z1,c1,l1,q1), \
      _gdSPDefPosLight(r2,g2,b2,x2,y2,z2,c2,l2,q2), \
      _gdSPDefPosLight(r3,g3,b3,x3,y3,z3,c3,l3,q3), \
      _gdSPDefPosLight(r4,g4,b4,x4,y4,z4,c4,l4,q4), \
      _gdSPDefPosLight(r5,g5,b5,x5,y5,z5,c5,l5,q5), \
      _gdSPDefPosLight(r6,g6,b6,x6,y6,z6,c6,l6,q6), \
      _gdSPDefPosLight(r7,g7,b7,x7,y7,z7,c7,l7,q7)} }

#endif /* _GBI_POSLIGHT_H_ */
