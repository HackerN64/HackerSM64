#include <PR/ultratypes.h>
#include <PR/gbi.h>

#include "config.h"
#include "macros.h"
#include "types.h"
#include "game/ingame_menu.h"
#include "game/puppyprint.h"
#include "game/paintings.h"

#include "make_const_nonconst.h"

// SM64 (US/JP/EU/SH) Segment 02
#ifdef PUPPYPRINT
ALIGNED8 static const Texture small_font_default[] = {
#include "textures/segment2/custom_text.i4.inc.c"
};
ALIGNED8 static const Texture small_font_outline[] = {
#include "textures/segment2/custom_text2.ia4.inc.c"
};
ALIGNED8 static const Texture small_font_plain[] = {
#include "textures/segment2/custom_text3.i4.inc.c"
};
ALIGNED8 static const Texture small_font_vanilla[] = {
#include "textures/segment2/custom_text4.i4.inc.c"
};

const u8 small_font_kerning_default[] = {
    /*!*/ 4, /*"*/ 5, /*#*/ 0, /*$*/ 0, /*%*/ 8, /*&*/ 8, /*'*/ 2, /*(*/ 5, /*)*/ 5, /***/ 0, /*+*/ 8, /*,*/ 3, /*-*/ 8, /*.*/ 3, /*/*/ 8, /*0*/ 6,
    /*1*/ 5, /*2*/ 7, /*3*/ 7, /*4*/ 7, /*5*/ 7, /*6*/ 8, /*7*/ 7, /*8*/ 7, /*9*/ 6, /*:*/ 3, /*;*/ 3, /*<*/ 0, /*=*/ 0, /*>*/ 0, /*?*/ 6, /*@*/ 0,
    /*A*/ 7, /*B*/ 7, /*C*/ 7, /*D*/ 7, /*E*/ 6, /*F*/ 5, /*G*/ 8, /*H*/ 6, /*I*/ 6, /*J*/ 5, /*K*/ 7, /*L*/ 6, /*M*/ 7, /*N*/ 7, /*O*/ 7, /*P*/ 6, 
    /*Q*/ 8, /*R*/ 6, /*S*/ 7, /*T*/ 7, /*U*/ 7, /*V*/ 7, /*W*/ 8, /*X*/ 7, /*Y*/ 7, /*Z*/ 7, /*[*/ 0, /*\\*/ 0, /*]*/ 0, /*^*/ 8, /*_*/ 0, /*`*/ 0, 
    /*a*/ 7, /*b*/ 7, /*c*/ 6, /*d*/ 7, /*e*/ 7, /*f*/ 7, /*g*/ 7, /*h*/ 7, /*i*/ 3, /*j*/ 5, /*k*/ 8, /*l*/ 4, /*m*/ 7, /*n*/ 7, /*o*/ 7, /*p*/ 7, 
    /*q*/ 7, /*r*/ 6, /*s*/ 6, /*t*/ 6, /*u*/ 6, /*v*/ 7, /*w*/ 8, /*x*/ 6, /*y*/ 8, /*z*/ 7, /*{*/ 0, /*|*/ 0, /*}*/ 0, /*~*/ 8, 
};

const u16 small_font_offsets_default[] = {
    /*!*/ 0, /*"*/ 8, /*#*/ 16, /*$*/ 24, /*%*/ 32, /*&*/ 40, /*'*/ 48, /*(*/ 56, /*)*/ 64, /***/ 72, /*+*/ 80, /*,*/ 88, /*-*/ 96, /*.*/ 104, /*/*/ 112,
    /*0*/ 120, /*1*/ 128, /*2*/ 136, /*3*/ 144, /*4*/ 152, /*5*/ 160, /*6*/ 168, /*7*/ 176, /*8*/ 184, /*9*/ 192, /*:*/ 200, /*;*/ 208, /*<*/ 216, /*=*/ 216,
    /*>*/ 216, /*?*/ 216, /*@*/ 224, /*A*/ 224, /*B*/ 232, /*C*/ 240, /*D*/ 248, /*E*/ 256, /*F*/ 264, /*G*/ 272, /*H*/ 280, /*I*/ 288, /*J*/ 296, /*K*/ 304,
    /*L*/ 312, /*M*/ 320, /*N*/ 328, /*O*/ 336, /*P*/ 344, /*Q*/ 352, /*R*/ 360, /*S*/ 368, /*T*/ 376, /*U*/ 384, /*V*/ 392, /*W*/ 400, /*X*/ 408, /*Y*/ 416,
    /*Z*/ 424, /*[*/ 432, /*\*/ 432, /*]*/ 432, /*^*/ 432, /*_*/ 440, /*`*/ 440, /*a*/ 440, /*b*/ 448, /*c*/ 456, /*d*/ 464, /*e*/ 472, /*f*/ 480, /*g*/ 488, 
    /*h*/ 496, /*i*/ 504, /*j*/ 512, /*k*/ 520, /*l*/ 528, /*m*/ 536, /*n*/ 544, /*o*/ 552, /*p*/ 560, /*q*/ 568, /*r*/ 576, /*s*/ 584, /*t*/ 592, /*u*/ 600, 
    /*v*/ 608, /*w*/ 616, /*x*/ 624, /*y*/ 632, /*z*/ 640, /*{*/ 648, /*|*/ 648, /*}*/ 648, /*~*/ 648, /*:)*/ 656
};

static const u8 small_font_kerning_outline[] = {
    /*!*/ 3, /*"*/ 4, /*#*/ 0, /*$*/ 0, /*%*/ 6, /*&*/ 6, /*'*/ 2, /*(*/ 4, /*)*/ 4, /***/ 0, /*+*/ 6, /*,*/ 2, /*-*/ 6, /*.*/ 2, /*/*/ 6, /*0*/ 6,
    /*1*/ 6, /*2*/ 6, /*3*/ 6, /*4*/ 6, /*5*/ 6, /*6*/ 6, /*7*/ 6, /*8*/ 6, /*9*/ 6, /*:*/ 2, /*;*/ 2, /*<*/ 0, /*=*/ 0, /*>*/ 0, /*?*/ 6, /*@*/ 0, 
    /*A*/ 6, /*B*/ 6, /*C*/ 6, /*D*/ 6, /*E*/ 6, /*F*/ 6, /*G*/ 6, /*H*/ 6, /*I*/ 5, /*J*/ 7, /*K*/ 6, /*L*/ 6, /*M*/ 6, /*N*/ 6, /*O*/ 6, /*P*/ 6, 
    /*Q*/ 6, /*R*/ 6, /*S*/ 6, /*T*/ 6, /*U*/ 6, /*V*/ 6, /*W*/ 6, /*X*/ 6, /*Y*/ 6, /*Z*/ 6, /*[*/ 0, /*\\*/ 0, /*]*/ 0, /*^*/ 7, /*_*/ 0, /*`*/ 0,  
    /*a*/ 5, /*b*/ 5, /*c*/ 5, /*d*/ 5, /*e*/ 5, /*f*/ 5, /*g*/ 5, /*h*/ 5, /*i*/ 2, /*j*/ 6, /*k*/ 4, /*l*/ 2, /*m*/ 5, /*n*/ 5, /*o*/ 5, /*p*/ 5, 
    /*q*/ 5, /*r*/ 5, /*s*/ 5, /*t*/ 5, /*u*/ 5, /*v*/ 5, /*w*/ 5, /*x*/ 5, /*y*/ 5, /*z*/ 5, /*{*/ 0, /*|*/ 0, /*}*/ 0, /*~*/ 6,   
};

const u16 small_font_offsets_outline[] = {
    /*!*/ 0, /*"*/ 8, /*#*/ 16, /*$*/ 24, /*%*/ 32, /*&*/ 40, /*'*/ 48, /*(*/ 56, /*)*/ 64, /***/ 72, /*+*/ 80, /*,*/ 88, /*-*/ 96, /*.*/ 104, /*/*/ 112,
    /*0*/ 120, /*1*/ 128, /*2*/ 136, /*3*/ 144, /*4*/ 152, /*5*/ 160, /*6*/ 168, /*7*/ 176, /*8*/ 184, /*9*/ 192, /*:*/ 200, /*;*/ 208, /*<*/ 216, /*=*/ 216,
    /*>*/ 216, /*?*/ 216, /*@*/ 224, /*A*/ 224, /*B*/ 232, /*C*/ 240, /*D*/ 248, /*E*/ 256, /*F*/ 264, /*G*/ 272, /*H*/ 280, /*I*/ 288, /*J*/ 296, /*K*/ 304,
    /*L*/ 312, /*M*/ 320, /*N*/ 328, /*O*/ 336, /*P*/ 344, /*Q*/ 352, /*R*/ 360, /*S*/ 368, /*T*/ 376, /*U*/ 384, /*V*/ 392, /*W*/ 400, /*X*/ 408, /*Y*/ 416,
    /*Z*/ 424, /*[*/ 432, /*\*/ 432, /*]*/ 432, /*^*/ 432, /*_*/ 440, /*`*/ 440, /*a*/ 440, /*b*/ 448, /*c*/ 456, /*d*/ 464, /*e*/ 472, /*f*/ 480, /*g*/ 488, 
    /*h*/ 496, /*i*/ 504, /*j*/ 512, /*k*/ 520, /*l*/ 528, /*m*/ 536, /*n*/ 544, /*o*/ 552, /*p*/ 560, /*q*/ 568, /*r*/ 576, /*s*/ 584, /*t*/ 592, /*u*/ 600, 
    /*v*/ 608, /*w*/ 616, /*x*/ 624, /*y*/ 632, /*z*/ 640, /*{*/ 648, /*|*/ 648, /*}*/ 648, /*~*/ 648, /*:)*/ 656
};

static const u8 small_font_kerning_plain[] = {
    /*!*/ 5, /*"*/ 4, /*#*/ 0, /*$*/ 0, /*%*/ 6, /*&*/ 7, /*'*/ 2, /*(*/ 4, /*)*/ 4, /***/ 0, /*+*/ 6, /*,*/ 2, /*-*/ 6, /*.*/ 2, /*/*/ 6, /*0*/ 6,
    /*1*/ 5, /*2*/ 5, /*3*/ 5, /*4*/ 5, /*5*/ 5, /*6*/ 5, /*7*/ 5, /*8*/ 5, /*9*/ 5, /*:*/ 3, /*;*/ 3, /*<*/ 0, /*=*/ 0, /*>*/ 0, /*?*/ 6, /*@*/ 0, 
    /*A*/ 6, /*B*/ 6, /*C*/ 6, /*D*/ 6, /*E*/ 6, /*F*/ 6, /*G*/ 6, /*H*/ 6, /*I*/ 4, /*J*/ 6, /*K*/ 6, /*L*/ 6, /*M*/ 7, /*N*/ 7, /*O*/ 6, /*P*/ 6, 
    /*Q*/ 6, /*R*/ 6, /*S*/ 6, /*T*/ 6, /*U*/ 6, /*V*/ 6, /*W*/ 7, /*X*/ 6, /*Y*/ 6, /*Z*/ 6, /*[*/ 0, /*\\*/ 0, /*]*/ 0, /*^*/ 7, /*_*/ 0, /*`*/ 0,
    /*a*/ 6, /*b*/ 6, /*c*/ 6, /*d*/ 6, /*e*/ 6, /*f*/ 6, /*g*/ 6, /*h*/ 6, /*i*/ 3, /*j*/ 4, /*k*/ 6, /*l*/ 5, /*m*/ 7, /*n*/ 6, /*o*/ 6, /*p*/ 6, 
    /*q*/ 6, /*r*/ 6, /*s*/ 6, /*t*/ 6, /*u*/ 6, /*v*/ 6, /*w*/ 7, /*x*/ 6, /*y*/ 6, /*z*/ 6, /*{*/ 0, /*|*/ 0, /*}*/ 0, /*~*/ 7,   
};

const u16 small_font_offsets_plain[] = {
    /*!*/ 0, /*"*/ 8, /*#*/ 16, /*$*/ 24, /*%*/ 32, /*&*/ 40, /*'*/ 48, /*(*/ 56, /*)*/ 64, /***/ 72, /*+*/ 80, /*,*/ 88, /*-*/ 96, /*.*/ 104, /*/*/ 112,
    /*0*/ 120, /*1*/ 128, /*2*/ 136, /*3*/ 144, /*4*/ 152, /*5*/ 160, /*6*/ 168, /*7*/ 176, /*8*/ 184, /*9*/ 192, /*:*/ 200, /*;*/ 208, /*<*/ 216, /*=*/ 216,
    /*>*/ 216, /*?*/ 216, /*@*/ 224, /*A*/ 224, /*B*/ 232, /*C*/ 240, /*D*/ 248, /*E*/ 256, /*F*/ 264, /*G*/ 272, /*H*/ 280, /*I*/ 288, /*J*/ 296, /*K*/ 304,
    /*L*/ 312, /*M*/ 320, /*N*/ 328, /*O*/ 336, /*P*/ 344, /*Q*/ 352, /*R*/ 360, /*S*/ 368, /*T*/ 376, /*U*/ 384, /*V*/ 392, /*W*/ 400, /*X*/ 408, /*Y*/ 416,
    /*Z*/ 424, /*[*/ 432, /*\*/ 432, /*]*/ 432, /*^*/ 432, /*_*/ 440, /*`*/ 440, /*a*/ 440, /*b*/ 448, /*c*/ 456, /*d*/ 464, /*e*/ 472, /*f*/ 480, /*g*/ 488, 
    /*h*/ 496, /*i*/ 504, /*j*/ 512, /*k*/ 520, /*l*/ 528, /*m*/ 536, /*n*/ 544, /*o*/ 552, /*p*/ 560, /*q*/ 568, /*r*/ 576, /*s*/ 584, /*t*/ 592, /*u*/ 600, 
    /*v*/ 608, /*w*/ 616, /*x*/ 624, /*y*/ 632, /*z*/ 640, /*{*/ 648, /*|*/ 648, /*}*/ 648, /*~*/ 648, /*:)*/ 656
};

static const u8 small_font_kerning_vanilla[] = {
    /*!*/ 4, /*"*/ 4, /*#*/ 0, /*$*/ 0, /*%*/ 6, /*&*/ 7, /*'*/ 3, /*(*/ 4, /*)*/ 4, /***/ 0, /*+*/ 6, /*,*/ 2, /*-*/ 4, /*.*/ 3, /*/*/ 4, /*0*/ 5,
    /*1*/ 5, /*2*/ 6, /*3*/ 6, /*4*/ 6, /*5*/ 6, /*6*/ 6, /*7*/ 6, /*8*/ 6, /*9*/ 6, /*:*/ 4, /*;*/ 4, /*<*/ 0, /*=*/ 0, /*>*/ 0, /*?*/ 5, /*@*/ 0,  
    /*A*/ 5, /*B*/ 5, /*C*/ 5, /*D*/ 5, /*E*/ 5, /*F*/ 5, /*G*/ 5, /*H*/ 5, /*I*/ 3, /*J*/ 5, /*K*/ 5, /*L*/ 5, /*M*/ 7, /*N*/ 7, /*O*/ 6, /*P*/ 5, 
    /*Q*/ 6, /*R*/ 5, /*S*/ 5, /*T*/ 5, /*U*/ 5, /*V*/ 5, /*W*/ 7, /*X*/ 6, /*Y*/ 5, /*Z*/ 5, /*[*/ 0, /*\\*/ 0, /*]*/ 0, /*^*/ 7, /*_*/ 0, /*`*/ 0, 
    /*a*/ 5, /*b*/ 4, /*c*/ 4, /*d*/ 4, /*e*/ 4, /*f*/ 5, /*g*/ 5, /*h*/ 4, /*i*/ 3, /*j*/ 4, /*k*/ 3, /*l*/ 2, /*m*/ 6, /*n*/ 4, /*o*/ 4, /*p*/ 4, 
    /*q*/ 5, /*r*/ 4, /*s*/ 4, /*t*/ 4, /*u*/ 4, /*v*/ 4, /*w*/ 7, /*x*/ 5, /*y*/ 4, /*z*/ 5, /*{*/ 0, /*|*/ 0, /*}*/ 0, /*~*/ 6,   
};

const u16 small_font_offsets_vanilla[] = {
    /*!*/ 0, /*"*/ 8, /*#*/ 16, /*$*/ 24, /*%*/ 32, /*&*/ 40, /*'*/ 48, /*(*/ 56, /*)*/ 64, /***/ 72, /*+*/ 80, /*,*/ 88, /*-*/ 96, /*.*/ 104, /*/*/ 112,
    /*0*/ 120, /*1*/ 128, /*2*/ 136, /*3*/ 144, /*4*/ 152, /*5*/ 160, /*6*/ 168, /*7*/ 176, /*8*/ 184, /*9*/ 192, /*:*/ 200, /*;*/ 208, /*<*/ 216, /*=*/ 216,
    /*>*/ 216, /*?*/ 216, /*@*/ 224, /*A*/ 224, /*B*/ 232, /*C*/ 240, /*D*/ 248, /*E*/ 256, /*F*/ 264, /*G*/ 272, /*H*/ 280, /*I*/ 288, /*J*/ 296, /*K*/ 304,
    /*L*/ 312, /*M*/ 320, /*N*/ 328, /*O*/ 336, /*P*/ 344, /*Q*/ 352, /*R*/ 360, /*S*/ 368, /*T*/ 376, /*U*/ 384, /*V*/ 392, /*W*/ 400, /*X*/ 408, /*Y*/ 416,
    /*Z*/ 424, /*[*/ 432, /*\*/ 432, /*]*/ 432, /*^*/ 432, /*_*/ 440, /*`*/ 440, /*a*/ 440, /*b*/ 448, /*c*/ 456, /*d*/ 464, /*e*/ 472, /*f*/ 480, /*g*/ 488, 
    /*h*/ 496, /*i*/ 504, /*j*/ 512, /*k*/ 520, /*l*/ 528, /*m*/ 536, /*n*/ 544, /*o*/ 552, /*p*/ 560, /*q*/ 568, /*r*/ 576, /*s*/ 584, /*t*/ 592, /*u*/ 600, 
    /*v*/ 608, /*w*/ 616, /*x*/ 624, /*y*/ 632, /*z*/ 640, /*{*/ 648, /*|*/ 648, /*}*/ 648, /*~*/ 648, /*:)*/ 656
};

const struct PPTextFont sPPFont_default = {
    small_font_default,
    small_font_kerning_default,
    small_font_offsets_default,
    NULL,
    G_IM_FMT_I, G_IM_SIZ_4b,
    672, 12,
    8, 12
};

const struct PPTextFont sPPFont_outline = {
    small_font_outline,
    small_font_kerning_outline,
    small_font_offsets_outline,
    NULL,
    G_IM_FMT_IA, G_IM_SIZ_4b,
    672, 12,
    8, 12
};

const struct PPTextFont sPPFont_plain = {
    small_font_plain,
    small_font_kerning_plain,
    small_font_offsets_plain,
    NULL,
    G_IM_FMT_I, G_IM_SIZ_4b,
    672, 12,
    8, 12
};

const struct PPTextFont sPPFont_vanilla = {
    small_font_vanilla,
    small_font_kerning_vanilla,
    small_font_offsets_vanilla,
    NULL,
    G_IM_FMT_I, G_IM_SIZ_4b,
    672, 12,
    8, 12
};

const struct PPTextFont *const gPuppyPrintFontTable[] = {
    &sPPFont_default, &sPPFont_outline, &sPPFont_plain, &sPPFont_vanilla
};


const Texture *const puppyprint_font_lut[] = {
    small_font_default, small_font_outline, small_font_plain, small_font_vanilla
};

const u8 *const puppyprint_kerning_lut[][95] = {
    small_font_kerning_default, small_font_kerning_outline, small_font_kerning_plain, small_font_kerning_vanilla
};

#endif

ALIGNED8 static const Texture texture_hud_char_0[] = {
#include "textures/segment2/segment2.00000.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_1[] = {
#include "textures/segment2/segment2.00200.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_2[] = {
#include "textures/segment2/segment2.00400.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_3[] = {
#include "textures/segment2/segment2.00600.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_4[] = {
#include "textures/segment2/segment2.00800.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_5[] = {
#include "textures/segment2/segment2.00A00.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_6[] = {
#include "textures/segment2/segment2.00C00.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_7[] = {
#include "textures/segment2/segment2.00E00.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_8[] = {
#include "textures/segment2/segment2.01000.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_9[] = {
#include "textures/segment2/segment2.01200.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_A[] = {
#include "textures/segment2/segment2.01400.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_B[] = {
#include "textures/segment2/segment2.01600.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_C[] = {
#include "textures/segment2/segment2.01800.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_D[] = {
#include "textures/segment2/segment2.01A00.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_E[] = {
#include "textures/segment2/segment2.01C00.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_F[] = {
#include "textures/segment2/segment2.01E00.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_G[] = {
#include "textures/segment2/segment2.02000.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_H[] = {
#include "textures/segment2/segment2.02200.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_I[] = {
#include "textures/segment2/segment2.02400.rgba16.inc.c"
};

#if defined(VERSION_JP) || defined(VERSION_SH) || defined(COMPLETE_EN_US_SEGMENT2)
ALIGNED8 static const Texture texture_hud_char_J[] = {
#include "textures/segment2/segment2.02600.rgba16.inc.c"
};
#else
ALIGNED8 static const Texture texture_hud_char_J[] = {
#include "textures/segment2/segment2.hud_char_j.rgba16.inc.c"
};
#endif

ALIGNED8 static const Texture texture_hud_char_K[] = {
#include "textures/segment2/segment2.02800.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_L[] = {
#include "textures/segment2/segment2.02A00.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_M[] = {
#include "textures/segment2/segment2.02C00.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_N[] = {
#include "textures/segment2/segment2.02E00.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_O[] = {
#include "textures/segment2/segment2.03000.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_P[] = {
#include "textures/segment2/segment2.03200.rgba16.inc.c"
};

#if defined(VERSION_JP) || defined(VERSION_SH) || defined(COMPLETE_EN_US_SEGMENT2)
ALIGNED8 static const Texture texture_hud_char_Q[] = {
#include "textures/segment2/segment2.03400.rgba16.inc.c"
};
#else
ALIGNED8 static const Texture texture_hud_char_Q[] = {
#include "textures/segment2/segment2.hud_char_q.rgba16.inc.c"
};
#endif

ALIGNED8 static const Texture texture_hud_char_R[] = {
#include "textures/segment2/segment2.03600.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_S[] = {
#include "textures/segment2/segment2.03800.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_T[] = {
#include "textures/segment2/segment2.03A00.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_U[] = {
#include "textures/segment2/segment2.03C00.rgba16.inc.c"
};

#if defined(VERSION_JP) || defined(VERSION_EU) || defined(VERSION_SH) || defined(COMPLETE_EN_US_SEGMENT2)
ALIGNED8 static const Texture texture_hud_char_V[] = {
#include "textures/segment2/segment2.03E00.rgba16.inc.c"
};
#else
ALIGNED8 static const Texture texture_hud_char_V[] = {
#include "textures/segment2/segment2.hud_char_v.rgba16.inc.c"
};
#endif

ALIGNED8 static const Texture texture_hud_char_W[] = {
#include "textures/segment2/segment2.04000.rgba16.inc.c"
};

#if defined(VERSION_JP) || defined(VERSION_SH) || defined(COMPLETE_EN_US_SEGMENT2)
ALIGNED8 static const Texture texture_hud_char_X[] = {
#include "textures/segment2/segment2.04200.rgba16.inc.c"
};
#else
ALIGNED8 static const Texture texture_hud_char_X[] = {
#include "textures/segment2/segment2.hud_char_x.rgba16.inc.c"
};
#endif

ALIGNED8 static const Texture texture_hud_char_Y[] = {
#include "textures/segment2/segment2.04400.rgba16.inc.c"
};

#if defined(VERSION_JP) || defined(VERSION_EU) || defined(VERSION_SH) || defined(COMPLETE_EN_US_SEGMENT2)
ALIGNED8 static const Texture texture_hud_char_Z[] = {
#include "textures/segment2/segment2.04600.rgba16.inc.c"
};
#else
ALIGNED8 static const Texture texture_hud_char_Z[] = {
#include "textures/segment2/segment2.hud_char_z.rgba16.inc.c"
};
#endif

ALIGNED8 static const Texture texture_hud_char_apostrophe[] = {
#include "textures/segment2/segment2.04800.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_double_quote[] = {
#include "textures/segment2/segment2.04A00.rgba16.inc.c"
};

#if defined(VERSION_EU) || defined(COMPLETE_EN_US_SEGMENT2)
ALIGNED8 static const Texture texture_hud_char_umlaut[] = {
#include "textures/segment2/segment2.umlaut.rgba16.inc.c"// EU ¨
};
#else
ALIGNED8 static const Texture texture_hud_char_umlaut[] = {
#include "textures/segment2/segment2.umlaut_us.rgba16.inc.c"// EU ¨
};
#endif

#if defined(VERSION_JP) || defined(VERSION_SH) || defined(COMPLETE_EN_US_SEGMENT2)
ALIGNED8 static const Texture texture_hud_char_exclamation[] = {
#include "textures/segment2/segment2.04C00.rgba16.inc.c"// JP !
};

ALIGNED8 static const Texture texture_hud_char_double_exclamation[] = {
#include "textures/segment2/segment2.04E00.rgba16.inc.c"// JP !!
};

ALIGNED8 static const Texture texture_hud_char_question[] = {
#include "textures/segment2/segment2.05000.rgba16.inc.c"// JP ?
};

ALIGNED8 static const Texture texture_hud_char_ampersand[] = {
#include "textures/segment2/segment2.05200.rgba16.inc.c"// JP &
};

ALIGNED8 static const Texture texture_hud_char_percent[] = {
#include "textures/segment2/segment2.05400.rgba16.inc.c"// JP %
};
#else
ALIGNED8 static const Texture texture_hud_char_exclamation[] = {
#include "textures/segment2/segment2.exclamation.rgba16.inc.c"// JP !
};

ALIGNED8 static const Texture texture_hud_char_double_exclamation[] = {
#include "textures/segment2/segment2.double_exclamation.rgba16.inc.c"// JP !!
};

ALIGNED8 static const Texture texture_hud_char_question[] = {
#include "textures/segment2/segment2.question.rgba16.inc.c"// JP ?
// #include "levels/menu/main_menu_seg7.0A1D0.rgba16.png"
};

ALIGNED8 static const Texture texture_hud_char_ampersand[] = {
#include "textures/segment2/segment2.ampersand.rgba16.inc.c"// JP &
};

ALIGNED8 static const Texture texture_hud_char_percent[] = {
#include "textures/segment2/segment2.percent.rgba16.inc.c"// JP %
};
#endif

ALIGNED8 static const Texture texture_hud_char_minus[] = {
#include "textures/segment2/segment2.minus.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_multiply[] = {
#include "textures/segment2/segment2.05600.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_coin[] = {
#include "textures/segment2/segment2.05800.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_red_coin[] = {
#include "textures/segment2/segment2.red_coin.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_silver_coin[] = {
#include "textures/segment2/segment2.silver_coin.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_mario_head[] = {
#include "textures/segment2/segment2.05A00.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_star[] = {
#include "textures/segment2/segment2.05C00.rgba16.inc.c"
};

#if defined(VERSION_JP) || defined(VERSION_SH) || defined(COMPLETE_EN_US_SEGMENT2)
ALIGNED8 static const Texture texture_hud_char_decimal_point[] = {
#include "textures/segment2/segment2.05E00.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_beta_key[] = {
#include "textures/segment2/segment2.06000.rgba16.inc.c"
};
#else
ALIGNED8 static const Texture texture_hud_char_decimal_point[] = {
#include "textures/segment2/segment2.decimal_point.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_beta_key[] = {
#include "textures/segment2/segment2.beta_key.rgba16.inc.c"
};
#endif

ALIGNED8 static const Texture texture_credits_char_3[] = {
#include "textures/segment2/segment2.06200.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_4[] = {
#include "textures/segment2/segment2.06280.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_6[] = {
#include "textures/segment2/segment2.06300.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_A[] = {
#include "textures/segment2/segment2.06380.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_B[] = {
#include "textures/segment2/segment2.06400.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_C[] = {
#include "textures/segment2/segment2.06480.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_D[] = {
#include "textures/segment2/segment2.06500.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_E[] = {
#include "textures/segment2/segment2.06580.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_F[] = {
#include "textures/segment2/segment2.06600.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_G[] = {
#include "textures/segment2/segment2.06680.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_H[] = {
#include "textures/segment2/segment2.06700.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_I[] = {
#include "textures/segment2/segment2.06780.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_J[] = {
#include "textures/segment2/segment2.06800.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_K[] = {
#include "textures/segment2/segment2.06880.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_L[] = {
#include "textures/segment2/segment2.06900.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_M[] = {
#include "textures/segment2/segment2.06980.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_N[] = {
#include "textures/segment2/segment2.06A00.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_O[] = {
#include "textures/segment2/segment2.06A80.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_P[] = {
#include "textures/segment2/segment2.06B00.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_Q[] = {
#include "textures/segment2/segment2.06B80.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_R[] = {
#include "textures/segment2/segment2.06C00.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_S[] = {
#include "textures/segment2/segment2.06C80.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_T[] = {
#include "textures/segment2/segment2.06D00.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_U[] = {
#include "textures/segment2/segment2.06D80.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_V[] = {
#include "textures/segment2/segment2.06E00.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_W[] = {
#include "textures/segment2/segment2.06E80.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_X[] = {
#include "textures/segment2/segment2.06F00.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_Y[] = {
#include "textures/segment2/segment2.06F80.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_Z[] = {
#include "textures/segment2/segment2.07000.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_credits_char_period[] = {
#include "textures/segment2/segment2.07080.rgba16.inc.c"
};

// JP Small Font
#if defined(VERSION_JP) || defined(VERSION_SH)
ALIGNED8 static const Texture texture_font_char_jp_0[] = {
#include "textures/segment2/segment2.07100.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_1[] = {
#include "textures/segment2/segment2.07110.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_2[] = {
#include "textures/segment2/segment2.07120.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_3[] = {
#include "textures/segment2/segment2.07130.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_4[] = {
#include "textures/segment2/segment2.07140.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_5[] = {
#include "textures/segment2/segment2.07150.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_6[] = {
#include "textures/segment2/segment2.07160.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_7[] = {
#include "textures/segment2/segment2.07170.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_8[] = {
#include "textures/segment2/segment2.07180.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_9[] = {
#include "textures/segment2/segment2.07190.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_A[] = {
#include "textures/segment2/segment2.071A0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_B[] = {
#include "textures/segment2/segment2.071B0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_C[] = {
#include "textures/segment2/segment2.071C0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_D[] = {
#include "textures/segment2/segment2.071D0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_E[] = {
#include "textures/segment2/segment2.071E0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_F[] = {
#include "textures/segment2/segment2.071F0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_G[] = {
#include "textures/segment2/segment2.07200.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_H[] = {
#include "textures/segment2/segment2.07210.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_I[] = {
#include "textures/segment2/segment2.07220.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_J[] = {
#include "textures/segment2/segment2.07230.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_K[] = {
#include "textures/segment2/segment2.07240.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_L[] = {
#include "textures/segment2/segment2.07250.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_M[] = {
#include "textures/segment2/segment2.07260.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_N[] = {
#include "textures/segment2/segment2.07270.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_O[] = {
#include "textures/segment2/segment2.07280.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_P[] = {
#include "textures/segment2/segment2.07290.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_Q[] = {
#include "textures/segment2/segment2.072A0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_R[] = {
#include "textures/segment2/segment2.072B0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_S[] = {
#include "textures/segment2/segment2.072C0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_T[] = {
#include "textures/segment2/segment2.072D0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_U[] = {
#include "textures/segment2/segment2.072E0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_V[] = {
#include "textures/segment2/segment2.072F0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_W[] = {
#include "textures/segment2/segment2.07300.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_X[] = {
#include "textures/segment2/segment2.07310.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_Y[] = {
#include "textures/segment2/segment2.07320.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_Z[] = {
#include "textures/segment2/segment2.07330.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_long_vowel[] = {
#include "textures/segment2/segment2.07340.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_exclamation[] = {
#include "textures/segment2/segment2.07350.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_dakuten[] = {
#include "textures/segment2/segment2.07360.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_handakuten[] = {
#include "textures/segment2/segment2.07370.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_percent[] = {
#include "textures/segment2/segment2.07380.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_question[] = {
#include "textures/segment2/segment2.07390.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_left_right_arrow[] = {
#include "textures/segment2/segment2.073A0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_open_parentheses[] = {
#include "textures/segment2/segment2.073B0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_close_open_parentheses[] = {
#include "textures/segment2/segment2.073C0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_close_parentheses[] = {
#include "textures/segment2/segment2.073D0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_coin[] = {
#include "textures/segment2/segment2.073E0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_multiply[] = {
#include "textures/segment2/segment2.073F0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_star_filled[] = {
#include "textures/segment2/segment2.07400.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_star_hollow[] = {
#include "textures/segment2/segment2.07410.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_a[] = {
#include "textures/segment2/segment2.07420.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_i[] = {
#include "textures/segment2/segment2.07430.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_u[] = {
#include "textures/segment2/segment2.07440.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_c[] = {
#include "textures/segment2/segment2.07450.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_o[] = {
#include "textures/segment2/segment2.07460.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_ka[] = {
#include "textures/segment2/segment2.07470.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_ki[] = {
#include "textures/segment2/segment2.07480.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_ku[] = {
#include "textures/segment2/segment2.07490.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_ke[] = {
#include "textures/segment2/segment2.074A0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_ko[] = {
#include "textures/segment2/segment2.074B0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_sa[] = {
#include "textures/segment2/segment2.074C0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_shi[] = {
#include "textures/segment2/segment2.074D0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_su[] = {
#include "textures/segment2/segment2.074E0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_se[] = {
#include "textures/segment2/segment2.074F0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_so[] = {
#include "textures/segment2/segment2.07500.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_ta[] = {
#include "textures/segment2/segment2.07510.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_chi[] = {
#include "textures/segment2/segment2.07520.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_tsu[] = {
#include "textures/segment2/segment2.07530.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_te[] = {
#include "textures/segment2/segment2.07540.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_to[] = {
#include "textures/segment2/segment2.07550.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_na[] = {
#include "textures/segment2/segment2.07560.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_ni[] = {
#include "textures/segment2/segment2.07570.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_nu[] = {
#include "textures/segment2/segment2.07580.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_ne[] = {
#include "textures/segment2/segment2.07590.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_no[] = {
#include "textures/segment2/segment2.075A0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_ha[] = {
#include "textures/segment2/segment2.075B0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_hi[] = {
#include "textures/segment2/segment2.075C0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_hu[] = {
#include "textures/segment2/segment2.075D0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_he[] = {
#include "textures/segment2/segment2.075E0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_ho[] = {
#include "textures/segment2/segment2.075F0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_ma[] = {
#include "textures/segment2/segment2.07600.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_mi[] = {
#include "textures/segment2/segment2.07610.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_mu[] = {
#include "textures/segment2/segment2.07620.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_me[] = {
#include "textures/segment2/segment2.07630.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_mo[] = {
#include "textures/segment2/segment2.07640.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_ya[] = {
#include "textures/segment2/segment2.07650.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_yu[] = {
#include "textures/segment2/segment2.07660.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_yo[] = {
#include "textures/segment2/segment2.07670.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_ra[] = {
#include "textures/segment2/segment2.07680.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_ri[] = {
#include "textures/segment2/segment2.07690.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_ru[] = {
#include "textures/segment2/segment2.076A0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_re[] = {
#include "textures/segment2/segment2.076B0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_ro[] = {
#include "textures/segment2/segment2.076C0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_wa[] = {
#include "textures/segment2/segment2.076D0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_wo[] = {
#include "textures/segment2/segment2.076E0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_n[] = {
#include "textures/segment2/segment2.076F0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_small_e[] = {
#include "textures/segment2/segment2.07700.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_small_tsu[] = {
#include "textures/segment2/segment2.07710.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_small_ya[] = {
#include "textures/segment2/segment2.07720.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_small_yu[] = {
#include "textures/segment2/segment2.07730.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_small_yo[] = {
#include "textures/segment2/segment2.07740.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_small_a[] = {
#include "textures/segment2/segment2.07750.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_small_i[] = {
#include "textures/segment2/segment2.07760.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_small_o[] = {
#include "textures/segment2/segment2.07770.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_hiragana_small_u[] = {
#include "textures/segment2/segment2.07780.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_interpunct[] = {
#include "textures/segment2/segment2.07790.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_comma[] = {
#include "textures/segment2/segment2.077A0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_a[] = {
#include "textures/segment2/segment2.077B0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_i[] = {
#include "textures/segment2/segment2.077C0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_u[] = {
#include "textures/segment2/segment2.077D0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_e[] = {
#include "textures/segment2/segment2.077E0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_o[] = {
#include "textures/segment2/segment2.077F0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_ka[] = {
#include "textures/segment2/segment2.07800.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_ki[] = {
#include "textures/segment2/segment2.07810.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_ku[] = {
#include "textures/segment2/segment2.07820.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_ke[] = {
#include "textures/segment2/segment2.07830.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_ko[] = {
#include "textures/segment2/segment2.07840.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_sa[] = {
#include "textures/segment2/segment2.07850.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_shi[] = {
#include "textures/segment2/segment2.07860.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_su[] = {
#include "textures/segment2/segment2.07870.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_se[] = {
#include "textures/segment2/segment2.07880.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_so[] = {
#include "textures/segment2/segment2.07890.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_ta[] = {
#include "textures/segment2/segment2.078A0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_chi[] = {
#include "textures/segment2/segment2.078B0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_tsu[] = {
#include "textures/segment2/segment2.078C0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_te[] = {
#include "textures/segment2/segment2.078D0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_to[] = {
#include "textures/segment2/segment2.078E0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_na[] = {
#include "textures/segment2/segment2.078F0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_ni[] = {
#include "textures/segment2/segment2.07900.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_nu[] = {
#include "textures/segment2/segment2.07910.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_ne[] = {
#include "textures/segment2/segment2.07920.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_no[] = {
#include "textures/segment2/segment2.07930.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_ha[] = {
#include "textures/segment2/segment2.07940.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_hi[] = {
#include "textures/segment2/segment2.07950.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_hu[] = {
#include "textures/segment2/segment2.07960.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_he[] = {
#include "textures/segment2/segment2.07970.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_ho[] = {
#include "textures/segment2/segment2.07980.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_ma[] = {
#include "textures/segment2/segment2.07990.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_mi[] = {
#include "textures/segment2/segment2.079A0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_mu[] = {
#include "textures/segment2/segment2.079B0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_me[] = {
#include "textures/segment2/segment2.079C0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_mo[] = {
#include "textures/segment2/segment2.079D0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_ya[] = {
#include "textures/segment2/segment2.079E0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_yu[] = {
#include "textures/segment2/segment2.079F0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_yo[] = {
#include "textures/segment2/segment2.07A00.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_ra[] = {
#include "textures/segment2/segment2.07A10.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_ri[] = {
#include "textures/segment2/segment2.07A20.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_ru[] = {
#include "textures/segment2/segment2.07A30.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_re[] = {
#include "textures/segment2/segment2.07A40.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_ro[] = {
#include "textures/segment2/segment2.07A50.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_wa[] = {
#include "textures/segment2/segment2.07A60.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_n[] = {
#include "textures/segment2/segment2.07A70.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_small_e[] = {
#include "textures/segment2/segment2.07A80.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_small_tsu[] = {
#include "textures/segment2/segment2.07A90.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_small_ya[] = {
#include "textures/segment2/segment2.07AA0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_small_yu[] = {
#include "textures/segment2/segment2.07AB0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_small_yo[] = {
#include "textures/segment2/segment2.07AC0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_small_a[] = {
#include "textures/segment2/segment2.07AD0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_small_i[] = {
#include "textures/segment2/segment2.07AE0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_small_u[] = {
#include "textures/segment2/segment2.07AF0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_katakana_small_o[] = {
#include "textures/segment2/segment2.07B00.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_double_quotation_open[] = {
#include "textures/segment2/segment2.07B10.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_double_quotation_close[] = {
#include "textures/segment2/segment2.07B20.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_wave_dash[] = {
#include "textures/segment2/segment2.07B30.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_jp_ellipsis[] = {
#include "textures/segment2/segment2.07B40.ia1.inc.c"
};

// EU Small Font
#elif defined(VERSION_EU)

ALIGNED8 static const Texture texture_font_char_eu_0[] = {
#include "textures/segment2/font_graphics.05F00.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_1[] = {
#include "textures/segment2/font_graphics.05F10.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_2[] = {
#include "textures/segment2/font_graphics.05F20.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_3[] = {
#include "textures/segment2/font_graphics.05F30.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_4[] = {
#include "textures/segment2/font_graphics.05F40.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_5[] = {
#include "textures/segment2/font_graphics.05F50.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_6[] = {
#include "textures/segment2/font_graphics.05F60.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_7[] = {
#include "textures/segment2/font_graphics.05F70.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_8[] = {
#include "textures/segment2/font_graphics.05F80.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_9[] = {
#include "textures/segment2/font_graphics.05F90.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_A[] = {
#include "textures/segment2/font_graphics.05FA0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_B[] = {
#include "textures/segment2/font_graphics.05FB0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_C[] = {
#include "textures/segment2/font_graphics.05FC0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_D[] = {
#include "textures/segment2/font_graphics.05FD0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_E[] = {
#include "textures/segment2/font_graphics.05FE0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_F[] = {
#include "textures/segment2/font_graphics.05FF0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_G[] = {
#include "textures/segment2/font_graphics.06000.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_H[] = {
#include "textures/segment2/font_graphics.06010.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_I[] = {
#include "textures/segment2/font_graphics.06020.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_J[] = {
#include "textures/segment2/font_graphics.06030.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_K[] = {
#include "textures/segment2/font_graphics.06040.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_L[] = {
#include "textures/segment2/font_graphics.06050.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_M[] = {
#include "textures/segment2/font_graphics.06060.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_N[] = {
#include "textures/segment2/font_graphics.06070.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_O[] = {
#include "textures/segment2/font_graphics.06080.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_P[] = {
#include "textures/segment2/font_graphics.06090.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_Q[] = {
#include "textures/segment2/font_graphics.060A0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_R[] = {
#include "textures/segment2/font_graphics.060B0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_S[] = {
#include "textures/segment2/font_graphics.060C0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_T[] = {
#include "textures/segment2/font_graphics.060D0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_U[] = {
#include "textures/segment2/font_graphics.060E0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_V[] = {
#include "textures/segment2/font_graphics.060F0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_W[] = {
#include "textures/segment2/font_graphics.06100.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_X[] = {
#include "textures/segment2/font_graphics.06110.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_Y[] = {
#include "textures/segment2/font_graphics.06120.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_Z[] = {
#include "textures/segment2/font_graphics.06130.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_a[] = {
#include "textures/segment2/font_graphics.06140.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_b[] = {
#include "textures/segment2/font_graphics.06150.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_c[] = {
#include "textures/segment2/font_graphics.06160.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_d[] = {
#include "textures/segment2/font_graphics.06170.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_e[] = {
#include "textures/segment2/font_graphics.06180.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_f[] = {
#include "textures/segment2/font_graphics.06190.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_g[] = {
#include "textures/segment2/font_graphics.061A0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_h[] = {
#include "textures/segment2/font_graphics.061B0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_i[] = {
#include "textures/segment2/font_graphics.061C0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_j[] = {
#include "textures/segment2/font_graphics.061D0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_k[] = {
#include "textures/segment2/font_graphics.061E0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_l[] = {
#include "textures/segment2/font_graphics.061F0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_m[] = {
#include "textures/segment2/font_graphics.06200.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_n[] = {
#include "textures/segment2/font_graphics.06210.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_o[] = {
#include "textures/segment2/font_graphics.06220.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_p[] = {
#include "textures/segment2/font_graphics.06230.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_q[] = {
#include "textures/segment2/font_graphics.06240.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_r[] = {
#include "textures/segment2/font_graphics.06250.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_s[] = {
#include "textures/segment2/font_graphics.06260.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_t[] = {
#include "textures/segment2/font_graphics.06270.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_u[] = {
#include "textures/segment2/font_graphics.06280.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_v[] = {
#include "textures/segment2/font_graphics.06290.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_w[] = {
#include "textures/segment2/font_graphics.062A0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_x[] = {
#include "textures/segment2/font_graphics.062B0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_y[] = {
#include "textures/segment2/font_graphics.062C0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_z[] = {
#include "textures/segment2/font_graphics.062D0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_left_right_arrow[] = {
#include "textures/segment2/font_graphics.062E0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_ampersand[] = {
#include "textures/segment2/font_graphics.062F0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_exclamation[] = {
#include "textures/segment2/font_graphics.06300.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_coin[] = {
#include "textures/segment2/font_graphics.06310.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_multiply[] = {
#include "textures/segment2/font_graphics.06320.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_open_parentheses[] = {
#include "textures/segment2/font_graphics.06330.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_close_open_parentheses[] = {
#include "textures/segment2/font_graphics.06340.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_close_parentheses[] = {
#include "textures/segment2/font_graphics.06350.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_tilde[] = {
#include "textures/segment2/font_graphics.06360.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_period[] = {
#include "textures/segment2/font_graphics.06370.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_percent[] = {
#include "textures/segment2/font_graphics.06380.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_interpunct[] = {
#include "textures/segment2/font_graphics.06390.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_comma[] = {
#include "textures/segment2/font_graphics.063A0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_apostrophe[] = {
#include "textures/segment2/font_graphics.063B0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_question[] = {
#include "textures/segment2/font_graphics.063C0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_star_filled[] = {
#include "textures/segment2/font_graphics.063D0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_star_hollow[] = {
#include "textures/segment2/font_graphics.063E0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_double_quote_open[] = {
#include "textures/segment2/font_graphics.063F0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_double_quote_close[] = {
#include "textures/segment2/font_graphics.06400.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_colon[] = {
#include "textures/segment2/font_graphics.06410.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_EU_slash[] = {
#include "textures/segment2/font_graphics.06420.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_button_A[] = {
#include "textures/segment2/font_graphics.06430.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_button_B[] = {
#include "textures/segment2/font_graphics.06440.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_button_C[] = {
#include "textures/segment2/font_graphics.06450.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_button_Z[] = {
#include "textures/segment2/font_graphics.06460.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_button_R[] = {
#include "textures/segment2/font_graphics.06470.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_button_C_up[] = {
#include "textures/segment2/font_graphics.06480.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_button_C_down[] = {
#include "textures/segment2/font_graphics.06490.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_button_C_left[] = {
#include "textures/segment2/font_graphics.064A0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_button_C_right[] = {
#include "textures/segment2/font_graphics.064B0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_umlaut[] = {
#include "textures/segment2/font_graphics.064C0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_circumflex[] = {
#include "textures/segment2/font_graphics.064D0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_grave[] = {
#include "textures/segment2/font_graphics.064E0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_acute[] = {
#include "textures/segment2/font_graphics.064F0.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_cedilla[] = {
#include "textures/segment2/font_graphics.06500.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_unknown[] = {
#include "textures/segment2/font_graphics.06510.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_Cedilla[] = {
#include "textures/segment2/font_graphics.06520.ia1.inc.c"
};

ALIGNED8 static const Texture texture_font_char_eu_eszeet[] = {
#include "textures/segment2/font_graphics.06530.ia1.inc.c"
};

// US Small Font
#else
ALIGNED8 static const Texture texture_font_char_us_0[] = {
#include "textures/segment2/font_graphics.05900.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_1[] = {
#include "textures/segment2/font_graphics.05940.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_2[] = {
#include "textures/segment2/font_graphics.05980.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_3[] = {
#include "textures/segment2/font_graphics.059C0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_4[] = {
#include "textures/segment2/font_graphics.05A00.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_5[] = {
#include "textures/segment2/font_graphics.05A40.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_6[] = {
#include "textures/segment2/font_graphics.05A80.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_7[] = {
#include "textures/segment2/font_graphics.05AC0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_8[] = {
#include "textures/segment2/font_graphics.05B00.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_9[] = {
#include "textures/segment2/font_graphics.05B40.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_A[] = {
#include "textures/segment2/font_graphics.05B80.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_B[] = {
#include "textures/segment2/font_graphics.05BC0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_C[] = {
#include "textures/segment2/font_graphics.05C00.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_D[] = {
#include "textures/segment2/font_graphics.05C40.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_E[] = {
#include "textures/segment2/font_graphics.05C80.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_F[] = {
#include "textures/segment2/font_graphics.05CC0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_G[] = {
#include "textures/segment2/font_graphics.05D00.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_H[] = {
#include "textures/segment2/font_graphics.05D40.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_I[] = {
#include "textures/segment2/font_graphics.05D80.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_J[] = {
#include "textures/segment2/font_graphics.05DC0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_K[] = {
#include "textures/segment2/font_graphics.05E00.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_L[] = {
#include "textures/segment2/font_graphics.05E40.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_M[] = {
#include "textures/segment2/font_graphics.05E80.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_N[] = {
#include "textures/segment2/font_graphics.05EC0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_O[] = {
#include "textures/segment2/font_graphics.05F00.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_P[] = {
#include "textures/segment2/font_graphics.05F40.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_Q[] = {
#include "textures/segment2/font_graphics.05F80.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_R[] = {
#include "textures/segment2/font_graphics.05FC0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_S[] = {
#include "textures/segment2/font_graphics.06000.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_T[] = {
#include "textures/segment2/font_graphics.06040.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_U[] = {
#include "textures/segment2/font_graphics.06080.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_V[] = {
#include "textures/segment2/font_graphics.060C0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_W[] = {
#include "textures/segment2/font_graphics.06100.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_X[] = {
#include "textures/segment2/font_graphics.06140.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_Y[] = {
#include "textures/segment2/font_graphics.06180.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_Z[] = {
#include "textures/segment2/font_graphics.061C0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_a[] = {
#include "textures/segment2/font_graphics.06200.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_b[] = {
#include "textures/segment2/font_graphics.06240.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_c[] = {
#include "textures/segment2/font_graphics.06280.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_d[] = {
#include "textures/segment2/font_graphics.062C0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_e[] = {
#include "textures/segment2/font_graphics.06300.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_f[] = {
#include "textures/segment2/font_graphics.06340.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_g[] = {
#include "textures/segment2/font_graphics.06380.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_h[] = {
#include "textures/segment2/font_graphics.063C0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_i[] = {
#include "textures/segment2/font_graphics.06400.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_j[] = {
#include "textures/segment2/font_graphics.06440.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_k[] = {
#include "textures/segment2/font_graphics.06480.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_l[] = {
#include "textures/segment2/font_graphics.064C0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_m[] = {
#include "textures/segment2/font_graphics.06500.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_n[] = {
#include "textures/segment2/font_graphics.06540.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_o[] = {
#include "textures/segment2/font_graphics.06580.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_p[] = {
#include "textures/segment2/font_graphics.065C0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_q[] = {
#include "textures/segment2/font_graphics.06600.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_r[] = {
#include "textures/segment2/font_graphics.06640.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_s[] = {
#include "textures/segment2/font_graphics.06680.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_t[] = {
#include "textures/segment2/font_graphics.066C0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_u[] = {
#include "textures/segment2/font_graphics.06700.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_v[] = {
#include "textures/segment2/font_graphics.06740.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_w[] = {
#include "textures/segment2/font_graphics.06780.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_x[] = {
#include "textures/segment2/font_graphics.067C0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_y[] = {
#include "textures/segment2/font_graphics.06800.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_z[] = {
#include "textures/segment2/font_graphics.06840.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_left_right_arrow[] = {
#include "textures/segment2/font_graphics.06880.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_exclamation[] = {
#include "textures/segment2/font_graphics.068C0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_coin[] = {
#include "textures/segment2/font_graphics.06900.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_multiply[] = {
#include "textures/segment2/font_graphics.06940.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_open_parentheses[] = {
#include "textures/segment2/font_graphics.06980.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_close_open_parentheses[] = {
#include "textures/segment2/font_graphics.069C0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_close_parentheses[] = {
#include "textures/segment2/font_graphics.06A00.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_tilde[] = {
#include "textures/segment2/font_graphics.06A40.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_period[] = {
#include "textures/segment2/font_graphics.06A80.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_percent[] = {
#include "textures/segment2/font_graphics.06AC0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_interpunct[] = {
#include "textures/segment2/font_graphics.06B00.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_comma[] = {
#include "textures/segment2/font_graphics.06B40.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_apostrophe[] = {
#include "textures/segment2/font_graphics.06B80.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_question[] = {
#include "textures/segment2/font_graphics.06BC0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_star_filled[] = {
#include "textures/segment2/font_graphics.06C00.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_star_hollow[] = {
#include "textures/segment2/font_graphics.06C40.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_double_quote_open[] = {
#include "textures/segment2/font_graphics.06C80.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_double_quote_close[] = {
#include "textures/segment2/font_graphics.06CC0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_ellipsis[] = {
#include "textures/segment2/font_graphics.06D00.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_slash[] = {
#include "textures/segment2/font_graphics.06D40.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_ampersand[] = {
#include "textures/segment2/font_graphics.06D80.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_button_A[] = {
#include "textures/segment2/font_graphics.06DC0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_button_B[] = {
#include "textures/segment2/font_graphics.06E00.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_button_C[] = {
#include "textures/segment2/font_graphics.06E40.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_button_Z[] = {
#include "textures/segment2/font_graphics.06E80.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_button_R[] = {
#include "textures/segment2/font_graphics.06EC0.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_button_C_up[] = {
#include "textures/segment2/font_graphics.06F00.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_button_C_down[] = {
#include "textures/segment2/font_graphics.06F40.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_button_C_left[] = {
#include "textures/segment2/font_graphics.06F80.ia4.inc.c"
};

ALIGNED8 static const Texture texture_font_char_us_button_C_right[] = {
#include "textures/segment2/font_graphics.06FC0.ia4.inc.c"
};
#endif

ALIGNED8 static const Texture texture_hud_char_camera[] = {
#include "textures/segment2/segment2.07B50.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_lakitu[] = {
#include "textures/segment2/segment2.07D50.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_no_camera[] = {
#include "textures/segment2/segment2.07F50.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_arrow_up[] = {
#include "textures/segment2/segment2.08150.rgba16.inc.c"
};

ALIGNED8 static const Texture texture_hud_char_arrow_down[] = {
#include "textures/segment2/segment2.081D0.rgba16.inc.c"
};

// Main HUD print table 0x02008250-0x02008337
const Texture *const main_hud_lut[] = {
    texture_hud_char_0, texture_hud_char_1, texture_hud_char_2, texture_hud_char_3,
    texture_hud_char_4, texture_hud_char_5, texture_hud_char_6, texture_hud_char_7,
    texture_hud_char_8, texture_hud_char_9, texture_hud_char_A, texture_hud_char_B,
    texture_hud_char_C, texture_hud_char_D, texture_hud_char_E, texture_hud_char_F,
    texture_hud_char_G, texture_hud_char_H, texture_hud_char_I, texture_hud_char_J,
    texture_hud_char_K, texture_hud_char_L, texture_hud_char_M, texture_hud_char_N,
    texture_hud_char_O, texture_hud_char_P, texture_hud_char_Q, texture_hud_char_R,
    texture_hud_char_S, texture_hud_char_T, texture_hud_char_U, texture_hud_char_V,
    texture_hud_char_W, texture_hud_char_X, texture_hud_char_Y, texture_hud_char_Z,
    texture_hud_char_exclamation, texture_hud_char_double_exclamation, texture_hud_char_question, texture_hud_char_ampersand,
    texture_hud_char_percent,                0x0,                0x0,                0x0,
                   0x0,                0x0,                0x0, texture_hud_char_minus,
    texture_hud_char_multiply, texture_hud_char_coin, texture_hud_char_red_coin, texture_hud_char_silver_coin,
    texture_hud_char_mario_head, texture_hud_char_star, texture_hud_char_decimal_point, texture_hud_char_beta_key,
    texture_hud_char_apostrophe, texture_hud_char_double_quote, texture_hud_char_umlaut,
};

// Main small font print table 0x02008338-0x02008737
const Texture *const main_font_lut[] = {
#ifdef VERSION_EU // EU Font Table
    texture_font_char_eu_0, texture_font_char_eu_1, texture_font_char_eu_2, texture_font_char_eu_3,
    texture_font_char_eu_4, texture_font_char_eu_5, texture_font_char_eu_6, texture_font_char_eu_7,
    texture_font_char_eu_8, texture_font_char_eu_9, texture_font_char_eu_A, texture_font_char_eu_B,
    texture_font_char_eu_C, texture_font_char_eu_D, texture_font_char_eu_E, texture_font_char_eu_F,
    texture_font_char_eu_G, texture_font_char_eu_H, texture_font_char_eu_I, texture_font_char_eu_J,
    texture_font_char_eu_K, texture_font_char_eu_L, texture_font_char_eu_M, texture_font_char_eu_N,
    texture_font_char_eu_O, texture_font_char_eu_P, texture_font_char_eu_Q, texture_font_char_eu_R,
    texture_font_char_eu_S, texture_font_char_eu_T, texture_font_char_eu_U, texture_font_char_eu_V,
    texture_font_char_eu_W, texture_font_char_eu_X, texture_font_char_eu_Y, texture_font_char_eu_Z,
    texture_font_char_eu_a, texture_font_char_eu_b, texture_font_char_eu_c, texture_font_char_eu_d,
    texture_font_char_eu_e, texture_font_char_eu_f, texture_font_char_eu_g, texture_font_char_eu_h,
    texture_font_char_eu_i, texture_font_char_eu_j, texture_font_char_eu_k, texture_font_char_eu_l,
    texture_font_char_eu_m, texture_font_char_eu_n, texture_font_char_eu_o, texture_font_char_eu_p,
    texture_font_char_eu_q, texture_font_char_eu_r, texture_font_char_eu_s, texture_font_char_eu_t,
    texture_font_char_eu_u, texture_font_char_eu_v, texture_font_char_eu_w, texture_font_char_eu_x,
    texture_font_char_eu_y, texture_font_char_eu_z, texture_font_char_eu_apostrophe, texture_font_char_eu_period,
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    texture_font_char_eu_button_C_up,
    texture_font_char_eu_button_C_down,
    texture_font_char_eu_button_C_left,
    texture_font_char_eu_button_C_right,
    texture_font_char_eu_button_A,
    texture_font_char_eu_button_B,
    texture_font_char_eu_button_C,
    texture_font_char_eu_button_Z,
    texture_font_char_eu_button_R,
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    texture_font_char_eu_comma,
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    texture_font_char_EU_slash,
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0,
    texture_font_char_eu_open_parentheses,
    texture_font_char_eu_close_open_parentheses,
    texture_font_char_eu_close_parentheses,
    texture_font_char_eu_left_right_arrow,
    texture_font_char_eu_ampersand,
    texture_font_char_eu_colon,
    texture_font_char_eu_acute,
    texture_font_char_eu_circumflex,
    texture_font_char_eu_umlaut,
    texture_font_char_eu_grave,
    texture_font_char_eu_unknown,
    texture_font_char_eu_eszeet,
    texture_font_char_eu_Cedilla,
    texture_font_char_eu_cedilla,
    0x0, 0x0, 0x0,
    texture_font_char_eu_exclamation,
    texture_font_char_eu_percent,
    texture_font_char_eu_question,
    texture_font_char_eu_double_quote_open,
    texture_font_char_eu_double_quote_close,
    texture_font_char_eu_tilde,
    0x0,
    texture_font_char_eu_coin,
    texture_font_char_eu_star_filled,
    texture_font_char_eu_multiply,
    texture_font_char_eu_interpunct,
    texture_font_char_eu_star_hollow,
    0x0, 0x0,
#elif defined(VERSION_US) // US Font Table
    texture_font_char_us_0, texture_font_char_us_1, texture_font_char_us_2, texture_font_char_us_3,
    texture_font_char_us_4, texture_font_char_us_5, texture_font_char_us_6, texture_font_char_us_7,
    texture_font_char_us_8, texture_font_char_us_9, texture_font_char_us_A, texture_font_char_us_B,
    texture_font_char_us_C, texture_font_char_us_D, texture_font_char_us_E, texture_font_char_us_F,
    texture_font_char_us_G, texture_font_char_us_H, texture_font_char_us_I, texture_font_char_us_J,
    texture_font_char_us_K, texture_font_char_us_L, texture_font_char_us_M, texture_font_char_us_N,
    texture_font_char_us_O, texture_font_char_us_P, texture_font_char_us_Q, texture_font_char_us_R,
    texture_font_char_us_S, texture_font_char_us_T, texture_font_char_us_U, texture_font_char_us_V,
    texture_font_char_us_W, texture_font_char_us_X, texture_font_char_us_Y, texture_font_char_us_Z,
    texture_font_char_us_a, texture_font_char_us_b, texture_font_char_us_c, texture_font_char_us_d,
    texture_font_char_us_e, texture_font_char_us_f, texture_font_char_us_g, texture_font_char_us_h,
    texture_font_char_us_i, texture_font_char_us_j, texture_font_char_us_k, texture_font_char_us_l,
    texture_font_char_us_m, texture_font_char_us_n, texture_font_char_us_o, texture_font_char_us_p,
    texture_font_char_us_q, texture_font_char_us_r, texture_font_char_us_s, texture_font_char_us_t,
    texture_font_char_us_u, texture_font_char_us_v, texture_font_char_us_w, texture_font_char_us_x,
    texture_font_char_us_y, texture_font_char_us_z, texture_font_char_us_apostrophe, texture_font_char_us_period,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
    texture_font_char_us_button_C_up, texture_font_char_us_button_C_down, texture_font_char_us_button_C_left, texture_font_char_us_button_C_right,
    texture_font_char_us_button_A, texture_font_char_us_button_B, texture_font_char_us_button_C, texture_font_char_us_button_Z,
    texture_font_char_us_button_R,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0, texture_font_char_us_comma,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0, texture_font_char_us_slash,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0, texture_font_char_us_open_parentheses, texture_font_char_us_close_open_parentheses, texture_font_char_us_close_parentheses,
    texture_font_char_us_left_right_arrow, texture_font_char_us_ampersand, texture_font_char_us_ellipsis,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0,               0x0,               0x0,
                  0x0,               0x0, texture_font_char_us_exclamation, texture_font_char_us_percent,
    texture_font_char_us_question, texture_font_char_us_double_quote_open, texture_font_char_us_double_quote_close, texture_font_char_us_tilde,
                  0x0, texture_font_char_us_coin, texture_font_char_us_star_filled, texture_font_char_us_multiply,
    texture_font_char_us_interpunct, texture_font_char_us_star_hollow,               0x0,               0x0,
#elif defined(VERSION_JP) || defined(VERSION_SH)
    texture_font_char_jp_0, texture_font_char_jp_1, texture_font_char_jp_2, texture_font_char_jp_3,
    texture_font_char_jp_4, texture_font_char_jp_5, texture_font_char_jp_6, texture_font_char_jp_7,
    texture_font_char_jp_8, texture_font_char_jp_9, texture_font_char_jp_A, texture_font_char_jp_B,
    texture_font_char_jp_C, texture_font_char_jp_D, texture_font_char_jp_E, texture_font_char_jp_F,
    texture_font_char_jp_G, texture_font_char_jp_H, texture_font_char_jp_I, texture_font_char_jp_J,
    texture_font_char_jp_K, texture_font_char_jp_L, texture_font_char_jp_M, texture_font_char_jp_N,
    texture_font_char_jp_O, texture_font_char_jp_P, texture_font_char_jp_Q, texture_font_char_jp_R,
    texture_font_char_jp_S, texture_font_char_jp_T, texture_font_char_jp_U, texture_font_char_jp_V,
    texture_font_char_jp_W, texture_font_char_jp_X, texture_font_char_jp_Y, texture_font_char_jp_Z,
                      0x0,                   0x0,                   0x0,                   0x0,
                      0x0,                   0x0,                   0x0,                   0x0,
                      0x0,                   0x0,                   0x0,                   0x0,
                      0x0,                   0x0,                   0x0,                   0x0,
                      0x0,                   0x0,                   0x0,                   0x0,
                      0x0,                   0x0,                   0x0,                   0x0,
                      0x0,                   0x0,                   0x0,                   0x0,
    texture_font_char_jp_hiragana_a, texture_font_char_jp_hiragana_i, texture_font_char_jp_hiragana_u, texture_font_char_jp_hiragana_c,
    texture_font_char_jp_hiragana_o, texture_font_char_jp_hiragana_ka, texture_font_char_jp_hiragana_ki, texture_font_char_jp_hiragana_ku,
    texture_font_char_jp_hiragana_ke, texture_font_char_jp_hiragana_ko, texture_font_char_jp_hiragana_sa, texture_font_char_jp_hiragana_shi,
    texture_font_char_jp_hiragana_su, texture_font_char_jp_hiragana_se, texture_font_char_jp_hiragana_so, texture_font_char_jp_hiragana_ta,
    texture_font_char_jp_hiragana_chi, texture_font_char_jp_hiragana_tsu, texture_font_char_jp_hiragana_te, texture_font_char_jp_hiragana_to,
    texture_font_char_jp_hiragana_na, texture_font_char_jp_hiragana_ni, texture_font_char_jp_hiragana_nu, texture_font_char_jp_hiragana_ne,
    texture_font_char_jp_hiragana_no, texture_font_char_jp_hiragana_ha, texture_font_char_jp_hiragana_hi, texture_font_char_jp_hiragana_hu,
    texture_font_char_jp_hiragana_he, texture_font_char_jp_hiragana_ho, texture_font_char_jp_hiragana_ma, texture_font_char_jp_hiragana_mi,
    texture_font_char_jp_hiragana_mu, texture_font_char_jp_hiragana_me, texture_font_char_jp_hiragana_mo, texture_font_char_jp_hiragana_ya,
    texture_font_char_jp_hiragana_yu, texture_font_char_jp_hiragana_yo, texture_font_char_jp_hiragana_ra, texture_font_char_jp_hiragana_ri,
    texture_font_char_jp_hiragana_ru, texture_font_char_jp_hiragana_re, texture_font_char_jp_hiragana_ro, texture_font_char_jp_hiragana_wa,
    texture_font_char_jp_hiragana_wo, texture_font_char_jp_hiragana_n,                   0x0, texture_font_char_jp_comma,
    texture_font_char_jp_katakana_a, texture_font_char_jp_katakana_i, texture_font_char_jp_katakana_u, texture_font_char_jp_katakana_e,
    texture_font_char_jp_katakana_o, texture_font_char_jp_katakana_ka, texture_font_char_jp_katakana_ki, texture_font_char_jp_katakana_ku,
    texture_font_char_jp_katakana_ke, texture_font_char_jp_katakana_ko, texture_font_char_jp_katakana_sa, texture_font_char_jp_katakana_shi,
    texture_font_char_jp_katakana_su, texture_font_char_jp_katakana_se, texture_font_char_jp_katakana_so, texture_font_char_jp_katakana_ta,
    texture_font_char_jp_katakana_chi, texture_font_char_jp_katakana_tsu, texture_font_char_jp_katakana_te, texture_font_char_jp_katakana_to,
    texture_font_char_jp_katakana_na, texture_font_char_jp_katakana_ni, texture_font_char_jp_katakana_nu, texture_font_char_jp_katakana_ne,
    texture_font_char_jp_katakana_no, texture_font_char_jp_katakana_ha, texture_font_char_jp_katakana_hi, texture_font_char_jp_katakana_hu,
    texture_font_char_jp_katakana_he, texture_font_char_jp_katakana_ho, texture_font_char_jp_katakana_ma, texture_font_char_jp_katakana_mi,
    texture_font_char_jp_katakana_mu, texture_font_char_jp_katakana_me, texture_font_char_jp_katakana_mo, texture_font_char_jp_katakana_ya,
    texture_font_char_jp_katakana_yu, texture_font_char_jp_katakana_yo, texture_font_char_jp_katakana_ra, texture_font_char_jp_katakana_ri,
    texture_font_char_jp_katakana_ru, texture_font_char_jp_katakana_re, texture_font_char_jp_katakana_ro, texture_font_char_jp_katakana_wa,
                      0x0, texture_font_char_jp_katakana_n,                   0x0, texture_font_char_jp_long_vowel,
    texture_font_char_jp_hiragana_small_e, texture_font_char_jp_hiragana_small_tsu, texture_font_char_jp_hiragana_small_ya, texture_font_char_jp_hiragana_small_yu,
    texture_font_char_jp_hiragana_small_yo, texture_font_char_jp_hiragana_small_a, texture_font_char_jp_hiragana_small_i, texture_font_char_jp_hiragana_small_u,
    texture_font_char_jp_hiragana_small_o,                   0x0,                   0x0,                   0x0,
                      0x0,                   0x0,                   0x0,                   0x0,
                      0x0,                   0x0,                   0x0,                   0x0,
                      0x0,                   0x0,                   0x0,                   0x0,
                      0x0,                   0x0,                   0x0,                   0x0,
                      0x0,                   0x0,                   0x0,                   0x0,
                      0x0,                   0x0,                   0x0,                   0x0,
                      0x0,                   0x0,                   0x0,                   0x0,
                      0x0,                   0x0,                   0x0,                   0x0,
                      0x0,                   0x0,                   0x0,                   0x0,
    texture_font_char_jp_katakana_small_e, texture_font_char_jp_katakana_small_tsu, texture_font_char_jp_katakana_small_ya, texture_font_char_jp_katakana_small_yu,
    texture_font_char_jp_katakana_small_yo, texture_font_char_jp_katakana_small_a, texture_font_char_jp_katakana_small_i, texture_font_char_jp_katakana_small_u,
    texture_font_char_jp_katakana_small_o,                   0x0,                   0x0,                   0x0,
                      0x0,                   0x0,                   0x0,                   0x0,
                      0x0, texture_font_char_jp_open_parentheses, texture_font_char_jp_close_open_parentheses, texture_font_char_jp_close_parentheses,
    texture_font_char_jp_left_right_arrow,                   0x0,                   0x0,                   0x0,
                      0x0,                   0x0,                   0x0,                   0x0,
                      0x0,                   0x0,                   0x0,                   0x0,
    texture_font_char_jp_dakuten, texture_font_char_jp_handakuten, texture_font_char_jp_exclamation, texture_font_char_jp_percent,
    texture_font_char_jp_question, texture_font_char_jp_double_quotation_open, texture_font_char_jp_double_quotation_close, texture_font_char_jp_wave_dash,
    texture_font_char_jp_ellipsis, texture_font_char_jp_coin, texture_font_char_jp_star_filled, texture_font_char_jp_multiply,
    texture_font_char_jp_interpunct, texture_font_char_jp_star_hollow,                   0x0,                   0x0,
#endif
};

// credits font LUT 0x02008738-0x020087CB
const Texture *const main_credits_font_lut[] = {
                       0x0,                    0x0,                    0x0, texture_credits_char_3,
    texture_credits_char_4,                    0x0, texture_credits_char_6,                    0x0,
                       0x0,                    0x0, texture_credits_char_A, texture_credits_char_B,
    texture_credits_char_C, texture_credits_char_D, texture_credits_char_E, texture_credits_char_F,
    texture_credits_char_G, texture_credits_char_H, texture_credits_char_I, texture_credits_char_J,
    texture_credits_char_K, texture_credits_char_L, texture_credits_char_M, texture_credits_char_N,
    texture_credits_char_O, texture_credits_char_P, texture_credits_char_Q, texture_credits_char_R,
    texture_credits_char_S, texture_credits_char_T, texture_credits_char_U, texture_credits_char_V,
    texture_credits_char_W, texture_credits_char_X, texture_credits_char_Y, texture_credits_char_Z,
    texture_credits_char_period,
};

// HUD camera table 0x020087CC-0x020087E3
const Texture *const main_hud_camera_lut[] = {
    texture_hud_char_camera, texture_hud_char_mario_head, texture_hud_char_lakitu, texture_hud_char_no_camera,
    texture_hud_char_arrow_up, texture_hud_char_arrow_down,
};

// If you change the language here, the following Makefile rule also needs to
// change, to generate the right version of define_text.inc.c:
// $(BUILD_DIR)/bin/segment2.o: $(BUILD_DIR)/text/$(VERSION)/define_text.inc.c
#if defined(VERSION_JP) || defined(VERSION_SH)
#include "text/jp/define_text.inc.c"
#elif defined(VERSION_US)
#include "text/us/define_text.inc.c"
#endif

// 0x0200EC60 - 0x0200EC98
const Gfx dl_hud_img_begin[] = {
    gsDPPipeSync(),
    gsDPSetCycleType(G_CYC_COPY),
    gsDPSetTexturePersp(G_TP_NONE),
    gsDPSetAlphaCompare(G_AC_THRESHOLD),
    gsDPSetBlendColor(255, 255, 255, 255),
    gsDPSetRenderMode(G_RM_NOOP, G_RM_NOOP2),
    gsDPSetTextureFilter(G_TF_POINT),
    gsSPEndDisplayList(),
};

// 0x0200EC98 - 0x0200ECC8
const Gfx dl_hud_img_load_tex_block[] = {
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, (G_TX_WRAP | G_TX_NOMIRROR), 4, G_TX_NOLOD, (G_TX_WRAP | G_TX_NOMIRROR), 4, G_TX_NOLOD),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, ((16 * 16) - 1), CALC_DXT(16, G_IM_SIZ_16b_BYTES)),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 4, 0, G_TX_RENDERTILE, 0, (G_TX_WRAP | G_TX_NOMIRROR), 4, G_TX_NOLOD, (G_TX_WRAP | G_TX_NOMIRROR), 4, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, ((16 - 1) << G_TEXTURE_IMAGE_FRAC), ((16 - 1) << G_TEXTURE_IMAGE_FRAC)),
    gsSPEndDisplayList(),
};

// 0x0200ECC8 - 0x0200ED00
const Gfx dl_hud_img_end[] = {
    gsDPPipeSync(),
    gsDPSetTexturePersp(G_TP_PERSP),
    gsDPSetRenderMode(G_RM_AA_ZB_OPA_SURF, G_RM_AA_ZB_OPA_SURF2),
    gsDPSetAlphaCompare(G_AC_NONE),
    gsDPSetTextureFilter(G_TF_BILERP),
    gsDPSetCycleType(G_CYC_1CYCLE),
    gsSPEndDisplayList(),
};

// 0x0200ED00 - 0x0200ED38
const Gfx dl_rgba16_text_begin[] = {
    gsDPPipeSync(),
    gsDPSetTexturePersp(G_TP_NONE),
    gsDPSetCombineMode(G_CC_FADEA, G_CC_FADEA),
    gsDPSetEnvColor(255, 255, 255, 255),
    gsDPSetRenderMode(G_RM_AA_XLU_SURF, G_RM_AA_XLU_SURF2),
    gsDPSetTextureFilter(G_TF_POINT),
    gsSPEndDisplayList(),
};

// 0x0200ED38 - 0x0200ED68
const Gfx dl_rgba16_load_tex_block[] = {
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, (G_TX_WRAP | G_TX_NOMIRROR), 4, G_TX_NOLOD, (G_TX_WRAP | G_TX_NOMIRROR), 4, G_TX_NOLOD),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, ((16 * 16) - 1), CALC_DXT(16, G_IM_SIZ_16b_BYTES)),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 4, 0, G_TX_RENDERTILE, 0, (G_TX_WRAP | G_TX_NOMIRROR), 4, G_TX_NOLOD, (G_TX_WRAP | G_TX_NOMIRROR), 4, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, ((16 - 1) << G_TEXTURE_IMAGE_FRAC), ((16 - 1) << G_TEXTURE_IMAGE_FRAC)),
    gsSPEndDisplayList(),
};

// 0x0200ED68 - 0x0200EDA8
const Gfx dl_rgba16_text_end[] = {
    gsDPPipeSync(),
    gsDPSetTexturePersp(G_TP_PERSP),
    gsDPSetRenderMode(G_RM_AA_ZB_OPA_SURF, G_RM_AA_ZB_OPA_SURF2),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsDPSetEnvColor(255, 255, 255, 255),
    gsDPSetTextureFilter(G_TF_BILERP),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
    gsSPEndDisplayList(),
};

const Gfx dl_shade_screen_begin[] = {
    gsDPPipeSync(),
    gsDPSetRenderMode(G_RM_CLD_SURF, G_RM_CLD_SURF2),
    gsDPSetCycleType(G_CYC_1CYCLE),
    gsDPSetPrimColor(0, 0, 0, 0, 0, 127),
    gsDPSetCombineMode(G_CC_PRIMITIVE, G_CC_PRIMITIVE),
    gsSPEndDisplayList(),
};

const Gfx dl_shade_screen_end[] = {
    gsDPPipeSync(),
    gsDPSetRenderMode(G_RM_AA_ZB_OPA_SURF, G_RM_AA_ZB_OPA_SURF2),
    gsDPSetPrimColor(0, 0, 255, 255, 255, 255),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPEndDisplayList(),
};

// 0x0200EDA8 - 0x0200EDE8
static const Vtx vertex_text_bg_box[] = {
    {{{     0,    -80,      0}, 0, {     0,      0}, {0xff, 0xff, 0xff, 0xff}}},
    {{{   130,    -80,      0}, 0, {     0,      0}, {0xff, 0xff, 0xff, 0xff}}},
    {{{   130,      0,      0}, 0, {     0,      0}, {0xff, 0xff, 0xff, 0xff}}},
    {{{     0,      0,      0}, 0, {     0,      0}, {0xff, 0xff, 0xff, 0xff}}},
};

// 0x0200EDE8 - 0x0200EE28
const Gfx dl_draw_text_bg_box[] = {
    gsDPPipeSync(),
    gsSPClearGeometryMode(G_LIGHTING),
    gsDPSetCombineMode(G_CC_FADE, G_CC_FADE),
    gsDPSetRenderMode(G_RM_XLU_SURF, G_RM_XLU_SURF2),
    gsSPVertex(vertex_text_bg_box, 4, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  0,  2,  3, 0x0),
    gsSPEndDisplayList(),
};

// 0x0200EE28 - 0x0200EE68
static const Vtx vertex_ia8_char[] = {
#if defined(VERSION_JP) || defined(VERSION_SH)
    {{{     0,      0,      0}, 0, {     0,   1024}, {0xff, 0xff, 0xff, 0xff}}},
    {{{     8,      0,      0}, 0, {   512,   1024}, {0xff, 0xff, 0xff, 0xff}}},
    {{{     8,     16,      0}, 0, {   512,      0}, {0xff, 0xff, 0xff, 0xff}}},
    {{{     0,     16,      0}, 0, {     0,      0}, {0xff, 0xff, 0xff, 0xff}}},
#else
    {{{     0,      0,      0}, 0, {     0,    256}, {0xff, 0xff, 0xff, 0xff}}},
    {{{     8,      0,      0}, 0, {     0,      0}, {0xff, 0xff, 0xff, 0xff}}},
    {{{     8,     16,      0}, 0, {   512,      0}, {0xff, 0xff, 0xff, 0xff}}},
    {{{     0,     16,      0}, 0, {   512,    256}, {0xff, 0xff, 0xff, 0xff}}},
#endif
};

#if defined(VERSION_US) || defined(VERSION_EU)
// 0x0200EE68 - 0x020073E8
const Gfx dl_ia_text_begin[] = {
    gsDPPipeSync(),
    gsSPClearGeometryMode(G_LIGHTING),
    gsDPSetCombineMode(G_CC_FADEA, G_CC_FADEA),
    gsDPSetEnvColor(255, 255, 255, 255),
    gsDPSetRenderMode(G_RM_XLU_SURF, G_RM_XLU_SURF2),
    gsDPSetTextureFilter(G_TF_POINT),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_ON),
    gsSPEndDisplayList(),
};

// 0x020073E8 - 0x02007418
const Gfx dl_ia_text_tex_settings[] = {
    gsDPSetTile(G_IM_FMT_IA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, 3, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, 4, G_TX_NOLOD),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, ((16 * 8 + G_IM_SIZ_4b_INCR) >> G_IM_SIZ_4b_SHIFT) - 1, CALC_DXT(16, G_IM_SIZ_4b_BYTES)),
    gsDPSetTile(G_IM_FMT_IA, G_IM_SIZ_4b, 1, 0, G_TX_RENDERTILE, 0, G_TX_WRAP | G_TX_NOMIRROR, 3, G_TX_NOLOD, G_TX_WRAP | G_TX_NOMIRROR, 4, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, (16 - 1) << G_TEXTURE_IMAGE_FRAC, (8 - 1) << G_TEXTURE_IMAGE_FRAC),
    gsSPVertex(vertex_ia8_char, 4, 0),
    gsSP2Triangles( 0,  1,  2, 0x0, 0,  2,  3, 0x0),
    gsSPEndDisplayList(),
};
#elif defined(VERSION_JP) || defined(VERSION_SH)
// 0x0200EE68 - 0x0200EEA8
const Gfx dl_ia_text_begin[] = {
    gsDPPipeSync(),
    gsSPClearGeometryMode(G_LIGHTING),
    gsDPSetCombineMode(G_CC_FADEA, G_CC_FADEA),
    gsDPSetEnvColor(255, 255, 255, 255),
    gsDPSetRenderMode(G_RM_XLU_SURF, G_RM_XLU_SURF2),
    gsDPSetTextureFilter(G_TF_POINT),
    gsSPTexture(0x8000, 0x8000, 0, G_TX_RENDERTILE, G_ON),
    gsSPEndDisplayList(),
};

// 0x0200EEA8 - 0x0200EEF0
const Gfx dl_ia_text_tex_settings[] = {
    gsDPSetTile(G_IM_FMT_IA, G_IM_SIZ_8b, 0, 0, G_TX_LOADTILE, 0, G_TX_CLAMP, 4, G_TX_NOLOD, G_TX_CLAMP, 3, G_TX_NOLOD),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, ((8 * 16) - 1), CALC_DXT(8, G_IM_SIZ_8b_BYTES)),
    gsDPSetTile(G_IM_FMT_IA, G_IM_SIZ_8b, 1, 0, G_TX_RENDERTILE, 0, G_TX_CLAMP, 4, G_TX_NOLOD, G_TX_CLAMP, 3, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, ((8 - 1) << G_TEXTURE_IMAGE_FRAC), ((16 - 1) << G_TEXTURE_IMAGE_FRAC)),
    gsSPVertex(vertex_ia8_char, 4, 0),
    gsSP2Triangles( 0,  1,  2, 0x0, 0,  2,  3, 0x0),
    gsSPEndDisplayList(),
};
#endif

// 0x0200EEF0 - 0x0200EF30
const Gfx dl_ia_text_end[] = {
    gsDPPipeSync(),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsDPSetEnvColor(255, 255, 255, 255),
    gsSPSetGeometryMode(G_LIGHTING | G_SHADING_SMOOTH),
    gsDPSetRenderMode(G_RM_AA_ZB_OPA_SURF, G_RM_AA_ZB_OPA_SURF2),
    gsDPSetTextureFilter(G_TF_BILERP),
    gsSPEndDisplayList(),
};

// 0x0200EF30 - 0x0200EF60
static const Vtx vertex_triangle[] = {
    {{{     0,      0,      0}, 0, {     0,      0}, {0xff, 0xff, 0xff, 0xff}}},
    {{{     8,      8,      0}, 0, {     0,      0}, {0xff, 0xff, 0xff, 0xff}}},
    {{{     0,     16,      0}, 0, {     0,      0}, {0xff, 0xff, 0xff, 0xff}}},
};

// 0x0200EF60 - 0x0200EFB0
const Gfx dl_draw_triangle[] = {
    gsSPClearGeometryMode(G_LIGHTING),
    gsDPSetCombineMode(G_CC_FADE, G_CC_FADE),
    gsDPSetRenderMode(G_RM_XLU_SURF, G_RM_XLU_SURF2),
    gsDPSetTextureFilter(G_TF_POINT),
    gsSPVertex(vertex_triangle, 3, 0),
    gsSP1Triangle( 0,  1,  2, 0x0),
    gsSPSetGeometryMode(G_LIGHTING),
    gsDPPipeSync(),
    gsDPSetRenderMode(G_RM_AA_ZB_OPA_SURF, G_RM_AA_ZB_OPA_SURF2),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPEndDisplayList(),
};

// 0x0200EFB0 - 0x0200EFF0
static const Vtx vertex_billboard_num[] = {
    {{{   -32,    -32,      0}, 0, {     0,  32<<5}, {0xff, 0xff, 0xff, 0xff}}},
    {{{    32,    -32,      0}, 0, { 32<<5,  32<<5}, {0xff, 0xff, 0xff, 0xff}}},
    {{{    32,     32,      0}, 0, { 32<<5,      0}, {0xff, 0xff, 0xff, 0xff}}},
    {{{   -32,     32,      0}, 0, {     0,      0}, {0xff, 0xff, 0xff, 0xff}}},
};

// 0x0200EFF0 - 0x0200F038
const Gfx dl_billboard_num_begin[] = {
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_DECALRGBA, G_CC_DECALRGBA),
    gsSPClearGeometryMode(G_LIGHTING),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 0, 0, G_TX_LOADTILE, 0, (G_TX_WRAP | G_TX_NOMIRROR), G_TX_NOMASK, G_TX_NOLOD, (G_TX_WRAP | G_TX_NOMIRROR), G_TX_NOMASK, G_TX_NOLOD),
    gsSPTexture(0x8000, 0x8000, 0, G_TX_RENDERTILE, G_ON),
    gsDPTileSync(),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 4, 0, G_TX_RENDERTILE, 0, G_TX_CLAMP, 4, G_TX_NOLOD, G_TX_CLAMP, 4, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, ((16 - 1) << G_TEXTURE_IMAGE_FRAC), ((16 - 1) << G_TEXTURE_IMAGE_FRAC)),
    gsSPEndDisplayList(),
};

// 0x0200F038 - 0x0200F078
const Gfx dl_billboard_num_end[] = {
    gsSPVertex(vertex_billboard_num, 4, 0),
    gsSP2Triangles( 0,  1,  2, 0x0,  0,  2,  3, 0x0),
    gsSPTexture(0x8000, 0x8000, 0, G_TX_RENDERTILE, G_OFF),
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPSetGeometryMode(G_LIGHTING),
    gsSPEndDisplayList(),
};

// 0x0200F078 - 0x0200F0A8
const Gfx dl_billboard_num_0[] = {
    gsSPDisplayList(dl_billboard_num_begin),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_hud_char_0),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, ((16 * 16) - 1), CALC_DXT(16, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_billboard_num_end),
    gsSPEndDisplayList(),
};

// 0x0200F0A8 - 0x0200F0D8
const Gfx dl_billboard_num_1[] = {
    gsSPDisplayList(dl_billboard_num_begin),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_hud_char_1),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, ((16 * 16) - 1), CALC_DXT(16, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_billboard_num_end),
    gsSPEndDisplayList(),
};

// 0x0200F0D8 - 0x0200F108
const Gfx dl_billboard_num_2[] = {
    gsSPDisplayList(dl_billboard_num_begin),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_hud_char_2),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, ((16 * 16) - 1), CALC_DXT(16, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_billboard_num_end),
    gsSPEndDisplayList(),
};

// 0x0200F108 - 0x0200F138
const Gfx dl_billboard_num_3[] = {
    gsSPDisplayList(dl_billboard_num_begin),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_hud_char_3),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, ((16 * 16) - 1), CALC_DXT(16, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_billboard_num_end),
    gsSPEndDisplayList(),
};

// 0x0200F138 - 0x0200F168
const Gfx dl_billboard_num_4[] = {
    gsSPDisplayList(dl_billboard_num_begin),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_hud_char_4),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, ((16 * 16) - 1), CALC_DXT(16, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_billboard_num_end),
    gsSPEndDisplayList(),
};

// 0x0200F168 - 0x0200F198
const Gfx dl_billboard_num_5[] = {
    gsSPDisplayList(dl_billboard_num_begin),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_hud_char_5),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, ((16 * 16) - 1), CALC_DXT(16, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_billboard_num_end),
    gsSPEndDisplayList(),
};

// 0x0200F198 - 0x0200F1C8
const Gfx dl_billboard_num_6[] = {
    gsSPDisplayList(dl_billboard_num_begin),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_hud_char_6),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, ((16 * 16) - 1), CALC_DXT(16, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_billboard_num_end),
    gsSPEndDisplayList(),
};

// 0x0200F1C8 - 0x0200F1F8
const Gfx dl_billboard_num_7[] = {
    gsSPDisplayList(dl_billboard_num_begin),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_hud_char_7),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, ((16 * 16) - 1), CALC_DXT(16, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_billboard_num_end),
    gsSPEndDisplayList(),
};

// 0x0200F1F8 - 0x0200F228
const Gfx dl_billboard_num_8[] = {
    gsSPDisplayList(dl_billboard_num_begin),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_hud_char_8),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, ((16 * 16) - 1), CALC_DXT(16, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_billboard_num_end),
    gsSPEndDisplayList(),
};

// 0x0200F228 - 0x0200F258
const Gfx dl_billboard_num_9[] = {
    gsSPDisplayList(dl_billboard_num_begin),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_hud_char_9),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, ((16 * 16) - 1), CALC_DXT(16, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_billboard_num_end),
    gsSPEndDisplayList(),
};

#ifdef DIALOG_INDICATOR
const Gfx dl_billboard_num_A[] = {
    gsSPDisplayList(dl_billboard_num_begin),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_hud_char_A),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, ((16 * 16) - 1), CALC_DXT(16, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_billboard_num_end),
    gsSPEndDisplayList(),
};

const Gfx dl_billboard_num_B[] = {
    gsSPDisplayList(dl_billboard_num_begin),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_hud_char_B),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, ((16 * 16) - 1), CALC_DXT(16, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_billboard_num_end),
    gsSPEndDisplayList(),
};

const Gfx dl_billboard_num_C[] = {
    gsSPDisplayList(dl_billboard_num_begin),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_hud_char_C),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, ((16 * 16) - 1), CALC_DXT(16, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_billboard_num_end),
    gsSPEndDisplayList(),
};

const Gfx dl_billboard_num_D[] = {
    gsSPDisplayList(dl_billboard_num_begin),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_hud_char_D),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, ((16 * 16) - 1), CALC_DXT(16, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_billboard_num_end),
    gsSPEndDisplayList(),
};

const Gfx dl_billboard_num_E[] = {
    gsSPDisplayList(dl_billboard_num_begin),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_hud_char_E),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, ((16 * 16) - 1), CALC_DXT(16, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_billboard_num_end),
    gsSPEndDisplayList(),
};

const Gfx dl_billboard_num_F[] = {
    gsSPDisplayList(dl_billboard_num_begin),
    gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b, 1, texture_hud_char_F),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, ((16 * 16) - 1), CALC_DXT(16, G_IM_SIZ_16b_BYTES)),
    gsSPDisplayList(dl_billboard_num_end),
    gsSPEndDisplayList(),
};
#endif

#ifdef HD_SHADOWS
ALIGNED8 static const Texture texture_shadow_quarter_circle_64[] = {
#include "textures/segment2/shadow_quarter_circle_64.ia8.inc.c"
};

ALIGNED8 static const Texture texture_shadow_quarter_square_64[] = {
#include "textures/segment2/shadow_quarter_square_64.ia8.inc.c"
};
#else
ALIGNED8 static const Texture texture_shadow_quarter_circle[] = {
#include "textures/segment2/shadow_quarter_circle.ia8.inc.c"
};

ALIGNED8 static const Texture texture_shadow_quarter_square[] = {
#include "textures/segment2/shadow_quarter_square.ia8.inc.c"
};
#endif

UNUSED ALIGNED8 static const Texture texture_radial_light[] = {
#include "textures/segment2/light_quarter_circle.ia16.inc.c"
};

const Texture texture_transition_star_half[] = {
#include "textures/segment2/segment2.0F458.ia8.inc.c"
};

const Texture texture_transition_circle_half[] = {
#include "textures/segment2/segment2.0FC58.ia8.inc.c"
};

const Texture texture_transition_mario[] = {
#include "textures/segment2/segment2.10458.ia8.inc.c"
};

const Texture texture_transition_bowser_half[] = {
#include "textures/segment2/segment2.11458.ia8.inc.c"
};

const Texture texture_waterbox_water[] = {
#include "textures/segment2/segment2.11C58.rgba16.inc.c"
};

const Texture texture_waterbox_jrb_water[] = {
#include "textures/segment2/segment2.12458.rgba16.inc.c"
};

const Texture texture_waterbox_unknown_water[] = {
#include "textures/segment2/segment2.12C58.rgba16.inc.c"
};

const Texture texture_waterbox_mist[] = {
#include "textures/segment2/segment2.13458.ia16.inc.c"
};

const Texture texture_waterbox_lava[] = {
#include "textures/segment2/segment2.13C58.rgba16.inc.c"
};

// Unreferenced light group

// 0x02014470 - 0x020144B0
static const Mtx matrix_identity = {
#ifndef GBI_FLOATS
    {{ 0x00010000, 0x00000000, 0x00000001, 0x00000000 },
     { 0x00000000, 0x00010000, 0x00000000, 0x00000001 },
     { 0x00000000, 0x00000000, 0x00000000, 0x00000000 },
     { 0x00000000, 0x00000000, 0x00000000, 0x00000000 }}
#else
    {{ 1.0f, 0.0f, 0.0f, 0.0f },
     { 0.0f, 1.0f, 0.0f, 0.0f },
     { 0.0f, 0.0f, 1.0f, 0.0f },
     { 0.0f, 0.0f, 0.0f, 1.0f }}
#endif
};


// 0x020144B0 - 0x020144F0
static const Mtx matrix_fullscreen = {
#ifndef GBI_FLOATS
    // {{                               0x00000000, 0x00000000,                              0x00000000, 0x00000000 },
    //  {                               0x00000000, 0xffff0000,                              0xffffffff, 0xffff0001 },
    //  { (((65536 * 2 / SCREEN_WIDTH) << 16) | 0), 0x00000000, (0 << 16) | (65536 * 2 / SCREEN_HEIGHT), 0x00000000 },
    //  {                               0x00000000, 0x00000000,                              0x00000000, 0x00000000 }}
    {{                      0x00000000, 0x00000000,               0x00000000, 0x00000000 },
     {                      0x00000000, 0xffff0000,               0xffffffff, 0xffff0001 },
     { ((131072 / SCREEN_WIDTH) << 16), 0x00000000, (131072 / SCREEN_HEIGHT), 0x00000000 },
     {                      0x00000000, 0x00000000,               0x00000000, 0x00000000 }}
#else
    {{ (2.0f / SCREEN_WIDTH),                   0.0f,  0.0f, 0.0f },
     {                  0.0f, (2.0f / SCREEN_HEIGHT),  0.0f, 0.0f },
     {                  0.0f,                   0.0f, -1.0f, 0.0f },
     {                 -1.0f,                  -1.0f, -1.0f, 1.0f }}
#endif
};


// 0x020144F0 - 0x02014508
const Gfx dl_draw_quad_verts_0123[] = {
    gsSP2Triangles( 0,  1,  2, 0x0,  0,  2,  3, 0x0),
    gsSPEndDisplayList(),
};

// 0x02014508 - 0x02014520
const Gfx dl_draw_quad_verts_4567[] = {
    gsSP2Triangles( 4,  5,  6, 0x0,  4,  6,  7, 0x0),
    gsSPEndDisplayList(),
};

const Gfx dl_shadow_begin[] = {
    gsDPPipeSync(),
    gsSPClearGeometryMode(G_LIGHTING | G_CULL_BACK),
    gsDPSetCombineMode(G_CC_MODULATEIFADEA, G_CC_MODULATEIFADEA),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_ON),
    gsSPEndDisplayList(),
};

#ifdef HD_SHADOWS
const Gfx dl_shadow_circle[] = {
    gsSPDisplayList(dl_shadow_begin),
    gsDPLoadTextureBlock(texture_shadow_quarter_circle_64, G_IM_FMT_IA, G_IM_SIZ_8b, 64, 64, 0, (G_TX_WRAP | G_TX_MIRROR), (G_TX_WRAP | G_TX_MIRROR), 6, 6, G_TX_NOLOD, G_TX_NOLOD),
    gsSPEndDisplayList(),
};

const Gfx dl_shadow_square[] = {
    gsSPDisplayList(dl_shadow_begin),
    gsDPLoadTextureBlock(texture_shadow_quarter_square_64, G_IM_FMT_IA, G_IM_SIZ_8b, 64, 64, 0, (G_TX_WRAP | G_TX_MIRROR), (G_TX_WRAP | G_TX_MIRROR), 6, 6, G_TX_NOLOD, G_TX_NOLOD),
    gsSPEndDisplayList(),
};
#else
const Gfx dl_shadow_circle[] = {
    gsSPDisplayList(dl_shadow_begin),
    gsDPLoadTextureBlock(texture_shadow_quarter_circle, G_IM_FMT_IA, G_IM_SIZ_8b, 16, 16, 0, (G_TX_WRAP | G_TX_MIRROR), (G_TX_WRAP | G_TX_MIRROR), 4, 4, G_TX_NOLOD, G_TX_NOLOD),
    gsSPEndDisplayList(),
};

const Gfx dl_shadow_square[] = {
    gsSPDisplayList(dl_shadow_begin),
    gsDPLoadTextureBlock(texture_shadow_quarter_square, G_IM_FMT_IA, G_IM_SIZ_8b, 16, 16, 0, (G_TX_WRAP | G_TX_MIRROR), (G_TX_WRAP | G_TX_MIRROR), 4, 4, G_TX_NOLOD, G_TX_NOLOD),
    gsSPEndDisplayList(),
};
#endif

static const Vtx vertex_shadow[] = {
#ifdef HD_SHADOWS
    {{{    -1,      0,     -1}, 0, { -2048,  -2048}, {0xff, 0xff, 0xff, 0xff}}},
    {{{     1,      0,     -1}, 0, {  2048,  -2048}, {0xff, 0xff, 0xff, 0xff}}},
    {{{    -1,      0,      1}, 0, { -2048,   2048}, {0xff, 0xff, 0xff, 0xff}}},
    {{{     1,      0,      1}, 0, {  2048,   2048}, {0xff, 0xff, 0xff, 0xff}}},
#else
    {{{    -1,      0,     -1}, 0, {  -512,   -512}, {0xff, 0xff, 0xff, 0xff}}},
    {{{     1,      0,     -1}, 0, {   512,   -512}, {0xff, 0xff, 0xff, 0xff}}},
    {{{    -1,      0,      1}, 0, {  -512,    512}, {0xff, 0xff, 0xff, 0xff}}},
    {{{     1,      0,      1}, 0, {   512,    512}, {0xff, 0xff, 0xff, 0xff}}},
#endif
};

// 0x02014638 - 0x02014660
const Gfx dl_shadow_end[] = {
    gsSPVertex(vertex_shadow, 4, 0),
    gsSP2Triangles( 0,  2,  1, 0x0,  1,  2,  3, 0x0),
    gsDPPipeSync(),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
    gsSPSetGeometryMode(G_LIGHTING | G_CULL_BACK),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPEndDisplayList(),
};

// 0x02014660 - 0x02014698
const Gfx dl_proj_mtx_fullscreen[] = {
    gsDPPipeSync(),
    gsSPClearGeometryMode(G_LIGHTING),
    gsSPMatrix(&matrix_identity,   (G_MTX_PROJECTION | G_MTX_LOAD | G_MTX_NOPUSH)),
    gsSPMatrix(&matrix_fullscreen, (G_MTX_PROJECTION | G_MTX_MUL  | G_MTX_NOPUSH)),
    gsSPMatrix(&matrix_identity,   (G_MTX_MODELVIEW  | G_MTX_LOAD | G_MTX_NOPUSH)),
    gsSPPerspNormalize(0xFFFF),
    gsSPEndDisplayList(),
};

// 0x02014698 - 0x020146C0
const Gfx dl_screen_transition_end[] = {
    gsDPPipeSync(),
    gsSPSetGeometryMode(G_LIGHTING),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsDPSetRenderMode(G_RM_OPA_SURF, G_RM_OPA_SURF2),
    gsSPEndDisplayList(),
};

// 0x020146C0 - 0x02014708
const Gfx dl_transition_draw_filled_region[] = {
    gsSP2Triangles( 0,  4,  1, 0x0,  1,  4,  5, 0x0),
    gsSP2Triangles( 1,  5,  2, 0x0,  2,  5,  6, 0x0),
    gsSP2Triangles( 2,  6,  7, 0x0,  2,  7,  3, 0x0),
    gsSP2Triangles( 3,  4,  0, 0x0,  3,  7,  4, 0x0),
    gsSPEndDisplayList(),
};

// 0x02014708 - 0x02014738
const Gfx dl_skybox_begin[] = {
    gsDPPipeSync(),
    gsSPClearGeometryMode(G_LIGHTING),
    gsDPSetCombineMode(G_CC_MODULATERGB, G_CC_MODULATERGB),
    gsSPPerspNormalize(0xFFFF),
    gsSPMatrix(&matrix_identity, (G_MTX_PROJECTION | G_MTX_LOAD | G_MTX_NOPUSH)),
    gsSPEndDisplayList(),
};

// 0x02014738 - 0x02014768
const Gfx dl_skybox_tile_tex_settings[] = {
    gsSPMatrix(&matrix_identity, (G_MTX_MODELVIEW | G_MTX_LOAD | G_MTX_NOPUSH)),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_ON),
    gsDPTileSync(),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 8, 0, G_TX_RENDERTILE, 0, G_TX_CLAMP, 5, G_TX_NOLOD, G_TX_CLAMP, 5, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, ((32 - 1) << G_TEXTURE_IMAGE_FRAC), ((32 - 1) << G_TEXTURE_IMAGE_FRAC)),
    gsSPEndDisplayList(),
};

// 0x02014768 - 0x02014790
const Gfx dl_skybox_end[] = {
    gsDPPipeSync(),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
    gsSPSetGeometryMode(G_LIGHTING),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPEndDisplayList(),
};

// 0x02014790 - 0x020147D0
const Gfx dl_waterbox_rgba16_begin[] = {
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_MODULATERGBA, G_CC_MODULATERGBA),
    gsSPClearGeometryMode(G_LIGHTING | G_CULL_BACK),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_ON),
#ifdef USE_FRUSTRATIO2
    gsSPClipRatio(FRUSTRATIO_1),
#endif
    gsDPTileSync(),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 8, 0, G_TX_RENDERTILE, 0, (G_TX_WRAP | G_TX_NOMIRROR), 5, G_TX_NOLOD, (G_TX_WRAP | G_TX_NOMIRROR), 5, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, ((32 - 1) << G_TEXTURE_IMAGE_FRAC), ((32 - 1) << G_TEXTURE_IMAGE_FRAC)),
    gsSPEndDisplayList(),
};

// 0x020147D0 - 0x02014810
const Gfx dl_waterbox_ia16_begin[] = {
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_MODULATEIA, G_CC_MODULATEIA),
    gsSPClearGeometryMode(G_LIGHTING | G_CULL_BACK),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_ON),
#ifdef USE_FRUSTRATIO2
    gsSPClipRatio(FRUSTRATIO_1),
#endif
    gsDPTileSync(),
    gsDPSetTile(G_IM_FMT_IA, G_IM_SIZ_16b, 8, 0, G_TX_RENDERTILE, 0, (G_TX_WRAP | G_TX_NOMIRROR), 5, G_TX_NOLOD, (G_TX_WRAP | G_TX_NOMIRROR), 5, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, ((32 - 1) << G_TEXTURE_IMAGE_FRAC), ((32 - 1) << G_TEXTURE_IMAGE_FRAC)),
    gsSPEndDisplayList(),
};

// 0x02014810 - 0x02014838
const Gfx dl_waterbox_end[] = {
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
#ifdef USE_FRUSTRATIO2
    gsSPClipRatio(FRUSTRATIO_2),
#endif
    gsDPPipeSync(),
    gsSPSetGeometryMode(G_LIGHTING | G_CULL_BACK),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPEndDisplayList(),
};

// 0x02014838 - 0x02014878
ALIGNED8 static const Texture texture_ia8_up_arrow[] = {
#include "textures/segment2/segment2.14838.ia8.inc.c"
};

// 0x02014878 - 0x020148B0
const Gfx dl_ia8_up_arrow_begin[] = {
    gsDPPipeSync(),
    gsSPClearGeometryMode(G_LIGHTING),
    gsDPSetCombineMode(G_CC_MODULATEIA, G_CC_MODULATEIA),
    gsDPSetRenderMode(G_RM_XLU_SURF, G_RM_NOOP2),
    gsSPPerspNormalize(0xFFFF),
    gsSPMatrix(&matrix_identity, (G_MTX_PROJECTION | G_MTX_LOAD | G_MTX_NOPUSH)),
    gsSPEndDisplayList(),
};

// 0x020148B0 - 0x020148E0
// Unused, seems to be an early DL for the power meter, seeing that is loading a 64x32 texture
const Gfx dl_rgba16_unused[] = {
    gsSPMatrix(&matrix_identity, (G_MTX_MODELVIEW | G_MTX_LOAD | G_MTX_NOPUSH)),
    gsDPTileSync(),
    gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 16, 0, G_TX_RENDERTILE, 0, G_TX_CLAMP, 5, G_TX_NOLOD, G_TX_CLAMP, 6, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, ((64 - 1) << G_TEXTURE_IMAGE_FRAC), ((32 - 1) << G_TEXTURE_IMAGE_FRAC)),
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_ON),
    gsSPEndDisplayList(),
};

// 0x020148E0 - 0x02014938
const Gfx dl_ia8_up_arrow_load_texture_block[] = {
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_MODULATEIA, G_CC_MODULATEIA),
    gsDPTileSync(),
    gsDPSetTile(G_IM_FMT_IA, G_IM_SIZ_8b, 1, 0, G_TX_RENDERTILE, 0, G_TX_CLAMP, 3, G_TX_NOLOD, G_TX_CLAMP, 3, G_TX_NOLOD),
    gsDPSetTileSize(0, 0, 0, ((8 - 1) << G_TEXTURE_IMAGE_FRAC), ((8 - 1) << G_TEXTURE_IMAGE_FRAC)),
    gsDPSetTextureImage(G_IM_FMT_IA, G_IM_SIZ_8b, 1, texture_ia8_up_arrow),
    gsDPTileSync(),
    gsDPSetTile(G_IM_FMT_IA, G_IM_SIZ_8b, 0, 0, G_TX_LOADTILE, 0, (G_TX_WRAP | G_TX_NOMIRROR), G_TX_NOMASK, G_TX_NOLOD, (G_TX_WRAP | G_TX_NOMIRROR), G_TX_NOMASK, G_TX_NOLOD),
    gsDPLoadSync(),
    gsDPLoadBlock(G_TX_LOADTILE, 0, 0, ((8 * 8) - 1), CALC_DXT(8, G_IM_SIZ_8b_BYTES)),
    gsSPEndDisplayList(),
};

// 0x02014938 - 0x02014958
const Gfx dl_ia8_up_arrow_end[] = {
    gsSPTexture(0xFFFF, 0xFFFF, 0, G_TX_RENDERTILE, G_OFF),
    gsDPPipeSync(),
    gsDPSetCombineMode(G_CC_SHADE, G_CC_SHADE),
    gsSPEndDisplayList(),
};

#include "segment2/paintings.c.in"
