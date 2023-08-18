#ifndef _os_bb_card_h_
#define _os_bb_card_h_

#ifdef _LANGUAGE_C_PLUS_PLUS
extern "C" {
#endif

#include <PR/ultratypes.h>

#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)

/**************************************************************************
 *
 * Type definitions
 *
 */


#endif /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */

/**************************************************************************
 *
 * Global definitions
 *
 */


#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)

/**************************************************************************
 *
 * Macro definitions
 *
 */

#define BBCARD_ERR_NO_CARD	(-1)	/* no card present */
#define BBCARD_ERR_FAIL		(-2)	/* operation failed */
#define BBCARD_ERR_INVALID	(-3)	/* invalid parameters */
#define BBCARD_ERR_CHANGED	(-4)	/* card changed */

/**************************************************************************
 *
 * Extern variables
 *
 */


/**************************************************************************
 *
 * Function prototypes
 *
 */

extern void osBbCardInit(void);
extern s32  osBbCardProbe(u32 dev);
extern s32  osBbCardUnhappy(void);
extern u32  osBbCardBlocks(u32 dev);
extern s32  osBbCardStatus(u32 dev, u8 *status);
extern s32  osBbCardReadId(u32 dev, u32 *mfg, u32* type);
extern s32  osBbCardEraseBlock(u32 dev, u16 block);
extern s32  osBbCardEraseBlocks(u32 dev, const u16 block[], u32 n);
extern s32  osBbCardReadBlock(u32 dev, u16 block, void* addr, void* spare);
extern s32  osBbCardWriteBlock(u32 dev, u16 block, const void* addr,
	                       const void* spare);
extern s32  osBbCardWriteBlocks(u32 dev, const u16 block[], u32 n,
	                        const void* addr, const void* spare);
#endif  /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */
#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif
#endif
