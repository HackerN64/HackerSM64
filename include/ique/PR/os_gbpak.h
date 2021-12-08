
/*---------------------------------------------------------------------*
        Copyright (C) 1998 Nintendo.
        
        $RCSfile: os_gbpak.h,v $
        $Revision: 1.1.1.1 $
        $Date: 2002/05/02 03:28:24 $
 *---------------------------------------------------------------------*/

#ifndef _OS_GBPAK_H_
#define	_OS_GBPAK_H_

#ifdef _LANGUAGE_C_PLUS_PLUS
extern "C" {
#endif

#include <PR/ultratypes.h>
#include "os_message.h"
#include "os_pfs.h"


#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)

/**************************************************************************
 *
 * Type definitions
 *
 */

typedef struct {
  u16	fixed1;
  u16	start_address;
  u8	nintendo_chr[0x30];
  u8	game_title[16];
  u16	company_code;
  u8	body_code;
  u8	cart_type;
  u8	rom_size;
  u8	ram_size;
  u8	country_code;
  u8	fixed2;
  u8	version;
  u8	isum;
  u16	sum;
} OSGbpakId;


#endif /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */

/**************************************************************************
 *
 * Global definitions
 *
 */

/* definition for 64GB-PAK */

#define OS_GBPAK_POWER          0x01
#define OS_GBPAK_RSTB_DETECTION 0x04
#define OS_GBPAK_RSTB_STATUS    0x08
#define OS_GBPAK_GBCART_PULL    0x40
#define OS_GBPAK_GBCART_ON      0x80

#define	OS_GBPAK_POWER_OFF	0x00 	/* power of 64GB-PAK */
#define	OS_GBPAK_POWER_ON	0x01

#define	OS_GBPAK_ROM_ID_SIZE	0x50	/* ID size of GB cartridge */


#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)

/**************************************************************************
 *
 * Macro definitions
 *
 */


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

/* 64GB-PAK */
extern  s32  osGbpakInit(OSMesgQueue *siMessageQ, OSPfs *pfs, int channel);
extern  s32  osGbpakPower(         OSPfs *pfs, s32 flag);
extern  s32  osGbpakGetStatus(     OSPfs *pfs, u8 *status);
extern  s32  osGbpakReadWrite(     OSPfs *pfs, u16 flag, u16 address, u8 *buffer, u16 size);
extern  s32  osGbpakReadId(        OSPfs *pfs, OSGbpakId *id, u8 *status);
extern  s32  osGbpakCheckConnector(OSPfs *pfs, u8 *status);


#endif  /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif

#endif /* !_OS_GBPAK_H_ */
