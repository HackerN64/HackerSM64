#ifndef __os_bb_fs_h__
#define __os_bb_fs_h__

#ifdef _LANGUAGE_C_PLUS_PLUS
extern "C" {
#endif

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

#define BBFS_ERR_NO_CARD	(-1)	/* card not present */
#define BBFS_ERR_FAIL		(-2)	/* operation failed */
#define BBFS_ERR_INVALID	(-3)	/* invalid parameters */
#define BBFS_ERR_CHANGED	(-4)    /* card changed */
#define BBFS_ERR_UNINIT		(-5)	/* fs uninitialized */
#define BBFS_ERR_EXISTS		(-6)	/* file exists */
#define BBFS_ERR_SPACE		(-7)	/* no space */
#define BBFS_ERR_ENTRY		(-8)	/* no entry */

/* Used for saving auxilliary game state data */
#define BBFS_ERR_STATE          (-9)   /* invalid state */
#define BBFS_ERR_STATE_LIMIT   (-10)   /* state limit reached */

#endif

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif
#endif /*__os_bb_fs_h__*/
