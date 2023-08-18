#ifndef __os_bb_exec_h__
#define __os_bb_exec_h__

#ifdef _LANGUAGE_C_PLUS_PLUS
extern "C" {
#endif

#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)

#define BB_MAX_AUXDATA               16

extern s32 osBbGameCommitState();
extern s32 osBbAuxDataInit(void* buf, u32 len);
extern s32 osBbAuxDataSave(u8 id, const void* buf, u32 len);
extern s32 osBbAuxDataLoad(u8 id, void* buf, u32 len);
extern s32 osBbAuxDataDelete(u8 id);
extern s32 osBbAuxDataSize(u8 id);
extern s32 osBbAuxDataIds(u8 ids[BB_MAX_AUXDATA]);

#endif  /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif

#endif  /* __os_bb_exec_h__ */
