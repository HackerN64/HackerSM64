#pragma once

#include <ultra64.h>

#include "types.h"

#include "pages/page_summary.h"
#include "pages/page_registers.h"
// #include "pages/page_threads.h"
#include "pages/page_logs.h"
#include "pages/page_stack.h"
#ifdef INCLUDE_DEBUG_MAP
#include "pages/page_map.h"
#endif // INCLUDE_DEBUG_MAP
#include "pages/page_memory.h"
#include "pages/page_disasm.h"
#include "pages/page_settings.h"
#include "pages/page_about.h"


#define CRASH_SCREEN_START_PAGE CS_PAGE_SUMMARY


enum CSPages {
    CS_FIRST_PAGE,

    CS_PAGE_SUMMARY = CS_FIRST_PAGE,
    CS_PAGE_STACK,
    // CS_PAGE_THREADS,
    CS_PAGE_REGISTERS,
    CS_PAGE_DISASM,
    CS_PAGE_MEMORY,
#ifdef INCLUDE_DEBUG_MAP
    CS_PAGE_MAP,
#endif // INCLUDE_DEBUG_MAP
#ifdef PUPPYPRINT_DEBUG
    CS_PAGE_LOGS,
#endif // PUPPYPRINT_DEBUG
    CS_PAGE_SETTINGS,
    CS_PAGE_ABOUT,

    CS_NUM_PAGES,
};


typedef struct CSPage {
    /*0x00*/ const char* name;
    /*0x04*/ void (*initFunc)(void);
    /*0x08*/ void (*drawFunc)(void);
    /*0x0C*/ void (*inputFunc)(void);
    /*0x10*/ void (*printFunc)(void);
    /*0x14*/ const enum ControlTypes* contList;
    /*0x18*/ struct CSSetting* settingsList; //! TODO: Allow page settings to be changed on the page via help popup.
    /*0x1C*/ union {
                struct PACKED {
                    /*0x00*/ u32             : 30;
                    /*0x03*/ u32 crashed     :  1;
                    /*0x03*/ u32 initialized :  1;
                }; /*0x04*/
                u32 raw;
            } flags; /*0x04*/
} CSPage; /*0x20*/


extern struct CSPage* gCSPages[CS_NUM_PAGES];
extern enum CSPages gCSPageID;
extern _Bool gCSSwitchedPage;


enum CSPopups {
    CS_POPUP_NONE,
    CS_POPUP_CONTROLS,
    CS_POPUP_PAGES,
    CS_POPUP_ADDRESS,
    CS_POPUP_REGINSPECT,
    CS_POPUP_THREADS,
    NUM_CS_POPUPS,
};

typedef struct CSPopup {
    /*0x00*/ const char* name;
    /*0x04*/ void (*initFunc)(void);
    /*0x08*/ void (*drawFunc)(void);
    /*0x0C*/ void (*inputFunc)(void);
    /*0x10*/ union {
                struct PACKED {
                    /*0x00*/ u32                 : 30;
                    /*0x03*/ u32 allowPageInput  :  1;
                    /*0x03*/ u32 allowChangePage :  1;
                };
                u32 raw;
            } flags; /*0x04*/
    //! TODO:
    // /*0x14*/ u16 bgStartX;
    // /*0x16*/ u16 bgStartY;
    // /*0x18*/ u16 bgW;
    // /*0x1A*/ u16 bgH;
} CSPopup; /*0x1C*/

extern struct CSPopup* gCSPopups[NUM_CS_POPUPS];
extern enum CSPopups gCSPopupID;
extern _Bool gCSSwitchedPopup;

void cs_set_page(enum CSPages page);
CSPage* cs_get_current_page(void);
void cs_reinitialize_pages(void);

void cs_open_popup(enum CSPopups popupID);
CSPopup* cs_get_current_popup(void);
