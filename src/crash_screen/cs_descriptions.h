#pragma once

#include <ultra64.h>

#include "types.h"
#include "level_table.h"

#include "game/emutest.h"
#include "game/level_update.h"
#include "game/main.h"

#include "util/cs_segments.h"


extern char HackerSM64_version_txt[];
extern char CrashScreen_version_txt[];


#define ID_LIST_END() { .id = -1, .name = NULL, }

typedef struct IdNamePair {
    /*0x00*/ const int id;
    /*0x04*/ const char* name;
} IdNamePair; /*0x08*/
const char* get_name_from_id_list_impl(int id, const IdNamePair* list, size_t count);
#define get_name_from_id_list(_id, _list) get_name_from_id_list_impl((_id), (_list), ARRAY_COUNT(_list))
const char* get_name_from_null_terminated_id_list(int id, const IdNamePair* list);
typedef struct RangeNamePair {
    /*0x00*/ const u32 start;
    /*0x04*/ const u32 end;
    /*0x08*/ const char* name;
} RangeNamePair; /*0x0C*/
const char* get_name_from_range_list_impl(u32 id, const RangeNamePair* list, size_t count);
#define get_name_from_range_list(_id, _list) get_name_from_range_list_impl((_id), (_list), ARRAY_COUNT(_list))
const char* get_name_from_null_terminated_range_list(u32 id, const RangeNamePair* list);

const char* str_null_fallback(const char* str, const char* fallback);

const char* get_thread_name(OSThread* thread);
const char* get_thread_state_str(OSThread* thread);
const char* get_thread_flags_str(OSThread* thread);

const char* get_warp_node_name(enum WarpNodes id);
const char* get_level_name(enum LevelNum levelNum);

const char* get_hardcoded_memory_str(Address addr);

const char* get_processor_name(u8 imp);

const char* get_cause_desc_simple(u32 cause);
const char* get_cause_desc(__OSThreadContext* tc, _Bool specific);
const char* get_fpcsr_desc(u32 fpcsr, _Bool specific);

const char* get_emulator_name(enum Emulator emu);

const char* get_map_symbol_type_desc(char c);
