// ddd_sub.inc.c

void bhv_bowsers_sub_loop(void) {
#ifdef DDD_ACT_SPECIFIC_OBJECTS
    if (gCurrActNum != 1) {
#else
    if (save_file_get_flags() & (SAVE_FLAG_HAVE_KEY_2 | SAVE_FLAG_UNLOCKED_UPSTAIRS_DOOR)) {
#endif
        obj_mark_for_deletion(o);
    }
}
