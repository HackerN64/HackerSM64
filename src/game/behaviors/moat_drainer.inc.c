// moat_drainer.inc.c

void bhv_invisible_objects_under_bridge_init(void) {
#ifdef UNLOCK_ALL
    if (save_file_get_flags() & SAVE_FLAG_MOAT_DRAINED) {
#endif
        gEnvironmentRegions[6] = -800;
        gEnvironmentRegions[12] = -800;
#ifdef UNLOCK_ALL
    }
#endif
}
