#ifdef LOAD
    KEEP(BUILD_DIR/src/menu*.o(.text));
    KEEP(BUILD_DIR/src/menu*.o(.data*));
    KEEP(BUILD_DIR/src/menu*.o(.rodata*));
#ifdef KEEP_MARIO_HEAD
    KEEP(BUILD_DIR/libgoddard.a:*.o(.text));
    KEEP(BUILD_DIR/libgoddard.a:gd_main.o(.data*));
    KEEP(BUILD_DIR/libgoddard.a:draw_objects.o(.data*));
    KEEP(BUILD_DIR/libgoddard.a:objects.o(.data*));
    KEEP(BUILD_DIR/libgoddard.a:particles.o(.data*));
    KEEP(BUILD_DIR/libgoddard.a:dynlist_proc.o(.data*));
    KEEP(BUILD_DIR/libgoddard.a:debug_utils.o(.data*));
    KEEP(BUILD_DIR/libgoddard.a:joints.o(.data*));
    KEEP(BUILD_DIR/libgoddard.a:shape_helper.o(.data*));
    KEEP(BUILD_DIR/libgoddard.a:renderer.o(.data*));
    KEEP(BUILD_DIR/libgoddard.a:gd_main.o(.rodata*));
    KEEP(BUILD_DIR/libgoddard.a:gd_memory.o(.rodata*));
    KEEP(BUILD_DIR/libgoddard.a:draw_objects.o(.rodata*));
    KEEP(BUILD_DIR/libgoddard.a:objects.o(.rodata*));
    KEEP(BUILD_DIR/libgoddard.a:skin_movement.o(.rodata*));
    KEEP(BUILD_DIR/libgoddard.a:particles.o(.rodata*));
    KEEP(BUILD_DIR/libgoddard.a:dynlist_proc.o(.rodata*));
    KEEP(BUILD_DIR/libgoddard.a:old_menu.o(.rodata*));
    KEEP(BUILD_DIR/libgoddard.a:debug_utils.o(.rodata*));
    KEEP(BUILD_DIR/libgoddard.a:joints.o(.rodata*));
    KEEP(BUILD_DIR/libgoddard.a:skin.o(.rodata*));
    KEEP(BUILD_DIR/libgoddard.a:gd_math.o(.rodata*));
    KEEP(BUILD_DIR/libgoddard.a:shape_helper.o(.rodata*));
    KEEP(BUILD_DIR/libgoddard.a:renderer.o(.rodata*));
#endif
#endif



#ifdef NOLOAD
    KEEP(BUILD_DIR/src/menu*.o(.bss*));
#ifdef KEEP_MARIO_HEAD
    KEEP(BUILD_DIR/libgoddard.a:*.o(.bss*));
#endif
#endif

