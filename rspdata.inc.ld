#ifndef F3DEX_GBI_SHARED
#if SUPER3D_GBI == 1
      lib/PR/super3d/fifo/gspSuper3D.o(.data);
#endif
#else /* Use one of the Fast3DEX series grucodes. */
    #ifndef F3DZEX_GBI_2
    #if F3DEX2PL_GBI == 1
      BUILD_DIR/lib/gspF3DEX2_PosLight.fifo.o(.data);
    #elif F3DEX_GBI_2 == 1
      lib/PR/f3dex2/fifo/gspF3DEX2.fifo.o(.data);
    #elif F3DEX_GBI == 1
      lib/PR/f3dex/fifo/gspF3DEX.fifo.o(.data);
    #endif
    #else /* Fast3DZEX */
      BUILD_DIR/lib/gspF3DZEX2_PosLight.fifo.o(.data);
    #endif
#endif

/*
 * LESS COMMON MICROCODES
 * These are setup to be loaded by G_LOAD_UCODE
 */

/* Fast3DEX NoN Data */
#ifdef F3DEX_NON_GBI
      lib/PR/f3dex/fifo/gspF3DEX.NoN.fifo.o(.data);
#endif

/* Fast3DLX Data */
#ifdef F3DLX_GBI
      lib/PR/f3dex/fifo/gspF3DLX.fifo.o(.data);
#endif

/* Fast3DLX NoN Data */
#ifdef F3DLX_NON_GBI
      lib/PR/f3dex/fifo/gspF3DLX.NoN.fifo.o(.data);
#endif

/* Fast3DLX Rej Data */
#ifdef F3DLX_REJ_GBI
      lib/PR/f3dex/fifo/gspF3DLX.Rej.fifo.o(.data);
#endif

/* Line3DEX Data */
#ifdef L3DEX_GBI
      lib/PR/f3dex/fifo/gspL3DEX.fifo.o(.data);
#endif

/* S2DEX Data */
#ifdef S2DEX_GBI
      lib/PR/s2dex/fifo/gspS2DEX.fifo.o(.data);
#endif

/* Fast3DEX2 series */

/* Fast3DEX2 NoN Data */
#ifdef F3DEX2_NON_GBI
      lib/PR/f3dex2/fifo/gspF3DEX2.NoN.fifo.o(.data);
#endif

/* Fast3DZEX NoN Data */
#ifdef F3DZEX_NON_GBI_2
      BUILD_DIR/lib/gspF3DZEX2.NoN_PosLight.fifo.o(.data);
#endif

/* Fast3DEX2 Rej Data */
#ifdef F3DEX2_REJ_GBI
      lib/PR/f3dex2/fifo/gspF3DEX2.Rej.fifo.o(.data);
#endif

/* Fast3DLX2 Rej Data */
#ifdef F3DLX2_REJ_GBI
      lib/PR/f3dex2/fifo/gspF3DLX2.Rej.fifo.o(.data);
#endif

/* Line3DEX2 Data */
#ifdef L3DEX2_GBI
      lib/PR/f3dex2/fifo/gspL3DEX2.fifo.o(.data);
#endif

/* Line3DZEX Data */
#ifdef L3DZEX_GBI
      BUILD_DIR/lib/gspL3DZEX2_PosLight.fifo.o(.data);
#endif

/* S2DEX2 Data */
#ifdef S2DEX_GBI_2
      lib/PR/s2dex2/fifo/gspS2DEX2.fifo.o(.data);
#endif
