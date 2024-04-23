#if F3DEX_GBI == 1
      lib/PR/f3dex/gspF3DEX.NoN.fifo.o(.data);
#else /* Fast3DZEX */
      lib/PR/f3dzex/gspF3DZEX2.NoN_PosLight.fifo.o(.data);
#endif

/*
 * LESS COMMON MICROCODES
 * These are setup to be loaded by G_LOAD_UCODE
 */

/* Fast3DLX Rej Data */
#ifdef F3DLX_REJ_GBI
      lib/PR/f3dex/gspF3DLX.Rej.fifo.o(.data);
#endif

/* Line3DEX Data */
#ifdef L3DEX_GBI
      lib/PR/f3dex/fifo/gspL3DEX.fifo.o(.data);
#endif

/* Fast3DEX2 series */

/* Fast3DEX2 Rej Data */
#ifdef F3DEX2_REJ_GBI
      lib/PR/f3dex2/fifo/gspF3DEX2.Rej.fifo.o(.data);
#endif

/* Line3DZEX Data */
#ifdef L3DZEX_GBI
      lib/PR/f3dzex/gspL3DZEX2_PosLight.fifo.o(.data);
#endif