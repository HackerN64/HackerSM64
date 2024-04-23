#if F3DEX_GBI == 1
      lib/PR/f3dex/gspF3DEX.NoN.fifo.o(.text);
#else /* Fast3DZEX */
      lib/PR/f3dzex/gspF3DZEX2.NoN.fifo.o(.text);
#endif

/*
 * LESS COMMON MICROCODES
 * These are setup to be loaded by G_LOAD_UCODE
 */

/* Fast3DLX Rej Data */
#ifdef F3DLX_REJ_GBI
      lib/PR/f3dex/gspF3DLX.Rej.fifo.o(.text);
#endif

/* Line3DEX Data */
#ifdef L3DEX_GBI
      lib/PR/f3dex/gspL3DEX.fifo.o(.text);
#endif

/* Fast3DEX2 series */

/* Fast3DEX2 Rej Data */
#ifdef F3DEX2_REJ_GBI
      lib/PR/f3dex2/fifo/gspF3DEX2.Rej.fifo.o(.text);
#endif

/* Line3DZEX Data */
#ifdef L3DZEX_GBI
      lib/PR/f3dzex/gspL3DZEX2.fifo.o(.text);
#endif