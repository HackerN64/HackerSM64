// 0x17000084
const GeoLayout burn_smoke_geo[] = {
   GEO_NODE_START(),
   GEO_OPEN_NODE(),
#ifdef BURN_SMOKE_FIX
      GEO_ASM(GEO_TRANSPARENCY_MODE_INTER, geo_update_layer_transparency),
#endif
      GEO_DISPLAY_LIST(LAYER_TRANSPARENT_INTER, burn_smoke_seg4_dl),
   GEO_CLOSE_NODE(),
   GEO_END(),
};
