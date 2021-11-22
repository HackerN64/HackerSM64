// 0x16000EA0
const GeoLayout star_geo[] = {
   GEO_SHADOW(SHADOW_CIRCLE_4_VERTS, 0x9B, 100),
   GEO_OPEN_NODE(),
      GEO_SCALE(0x00, 16384),
      GEO_OPEN_NODE(),
         GEO_DISPLAY_LIST(LAYER_OCCLUDE_SILHOUETTE_OPAQUE, star_seg3_dl_body),
         GEO_DISPLAY_LIST(LAYER_OCCLUDE_SILHOUETTE_ALPHA, star_seg3_dl_eyes),
#if STAR_GLOW
         GEO_BILLBOARD(),
         GEO_OPEN_NODE(),
            GEO_Z_OFFSET(48),
            GEO_OPEN_NODE(),
               GEO_DISPLAY_LIST(LAYER_TRANSPARENT_INTER, dl_star_glow),
            GEO_CLOSE_NODE(),
         GEO_CLOSE_NODE(),
#endif
      GEO_CLOSE_NODE(),
   GEO_CLOSE_NODE(),
   GEO_END(),
};
