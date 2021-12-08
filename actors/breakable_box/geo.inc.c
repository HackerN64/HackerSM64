// 0x0F0005D0
const GeoLayout breakable_box_geo[] = {
   GEO_CULLING_RADIUS(500),
   GEO_OPEN_NODE(),
      GEO_SHADOW(SHADOW_SQUARE_PERMANENT, 0xA0, 240),
      GEO_OPEN_NODE(),
         GEO_SWITCH_CASE(2, geo_switch_anim_state),
         GEO_OPEN_NODE(),
            GEO_DISPLAY_LIST(LAYER_OPAQUE, breakable_box_seg8_dl_crazy_box),
            GEO_DISPLAY_LIST(LAYER_OPAQUE, breakable_box_seg8_dl_cork_box),
         GEO_CLOSE_NODE(),
      GEO_CLOSE_NODE(),
   GEO_CLOSE_NODE(),
   GEO_END(),
};

// 0x0F000610
const GeoLayout breakable_box_no_shadow_geo[] = {
   GEO_CULLING_RADIUS(500),
   GEO_OPEN_NODE(),
      GEO_SWITCH_CASE(2, geo_switch_anim_state),
      GEO_OPEN_NODE(),
         GEO_DISPLAY_LIST(LAYER_OPAQUE, breakable_box_seg8_dl_crazy_box),
         GEO_DISPLAY_LIST(LAYER_OPAQUE, breakable_box_seg8_dl_cork_box),
      GEO_CLOSE_NODE(),
   GEO_CLOSE_NODE(),
   GEO_END(),
};
