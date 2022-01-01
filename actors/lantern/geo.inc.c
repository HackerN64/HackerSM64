#include "src/game/envfx_snow.h"

const GeoLayout lantern_geo[] = {
	GEO_NODE_START(),
	GEO_OPEN_NODE(),
		GEO_DISPLAY_LIST(LAYER_OPAQUE, lantern_Lantern_mesh),
		GEO_BILLBOARD_WITH_PARAMS_AND_DL(LAYER_TRANSPARENT, 0, 0, 0, lantern_Flame_mesh),
		GEO_DISPLAY_LIST(LAYER_TRANSPARENT, lantern_Glass_mesh),
		GEO_DISPLAY_LIST(LAYER_OPAQUE, lantern_material_revert_render_settings),
		GEO_DISPLAY_LIST(LAYER_TRANSPARENT, lantern_material_revert_render_settings),
	GEO_CLOSE_NODE(),
	GEO_END(),
};
