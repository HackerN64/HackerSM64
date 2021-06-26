#include "src/game/envfx_snow.h"

const GeoLayout plush_mario_geo[] = {
	GEO_NODE_START(),
	GEO_OPEN_NODE(),
		GEO_DISPLAY_LIST(LAYER_OPAQUE, plush_mario_skinned_mesh),
		GEO_DISPLAY_LIST(1, plush_mario_skinned_mesh_layer_1),
		GEO_DISPLAY_LIST(LAYER_OPAQUE, plush_mario_material_revert_render_settings),
		GEO_DISPLAY_LIST(1, plush_mario_material_revert_render_settings),
	GEO_CLOSE_NODE(),
	GEO_END(),
};
