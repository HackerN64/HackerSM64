#include "src/game/envfx_snow.h"

const GeoLayout rosedrift_geo[] = {
	GEO_NODE_START(),
	GEO_OPEN_NODE(),
		GEO_ANIMATED_PART(LAYER_OPAQUE, 0, -1, 0, NULL),
		GEO_OPEN_NODE(),
			GEO_ANIMATED_PART(LAYER_OPAQUE, 0, 100, 0, rosedrift_Bone_001_mesh_layer_1),
			GEO_ANIMATED_PART(LAYER_ALPHA, 0, 66, 0, rosedrift_Bone_002_mesh_layer_4),
		GEO_CLOSE_NODE(),
		GEO_DISPLAY_LIST(LAYER_OPAQUE, rosedrift_material_revert_render_settings),
		GEO_DISPLAY_LIST(LAYER_ALPHA, rosedrift_material_revert_render_settings),
	GEO_CLOSE_NODE(),
	GEO_END(),
};
