#include "src/game/envfx_snow.h"

const GeoLayout m_thigh_r_geo[] = {
	GEO_NODE_START(),
	GEO_OPEN_NODE(),
		GEO_DISPLAY_LIST(LAYER_OPAQUE, m_thigh_r_skinned_012_mesh_layer_1),
		GEO_DISPLAY_LIST(LAYER_OPAQUE, m_thigh_r_material_revert_render_settings),
	GEO_CLOSE_NODE(),
	GEO_END(),
};
