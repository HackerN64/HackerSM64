#include "src/game/envfx_snow.h"

const GeoLayout m_foot_r_geo[] = {
	GEO_NODE_START(),
	GEO_OPEN_NODE(),
		GEO_DISPLAY_LIST(LAYER_OPAQUE, m_foot_r_skinned_017_mesh_layer_1),
		GEO_DISPLAY_LIST(LAYER_OPAQUE, m_foot_r_material_revert_render_settings),
	GEO_CLOSE_NODE(),
	GEO_END(),
};
