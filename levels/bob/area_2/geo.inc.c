#include "src/game/envfx_snow.h"

const GeoLayout bob_area_2_geo[] = {
	GEO_NODE_START(),
	GEO_OPEN_NODE(),
		GEO_TRANSLATE_NODE_WITH_DL(1, 28, 100, -37, bob_dl_Circle_004_mesh_layer_1),
		GEO_TRANSLATE_NODE_WITH_DL(5, 28, 100, -37, bob_dl_Circle_006_mesh_layer_5),
		GEO_TRANSLATE_NODE_WITH_DL(6, -2, 82, -37, bob_dl_Circle_008_mesh_layer_6),
		GEO_TRANSLATE_NODE_WITH_DL(6, -5071, -363, -37, bob_dl_Circle_011_mesh_layer_6),
		GEO_TRANSLATE_NODE_WITH_DL(1, 28, 100, -37, bob_dl_Circle_012_mesh_layer_1),
		GEO_TRANSLATE_NODE_WITH_DL(1, 0, -1161, 1, bob_dl_Plane_044_mesh_layer_1),
	GEO_CLOSE_NODE(),
	GEO_RETURN(),
};
const GeoLayout bob_area_2[] = {
	GEO_NODE_SCREEN_AREA(10, SCREEN_WIDTH/2, SCREEN_HEIGHT/2, SCREEN_WIDTH/2, SCREEN_HEIGHT/2),
	GEO_OPEN_NODE(),
		GEO_ZBUFFER(0),
		GEO_OPEN_NODE(),
			GEO_NODE_ORTHO(100.0000),
			GEO_OPEN_NODE(),
				GEO_BACKGROUND_COLOR(0x0001),
			GEO_CLOSE_NODE(),
		GEO_CLOSE_NODE(),
		GEO_ZBUFFER(1),
		GEO_OPEN_NODE(),
			GEO_CAMERA_FRUSTUM_WITH_FUNC(45.0000, 100, 30000, geo_camera_fov),
			GEO_OPEN_NODE(),
				GEO_CAMERA(CAMERA_MODE_8_DIRECTIONS, 0, 0, 0, 0, -100, 0, geo_camera_main),
				GEO_OPEN_NODE(),
					GEO_BRANCH(1, bob_area_2_geo),
					GEO_RENDER_OBJ(),
					GEO_ASM(ENVFX_FLOWERS, geo_envfx_main),
				GEO_CLOSE_NODE(),
			GEO_CLOSE_NODE(),
		GEO_CLOSE_NODE(),
		GEO_DISPLAY_LIST(1, bob_dl_material_revert_render_settings),
		GEO_DISPLAY_LIST(5, bob_dl_material_revert_render_settings),
		GEO_DISPLAY_LIST(6, bob_dl_material_revert_render_settings),
	GEO_CLOSE_NODE(),
	GEO_END(),
};
