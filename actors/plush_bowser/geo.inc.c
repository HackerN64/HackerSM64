#include "src/game/envfx_snow.h"

const GeoLayout plush_bowser_geo[] = {
	GEO_NODE_START(),
	GEO_OPEN_NODE(),
		GEO_ANIMATED_PART(LAYER_OPAQUE, -2, 0, 0, NULL),
		GEO_OPEN_NODE(),
			GEO_TRANSLATE_ROTATE(LAYER_OPAQUE, 0, 23, -8, 19, 0, 0),
			GEO_OPEN_NODE(),
				GEO_ANIMATED_PART(LAYER_OPAQUE, 0, 0, 0, plush_bowser_Torso_mesh_layer_1),
				GEO_OPEN_NODE(),
					GEO_TRANSLATE_ROTATE(LAYER_OPAQUE, 0, 65, -9, -19, 0, 0),
					GEO_OPEN_NODE(),
						GEO_ANIMATED_PART(LAYER_OPAQUE, 0, 0, 0, plush_bowser_Head_mesh_layer_1),
						GEO_OPEN_NODE(),
							GEO_DISPLAY_LIST(LAYER_ALPHA, plush_bowser_Head_mesh_layer_4),
						GEO_CLOSE_NODE(),
					GEO_CLOSE_NODE(),
					GEO_TRANSLATE_ROTATE(LAYER_OPAQUE, 30, 49, 6, 145, 0, -36),
					GEO_OPEN_NODE(),
						GEO_ANIMATED_PART(LAYER_OPAQUE, 0, 0, 0, plush_bowser_Left_arm_1_mesh_layer_1),
						GEO_OPEN_NODE(),
							GEO_DISPLAY_LIST(LAYER_OPAQUE, plush_bowser_Left_arm_2_skinned_mesh_layer_1),
							GEO_TRANSLATE_ROTATE(LAYER_OPAQUE, -4, 15, -1, 12, 171, -57),
							GEO_OPEN_NODE(),
								GEO_ANIMATED_PART(LAYER_OPAQUE, 0, 0, 0, plush_bowser_Left_arm_2_mesh_layer_1),
								GEO_OPEN_NODE(),
									GEO_DISPLAY_LIST(LAYER_OPAQUE, plush_bowser_Left_hand_skinned_mesh_layer_1),
									GEO_TRANSLATE_ROTATE(LAYER_OPAQUE, 2, 16, 1, 4, 80, 19),
									GEO_OPEN_NODE(),
										GEO_ANIMATED_PART(LAYER_OPAQUE, 0, 0, 0, plush_bowser_Left_hand_mesh_layer_1),
									GEO_CLOSE_NODE(),
								GEO_CLOSE_NODE(),
							GEO_CLOSE_NODE(),
						GEO_CLOSE_NODE(),
					GEO_CLOSE_NODE(),
					GEO_TRANSLATE_ROTATE(LAYER_OPAQUE, 27, 0, -1, 165, 38, 12),
					GEO_OPEN_NODE(),
						GEO_ANIMATED_PART(LAYER_OPAQUE, 0, 0, 0, plush_bowser_Left_leg_mesh_layer_1),
						GEO_OPEN_NODE(),
							GEO_DISPLAY_LIST(LAYER_OPAQUE, plush_bowser_Left_foot_skinned_mesh_layer_1),
							GEO_TRANSLATE_ROTATE(LAYER_OPAQUE, -1, 17, -1, -87, 36, 0),
							GEO_OPEN_NODE(),
								GEO_ANIMATED_PART(LAYER_OPAQUE, 0, 0, 0, plush_bowser_Left_foot_mesh_layer_1),
							GEO_CLOSE_NODE(),
						GEO_CLOSE_NODE(),
					GEO_CLOSE_NODE(),
					GEO_TRANSLATE_ROTATE(LAYER_OPAQUE, -30, 49, 6, 145, 0, 36),
					GEO_OPEN_NODE(),
						GEO_ANIMATED_PART(LAYER_OPAQUE, 0, 0, 0, plush_bowser_Right_arm_1_mesh_layer_1),
						GEO_OPEN_NODE(),
							GEO_DISPLAY_LIST(LAYER_OPAQUE, plush_bowser_Right_arm_2_skinned_mesh_layer_1),
							GEO_TRANSLATE_ROTATE(LAYER_OPAQUE, 4, 15, -1, 12, -171, 57),
							GEO_OPEN_NODE(),
								GEO_ANIMATED_PART(LAYER_OPAQUE, 0, 0, 0, plush_bowser_Right_arm_2_mesh_layer_1),
								GEO_OPEN_NODE(),
									GEO_DISPLAY_LIST(LAYER_OPAQUE, plush_bowser_Right_hand_skinned_mesh_layer_1),
									GEO_TRANSLATE_ROTATE(LAYER_OPAQUE, -2, 16, 1, 4, -80, -19),
									GEO_OPEN_NODE(),
										GEO_ANIMATED_PART(LAYER_OPAQUE, 0, 0, 0, plush_bowser_Right_hand_mesh_layer_1),
									GEO_CLOSE_NODE(),
								GEO_CLOSE_NODE(),
							GEO_CLOSE_NODE(),
						GEO_CLOSE_NODE(),
					GEO_CLOSE_NODE(),
					GEO_TRANSLATE_ROTATE(LAYER_OPAQUE, -23, 0, -1, 165, -38, -12),
					GEO_OPEN_NODE(),
						GEO_ANIMATED_PART(LAYER_OPAQUE, 0, 0, 0, plush_bowser_Right_leg_mesh_layer_1),
						GEO_OPEN_NODE(),
							GEO_DISPLAY_LIST(LAYER_OPAQUE, plush_bowser_Right_foot_skinned_mesh_layer_1),
							GEO_TRANSLATE_ROTATE(LAYER_OPAQUE, 1, 17, -1, -87, -36, 0),
							GEO_OPEN_NODE(),
								GEO_ANIMATED_PART(LAYER_OPAQUE, 0, 0, 0, plush_bowser_Right_foot_mesh_layer_1),
							GEO_CLOSE_NODE(),
						GEO_CLOSE_NODE(),
					GEO_CLOSE_NODE(),
					GEO_TRANSLATE_ROTATE(LAYER_OPAQUE, 0, -4, -14, -144, 0, 0),
					GEO_OPEN_NODE(),
						GEO_ANIMATED_PART(LAYER_OPAQUE, 0, 0, 0, plush_bowser_Tail_mesh_layer_1),
					GEO_CLOSE_NODE(),
				GEO_CLOSE_NODE(),
			GEO_CLOSE_NODE(),
		GEO_CLOSE_NODE(),
		GEO_DISPLAY_LIST(LAYER_OPAQUE, plush_bowser_material_revert_render_settings),
		GEO_DISPLAY_LIST(LAYER_ALPHA, plush_bowser_material_revert_render_settings),
	GEO_CLOSE_NODE(),
	GEO_END(),
};
