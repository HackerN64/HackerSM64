Lights1 motos_f3dlite_material_028_lights = gdSPDefLights1(
	0x7F, 0x7F, 0x7F,
	0xFF, 0xFF, 0xFF, 0x28, 0x28, 0x28);

Lights1 motos_f3dlite_material_023_lights = gdSPDefLights1(
	0x7F, 0x7F, 0x7F,
	0xFF, 0xFF, 0xFF, 0x28, 0x28, 0x28);

Lights1 motos_f3dlite_material_043_lights = gdSPDefLights1(
	0x7F, 0x7F, 0x7F,
	0xFF, 0xFF, 0xFF, 0x28, 0x28, 0x28);

Lights1 motos_f3dlite_material_053_lights = gdSPDefLights1(
	0x7F, 0x7F, 0x7F,
	0xFF, 0xFF, 0xFF, 0x28, 0x28, 0x28);

Lights1 motos_f3dlite_material_030_lights = gdSPDefLights1(
	0x7F, 0x7F, 0x7F,
	0xFF, 0xFF, 0xFF, 0x28, 0x28, 0x28);

Lights1 motos_f3dlite_material_057_lights = gdSPDefLights1(
	0x7F, 0x7F, 0x7F,
	0xFF, 0xFF, 0xFF, 0x28, 0x28, 0x28);

Lights1 motos_f3dlite_material_059_lights = gdSPDefLights1(
	0x28, 0x28, 0x28,
	0x59, 0x59, 0x59, 0x28, 0x28, 0x28);

Gfx motos_motos_body1_txt_rgba16_aligner[] = {gsSPEndDisplayList()};
u8 motos_motos_body1_txt_rgba16[] = {
	#include "actors/motos/motos_body1_txt.rgba16.inc.c"
};

Gfx motos_motos_body2_txt_rgba16_aligner[] = {gsSPEndDisplayList()};
u8 motos_motos_body2_txt_rgba16[] = {
	#include "actors/motos/motos_body2_txt.rgba16.inc.c"
};

Gfx motos_motos_parts_txt_rgba16_aligner[] = {gsSPEndDisplayList()};
u8 motos_motos_parts_txt_rgba16[] = {
	#include "actors/motos/motos_parts_txt.rgba16.inc.c"
};

Gfx motos_motos_skinD_txt_rgba16_aligner[] = {gsSPEndDisplayList()};
u8 motos_motos_skinD_txt_rgba16[] = {
	#include "actors/motos/motos_skinD_txt.rgba16.inc.c"
};

Gfx motos_motos_skinB_txt_rgba16_aligner[] = {gsSPEndDisplayList()};
u8 motos_motos_skinB_txt_rgba16[] = {
	#include "actors/motos/motos_skinB_txt.rgba16.inc.c"
};

Gfx motos_motos_eye_txt_rgba16_aligner[] = {gsSPEndDisplayList()};
u8 motos_motos_eye_txt_rgba16[] = {
	#include "actors/motos/motos_eye_txt.rgba16.inc.c"
};

Vtx motos_pelvis_mesh_layer_4_vtx_0[4] = {
	{{ {-76, 81, 4}, 0, {0, -8}, {0, 0, 129, 254} }},
	{{ {3, -80, 4}, 0, {992, 2008}, {0, 0, 129, 254} }},
	{{ {-77, -80, 4}, 0, {0, 2008}, {0, 0, 129, 254} }},
	{{ {3, 81, 4}, 0, {992, -8}, {0, 0, 129, 254} }},
};

Gfx motos_pelvis_mesh_layer_4_tri_0[] = {
	gsSPVertex(motos_pelvis_mesh_layer_4_vtx_0 + 0, 4, 0),
	gsSP2Triangles(0, 1, 2, 0, 0, 3, 1, 0),
	gsSPEndDisplayList(),
};

Vtx motos_pelvis_mesh_layer_4_vtx_1[4] = {
	{{ {3, -80, 4}, 0, {0, 2008}, {0, 0, 129, 254} }},
	{{ {3, 81, 4}, 0, {0, -8}, {0, 0, 129, 254} }},
	{{ {82, 81, 4}, 0, {992, -8}, {0, 0, 129, 254} }},
	{{ {82, -80, 4}, 0, {992, 2008}, {0, 0, 129, 254} }},
};

Gfx motos_pelvis_mesh_layer_4_tri_1[] = {
	gsSPVertex(motos_pelvis_mesh_layer_4_vtx_1 + 0, 4, 0),
	gsSP2Triangles(0, 1, 2, 0, 0, 2, 3, 0),
	gsSPEndDisplayList(),
};

Vtx motos_leg_L_mesh_layer_4_vtx_0[4] = {
	{{ {-7, 21, 49}, 0, {970, 992}, {127, 0, 0, 254} }},
	{{ {-7, 21, 88}, 0, {-22, 992}, {127, 0, 0, 254} }},
	{{ {-7, -17, 88}, 0, {-22, 0}, {127, 0, 0, 254} }},
	{{ {-7, -17, 49}, 0, {970, 0}, {127, 0, 0, 254} }},
};

Gfx motos_leg_L_mesh_layer_4_tri_0[] = {
	gsSPVertex(motos_leg_L_mesh_layer_4_vtx_0 + 0, 4, 0),
	gsSP2Triangles(0, 1, 2, 0, 0, 2, 3, 0),
	gsSPEndDisplayList(),
};

Vtx motos_foot_L_mesh_layer_1_vtx_0[18] = {
	{{ {-21, -4, 51}, 0, {1008, 340}, {221, 152, 191, 254} }},
	{{ {-37, 22, 41}, 0, {-16, -16}, {190, 49, 159, 254} }},
	{{ {16, 22, 41}, 0, {5, 1008}, {59, 56, 158, 254} }},
	{{ {12, -4, 53}, 0, {959, 936}, {55, 157, 198, 254} }},
	{{ {-21, -4, 51}, 0, {1008, 225}, {221, 152, 191, 254} }},
	{{ {-53, 23, 68}, 0, {-16, -56}, {135, 40, 0, 254} }},
	{{ {-37, 22, 41}, 0, {-6, 383}, {190, 49, 159, 254} }},
	{{ {-21, -4, 86}, 0, {1008, -355}, {223, 154, 68, 254} }},
	{{ {-37, 22, 96}, 0, {-6, -505}, {191, 49, 98, 254} }},
	{{ {12, -4, 84}, 0, {959, 936}, {54, 158, 61, 254} }},
	{{ {16, 22, 96}, 0, {5, 1008}, {58, 56, 98, 254} }},
	{{ {-37, 22, 96}, 0, {-16, -16}, {191, 49, 98, 254} }},
	{{ {-21, -4, 86}, 0, {1008, 340}, {223, 154, 68, 254} }},
	{{ {12, -4, 84}, 0, {1008, 1217}, {54, 158, 61, 254} }},
	{{ {29, 22, 68}, 0, {-7, 984}, {115, 55, 0, 254} }},
	{{ {16, 22, 96}, 0, {-16, 1380}, {58, 56, 98, 254} }},
	{{ {12, -4, 53}, 0, {1008, 767}, {55, 157, 198, 254} }},
	{{ {16, 22, 41}, 0, {-16, 596}, {59, 56, 158, 254} }},
};

Gfx motos_foot_L_mesh_layer_1_tri_0[] = {
	gsSPVertex(motos_foot_L_mesh_layer_1_vtx_0 + 0, 18, 0),
	gsSP2Triangles(0, 1, 2, 0, 0, 2, 3, 0),
	gsSP2Triangles(4, 5, 6, 0, 4, 7, 5, 0),
	gsSP2Triangles(5, 7, 8, 0, 9, 10, 11, 0),
	gsSP2Triangles(9, 11, 12, 0, 13, 14, 15, 0),
	gsSP2Triangles(13, 16, 14, 0, 17, 14, 16, 0),
	gsSPEndDisplayList(),
};

Vtx motos_foot_L_mesh_layer_1_vtx_1[6] = {
	{{ {16, 22, 96}, 0, {52, -16}, {58, 56, 98, 254} }},
	{{ {29, 22, 68}, 0, {-384, 501}, {115, 55, 0, 254} }},
	{{ {16, 22, 41}, 0, {52, 1008}, {59, 56, 158, 254} }},
	{{ {-37, 22, 41}, 0, {1891, 1008}, {190, 49, 159, 254} }},
	{{ {-53, 23, 68}, 0, {2400, 501}, {135, 40, 0, 254} }},
	{{ {-37, 22, 96}, 0, {1891, -16}, {191, 49, 98, 254} }},
};

Gfx motos_foot_L_mesh_layer_1_tri_1[] = {
	gsSPVertex(motos_foot_L_mesh_layer_1_vtx_1 + 0, 6, 0),
	gsSP2Triangles(0, 1, 2, 0, 0, 2, 3, 0),
	gsSP2Triangles(0, 3, 4, 0, 0, 4, 5, 0),
	gsSPEndDisplayList(),
};

Vtx motos_foot_L_mesh_layer_1_vtx_2[4] = {
	{{ {-21, -4, 86}, 0, {381, 990}, {223, 154, 68, 254} }},
	{{ {-21, -4, 51}, 0, {1567, 990}, {221, 152, 191, 254} }},
	{{ {12, -4, 53}, 0, {1541, -22}, {55, 157, 198, 254} }},
	{{ {12, -4, 84}, 0, {462, -22}, {54, 158, 61, 254} }},
};

Gfx motos_foot_L_mesh_layer_1_tri_2[] = {
	gsSPVertex(motos_foot_L_mesh_layer_1_vtx_2 + 0, 4, 0),
	gsSP2Triangles(0, 1, 2, 0, 0, 2, 3, 0),
	gsSPEndDisplayList(),
};

Vtx motos_leg_R_mesh_layer_4_vtx_0[4] = {
	{{ {7, 21, 49}, 0, {0, 992}, {129, 0, 0, 254} }},
	{{ {7, -17, 49}, 0, {0, 0}, {129, 0, 0, 254} }},
	{{ {7, -17, 88}, 0, {992, 0}, {129, 0, 0, 254} }},
	{{ {7, 21, 88}, 0, {992, 992}, {129, 0, 0, 254} }},
};

Gfx motos_leg_R_mesh_layer_4_tri_0[] = {
	gsSPVertex(motos_leg_R_mesh_layer_4_vtx_0 + 0, 4, 0),
	gsSP2Triangles(0, 1, 2, 0, 0, 2, 3, 0),
	gsSPEndDisplayList(),
};

Vtx motos_foot_R_mesh_layer_1_vtx_0[6] = {
	{{ {-16, 22, 96}, 0, {52, 1008}, {198, 56, 98, 254} }},
	{{ {37, 22, 96}, 0, {1891, 1008}, {65, 49, 98, 254} }},
	{{ {53, 23, 68}, 0, {2400, 491}, {121, 40, 0, 254} }},
	{{ {37, 22, 41}, 0, {1891, -16}, {66, 49, 159, 254} }},
	{{ {-16, 22, 41}, 0, {52, -16}, {197, 56, 158, 254} }},
	{{ {-29, 22, 68}, 0, {-384, 491}, {141, 55, 0, 254} }},
};

Gfx motos_foot_R_mesh_layer_1_tri_0[] = {
	gsSPVertex(motos_foot_R_mesh_layer_1_vtx_0 + 0, 6, 0),
	gsSP2Triangles(0, 1, 2, 0, 0, 2, 3, 0),
	gsSP2Triangles(0, 3, 4, 0, 0, 4, 5, 0),
	gsSPEndDisplayList(),
};

Vtx motos_foot_R_mesh_layer_1_vtx_1[18] = {
	{{ {21, -4, 51}, 0, {1008, 340}, {35, 152, 191, 254} }},
	{{ {-16, 22, 41}, 0, {5, 1008}, {197, 56, 158, 254} }},
	{{ {37, 22, 41}, 0, {-16, -16}, {66, 49, 159, 254} }},
	{{ {-12, -4, 53}, 0, {959, 936}, {201, 157, 198, 254} }},
	{{ {21, -4, 51}, 0, {1008, 767}, {35, 152, 191, 254} }},
	{{ {37, 22, 41}, 0, {-6, 609}, {66, 49, 159, 254} }},
	{{ {53, 23, 68}, 0, {-16, 1048}, {121, 40, 0, 254} }},
	{{ {21, -4, 86}, 0, {1008, 1347}, {33, 154, 68, 254} }},
	{{ {37, 22, 96}, 0, {-6, 1497}, {65, 49, 98, 254} }},
	{{ {-12, -4, 84}, 0, {1008, -225}, {202, 158, 61, 254} }},
	{{ {-16, 22, 96}, 0, {-16, -388}, {198, 56, 98, 254} }},
	{{ {-29, 22, 68}, 0, {-7, 8}, {141, 55, 0, 254} }},
	{{ {-12, -4, 53}, 0, {1008, 225}, {201, 157, 198, 254} }},
	{{ {-16, 22, 41}, 0, {-16, 396}, {197, 56, 158, 254} }},
	{{ {-12, -4, 84}, 0, {959, 936}, {202, 158, 61, 254} }},
	{{ {37, 22, 96}, 0, {-16, -16}, {65, 49, 98, 254} }},
	{{ {-16, 22, 96}, 0, {5, 1008}, {198, 56, 98, 254} }},
	{{ {21, -4, 86}, 0, {1008, 340}, {33, 154, 68, 254} }},
};

Gfx motos_foot_R_mesh_layer_1_tri_1[] = {
	gsSPVertex(motos_foot_R_mesh_layer_1_vtx_1 + 0, 18, 0),
	gsSP2Triangles(0, 1, 2, 0, 0, 3, 1, 0),
	gsSP2Triangles(4, 5, 6, 0, 4, 6, 7, 0),
	gsSP2Triangles(6, 8, 7, 0, 9, 10, 11, 0),
	gsSP2Triangles(9, 11, 12, 0, 13, 12, 11, 0),
	gsSP2Triangles(14, 15, 16, 0, 14, 17, 15, 0),
	gsSPEndDisplayList(),
};

Vtx motos_foot_R_mesh_layer_1_vtx_2[4] = {
	{{ {21, -4, 86}, 0, {1591, 990}, {33, 154, 68, 254} }},
	{{ {-12, -4, 84}, 0, {1510, -22}, {202, 158, 61, 254} }},
	{{ {-12, -4, 53}, 0, {431, -22}, {201, 157, 198, 254} }},
	{{ {21, -4, 51}, 0, {405, 990}, {35, 152, 191, 254} }},
};

Gfx motos_foot_R_mesh_layer_1_tri_2[] = {
	gsSPVertex(motos_foot_R_mesh_layer_1_vtx_2 + 0, 4, 0),
	gsSP2Triangles(0, 1, 2, 0, 0, 2, 3, 0),
	gsSPEndDisplayList(),
};

Vtx motos_arm_L_mesh_layer_4_vtx_0[6] = {
	{{ {-134, -1, 18}, 0, {970, 992}, {0, 129, 0, 254} }},
	{{ {-172, -1, -21}, 0, {-22, 0}, {0, 129, 0, 254} }},
	{{ {-134, -1, -21}, 0, {970, 0}, {0, 129, 0, 254} }},
	{{ {-134, -1, 18}, 0, {970, 992}, {0, 127, 0, 254} }},
	{{ {-172, -1, -21}, 0, {-22, 0}, {0, 127, 0, 254} }},
	{{ {-172, -1, 18}, 0, {-22, 992}, {0, 127, 0, 254} }},
};

Gfx motos_arm_L_mesh_layer_4_tri_0[] = {
	gsSPVertex(motos_arm_L_mesh_layer_4_vtx_0 + 0, 6, 0),
	gsSP2Triangles(0, 1, 2, 0, 3, 4, 5, 0),
	gsSPEndDisplayList(),
};

Vtx motos_hand_bottom_L_mesh_layer_1_vtx_0[16] = {
	{{ {-144, 1, -1}, 0, {20, -3084}, {177, 91, 40, 255} }},
	{{ {-165, 1, -1}, 0, {982, -3084}, {104, 63, 220, 255} }},
	{{ {-155, 62, 11}, 0, {462, 1048}, {0, 161, 171, 255} }},
	{{ {-144, 1, -1}, 0, {170, -12}, {28, 122, 236, 255} }},
	{{ {-155, 62, 11}, 0, {324, 986}, {59, 149, 34, 255} }},
	{{ {-144, 17, 21}, 0, {800, 290}, {20, 47, 140, 255} }},
	{{ {-165, 17, 21}, 0, {854, 962}, {48, 96, 67, 255} }},
	{{ {-144, 17, 21}, 0, {106, 958}, {186, 67, 82, 255} }},
	{{ {-155, 62, 11}, 0, {416, -34}, {0, 129, 8, 255} }},
	{{ {-165, 1, -1}, 0, {976, 8}, {165, 84, 28, 255} }},
	{{ {-165, 17, 21}, 0, {248, 156}, {8, 68, 149, 255} }},
	{{ {-155, 62, 11}, 0, {242, 974}, {199, 148, 35, 255} }},
	{{ {-144, 1, -1}, 0, {-20, 990}, {177, 223, 94, 255} }},
	{{ {-144, 17, 21}, 0, {970, 990}, {186, 150, 252, 255} }},
	{{ {-165, 17, 21}, 0, {970, 0}, {48, 141, 27, 255} }},
	{{ {-165, 1, -1}, 0, {-20, 0}, {104, 41, 60, 255} }},
};

Gfx motos_hand_bottom_L_mesh_layer_1_tri_0[] = {
	gsSPVertex(motos_hand_bottom_L_mesh_layer_1_vtx_0 + 0, 16, 0),
	gsSP2Triangles(0, 1, 2, 0, 3, 4, 5, 0),
	gsSP2Triangles(6, 7, 8, 0, 9, 10, 11, 0),
	gsSP2Triangles(12, 13, 14, 0, 12, 14, 15, 0),
	gsSPEndDisplayList(),
};

Vtx motos_hand_top_L_mesh_layer_1_vtx_0[16] = {
	{{ {-144, 14, -23}, 0, {960, 986}, {208, 90, 180, 255} }},
	{{ {-165, 14, -23}, 0, {36, 986}, {70, 60, 168, 255} }},
	{{ {-155, 61, -18}, 0, {460, -34}, {0, 129, 2, 255} }},
	{{ {-165, 14, -23}, 0, {580, -62}, {234, 57, 111, 255} }},
	{{ {-165, 0, 0}, 0, {906, 988}, {226, 123, 9, 255} }},
	{{ {-155, 61, -18}, 0, {-18, 988}, {199, 145, 231, 255} }},
	{{ {-165, 0, 0}, 0, {926, -3090}, {79, 87, 207, 255} }},
	{{ {-144, 0, 0}, 0, {94, -3076}, {152, 66, 31, 255} }},
	{{ {-155, 61, -18}, 0, {528, 984}, {0, 170, 93, 255} }},
	{{ {-144, 14, -23}, 0, {834, 178}, {246, 78, 100, 255} }},
	{{ {-155, 61, -18}, 0, {492, 928}, {59, 146, 232, 255} }},
	{{ {-144, 0, 0}, 0, {72, -20}, {91, 82, 221, 255} }},
	{{ {-144, 14, -23}, 0, {992, -50}, {208, 140, 239, 255} }},
	{{ {-165, 0, 0}, 0, {-20, 990}, {79, 214, 166, 255} }},
	{{ {-165, 14, -23}, 0, {992, 972}, {70, 150, 12, 255} }},
	{{ {-144, 0, 0}, 0, {-20, 0}, {152, 36, 193, 255} }},
};

Gfx motos_hand_top_L_mesh_layer_1_tri_0[] = {
	gsSPVertex(motos_hand_top_L_mesh_layer_1_vtx_0 + 0, 16, 0),
	gsSP2Triangles(0, 1, 2, 0, 3, 4, 5, 0),
	gsSP2Triangles(6, 7, 8, 0, 9, 10, 11, 0),
	gsSP2Triangles(12, 13, 14, 0, 12, 15, 13, 0),
	gsSPEndDisplayList(),
};

Vtx motos_arm_R_mesh_layer_4_vtx_0[4] = {
	{{ {134, -1, 18}, 0, {0, 992}, {0, 129, 0, 254} }},
	{{ {172, -1, -21}, 0, {992, 0}, {0, 129, 0, 254} }},
	{{ {172, -1, 18}, 0, {992, 992}, {0, 129, 0, 254} }},
	{{ {134, -1, -21}, 0, {0, 0}, {0, 129, 0, 254} }},
};

Gfx motos_arm_R_mesh_layer_4_tri_0[] = {
	gsSPVertex(motos_arm_R_mesh_layer_4_vtx_0 + 0, 4, 0),
	gsSP2Triangles(0, 1, 2, 0, 0, 3, 1, 0),
	gsSPEndDisplayList(),
};

Vtx motos_hand_bottom_R_mesh_layer_1_vtx_0[16] = {
	{{ {144, 1, -1}, 0, {800, -12}, {177, 156, 3, 255} }},
	{{ {144, 17, 21}, 0, {170, 290}, {186, 228, 102, 255} }},
	{{ {155, 62, 11}, 0, {646, 986}, {0, 120, 213, 255} }},
	{{ {144, 1, -1}, 0, {950, -3084}, {177, 156, 3, 255} }},
	{{ {155, 62, 11}, 0, {508, 1048}, {0, 120, 213, 255} }},
	{{ {165, 1, -1}, 0, {-12, -3084}, {104, 211, 199, 255} }},
	{{ {165, 17, 21}, 0, {116, 962}, {48, 196, 101, 255} }},
	{{ {155, 62, 11}, 0, {554, -34}, {0, 120, 213, 255} }},
	{{ {144, 17, 21}, 0, {864, 958}, {186, 228, 102, 255} }},
	{{ {165, 1, -1}, 0, {-6, 8}, {104, 211, 199, 255} }},
	{{ {155, 62, 11}, 0, {728, 974}, {0, 120, 213, 255} }},
	{{ {165, 17, 21}, 0, {722, 156}, {48, 196, 101, 255} }},
	{{ {144, 1, -1}, 0, {990, 990}, {177, 156, 3, 255} }},
	{{ {165, 17, 21}, 0, {0, 0}, {48, 196, 101, 255} }},
	{{ {144, 17, 21}, 0, {0, 990}, {186, 228, 102, 255} }},
	{{ {165, 1, -1}, 0, {990, 0}, {104, 211, 199, 255} }},
};

Gfx motos_hand_bottom_R_mesh_layer_1_tri_0[] = {
	gsSPVertex(motos_hand_bottom_R_mesh_layer_1_vtx_0 + 0, 16, 0),
	gsSP2Triangles(0, 1, 2, 0, 3, 4, 5, 0),
	gsSP2Triangles(6, 7, 8, 0, 9, 10, 11, 0),
	gsSP2Triangles(12, 13, 14, 0, 12, 15, 13, 0),
	gsSPEndDisplayList(),
};

Vtx motos_hand_top_R_mesh_layer_1_vtx_0[16] = {
	{{ {165, 14, -23}, 0, {390, -62}, {70, 218, 157, 255} }},
	{{ {155, 61, -18}, 0, {988, 988}, {0, 123, 32, 255} }},
	{{ {165, 0, 0}, 0, {64, 988}, {79, 156, 4, 255} }},
	{{ {144, 14, -23}, 0, {10, 986}, {208, 187, 161, 255} }},
	{{ {155, 61, -18}, 0, {510, -34}, {0, 123, 32, 255} }},
	{{ {165, 14, -23}, 0, {934, 986}, {70, 218, 157, 255} }},
	{{ {165, 0, 0}, 0, {44, -3090}, {79, 156, 4, 255} }},
	{{ {155, 61, -18}, 0, {442, 984}, {0, 123, 32, 255} }},
	{{ {144, 0, 0}, 0, {876, -3076}, {151, 217, 61, 255} }},
	{{ {144, 14, -23}, 0, {136, 178}, {208, 187, 161, 255} }},
	{{ {144, 0, 0}, 0, {898, -20}, {151, 217, 61, 255} }},
	{{ {155, 61, -18}, 0, {478, 928}, {0, 123, 32, 255} }},
	{{ {144, 14, -23}, 0, {-22, -50}, {208, 187, 161, 255} }},
	{{ {165, 14, -23}, 0, {-22, 972}, {70, 218, 157, 255} }},
	{{ {165, 0, 0}, 0, {990, 990}, {79, 156, 4, 255} }},
	{{ {144, 0, 0}, 0, {990, 0}, {151, 217, 61, 255} }},
};

Gfx motos_hand_top_R_mesh_layer_1_tri_0[] = {
	gsSPVertex(motos_hand_top_R_mesh_layer_1_vtx_0 + 0, 16, 0),
	gsSP2Triangles(0, 1, 2, 0, 3, 4, 5, 0),
	gsSP2Triangles(6, 7, 8, 0, 9, 10, 11, 0),
	gsSP2Triangles(12, 13, 14, 0, 12, 14, 15, 0),
	gsSPEndDisplayList(),
};

Vtx motos_head_mesh_layer_1_vtx_0[38] = {
	{{ {-13, 19, -25}, 0, {331, -96}, {232, 119, 217, 254} }},
	{{ {-28, 19, -1}, 0, {-194, -98}, {211, 119, 255, 254} }},
	{{ {1, 30, -1}, 0, {825, -400}, {253, 127, 254, 254} }},
	{{ {16, 19, -25}, 0, {1330, -92}, {21, 119, 216, 254} }},
	{{ {-13, 19, -25}, 0, {331, -96}, {235, 118, 215, 254} }},
	{{ {1, 30, -1}, 0, {825, -400}, {0, 127, 253, 254} }},
	{{ {-28, 19, -1}, 0, {-194, -98}, {211, 119, 1, 254} }},
	{{ {-13, 19, 25}, 0, {331, -96}, {233, 119, 38, 254} }},
	{{ {1, 30, -1}, 0, {825, -400}, {253, 127, 1, 254} }},
	{{ {-13, 19, 25}, 0, {331, -96}, {236, 119, 40, 254} }},
	{{ {16, 19, 25}, 0, {1330, -92}, {20, 119, 39, 254} }},
	{{ {1, 30, -1}, 0, {825, -400}, {0, 127, 2, 254} }},
	{{ {31, 19, -1}, 0, {1854, -90}, {44, 119, 255, 254} }},
	{{ {16, 19, -25}, 0, {1330, -92}, {24, 119, 217, 254} }},
	{{ {1, 30, -1}, 0, {825, -400}, {2, 127, 254, 254} }},
	{{ {16, 19, 25}, 0, {1330, -92}, {23, 119, 38, 254} }},
	{{ {31, 19, -1}, 0, {1854, -90}, {44, 119, 1, 254} }},
	{{ {1, 30, -1}, 0, {825, -400}, {2, 127, 1, 254} }},
	{{ {23, -7, 37}, 0, {10, 1138}, {73, 61, 84, 254} }},
	{{ {31, 19, -1}, 0, {1710, -123}, {44, 119, 2, 254} }},
	{{ {16, 19, 25}, 0, {330, -123}, {23, 119, 38, 254} }},
	{{ {45, -7, 0}, 0, {2022, 1138}, {110, 61, 236, 254} }},
	{{ {-42, -7, 0}, 0, {10, 1138}, {146, 60, 21, 254} }},
	{{ {-20, -7, 37}, 0, {2022, 1138}, {219, 61, 105, 254} }},
	{{ {-13, 19, 25}, 0, {1646, -125}, {232, 119, 37, 254} }},
	{{ {-28, 19, -1}, 0, {266, -125}, {210, 118, 1, 254} }},
	{{ {23, -7, -37}, 0, {10, 1138}, {35, 61, 150, 254} }},
	{{ {-13, 19, -25}, 0, {1690, -120}, {236, 119, 215, 254} }},
	{{ {16, 19, -25}, 0, {306, -120}, {21, 118, 215, 254} }},
	{{ {-20, -7, -37}, 0, {2022, 1138}, {185, 61, 170, 254} }},
	{{ {45, -7, 0}, 0, {10, 1138}, {109, 61, 234, 254} }},
	{{ {16, 19, -25}, 0, {1678, -116}, {24, 119, 218, 254} }},
	{{ {31, 19, -1}, 0, {298, -118}, {45, 119, 255, 254} }},
	{{ {23, -7, -37}, 0, {2018, 1138}, {37, 61, 151, 254} }},
	{{ {-20, -7, -37}, 0, {10, 1138}, {183, 61, 171, 254} }},
	{{ {-28, 19, -1}, 0, {1678, -130}, {211, 119, 254, 254} }},
	{{ {-13, 19, -25}, 0, {298, -130}, {232, 118, 216, 254} }},
	{{ {-42, -7, 0}, 0, {2022, 1138}, {146, 61, 19, 254} }},
};

Gfx motos_head_mesh_layer_1_tri_0[] = {
	gsSPVertex(motos_head_mesh_layer_1_vtx_0 + 0, 38, 0),
	gsSP2Triangles(0, 1, 2, 0, 3, 4, 5, 0),
	gsSP2Triangles(6, 7, 8, 0, 9, 10, 11, 0),
	gsSP2Triangles(12, 13, 14, 0, 15, 16, 17, 0),
	gsSP2Triangles(18, 19, 20, 0, 18, 21, 19, 0),
	gsSP2Triangles(22, 23, 24, 0, 22, 24, 25, 0),
	gsSP2Triangles(26, 27, 28, 0, 26, 29, 27, 0),
	gsSP2Triangles(30, 31, 32, 0, 30, 33, 31, 0),
	gsSP2Triangles(34, 35, 36, 0, 34, 37, 35, 0),
	gsSPEndDisplayList(),
};

Vtx motos_head_mesh_layer_1_vtx_1[4] = {
	{{ {-20, -7, 37}, 0, {10, 1138}, {221, 61, 106, 254} }},
	{{ {16, 19, 25}, 0, {1670, -101}, {20, 119, 39, 254} }},
	{{ {-13, 19, 25}, 0, {290, -101}, {235, 119, 40, 254} }},
	{{ {23, -7, 37}, 0, {2022, 1138}, {72, 62, 85, 254} }},
};

Gfx motos_head_mesh_layer_1_tri_1[] = {
	gsSPVertex(motos_head_mesh_layer_1_vtx_1 + 0, 4, 0),
	gsSP2Triangles(0, 1, 2, 0, 0, 3, 1, 0),
	gsSPEndDisplayList(),
};


Gfx mat_motos_f3dlite_material_028[] = {
	gsDPPipeSync(),
	gsDPSetCombineLERP(TEXEL0, 0, SHADE, 0, TEXEL0, 0, ENVIRONMENT, 0, TEXEL0, 0, SHADE, 0, TEXEL0, 0, ENVIRONMENT, 0),
	gsSPGeometryMode(G_CULL_BACK, 0),
	gsSPTexture(65535, 65535, 0, 0, 1),
	gsSPSetLights1(motos_f3dlite_material_028_lights),
	gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b_LOAD_BLOCK, 1, motos_motos_body1_txt_rgba16),
	gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b_LOAD_BLOCK, 0, 0, 7, 0, G_TX_WRAP | G_TX_NOMIRROR, 0, 0, G_TX_WRAP | G_TX_NOMIRROR, 0, 0),
	gsDPLoadBlock(7, 0, 0, 2047, 256),
	gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 8, 0, 0, 0, G_TX_WRAP | G_TX_NOMIRROR, 6, 0, G_TX_WRAP | G_TX_NOMIRROR, 5, 0),
	gsDPSetTileSize(0, 0, 0, 124, 252),
	gsSPEndDisplayList(),
};

Gfx mat_revert_motos_f3dlite_material_028[] = {
	gsDPPipeSync(),
	gsSPGeometryMode(0, G_CULL_BACK),
	gsSPEndDisplayList(),
};

Gfx mat_motos_f3dlite_material_023[] = {
	gsDPPipeSync(),
	gsDPSetCombineLERP(TEXEL0, 0, SHADE, 0, TEXEL0, 0, ENVIRONMENT, 0, TEXEL0, 0, SHADE, 0, TEXEL0, 0, ENVIRONMENT, 0),
	gsSPGeometryMode(G_CULL_BACK, 0),
	gsSPTexture(65535, 65535, 0, 0, 1),
	gsSPSetLights1(motos_f3dlite_material_023_lights),
	gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b_LOAD_BLOCK, 1, motos_motos_body2_txt_rgba16),
	gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b_LOAD_BLOCK, 0, 0, 7, 0, G_TX_WRAP | G_TX_NOMIRROR, 0, 0, G_TX_WRAP | G_TX_NOMIRROR, 0, 0),
	gsDPLoadBlock(7, 0, 0, 2047, 256),
	gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 8, 0, 0, 0, G_TX_WRAP | G_TX_NOMIRROR, 6, 0, G_TX_WRAP | G_TX_NOMIRROR, 5, 0),
	gsDPSetTileSize(0, 0, 0, 124, 252),
	gsSPEndDisplayList(),
};

Gfx mat_revert_motos_f3dlite_material_023[] = {
	gsDPPipeSync(),
	gsSPGeometryMode(0, G_CULL_BACK),
	gsSPEndDisplayList(),
};

Gfx mat_motos_f3dlite_material_043[] = {
	gsDPPipeSync(),
	gsDPSetCombineLERP(TEXEL0, 0, SHADE, 0, TEXEL0, 0, ENVIRONMENT, 0, TEXEL0, 0, SHADE, 0, TEXEL0, 0, ENVIRONMENT, 0),
	gsSPGeometryMode(G_CULL_BACK, 0),
	gsSPTexture(65535, 65535, 0, 0, 1),
	gsSPSetLights1(motos_f3dlite_material_043_lights),
	gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b_LOAD_BLOCK, 1, motos_motos_parts_txt_rgba16),
	gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b_LOAD_BLOCK, 0, 0, 7, 0, G_TX_WRAP | G_TX_NOMIRROR, 0, 0, G_TX_WRAP | G_TX_NOMIRROR, 0, 0),
	gsDPLoadBlock(7, 0, 0, 1023, 256),
	gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 8, 0, 0, 0, G_TX_WRAP | G_TX_NOMIRROR, 5, 0, G_TX_WRAP | G_TX_NOMIRROR, 5, 0),
	gsDPSetTileSize(0, 0, 0, 124, 124),
	gsSPEndDisplayList(),
};

Gfx mat_revert_motos_f3dlite_material_043[] = {
	gsDPPipeSync(),
	gsSPGeometryMode(0, G_CULL_BACK),
	gsSPEndDisplayList(),
};

Gfx mat_motos_f3dlite_material_053[] = {
	gsDPPipeSync(),
	gsDPSetCombineLERP(TEXEL0, 0, SHADE, 0, 0, 0, 0, ENVIRONMENT, TEXEL0, 0, SHADE, 0, 0, 0, 0, ENVIRONMENT),
	gsSPTexture(65535, 65535, 0, 0, 1),
	gsSPSetLights1(motos_f3dlite_material_053_lights),
	gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b_LOAD_BLOCK, 1, motos_motos_skinD_txt_rgba16),
	gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b_LOAD_BLOCK, 0, 0, 7, 0, G_TX_WRAP | G_TX_NOMIRROR, 0, 0, G_TX_WRAP | G_TX_NOMIRROR, 0, 0),
	gsDPLoadBlock(7, 0, 0, 1023, 256),
	gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 8, 0, 0, 0, G_TX_WRAP | G_TX_NOMIRROR, 5, 0, G_TX_CLAMP | G_TX_NOMIRROR, 5, 0),
	gsDPSetTileSize(0, 0, 0, 124, 124),
	gsSPEndDisplayList(),
};

Gfx mat_motos_f3dlite_material_030[] = {
	gsDPPipeSync(),
	gsDPSetCombineLERP(TEXEL0, 0, SHADE, 0, 0, 0, 0, ENVIRONMENT, TEXEL0, 0, SHADE, 0, 0, 0, 0, ENVIRONMENT),
	gsSPTexture(65535, 65535, 0, 0, 1),
	gsSPSetLights1(motos_f3dlite_material_030_lights),
	gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b_LOAD_BLOCK, 1, motos_motos_skinB_txt_rgba16),
	gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b_LOAD_BLOCK, 0, 0, 7, 0, G_TX_WRAP | G_TX_NOMIRROR, 0, 0, G_TX_WRAP | G_TX_NOMIRROR, 0, 0),
	gsDPLoadBlock(7, 0, 0, 2047, 128),
	gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 16, 0, 0, 0, G_TX_CLAMP | G_TX_NOMIRROR, 5, 0, G_TX_CLAMP | G_TX_NOMIRROR, 6, 0),
	gsDPSetTileSize(0, 0, 0, 252, 124),
	gsSPEndDisplayList(),
};

Gfx mat_motos_f3dlite_material_057[] = {
	gsDPPipeSync(),
	gsDPSetCombineLERP(TEXEL0, 0, SHADE, 0, 0, 0, 0, ENVIRONMENT, TEXEL0, 0, SHADE, 0, 0, 0, 0, ENVIRONMENT),
	gsSPTexture(65535, 65535, 0, 0, 1),
	gsSPSetLights1(motos_f3dlite_material_057_lights),
	gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b_LOAD_BLOCK, 1, motos_motos_skinB_txt_rgba16),
	gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b_LOAD_BLOCK, 0, 0, 7, 0, G_TX_WRAP | G_TX_NOMIRROR, 0, 0, G_TX_WRAP | G_TX_NOMIRROR, 0, 0),
	gsDPLoadBlock(7, 0, 0, 2047, 128),
	gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 16, 0, 0, 0, G_TX_WRAP | G_TX_NOMIRROR, 5, 0, G_TX_WRAP | G_TX_NOMIRROR, 6, 0),
	gsDPSetTileSize(0, 0, 0, 252, 124),
	gsSPEndDisplayList(),
};

Gfx mat_motos_f3dlite_material_059[] = {
	gsDPPipeSync(),
	gsDPSetCombineLERP(TEXEL0, 0, SHADE, 0, 0, 0, 0, SHADE, TEXEL0, 0, SHADE, 0, 0, 0, 0, SHADE),
	gsSPTexture(65535, 65535, 0, 0, 1),
	gsSPSetLights1(motos_f3dlite_material_059_lights),
	gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b_LOAD_BLOCK, 1, motos_motos_skinD_txt_rgba16),
	gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b_LOAD_BLOCK, 0, 0, 7, 0, G_TX_WRAP | G_TX_NOMIRROR, 0, 0, G_TX_WRAP | G_TX_NOMIRROR, 0, 0),
	gsDPLoadBlock(7, 0, 0, 1023, 256),
	gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 8, 0, 0, 0, G_TX_WRAP | G_TX_NOMIRROR, 5, 0, G_TX_WRAP | G_TX_NOMIRROR, 5, 0),
	gsDPSetTileSize(0, 0, 0, 124, 124),
	gsSPEndDisplayList(),
};

Gfx mat_motos_f3dlite_material_024[] = {
	gsDPPipeSync(),
	gsDPSetCombineLERP(TEXEL0, 0, SHADE, 0, 0, 0, 0, SHADE, TEXEL0, 0, SHADE, 0, 0, 0, 0, SHADE),
	gsSPTexture(65535, 65535, 0, 0, 1),
	gsDPSetTextureImage(G_IM_FMT_RGBA, G_IM_SIZ_16b_LOAD_BLOCK, 1, motos_motos_eye_txt_rgba16),
	gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b_LOAD_BLOCK, 0, 0, 7, 0, G_TX_WRAP | G_TX_NOMIRROR, 0, 0, G_TX_WRAP | G_TX_NOMIRROR, 0, 0),
	gsDPLoadBlock(7, 0, 0, 2047, 128),
	gsDPSetTile(G_IM_FMT_RGBA, G_IM_SIZ_16b, 16, 0, 0, 0, G_TX_CLAMP | G_TX_NOMIRROR, 5, 0, G_TX_CLAMP | G_TX_NOMIRROR, 6, 0),
	gsDPSetTileSize(0, 0, 0, 252, 124),
	gsSPEndDisplayList(),
};

Gfx motos_pelvis_mesh_layer_4[] = {
	gsSPDisplayList(mat_motos_f3dlite_material_028),
	gsSPDisplayList(motos_pelvis_mesh_layer_4_tri_0),
	gsSPDisplayList(mat_revert_motos_f3dlite_material_028),
	gsSPDisplayList(mat_motos_f3dlite_material_023),
	gsSPDisplayList(motos_pelvis_mesh_layer_4_tri_1),
	gsSPDisplayList(mat_revert_motos_f3dlite_material_023),
	gsSPEndDisplayList(),
};

Gfx motos_leg_L_mesh_layer_4[] = {
	gsSPDisplayList(mat_motos_f3dlite_material_043),
	gsSPDisplayList(motos_leg_L_mesh_layer_4_tri_0),
	gsSPDisplayList(mat_revert_motos_f3dlite_material_043),
	gsSPEndDisplayList(),
};

Gfx motos_foot_L_mesh_layer_1[] = {
	gsSPDisplayList(mat_motos_f3dlite_material_053),
	gsSPDisplayList(motos_foot_L_mesh_layer_1_tri_0),
	gsSPDisplayList(mat_motos_f3dlite_material_030),
	gsSPDisplayList(motos_foot_L_mesh_layer_1_tri_1),
	gsSPDisplayList(mat_motos_f3dlite_material_057),
	gsSPDisplayList(motos_foot_L_mesh_layer_1_tri_2),
	gsSPEndDisplayList(),
};

Gfx motos_leg_R_mesh_layer_4[] = {
	gsSPDisplayList(mat_motos_f3dlite_material_043),
	gsSPDisplayList(motos_leg_R_mesh_layer_4_tri_0),
	gsSPDisplayList(mat_revert_motos_f3dlite_material_043),
	gsSPEndDisplayList(),
};

Gfx motos_foot_R_mesh_layer_1[] = {
	gsSPDisplayList(mat_motos_f3dlite_material_030),
	gsSPDisplayList(motos_foot_R_mesh_layer_1_tri_0),
	gsSPDisplayList(mat_motos_f3dlite_material_053),
	gsSPDisplayList(motos_foot_R_mesh_layer_1_tri_1),
	gsSPDisplayList(mat_motos_f3dlite_material_057),
	gsSPDisplayList(motos_foot_R_mesh_layer_1_tri_2),
	gsSPEndDisplayList(),
};

Gfx motos_arm_L_mesh_layer_4[] = {
	gsSPDisplayList(mat_motos_f3dlite_material_043),
	gsSPDisplayList(motos_arm_L_mesh_layer_4_tri_0),
	gsSPDisplayList(mat_revert_motos_f3dlite_material_043),
	gsSPEndDisplayList(),
};

Gfx motos_hand_bottom_L_mesh_layer_1[] = {
	gsSPDisplayList(mat_motos_f3dlite_material_059),
	gsSPDisplayList(motos_hand_bottom_L_mesh_layer_1_tri_0),
	gsSPEndDisplayList(),
};

Gfx motos_hand_top_L_mesh_layer_1[] = {
	gsSPDisplayList(mat_motos_f3dlite_material_059),
	gsSPDisplayList(motos_hand_top_L_mesh_layer_1_tri_0),
	gsSPEndDisplayList(),
};

Gfx motos_arm_R_mesh_layer_4[] = {
	gsSPDisplayList(mat_motos_f3dlite_material_043),
	gsSPDisplayList(motos_arm_R_mesh_layer_4_tri_0),
	gsSPDisplayList(mat_revert_motos_f3dlite_material_043),
	gsSPEndDisplayList(),
};

Gfx motos_hand_bottom_R_mesh_layer_1[] = {
	gsSPDisplayList(mat_motos_f3dlite_material_059),
	gsSPDisplayList(motos_hand_bottom_R_mesh_layer_1_tri_0),
	gsSPEndDisplayList(),
};

Gfx motos_hand_top_R_mesh_layer_1[] = {
	gsSPDisplayList(mat_motos_f3dlite_material_059),
	gsSPDisplayList(motos_hand_top_R_mesh_layer_1_tri_0),
	gsSPEndDisplayList(),
};

Gfx motos_head_mesh_layer_1[] = {
	gsSPDisplayList(mat_motos_f3dlite_material_030),
	gsSPDisplayList(motos_head_mesh_layer_1_tri_0),
	gsSPDisplayList(mat_motos_f3dlite_material_024),
	gsSPDisplayList(motos_head_mesh_layer_1_tri_1),
	gsSPEndDisplayList(),
};

Gfx motos_material_revert_render_settings[] = {
	gsDPPipeSync(),
	gsSPSetGeometryMode(G_LIGHTING),
	gsSPClearGeometryMode(G_TEXTURE_GEN),
	gsDPSetCombineLERP(0, 0, 0, SHADE, 0, 0, 0, ENVIRONMENT, 0, 0, 0, SHADE, 0, 0, 0, ENVIRONMENT),
	gsSPTexture(65535, 65535, 0, 0, 0),
	gsDPSetEnvColor(255, 255, 255, 255),
	gsDPSetAlphaCompare(G_AC_NONE),
	gsSPEndDisplayList(),
};

