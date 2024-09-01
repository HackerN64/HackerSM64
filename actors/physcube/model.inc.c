
Vtx physcube_Cube_mesh_layer_1_vtx_0[24] = {
	{{ {100, 100, -100}, 0, {624, 496}, {0, 127, 0, 255} }},
	{{ {-100, 100, -100}, 0, {880, 496}, {0, 127, 0, 255} }},
	{{ {-100, 100, 100}, 0, {880, 240}, {0, 127, 0, 255} }},
	{{ {100, 100, 100}, 0, {624, 240}, {0, 127, 0, 255} }},
	{{ {100, -100, 100}, 0, {368, 240}, {0, 0, 127, 255} }},
	{{ {100, 100, 100}, 0, {624, 240}, {0, 0, 127, 255} }},
	{{ {-100, 100, 100}, 0, {624, -16}, {0, 0, 127, 255} }},
	{{ {-100, -100, 100}, 0, {368, -16}, {0, 0, 127, 255} }},
	{{ {-100, -100, 100}, 0, {368, 1008}, {129, 0, 0, 255} }},
	{{ {-100, 100, 100}, 0, {624, 1008}, {129, 0, 0, 255} }},
	{{ {-100, 100, -100}, 0, {624, 752}, {129, 0, 0, 255} }},
	{{ {-100, -100, -100}, 0, {368, 752}, {129, 0, 0, 255} }},
	{{ {-100, -100, -100}, 0, {112, 496}, {0, 129, 0, 255} }},
	{{ {100, -100, -100}, 0, {368, 496}, {0, 129, 0, 255} }},
	{{ {100, -100, 100}, 0, {368, 240}, {0, 129, 0, 255} }},
	{{ {-100, -100, 100}, 0, {112, 240}, {0, 129, 0, 255} }},
	{{ {100, -100, -100}, 0, {368, 496}, {127, 0, 0, 255} }},
	{{ {100, 100, -100}, 0, {624, 496}, {127, 0, 0, 255} }},
	{{ {100, 100, 100}, 0, {624, 240}, {127, 0, 0, 255} }},
	{{ {100, -100, 100}, 0, {368, 240}, {127, 0, 0, 255} }},
	{{ {-100, -100, -100}, 0, {368, 752}, {0, 0, 129, 255} }},
	{{ {-100, 100, -100}, 0, {624, 752}, {0, 0, 129, 255} }},
	{{ {100, 100, -100}, 0, {624, 496}, {0, 0, 129, 255} }},
	{{ {100, -100, -100}, 0, {368, 496}, {0, 0, 129, 255} }},
};

Gfx physcube_Cube_mesh_layer_1_tri_0[] = {
	gsSPVertex(physcube_Cube_mesh_layer_1_vtx_0 + 0, 16, 0),
	gsSP1Triangle(0, 1, 2, 0),
	gsSP1Triangle(0, 2, 3, 0),
	gsSP1Triangle(4, 5, 6, 0),
	gsSP1Triangle(4, 6, 7, 0),
	gsSP1Triangle(8, 9, 10, 0),
	gsSP1Triangle(8, 10, 11, 0),
	gsSP1Triangle(12, 13, 14, 0),
	gsSP1Triangle(12, 14, 15, 0),
	gsSPVertex(physcube_Cube_mesh_layer_1_vtx_0 + 16, 8, 0),
	gsSP1Triangle(0, 1, 2, 0),
	gsSP1Triangle(0, 2, 3, 0),
	gsSP1Triangle(4, 5, 6, 0),
	gsSP1Triangle(4, 6, 7, 0),
	gsSPEndDisplayList(),
};


Gfx mat_physcube_f3dlite_material[] = {
	gsDPPipeSync(),
	gsDPSetCombineLERP(0, 0, 0, SHADE, 0, 0, 0, ENVIRONMENT, 0, 0, 0, SHADE, 0, 0, 0, ENVIRONMENT),
	gsSPTexture(65535, 65535, 0, 0, 1),
    gsSPLightColor(LIGHT_1, 0xffffffff),
    gsSPLightColor(LIGHT_2, 0x7f7f7fff),
	gsSPEndDisplayList(),
};

Gfx physcube_Cube_mesh_layer_1[] = {
	gsSPDisplayList(mat_physcube_f3dlite_material),
	gsSPDisplayList(physcube_Cube_mesh_layer_1_tri_0),
	gsSPEndDisplayList(),
};

Gfx physcube_material_revert_render_settings[] = {
	gsDPPipeSync(),
	gsSPSetGeometryMode(G_LIGHTING),
	gsSPClearGeometryMode(G_TEXTURE_GEN),
	gsDPSetCombineLERP(0, 0, 0, SHADE, 0, 0, 0, ENVIRONMENT, 0, 0, 0, SHADE, 0, 0, 0, ENVIRONMENT),
	gsSPTexture(65535, 65535, 0, 0, 0),
	gsDPSetEnvColor(255, 255, 255, 255),
	gsDPSetAlphaCompare(G_AC_NONE),
	gsSPEndDisplayList(),
};

