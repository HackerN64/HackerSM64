// 0x07029038 - 0x07029152
const Collision rr_seg7_collision_flying_carpet[] = {
    COL_INIT(),
    COL_VERTEX_INIT(0x4),
    COL_VERTEX( 307, 20, -205),
    COL_VERTEX(-307, 20, -205),
    COL_VERTEX(-307, 20,  205),
    COL_VERTEX( 307, 20,  205),
    COL_TRI_INIT(SURFACE_NOT_SLIPPERY, 2),
    COL_TRI(0, 1, 2),
    COL_TRI(0, 2, 3),
    COL_TRI_STOP(),
    COL_END(),
};
