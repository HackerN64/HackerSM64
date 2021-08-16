#include "actors/piranha_creeper/geo_header.h"

void piranha_creeper_init(void) {
    o->oPiranhaCreeperInitX = o->oPosX;
    o->oPiranhaCreeperInitY = o->oPosY;
    o->oPiranhaCreeperInitZ = o->oPosZ;

}

void piranha_creeper_loop(void) {
    struct Object *nearestCP;
    f32 sp;
    int speedMod;
    nearestCP = cur_obj_find_nearest_object_with_behavior(bhvPiranhaCreeperCheckpoint, &sp);

    u16 posDiff;
    posDiff = sqrtf(
    sqr(o->oPosX - nearestCP->oPosX) + sqr(o->oPosZ - nearestCP->oPosZ) + sqr(o->oPosY - nearestCP->oPosY));

    u16 initPosDiff;
    initPosDiff = sqrtf(
    sqr(o->oPosX - o->oPiranhaCreeperInitX ) + sqr(o->oPosZ - o->oPiranhaCreeperInitZ) + sqr(o->oPosY - o->oPiranhaCreeperInitY) + 1);

    if (posDiff < 400) {
        speedMod = -8;
        /*
        o->oMoveAngleYaw = nearestCP->oMoveAngleYaw;
        o->oMoveAnglePitch = nearestCP->oMoveAnglePitch;

        if (nearestCP->oPiranhaCreeperCheckpointState == 0) {
            nearestCP->oPiranhaCreeperCheckpointState = 1;
            o->oPiranhaCreeperPathIndex += 1;
        }
*/
    }

    else if (initPosDiff < 20) {
        speedMod = 8;
    }        
    

    //print_text_fmt_int(20, 20, "DIST %d", posDiff);

    
    u16 totalVel;

    
    totalVel = sqrtf(sqr(sins(o->oMoveAngleYaw)) + sqr(coss(o->oMoveAngleYaw)) + sqr(sins(o->oMoveAnglePitch)));

   o->oPosX += speedMod * sins(o->oMoveAngleYaw) / totalVel;
   o->oPosZ += speedMod * coss(o->oMoveAngleYaw) / totalVel;

   o->oPosY -= speedMod * sins(o->oMoveAnglePitch) / totalVel;
   //o->oMoveAngleYaw += 0x800;
}

//NOTE: could have a B param system where for each bit 0 = straight, 1 = left and so on
//OTHER NOTE: thats dumb


Gfx *geo_piranha_creeper_vine(s32 callContext, struct GraphNode *node, UNUSED Mat4 *mtx) {
     Vtx *vertexBuffer;
    Gfx *dlStart, *dlHead;
    struct Object *obj;
    struct GraphNode *graphNode;
    s32 objDispX;
    static s32 objDispY = 75;
    s16 s, t;
    s16 baseS;
    s16 baseT;
    f32 offS;
    f32 offT;
    graphNode = node;

    if (callContext == GEO_CONTEXT_RENDER) {
         obj = (struct Object *) gCurGraphNodeObject; 
        
        
        objDispY -= 8; //* coss(o->oMoveAngleYaw);
        baseT = -32768;
        offS = 1;
        offT = (f32)objDispY / 100;
        s = baseS - ((offS * 64) * 32U);
        t = baseT - ((offT * 64) * 32U);
        //set the render mode
        graphNode->flags = (graphNode->flags & 0xFF) | (LAYER_ALPHA << 8);
        // allocate enough room for 4 vertices
        // Don't do this between displaylist commands or it'll make a mess of everything
        vertexBuffer = alloc_display_list(8 * sizeof(Vtx));
        // make a vertex
        // void make_vertex(Vtx *vtx, s32 n, s16 x, s16 y, s16 z, s16 tx, s16 ty, u8 r, u8 g, u8 b, u8 a)
        
        if (obj->oPiranhaCreeperPathIndex == 0) {
        make_vertex(vertexBuffer, 0, 0, objDispY, 75, baseS, t, 0xA6, 0xA6, 0x0, 0xFF);
        make_vertex(vertexBuffer, 1, 75, objDispY, 0, s, t, 0x5A, 0xA6, 0x0, 0xFF);
        make_vertex(vertexBuffer, 2, 0, 75, 75, baseS, baseT, 0x5A, 0x5A, 0x0, 0xFF);
        make_vertex(vertexBuffer, 3, 75, 75, 0, s, baseT, 0xA6, 0x5A, 0x0, 0xFF);
        //2
        make_vertex(vertexBuffer, 4, 0, objDispY, -75, baseS, t, 0x5A, 0x5A, 0x0, 0xFF);
        make_vertex(vertexBuffer, 5, 0, 75, -75, baseS, baseT, 0xA6, 0x5A, 0x0, 0xFF);
        //3
        make_vertex(vertexBuffer, 6, -75, objDispY, 0, s, t, 0x5A, 0x5A, 0x0, 0xFF);
        make_vertex(vertexBuffer, 7, -75, 75, 0, s, baseT, 0xA6, 0x5A, 0x0, 0xFF);
        }
        dlHead = alloc_display_list(sizeof(Gfx) * (32 + 4)); // allocate enough room for 32 displaylist commands
        dlStart = dlHead; // Store the pointer to the start of the displaylist
        //do whatever other f3d things you want to, just make sure you allocate enough spac
        gSPDisplayList(dlHead++, mat_piranha_creeper_f3d_material_003);

        gSPVertex(dlHead++, VIRTUAL_TO_PHYSICAL(vertexBuffer), 8, 0); // load the 4 verts into the f3d vertex buffer, starting at index 0
        gSP2Triangles(dlHead++, 0, 1, 2, 0, 1, 3, 2, 0); //draw a square
        gSP2Triangles(dlHead++, 1, 4, 3, 1, 3, 4, 5, 1); //draw a square
        gSP2Triangles(dlHead++, 4, 6, 5, 4, 5, 6, 7, 4); //draw a square
        gSP2Triangles(dlHead++, 0, 2, 6, 0, 2, 7, 6, 0); //draw a square
        
        gSPDisplayList(dlHead++, mat_revert_piranha_creeper_f3d_material_003);
        
        gSPEndDisplayList(dlHead++); // end the displaylist (dlHead is now the pointer to the end of the DL)
        
    }

    return dlStart;

}