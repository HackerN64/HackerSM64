/**
 * @file Behavior file for the manta ray in DDD.
 *
 * The manta ray spawns on stars 2-6. It generally follows a fixed path, leaving rings.
 * These rings contain a significant bug that is documented in water_ring.inc.c
 */

static Trajectory sMantaRayTraj[] = { 
    TRAJECTORY_POS(0, /*pos*/ -4500, -1380,   -40), 
    TRAJECTORY_POS(1, /*pos*/ -4120, -2240,   740), 
    TRAJECTORY_POS(2, /*pos*/ -3280, -3080,  1040), 
    TRAJECTORY_POS(3, /*pos*/ -2240, -3320,   720), 
    TRAJECTORY_POS(4, /*pos*/ -1840, -3140,  -280), 
    TRAJECTORY_POS(5, /*pos*/ -2320, -2480, -1100), 
    TRAJECTORY_POS(6, /*pos*/ -3220, -1600, -1360), 
    TRAJECTORY_POS(7, /*pos*/ -4180, -1020, -1040), 
    TRAJECTORY_END(),
};

static struct ObjectHitbox sMantaRayHitbox = {
    /* interactType:      */ INTERACT_DAMAGE,
    /* downOffset:        */ 0,
    /* damageOrCoinValue: */ 0,
    /* health:            */ 3,
    /* numLootCoins:      */ 0,
    /* radius:            */ 210,
    /* height:            */ 60,
    /* hurtboxRadius:     */ 200,
    /* hurtboxHeight:     */ 50,
};

/**
 * Initializes the manta ray when spawned.
 */
void bhv_manta_ray_init(void) {
    struct Object *ringManager;
    ringManager = spawn_object(o, MODEL_NONE, bhvMantaRayRingManager);
    o->parentObj = ringManager;
    obj_set_hitbox(o, &sMantaRayHitbox);
    cur_obj_scale(2.5f);
}

static void manta_ray_move(void) {
    
}

static void manta_ray_act_spawn_ring(void) {
    
}

/**
 * Behavior that occurs every frame.
 */
void bhv_manta_ray_loop(void) {
    
}
