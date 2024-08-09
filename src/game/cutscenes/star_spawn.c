/**
 * Cutscene that plays when a star spawns from ie a box or after a boss fight.
 */
struct Cutscene sCutsceneStarSpawn[] = {
    { cutscene_star_spawn, CUTSCENE_LOOP },
    { cutscene_star_spawn_back, 15 },
    { cutscene_star_spawn_end, 0 }
};
