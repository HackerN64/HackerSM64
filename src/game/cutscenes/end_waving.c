/**
 * Cutscene that plays after the credits, when Lakitu is flying away from the castle.
 */

/**
 * Reset the spline progress.
 */
void cutscene_end_waving_start(UNUSED struct Camera *c) {
    cutscene_reset_spline();
}

struct CutsceneSplinePoint gEndWavingPos[] = {
    { 0, 0, { -5, 975, -917 } },    { 0, 0, { -5, 975, -917 } },    { 0, 0, { -5, 975, -917 } },
    { 0, 0, { -76, 1067, 742 } },   { 0, 0, { -105, 1576, 3240 } }, { 0, 0, { -177, 1709, 5586 } },
    { 0, 0, { -177, 1709, 5586 } }, { 0, 0, { -177, 1709, 5586 } }, { 0, 0, { -177, 1709, 5586 } }
};

struct CutsceneSplinePoint gEndWavingFocus[] = {
    { 0, 50, { 18, 1013, -1415 } }, { 0, 100, { 17, 1037, -1412 } }, { 0, 100, { 16, 1061, -1408 } },
    { 0, 100, { -54, 1053, 243 } }, { 0, 100, { -84, 1575, 2740 } }, { 0, 50, { -156, 1718, 5086 } },
    { 0, 0, { -156, 1718, 5086 } }, { 0, 0, { -156, 1718, 5086 } },  { 0, 0, { -156, 1718, 5086 } }
};

void cutscene_end_waving(struct Camera *c) {
    cutscene_event(cutscene_end_waving_start, c, 0, 0);
    move_point_along_spline(c->pos, gEndWavingPos, &sCutsceneSplineSegment, &sCutsceneSplineSegmentProgress);
    move_point_along_spline(c->focus, gEndWavingFocus, &sCutsceneSplineSegment, &sCutsceneSplineSegmentProgress);
    cutscene_spawn_obj(CUTSCENE_OBJ_BEGINNING_LAKITU, 120);
}

struct Cutscene sCutsceneEndWaving[] = {
    { cutscene_end_waving, CUTSCENE_LOOP }
};
