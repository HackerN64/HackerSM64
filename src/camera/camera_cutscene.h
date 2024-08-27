#pragma once

#include "game/camera.h"

/**
 * The same type as a CameraEvent, but because these are generally longer, and happen in sequential
 * order, they're are called "shots," a term taken from cinematography.
 *
 * To further tell the difference: CutsceneShots usually call multiple CameraEvents at once, but only
 * one CutsceneShot is ever called on a given frame.
 */
typedef CameraEvent CutsceneShot;

/**
 * A camera shot that is active for a number of frames.
 * Together, a sequence of shots makes up a cutscene.
 */
struct Cutscene {
    /// The function that gets called.
    CutsceneShot shot;
    /// How long the shot lasts.
    s16 duration;
};

extern struct Cutscene sCutsceneCapSwitchPress[];
extern struct Cutscene sCutsceneCredits[];
extern struct Cutscene sCutsceneDanceCloseup[];
extern struct Cutscene sCutsceneDanceDefaultRotate[];
extern struct Cutscene sCutsceneDanceFlyAway[];
extern struct Cutscene sCutsceneDeathOnBack[];
extern struct Cutscene sCutsceneStandingDeath[];
extern struct Cutscene sCutsceneDeathStomach[];
extern struct Cutscene sCutsceneDialog[];
extern struct Cutscene sCutsceneDoorPull[];
extern struct Cutscene sCutsceneDoorPullMode[];
extern struct Cutscene sCutsceneDoorPush[];
extern struct Cutscene sCutsceneDoorPushMode[];
extern struct Cutscene sCutsceneDoorWarp[];
extern struct Cutscene sCutsceneEndWaving[];
extern struct Cutscene sCutsceneEnding[];
extern struct Cutscene sCutsceneEnterBowserArena[];
extern struct Cutscene sCutsceneEnterCannon[];
extern struct Cutscene sCutsceneEnterPainting[];
extern struct Cutscene sCutsceneEnterPool[];
extern struct Cutscene sCutsceneEnterPyramidTop[];
extern struct Cutscene sCutsceneGrandStar[];
extern struct Cutscene sCutsceneIntroPeach[];
extern struct Cutscene sCutsceneKeyDance[];
extern struct Cutscene sCutsceneDeathExit[];
extern struct Cutscene sCutsceneExitBowserDeath[];
extern struct Cutscene sCutsceneExitBowserSuccess[];
extern struct Cutscene sCutsceneExitPaintingSuccess[];
extern struct Cutscene sCutsceneExitSpecialSuccess[];
extern struct Cutscene sCutsceneExitWaterfall[];
extern struct Cutscene sCutsceneFallToCastleGrounds[];
extern struct Cutscene sCutsceneNonPaintingDeath[];
extern struct Cutscene sCutsceneUnusedExit[];
extern struct Cutscene sCutscenePrepareCannon[];
extern struct Cutscene sCutscenePyramidTopExplode[];
extern struct Cutscene sCutsceneQuicksandDeath[];
extern struct Cutscene sCutsceneReadMessage[];
extern struct Cutscene sCutsceneRedCoinStarSpawn[];
extern struct Cutscene sCutsceneSlidingDoorsOpen[];
extern struct Cutscene sCutsceneStarSpawn[];
extern struct Cutscene sCutsceneSuffocation[];
extern struct Cutscene sCutsceneUnlockKeyDoor[];
extern struct Cutscene sCutsceneUnused[];
extern struct Cutscene sCutsceneWaterDeath[];
