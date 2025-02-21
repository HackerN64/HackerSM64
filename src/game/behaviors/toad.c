#include <ultra64.h>
#include "config/config_debug.h"
#include "behavior_data.h"
#include "global_object_fields.h"
#include "audio/external.h"
#include "game/interaction.h"
#include "game/object_helpers.h"
#include "game/save_file.h"

#define TOAD_STAR_1_REQUIREMENT 12
#define TOAD_STAR_2_REQUIREMENT 25
#define TOAD_STAR_3_REQUIREMENT 35

#define TOAD_STAR_1_DIALOG DIALOG_082
#define TOAD_STAR_2_DIALOG DIALOG_076
#define TOAD_STAR_3_DIALOG DIALOG_083

#define TOAD_STAR_1_DIALOG_AFTER DIALOG_154
#define TOAD_STAR_2_DIALOG_AFTER DIALOG_155
#define TOAD_STAR_3_DIALOG_AFTER DIALOG_156

enum ToadMessageStates {
    TOAD_MESSAGE_FADED,
    TOAD_MESSAGE_OPAQUE,
    TOAD_MESSAGE_OPACIFYING,
    TOAD_MESSAGE_FADING,
    TOAD_MESSAGE_TALKING
};

/* Toad Message */
#define /*0x108*/ oToadMessageDialogId       OBJECT_FIELD_U32(0x20)
#define /*0x10C*/ oToadMessageRecentlyTalked OBJECT_FIELD_S32(0x21)
#define /*0x110*/ oToadMessageState          OBJECT_FIELD_S32(0x22)

static void toad_message_faded(void) {
    if (o->oDistanceToMario > 700.0f) {
        o->oToadMessageRecentlyTalked = FALSE;
    }
    if (!o->oToadMessageRecentlyTalked && o->oDistanceToMario < 600.0f) {
        o->oToadMessageState = TOAD_MESSAGE_OPACIFYING;
    }
}

static void toad_message_opaque(void) {
    if (o->oDistanceToMario > 700.0f) {
        o->oToadMessageState = TOAD_MESSAGE_FADING;
    } else if (!o->oToadMessageRecentlyTalked) {
        o->oInteractionSubtype = INT_SUBTYPE_NPC;
        if (o->oInteractStatus & INT_STATUS_INTERACTED) {
            o->oInteractStatus = INT_STATUS_NONE;
            o->oToadMessageState = TOAD_MESSAGE_TALKING;
            play_toads_jingle();
        }
    }
}

static void toad_message_talking(void) {
    if (cur_obj_update_dialog_with_cutscene(MARIO_DIALOG_LOOK_DOWN,
        DIALOG_FLAG_TURN_TO_MARIO, CUTSCENE_DIALOG, o->oToadMessageDialogId)) {
        o->oToadMessageRecentlyTalked = TRUE;
        o->oToadMessageState = TOAD_MESSAGE_FADING;
        switch (o->oToadMessageDialogId) {
            case TOAD_STAR_1_DIALOG:
                o->oToadMessageDialogId = TOAD_STAR_1_DIALOG_AFTER;
                bhv_spawn_star_no_level_exit(STAR_BP_ACT_1);
                break;
            case TOAD_STAR_2_DIALOG:
                o->oToadMessageDialogId = TOAD_STAR_2_DIALOG_AFTER;
                bhv_spawn_star_no_level_exit(STAR_BP_ACT_2);
                break;
            case TOAD_STAR_3_DIALOG:
                o->oToadMessageDialogId = TOAD_STAR_3_DIALOG_AFTER;
                bhv_spawn_star_no_level_exit(STAR_BP_ACT_3);
                break;
        }
    }
}

static void toad_message_opacifying(void) {
    if ((o->oOpacity += 6) == 255) {
        o->oToadMessageState = TOAD_MESSAGE_OPAQUE;
    }
}

static void toad_message_fading(void) {
    if ((o->oOpacity -= 6) == 81) {
        o->oToadMessageState = TOAD_MESSAGE_FADED;
    }
}

void bhv_toad_message_loop(void) {
    if (o->header.gfx.node.flags & GRAPH_RENDER_ACTIVE) {
        o->oInteractionSubtype = INT_STATUS_NONE;
        switch (o->oToadMessageState) {
            case TOAD_MESSAGE_FADED:
                toad_message_faded();
                break;
            case TOAD_MESSAGE_OPAQUE:
                toad_message_opaque();
                break;
            case TOAD_MESSAGE_OPACIFYING:
                toad_message_opacifying();
                break;
            case TOAD_MESSAGE_FADING:
                toad_message_fading();
                break;
            case TOAD_MESSAGE_TALKING:
                toad_message_talking();
                break;
        }
    }
}

void bhv_toad_message_init(void) {
    s32 saveFlags = save_file_get_flags();
#ifdef UNLOCK_ALL
    s32 starCount = 999;
#else
    s32 starCount = save_file_get_total_star_count(gCurrSaveFileNum - 1, COURSE_MIN - 1, COURSE_MAX - 1);
#endif
    s32 dialogId = GET_BPARAM1(o->oBehParams);
    s32 enoughStars = TRUE;

    switch (dialogId) {
        case TOAD_STAR_1_DIALOG:
            enoughStars = (starCount >= TOAD_STAR_1_REQUIREMENT);
            if (saveFlags & SAVE_FLAG_COLLECTED_TOAD_STAR_1) {
                dialogId = TOAD_STAR_1_DIALOG_AFTER;
            }
            break;
        case TOAD_STAR_2_DIALOG:
            enoughStars = (starCount >= TOAD_STAR_2_REQUIREMENT);
            if (saveFlags & SAVE_FLAG_COLLECTED_TOAD_STAR_2) {
                dialogId = TOAD_STAR_2_DIALOG_AFTER;
            }
            break;
        case TOAD_STAR_3_DIALOG:
            enoughStars = (starCount >= TOAD_STAR_3_REQUIREMENT);
            if (saveFlags & SAVE_FLAG_COLLECTED_TOAD_STAR_3) {
                dialogId = TOAD_STAR_3_DIALOG_AFTER;
            }
            break;
    }
    if (enoughStars) {
        o->oToadMessageDialogId = dialogId;
        o->oToadMessageRecentlyTalked = FALSE;
        o->oToadMessageState = TOAD_MESSAGE_FADED;
        o->oOpacity = 81;
    } else {
        obj_mark_for_deletion(o);
    }
}

