/**
 * Cutscene that plays when Mario reads a sign or message.
 */
struct Cutscene sCutsceneReadMessage[] = {
    { cutscene_read_message, CUTSCENE_LOOP },
    { cutscene_read_message_set_flag, 15 },
    { cutscene_read_message_end, 0 }
};
