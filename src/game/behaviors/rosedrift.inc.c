void rosedrift_init(void) {
    
}

void rosedrift_loop(void) {
    if (gMarioState->action == ACT_READING_NPC_DIALOG) {
    gMarioState->actionArg = 2;
    }
}