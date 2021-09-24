// sound_spawner.c.inc

void bhv_sound_spawner_init(void) {
    play_sound(o->oSoundEffectBits, o->header.gfx.cameraToObject);
}
