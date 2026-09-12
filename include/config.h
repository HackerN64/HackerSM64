#pragma once

/**
 * @file config.h
 * A catch-all file for configuring various bugfixes and other settings in SM64
 */

#include "config/config_audio.h"
#include "config/config_benchmark.h"
#include "config/config_camera.h"
#include "config/config_collision.h"
#include "config/config_cutscenes.h"
#include "config/config_debug.h"
#include "config/config_game.h"
#include "config/config_goddard.h"
#include "config/config_graphics.h"
#include "config/config_menu.h"
#include "config/config_movement.h"
#include "config/config_objects.h"
#include "config/config_rom.h"

// Local config - include a gitignore'd config file that's specific to just the user (if the file exists). Ignored for release builds.
#if __has_include("config/config_local.h") && !defined(DISABLE_ALL)
    #include "config/config_local.h"
#endif

// WARNING: Compatibility safeguards - don't remove this file unless you know what you're doing
#include "config/config_safeguards.h"
