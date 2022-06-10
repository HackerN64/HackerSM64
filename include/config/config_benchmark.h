#pragma once

/**********************
 * BENCHMARK SETTINGS *
 **********************/
/**
 * Enabling this will enable a set of defines in order to run a benchmark on the game
 * It will initiate the credits, and then output the results to puppyprint's log.
 * It is HIGHLY recommended you have UNF Reader enabled for this, which can be switched
 * in the makefile.
*/

// #define ENABLE_CREDITS_BENCHMARK

#ifdef ENABLE_CREDITS_BENCHMARK
    #define DEBUG_ALL
    #define ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS
    #define TEST_LEVEL LEVEL_CASTLE_GROUNDS
    #define SKIP_TITLE_SCREEN
    #define PUPPYPRINT
    #define PUPPYPRINT_DEBUG 1
    #undef DISABLE_ALL
    #undef ENABLE_DEBUG_FREE_MOVE
    #define UNLOCK_FPS
#endif
