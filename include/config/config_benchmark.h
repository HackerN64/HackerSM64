#pragma once

/**********************
 * BENCHMARK SETTINGS *
 **********************/

/**
 * Enabling this will enable a set of defines in order to run a benchmark on the game.
 * Currently this just starts the credits, and enables PUPPYPRINT_DEBUG.
*/
// #define ENABLE_CREDITS_BENCHMARK

#ifdef ENABLE_CREDITS_BENCHMARK
    #define DEBUG_ALL
    #define ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS
    #define TEST_LEVEL LEVEL_CASTLE_GROUNDS
#endif
