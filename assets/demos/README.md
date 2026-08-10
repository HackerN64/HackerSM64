## Demos
To record a demo, a few steps have to be taken.
- Enable `DEMO_RECORDING_MODE` in `include/config/config_goddard.h`.
- Set a `START_LEVEL` or `TEST_LEVEL` in `config_game.h` or `config_debug.h`, respectively.
- Set `ISVPRINT=1` in the `Makefile` (or `UNF=1` if using a USB-enabled flashcart on console)
- Rebuild the repo, and launch an emulator with debug console support such as Parallel Launcher.

The demo recording mode will boot into the level you set, from which you can start moving around and interacting with the level and camera. Press the Start button to end the demo

To test the demo after it's done:

- Enable `KEEP_MARIO_HEAD` and comment out `DISABLE_DEMO` and `DEMO_RECORDING_MODE` in `config_goddard.h`.
- If you set a `TEST_LEVEL`, comment that out too.
- Set `ISVPRINT=0` in the Makefile if you don't need debug printing anymore.
- Build the game again and wait on the title screen.

To see demos faster on the title screen, edit `PRESS_START_DEMO_TIMER` in `src/game/demo_system.h`. The default is 800 frames (close to 27 seconds).