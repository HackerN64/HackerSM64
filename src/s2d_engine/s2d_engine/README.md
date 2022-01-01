# S2DEX Text Engine

## How to "Install"
- Clone this repo into a folder in your SM64 decomp or homebrew repo
- Include that folder in your makefile's `SRC_DIRS`
- Add that folder to the linker script (`sm64.ld`) or specfile
- Wherever you're including your font (I use actors/group0.c for SM64), insert:
```c
#include "src/s2d_engine/config.h"
#include FONT_C_FILE
```
- You're ready to use S2DEX Text Engine!

## Simple Usage (Sample Program)
```c
#include "s2d_engine/init.h"
#include "s2d_engine/s2d_draw.h"
#include "s2d_engine/s2d_print.h"

char myString[] = "This is a " SCALE "200" "test string!\n"
                "Supports a bunch of standard " ROTATE "-36" "escape characters!\n"
                "\tIncluding " COLOR "255 0 0 0" "Colorful text!\n"
                COLOR "255 255 255 0" DROPSHADOW "And look at these shadows!";

// ...
void render_game(void) {
	// do game rendering and stuff
	// ...

	// initialized S2DEX; only needed once before all prints
	s2d_init();

	// Allows you to print from anywhere in the game
	// as long as this is run directly afterwards
	s2d_handle_deferred();

	// reloads the original microcode; only needed once after all prints
	s2d_stop();
}

void some_actor_function(void) {
	s2d_print_deferred(50, 50, myString); // It's that easy!
}

```


## Command Usage
(All numbers must be in base 10)
- `SCALE "N"` - Scales text by an percentage (`25` for 25%, `200` for 200%, `-50` for upside down at 50%, etc.)
- `TRANSLATE "X Y"` - Moves the rest of the string to the specified X and Y position
- `COLOR "R G B A"` - Sets the text color to the specified value
- `DROPSHADOW` - Toggles a drop shadow that shares the color of the current text
- `ROTATE "N"` - (Deprecated due to plugin incompatibility) Rotates each glyph by N degrees (Can be negative, useful for italics)
