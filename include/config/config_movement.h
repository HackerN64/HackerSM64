#pragma once

/*********************
 * MOVEMENT SETTINGS *
 *********************/

// Fixes Mario's turn ground radius by making it dependent on the analog stick magnitude.
//#define GROUND_TURN_FIX

// Fixes Mario's turn ground radius by allowing Mario to turn around at any speed.
// Basically a simpler version of GROUND_TURN_FIX but smoother & closer to vanilla.
#define GROUND_TURNING_AROUND_FIX

// Improved hanging:
// - Doesn't require holding down the A button
// - Percise turning control
// - Preventis falling from the edges
#define BETTER_HANGING

// Change the movement speed when hanging from a ceiling (the vanilla value is 4.0f, has no effect if BETTER_HANGING is enabled)
#define HANGING_SPEED 12.0f

// Prevents Mario from falling asleep while idle
#define NO_SLEEP

// Disables fall damage
#define NO_FALL_DAMAGE

// Disables the scream that mario makes when falling off a great height (this is separate from actual fall damage)
//#define NO_FALL_DAMAGE_SOUND

// Fall height for normal fall damage. Vanilla is 1150.0f
#define FALL_DAMAGE_HEIGHT_SMALL 1150.0f

// Fall height for double fall damage. Vanilla is 3000.0f
#define FALL_DAMAGE_HEIGHT_LARGE 3000.0f

// Disables Mario getting stuck in snow and sand when falling
// #define NO_GETTING_BURIED

// Prevents hands-free holding. (WARNING: Enabling this may make you a total buzzkill)
// #define HANDS_FREE_HOLDING_FIX

// Prevents Mario losing his cap.
//#define PREVENT_CAP_LOSS

// Platform displacement 2 also known as momentum patch. Makes Mario keep the momemtum from moving platforms. Doesn't break treadmills anymore!
#define PLATFORM_DISPLACEMENT_2

// Use Shindou's pole behavior
//#define SHINDOU_POLES

// If A and Z are pressed on the same frame, Mario will long jump instead of ground pound.
#define EASIER_LONG_JUMPS

// Hold Z while twirling to descend faster
#define Z_TWIRL

// Disables bonks when ground pounding next to a wall
#define DISABLE_GROUNDPOUND_BONK

// Allows Mario to jump kick on steep surfaces that are set to be non slippery, instead of being forced to dive
#define JUMP_KICK_FIX

// Allow Mario to grab hangable ceilings from any state
#define HANGING_FIX

// The last frame that will be considered a firsty when wallkicking
#define FIRSTY_LAST_FRAME 0

// The maximum angle the player can wall kick, in degrees. 0..90. To allow 45 degree wall kicks, you must supply `46` to allow 45 and under.
#define WALL_KICK_DEGREES 45

// This is vanilla behavior, disable to allow ledge grabbing on
// any surface angle
// #define LEDGE_GRABS_CHECK_SLOPE_ANGLE

// Disable BLJs and crush SimpleFlips's dreams
//#define DISABLE_BLJ


// -- Compatibility safeguards. Don't mess with these unless you know what you're doing. --

// #ifdef GROUND_TURNING_AROUND_FIX
// #undef GROUND_TURN_FIX
// #endif // GROUND_TURNING_AROUND_FIX
