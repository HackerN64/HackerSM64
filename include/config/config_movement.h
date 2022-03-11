#pragma once

/*********************
 * MOVEMENT SETTINGS *
 *********************/

// Changes Mario's ground turn radius by making it dependent on the analog stick magnitude and speed.
// #define VELOCITY_BASED_TURN_SPEED

// Allows Mario to easily side flip when moving forwards at any speed.
// #define SIDE_FLIP_AT_LOW_SPEEDS

// Allows Mario to aim towards a new direction at the end of turning around,
// and allows Mario to turn around multiple times in a row.
// #define RESET_DIRECTION_WHEN_TURNING_AROUND

// Improved hanging:
// - Doesn't require holding down the A button.
// - Precise turning control.
// - Prevents falling from the edges.
#define BETTER_HANGING

// Change the movement speed when hanging from a ceiling (the vanilla value is 4.0f, has no effect if BETTER_HANGING is enabled).
#define HANGING_SPEED 12.0f

// Prevents Mario from falling asleep while idle.
// #define NO_SLEEP

// Disables fall damage.
#define NO_FALL_DAMAGE

// Disables the scream that mario makes when falling off a great height (this is separate from actual fall damage).
// #define NO_FALL_DAMAGE_SOUND

// Fall height for normal fall damage. Vanilla is 1150.
#define FALL_DAMAGE_HEIGHT_SMALL 1150

// Fall height for double fall damage. Vanilla is 3000.
#define FALL_DAMAGE_HEIGHT_LARGE 3000

// Disables Mario getting stuck in snow and sand when falling.
// #define NO_GETTING_BURIED

// Prevents hands-free holding.
#define HANDS_FREE_HOLDING_FIX

// Prevents Mario from losing his cap.
// #define PREVENT_CAP_LOSS

// Enables Platform Displacement 2, also known as momentum patch. Makes Mario keep the momemtum from moving platforms.
#define PLATFORM_DISPLACEMENT_2

// Uses Shindou's pole behavior.
#define SHINDOU_POLES (0 | VERSION_SH)

// If A and Z are pressed on the same frame while running, Mario will long jump instead of ground pound.
#define EASIER_LONG_JUMPS

// Enable being able to immediately long jump after landing from a long jump.
#define IMMEDIATE_LONG_JUMPS

// Prevents long jumps getting canceled by steep floors by increasing Mario's initial vertical velocity when long jumping uphill.
#define PITCHED_LONG_JUMPS

// Enables the ability to hold Z while twirling to descend faster.
#define Z_TWIRL

// Enables the ability to dive during the ground pound spin animation, similar to Super Mario Odyssey.
// #define GROUND_POUND_DIVE

// Makes GROUND_POUND_DIVE change Mario's yaw to the exact direction of the analog stick.
// Has no effect if GROUND_POUND_DIVE is disabled.
// #define GROUND_POUND_DIVE_CHANGES_DIRECTION

// Disables bonks when ground pounding next to a wall.
#define DISABLE_GROUNDPOUND_BONK

// Allows Mario to jump kick on steep surfaces that are set to be non slippery, instead of being forced to dive.
#define JUMP_KICK_FIX

// Allows Mario to transition into a punch/dive/jump kick action when landing.
#define ATTACK_FROM_LANDING

// Allows Mario to grab hangable ceilings from any state.
#define HANGING_FIX

// The last frame after hitting a wall that will be considered a firsty when wallkicking.
#define FIRSTY_LAST_FRAME 1

// The maximum angle the player can wall kick, in degrees. 0..90. To allow 45 degree wall kicks, you must supply `46` to allow 45 and under.
#define WALL_KICK_DEGREES 45

// This is vanilla behavior, disable it to allow ledge grabbing regardless of floor pitch.
#define LEDGE_GRABS_CHECK_SLOPE_ANGLE

// Enables bonking and wall kicking at the top of walls. This fixes janky vanilla behavior caused by ledge grab mechanics.
#define WALL_KICK_TOP_OF_WALLS

// Disables BLJs and crushes SimpleFlips's dreams.
// #define DISABLE_BLJ

// Re-enables upwarping when entering water. Forces you to only enter water from the top.
// #define WATER_PLUNGE_UPWARP

// When Mario hits a floor underwater, his pitch approaches the floor pitch instead of instantly snapping to it.
#define SMOOTH_PITCH_WHEN_HITTING_FLOOR_UNDERWATER

// Number of subframe steps (quarter steps).
#define NUM_STEPS_GROUND 16 // Vanilla is 4. 16 is recommended.
#define NUM_STEPS_AIR    16 // Vanilla is 4. 16 is recommended.
#define NUM_STEPS_WATER   4 // Vanilla is 1.  4 is recommended.

// Increases/decreases the number of subframe steps (quarter steps) depending on Mario's speed.
// Not needed if the above values are high enough.
// #define VARIABLE_NUM_STEPS

// Sends a raycast between Mario's current and next position so that walls don't get skipped.
// This also removes subframe steps (quarter steps).
#define RAYCAST_WALL_COLLISION
