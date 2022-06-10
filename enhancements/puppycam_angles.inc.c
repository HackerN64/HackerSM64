#include "game/puppycam2.h"
///This is the bit that defines where the angles happen. They're basically environment boxes that dictate camera behaviour.
///Permaswap is a boolean that simply determines wether or not when the camera changes at this point it stays changed. 0 means it resets when you leave, and 1 means it stays changed.
///The camera position fields accept "32767" as an ignore flag.
///The script supports anything that does not take an argument. It's reccomended to keep the scripts in puppycam_scripts.inc.c for the sake of cleanliness.
///If you do not wish to use a script in the angle, then just leave the field as 0.

// #define PUPPYCAM_SAMPLES

struct newcam_hardpos newcam_fixedcam[] =
{

#ifdef PUPPYCAM_SAMPLES
// Example Scripts
{/*Level ID*/ 16,/*Area ID*/ 1,/*Permaswap*/ 0,/*Mode*/ NC_MODE_FIXED_NOMOVE,/*Script*/ 0, //Standard params.
/*X begin*/ -540,/*Y begin*/ 800,/*Z begin*/ -3500, //Where the activation box begins
/*X end*/ 540,/*Y end*/ 2000,/*Z end*/ -1500, //Where the activation box ends.
/*Cam X*/ 0,/*Cam Y*/ 1500,/*Cam Z*/ -1000, //The position the camera gets placed for NC_MODE_FIXED and NC_MODE_FIXED_NOMOVE
/*Look X*/ 0,/*Look Y*/ 800,/*Look Z*/ -2500}, //The position the camera looks at for NC_MODE_FIXED_NOMOVE

///Another example angle. This activates a script that slowly rotates the camera around the area.
{/*Level ID*/ 16,/*Area ID*/ 1,/*Permaswap*/ 0,/*Mode*/ NC_MODE_NOROTATE,/*Script*/ 0, //Standard params.
/*X begin*/ 5716,/*Y begin*/ 400,/*Z begin*/ -859, //Where the activation box begins
/*X end*/ 6908,/*Y end*/ 1000,/*Z end*/ 62, //Where the activation box ends.
/*Cam X*/ 32767,/*Cam Y*/ 32767,/*Cam Z*/ 32767, //The position the camera gets placed for NC_MODE_FIXED and NC_MODE_FIXED_NOMOVE
/*Look X*/ 32767,/*Look Y*/ 32767,/*Look Z*/ 32767}, //The position the camera looks at for NC_MODE_FIXED_NOMOVE
#endif

};