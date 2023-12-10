#ifndef DIALOG_CONTROL_H
#define DIALOG_CONTROL_H

#include "dialog_ids.h"
#include "course_table.h"

// General string macros
#define HEX(s)       GLUE2(0x, s)
#define HEX_STR(s)   STRING2(GLUE2(\x, s))

// Control characters (Must be hexadecimal without the
// '0x' prefix, possible values range from 00-1F)
#define CONTROL_CHAR_TERMINATOR 00 // '\0' (RESERVED!)
#define CONTROL_CHAR_TAB        09 // '\t' (RESERVED!)
#define CONTROL_CHAR_NEWLINE    0A // '\n' (RESERVED!)
#define CONTROL_CHAR_COL        1B // '\033'
#define CONTROL_CHAR_RESET      1C // '\034'

// Additional control macros
#define CHAR_VALUE_IGNORE "-"


/********************************** BEGIN USER DIALOG CONTROL **********************************/


/**
 * TEXT_SET_RGB("XXXXXX"), TEXT_SET_RGBA("XXXXXXXX")
 *
 * Set color of text to an RGB value.
 * e.g. TEXT_SET_RGB("00FF00") will set the color to green.
 * Will use gDialogTextAlpha as the alpha value if alpha is not specified.
 * 
 * Example: "normal text " TEXT_SET_RGBA("FF00007F") "transparent red text"
 */
#define TEXT_SET_RGB(color) \
    HEX_STR(CONTROL_CHAR_COL) color CHAR_VALUE_IGNORE CHAR_VALUE_IGNORE
#define TEXT_SET_RGBA(color) \
    HEX_STR(CONTROL_CHAR_COL) color

/**
 * TEXT_RESET_RGBA
 *
 * Reset the text color back to the default text color.
 * 
 * Example: "colored text " TEXT_RESET_RGBA "normal text"
 */
#define TEXT_RESET_RGBA \
    HEX_STR(CONTROL_CHAR_RESET)


/*********************************** END USER DIALOG CONTROL ***********************************/


//! NOTE: The following section is just to assist with VSCode autocomplete and should not be considered functional:
#undef DEFINE_DIALOG
#define DEFINE_DIALOG(id, voice, linesPerBox, leftOffset, bottomOffset, dialogText)

#undef COURSE_ACTS
#define COURSE_ACTS(id, name, a, b, c, d, e, f)

#undef SECRET_STAR
#define SECRET_STAR(id, name)

#undef CASTLE_SECRET_STARS
#define CASTLE_SECRET_STARS(str)

#undef EXTRA_TEXT
#define EXTRA_TEXT(id, str)

#endif // DIALOG_CONTROL_H
