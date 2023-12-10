/**
 * DEFINE_DIALOG macro
 *    - Dialog enum ID (see dialog_ids.h)
 *    - Voice Sound (see sounds.h)
 *    - Lines per box
 *    - Left offset (Distance from left edge of the screen to dialog box)
 *    - Bottom offset (Distance from bottom of the screen to the top of the dialog box, SCREEN_HEIGHT would be the top)
 *    - Text
 * 
 * 
 * Control characters:
 *   Special text commands are supported, for functions such as custom text color.
 *   See "ingame_menu.h" for more details.
 * 
 * 
 * Special Characters:
 *   Here are some special characters you can use:
 *      Ⓐ
 *      Ⓑ
 *      Ⓒ
 *      Ⓩ
 *      Ⓡ
 *      “ and ” (left and right quotes)
 *      ◀▶▼▲
 *      ★ ✪
 *
 * 
 * See segment2.c LUTs for more info:
 *    - main_font_lut
 *    - main_hud_utf8_2byte_lut
 *    - main_hud_utf8_3byte_lut
 *    - main_hud_utf8_4byte_lut
 */

DEFINE_DIALOG(DIALOG_000, NO_SOUND, 6, 30, 200, "\
Wow! You're smack in the\n\
middle of the battlefield.\n\
You'll find the Power\n\
Stars that Bowser stole\n\
inside the painting\n\
worlds.\n\
First, talk to the\n\
Bob-omb Buddy. (Press Ⓑ\n\
to talk.) He'll certainly\n\
help you out, and so will\n\
his comrades in other\n\
areas.\n\
To read signs, stop, face\n\
them and press Ⓑ. Press Ⓐ\n\
or Ⓑ to scroll ahead. You\n\
can talk to some other\n\
characters by facing them\n\
and pressing Ⓑ.")

DEFINE_DIALOG(DIALOG_001, SOUND_OBJ_BOBOMB_BUDDY_TALK, 4, 95, 200, "\
Watch out! If you wander\n\
around here, you're liable\n\
to be plastered by a\n\
water bomb!\n\
Those enemy Bob-ombs love\n\
to fight, and they're\n\
always finding ways to\n\
attack.\n\
This meadow has become\n\
a battlefield ever since\n\
the Big Bob-omb got his\n\
paws on the Power Star.\n\
Can you recover the Star\n\
for us? Cross the bridge\n\
and go left up the path\n\
to find the Big Bob-omb.\n\
Please come back to see\n\
me after you've retrieved\n\
the Power Star!")

DEFINE_DIALOG(DIALOG_002, SOUND_OBJ_BOBOMB_BUDDY_TALK, 4, 95, 200, "\
Hey, you! It's dangerous\n\
ahead, so listen up! Take\n\
my advice.\n\
\n\
Cross the two\n\
bridges ahead, then\n\
watch for falling\n\
water bombs.\n\
The Big Bob-omb at the\n\
top of the mountain is\n\
very powerful--don't let\n\
him grab you!\n\
We're Bob-omb Buddies,\n\
and we're on your side.\n\
You can talk to us\n\
whenever you'd like to!")

DEFINE_DIALOG(DIALOG_003, SOUND_OBJ_BOBOMB_BUDDY_TALK, 5, 95, 200, "\
Thank you, Mario! The Big\n\
Bob-omb is nothing but a\n\
big dud now! But the\n\
battle for the castle has\n\
just begun.\n\
Other enemies are holding\n\
the other Power Stars. If\n\
you recover more Stars,\n\
you can open new doors\n\
that lead to new worlds!\n\
My Bob-omb Buddies are\n\
waiting for you. Be sure\n\
to talk to them--they'll\n\
set up cannons for you.")

DEFINE_DIALOG(DIALOG_004, SOUND_OBJ_BOBOMB_BUDDY_TALK, 3, 95, 200, "\
We're peace-loving\n\
Bob-ombs, so we don't use\n\
cannons.\n\
But if you'd like\n\
to blast off, we don't\n\
mind. Help yourself.\n\
We'll prepare all of the\n\
cannons in this course for\n\
you to use. Bon Voyage!")

DEFINE_DIALOG(DIALOG_005, SOUND_OBJ_KOOPA_TALK, 3, 30, 200, "\
Hey, Mario! Is it true\n\
that you beat the Big\n\
Bob-omb? Cool!\n\
You must be strong. And\n\
pretty fast. So, how fast\n\
are you, anyway?\n\
Fast enough to beat me...\n\
Koopa the Quick? I don't\n\
think so. Just try me.\n\
How about a race to the\n\
mountaintop, where the\n\
Big Bob-omb was?\n\
Whaddya say? When I say\n\
“Go,” let the race begin!\n\
\n\
Ready....\n\
\n\
\tGo!\t\tDon't Go")

DEFINE_DIALOG(DIALOG_006, SOUND_OBJ_KOOPA_TALK, 3, 30, 200, "\
Hey!!! Don't try to scam\n\
ME. You've gotta run\n\
the whole course.\n\
Later. Look me up when\n\
you want to race for\n\
real.")

DEFINE_DIALOG(DIALOG_007, SOUND_OBJ_KOOPA_TALK, 5, 30, 200, "\
Hufff...fff...pufff...\n\
Whoa! You...really...are...\n\
fast! A human blur!\n\
Here you go--you've won\n\
it, fair and square!")

DEFINE_DIALOG(DIALOG_008, NO_SOUND, 4, 30, 200, "\
BEWARE OF CHAIN CHOMP\n\
Extreme Danger!\n\
Get close and press Ⓒ▲\n\
for a better look.\n\
Scary, huh?\n\
See the Red Coin on top\n\
of the stake?\n\
\n\
When you collect eight of\n\
them, a Power Star will\n\
appear in the meadow\n\
across the bridge.")

DEFINE_DIALOG(DIALOG_009, SOUND_OBJ_KOOPA_TALK, 5, 30, 200, "\
Long time, no see! Wow,\n\
have you gotten fast!\n\
Have you been training\n\
on the sly, or is it the\n\
power of the Stars?\n\
I've been feeling down\n\
about losing the last\n\
race. This is my home\n\
course--how about a\n\
rematch?\n\
The goal is in\n\
Windswept Valley.\n\
Ready?\n\
\n\
\tGo\t\t Don't Go")

/**
 * Special case: if the voice is SEQ_EVENT_SOLVE_PUZZLE,
 * then play_dialog_sound will run play_puzzle_jingle
 * instead of playing a sound effect
 */
DEFINE_DIALOG(DIALOG_010, SEQ_EVENT_SOLVE_PUZZLE, 4, 30, 200, "\
You've stepped on the\n\
Wing Cap Switch. Wearing\n\
the Wing Cap, you can\n\
soar through the sky.\n\
Now Wing Caps will pop\n\
out of all the red blocks\n\
you find.\n\
\n\
Would you like to Save?\n\
\n\
\tYes\t\tNo")

DEFINE_DIALOG(DIALOG_011, SEQ_EVENT_SOLVE_PUZZLE, 4, 30, 200, "\
You've just stepped on\n\
the Metal Cap Switch!\n\
The Metal Cap makes\n\
Mario invincible.\n\
Now Metal Caps will\n\
pop out of all of the\n\
green blocks you find.\n\
\n\
Would you like to Save?\n\
\n\
\tYes\t\tNo")

DEFINE_DIALOG(DIALOG_012, SEQ_EVENT_SOLVE_PUZZLE, 4, 30, 200, "\
You've just stepped on\n\
the Vanish Cap Switch.\n\
The Vanish Cap makes\n\
Mario disappear.\n\
Now Vanish Caps will pop\n\
from all of the blue\n\
blocks you find.\n\
\n\
Would you like to Save?\n\
\n\
\tYes\t\tNo")

DEFINE_DIALOG(DIALOG_013, NO_SOUND, 5, 30, 200, "\
You've collected 100\n\
coins! Mario gains more\n\
power from the castle.\n\
Do you want to Save?\n\
\tYes\t\tNo")

DEFINE_DIALOG(DIALOG_014, NO_SOUND, 4, 30, 200, "\
Wow! Another Power Star!\n\
Mario gains more courage\n\
from the power of the\n\
castle.\n\
Do you want to Save?\n\
\n\
\tYou Bet\tNot Now")

DEFINE_DIALOG(DIALOG_015, NO_SOUND, 4, 30, 200, "\
You can punch enemies to\n\
knock them down. Press Ⓐ\n\
to jump, Ⓑ to punch.\n\
Press Ⓐ then Ⓑ to Kick.\n\
To pick something up,\n\
press Ⓑ, too. To throw\n\
something you're holding,\n\
press Ⓑ again.")

DEFINE_DIALOG(DIALOG_016, NO_SOUND, 3, 30, 200, "\
Hop on the shiny shell and\n\
ride wherever you want to\n\
go! Shred those enemies!")

DEFINE_DIALOG(DIALOG_017, SOUND_OBJ_KING_BOBOMB_TALK, 4, 30, 200, "\
I'm the Big Bob-omb, lord\n\
of all blasting matter,\n\
king of ka-booms the\n\
world over!\n\
How dare you scale my\n\
mountain? By what right\n\
do you set foot on my\n\
imperial mountaintop?\n\
You may have eluded my\n\
guards, but you'll never\n\
escape my grasp...\n\
\n\
...and you'll never take\n\
away my Power Star. I\n\
hereby challenge you,\n\
Mario!\n\
If you want the Star I\n\
hold, you must prove\n\
yourself in battle.\n\
\n\
Can you pick me up from\n\
the back and hurl me to\n\
this royal turf? I think\n\
that you cannot!")

DEFINE_DIALOG(DIALOG_018, NO_SOUND, 4, 30, 200, "\
I'm sleeping because...\n\
...I'm sleepy. I don't\n\
like being disturbed.\n\
Please walk quietly.")

DEFINE_DIALOG(DIALOG_019, NO_SOUND, 2, 30, 200, "\
Shhh! Please walk\n\
quietly in the hallway!")

DEFINE_DIALOG(DIALOG_020, NO_SOUND, 6, 95, 150, "\
Dear Mario:\n\
Please come to the\n\
castle. I've baked\n\
a cake for you.\n\
Yours truly--\n\
Princess Toadstool")

DEFINE_DIALOG(DIALOG_021, SOUND_OBJ_BOWSER_INTRO_LAUGH, 5, 95, 200, "\
Welcome.\n\
No one's home!\n\
Now scram--\n\
and don't come back!\n\
Gwa ha ha!")

DEFINE_DIALOG(DIALOG_022, SOUND_OBJ_BOWSER_INTRO_LAUGH, 2, 95, 200, "\
You need a key to open\n\
this door.")

DEFINE_DIALOG(DIALOG_023, SOUND_OBJ_BOWSER_INTRO_LAUGH, 3, 95, 200, "\
This key doesn't fit!\n\
Maybe it's for the\n\
basement...")

DEFINE_DIALOG(DIALOG_024, SOUND_OBJ_BOWSER_INTRO_LAUGH, 5, 95, 200, "\
You need Star power to\n\
open this door. Recover a\n\
Power Star from an enemy\n\
inside one of the castle's\n\
paintings.")

DEFINE_DIALOG(DIALOG_025, SOUND_OBJ_BOWSER_INTRO_LAUGH, 4, 95, 200, "\
It takes the power of\n\
3 Stars to open this\n\
door. You need %d more\n\
Stars.")

DEFINE_DIALOG(DIALOG_026, SOUND_OBJ_BOWSER_INTRO_LAUGH, 4, 95, 200, "\
It takes the power of\n\
8 Stars to open this\n\
door. You need %d more\n\
Stars.")

DEFINE_DIALOG(DIALOG_027, SOUND_OBJ_BOWSER_INTRO_LAUGH, 4, 95, 200, "\
It takes the power of\n\
30 Stars to open this\n\
door. You need %d more\n\
Stars.")

DEFINE_DIALOG(DIALOG_028, SOUND_OBJ_BOWSER_INTRO_LAUGH, 4, 95, 200, "\
It takes the power of\n\
50 Stars to open this\n\
door. You need %d more\n\
Stars.")

DEFINE_DIALOG(DIALOG_029, SOUND_OBJ_BOWSER_INTRO_LAUGH, 5, 95, 200, "\
To open the door that\n\
leads to the “endless”\n\
stairs, you need 70\n\
Stars.\n\
Bwa ha ha!")

DEFINE_DIALOG(DIALOG_030, NO_SOUND, 6, 30, 200, "\
Hello! The Lakitu Bros.,\n\
cutting in with a live\n\
update on Mario's\n\
progress. He's about to\n\
learn a technique for\n\
sneaking up on enemies.\n\
The trick is this: He has\n\
to walk very slowly in\n\
order to walk quietly.\n\
\n\
\n\
\n\
And wrapping up filming\n\
techniques reported on\n\
earlier, you can take a\n\
look around using Ⓒ▶ and\n\
Ⓒ◀. Press Ⓒ▼ to view the\n\
action from a distance.\n\
When you can't move the\n\
camera any farther, the\n\
buzzer will sound. This is\n\
the Lakitu Bros.,\n\
signing off.")

DEFINE_DIALOG(DIALOG_031, NO_SOUND, 5, 30, 200, "\
No way! You beat me...\n\
again!! And I just spent\n\
my entire savings on\n\
these new Koopa\n\
Mach 1 Sprint shoes!\n\
Here, I guess I have to\n\
hand over this Star to\n\
the winner of the race.\n\
Congrats, Mario!")

DEFINE_DIALOG(DIALOG_032, NO_SOUND, 5, 30, 200, "\
If you get the Wing Cap,\n\
you can fly! Put the cap\n\
on, then do a Triple\n\
Jump--jump three times\n\
in a row--to take off.\n\
You can fly even higher\n\
if you blast out of a\n\
cannon wearing the\n\
Wing Cap!\n\
\n\
Use the Ⓒ Buttons to look\n\
around while flying, and\n\
press Ⓩ to land.")

DEFINE_DIALOG(DIALOG_033, NO_SOUND, 6, 30, 200, "\
Ciao! You've reached\n\
Princess Toadstool's\n\
castle via a warp pipe.\n\
Using the controller is a\n\
piece of cake. Press Ⓐ to\n\
jump and Ⓑ to attack.\n\
Press Ⓑ to read signs,\n\
too. Use the Control Stick\n\
in the center of the\n\
controller to move Mario\n\
around. Now, head for\n\
the castle.")

DEFINE_DIALOG(DIALOG_034, NO_SOUND, 6, 30, 200, "\
Good afternoon. The\n\
Lakitu Bros., here,\n\
reporting live from just\n\
outside the Princess's\n\
castle.\n\
\n\
Mario has just arrived\n\
on the scene, and we'll\n\
be filming the action live\n\
as he enters the castle\n\
and pursues the missing\n\
Power Stars.\n\
As seasoned cameramen,\n\
we'll be shooting from the\n\
recommended angle, but\n\
you can change the\n\
camera angle by pressing\n\
the Ⓒ Buttons.\n\
If we can't adjust the\n\
view any further, we'll\n\
buzz. To take a look at\n\
the surroundings, stop\n\
and press Ⓒ▲.\n\
\n\
Press Ⓐ to resume play.\n\
Switch camera modes with\n\
the Ⓡ Button. Signs along\n\
the way will review these\n\
instructions.\n\
\n\
For now, reporting live,\n\
this has been the\n\
Lakitu Bros.")

DEFINE_DIALOG(DIALOG_035, NO_SOUND, 5, 30, 200, "\
There are four camera, or\n\
“Ⓒ,” Buttons. Press Ⓒ▲\n\
to look around using the\n\
Control Stick.\n\
\n\
You'll usually see Mario\n\
through Lakitu's camera.\n\
It is the camera\n\
recommended for normal\n\
play.\n\
You can change angles by\n\
pressing Ⓒ▶. If you press\n\
Ⓡ, the view switches to\n\
Mario's camera, which\n\
is directly behind him.\n\
Press Ⓡ again to return\n\
to Lakitu's camera. Press\n\
Ⓒ▼ to see Mario from\n\
afar, using either\n\
Lakitu's or Mario's view.")

DEFINE_DIALOG(DIALOG_036, NO_SOUND, 5, 30, 200, "\
OBSERVATION PLATFORM\n\
Press Ⓒ▲ to take a look\n\
around. Don't miss\n\
anything!\n\
\n\
Press Ⓡ to switch to\n\
Mario's camera. It\n\
always follows Mario.\n\
Press Ⓡ again to switch\n\
to Lakitu's camera.\n\
Pause the game and\n\
switch the mode to “fix”\n\
the camera in place while\n\
holding Ⓡ. Give it a try!")

DEFINE_DIALOG(DIALOG_037, SOUND_OBJ_BIG_PENGUIN_YELL, 2, 30, 200, "\
I win! You lose!\n\
Ha ha ha ha!\n\
You're no slouch, but I'm\n\
a better sledder!\n\
Better luck next time!")

DEFINE_DIALOG(DIALOG_038, NO_SOUND, 3, 95, 200, "\
Reacting to the Star\n\
power, the door slowly\n\
opens.")

DEFINE_DIALOG(DIALOG_039, NO_SOUND, 4, 30, 200, "\
No visitors allowed,\n\
by decree of\n\
the Big Bob-omb\n\
\n\
I shall never surrender my\n\
Stars, for they hold the\n\
power of the castle in\n\
their glow.\n\
They were a gift from\n\
Bowser, the Koopa King\n\
himself, and they lie well\n\
hidden within my realm.\n\
Not a whisper of their\n\
whereabouts shall leave\n\
my lips. Oh, all right,\n\
perhaps one hint:\n\
Heed the Star names at\n\
the beginning of the\n\
course.\n\
\t--The Big Bob-omb")

DEFINE_DIALOG(DIALOG_040, NO_SOUND, 3, 30, 200, "\
Warning!\n\
Cold, Cold Crevasse\n\
Below!")

DEFINE_DIALOG(DIALOG_041, SOUND_OBJ_KOOPA_TALK, 3, 30, 200, "\
I win! You lose!\n\
Ha ha ha!\n\
\n\
That's what you get for\n\
messin' with Koopa the\n\
Quick.\n\
Better luck next time!")

DEFINE_DIALOG(DIALOG_042, NO_SOUND, 4, 30, 200, "\
Caution! Narrow Bridge!\n\
Cross slowly!\n\
\n\
\n\
You can jump to the edge\n\
of the cliff and hang on,\n\
and you can climb off the\n\
edge if you move slowly.\n\
When you want to let go,\n\
either press Ⓩ or press\n\
the Control Stick in the\n\
direction of Mario's back.\n\
To climb up, press Up on\n\
the Control Stick. To\n\
scurry up quickly, press\n\
the Ⓐ Button.")

DEFINE_DIALOG(DIALOG_043, NO_SOUND, 5, 30, 200, "\
If you jump and hold the\n\
Ⓐ Button, you can hang on\n\
to some objects overhead.\n\
It's the same as grabbing\n\
a flying bird!")

DEFINE_DIALOG(DIALOG_044, NO_SOUND, 5, 95, 200, "\
Whooo's there? Whooo\n\
woke me up? It's still\n\
daylight--I should be\n\
sleeping!\n\
\n\
Hey, as long as I'm\n\
awake, why not take a\n\
short flight with me?\n\
Press and hold Ⓐ to grab\n\
on. Release Ⓐ to let go.\n\
I'll take you wherever\n\
you want to go, as long\n\
as my wings hold out.\n\
Watch my shadow, and\n\
grab on.")

DEFINE_DIALOG(DIALOG_045, NO_SOUND, 6, 95, 200, "\
Whew! I'm just about\n\
flapped out. You should\n\
lay off the pasta, Mario!\n\
That's it for now. Press\n\
Ⓐ to let go. Okay,\n\
bye byyyyyyeeee!")

DEFINE_DIALOG(DIALOG_046, NO_SOUND, 5, 30, 200, "\
You have to master three\n\
important jumping\n\
techniques.\n\
First try the Triple Jump.\n\
\n\
Run fast, then jump three\n\
times, one, two, three.\n\
If you time the jumps\n\
right, you'll hop, skip,\n\
then jump really high.\n\
Next, go for distance\n\
with the Long Jump. Run,\n\
press Ⓩ to crouch then Ⓐ\n\
to jump really far.\n\
\n\
To do the Wall Kick, press\n\
Ⓐ to jump at a wall, then\n\
jump again when you hit\n\
the wall.\n\
\n\
Got that? Triple Jump,\n\
Long Jump, Wall Kick.\n\
Practice, practice,\n\
practice. You don't stand\n\
a chance without them.")

DEFINE_DIALOG(DIALOG_047, SOUND_OBJ_BOBOMB_BUDDY_TALK, 2, 95, 200, "\
Hi! I'll prepare the\n\
cannon for you!")

DEFINE_DIALOG(DIALOG_048, NO_SOUND, 4, 30, 200, "\
Snow Mountain Summit\n\
Watch for slippery\n\
conditions! Please enter\n\
the cottage first.")

DEFINE_DIALOG(DIALOG_049, NO_SOUND, 5, 30, 200, "\
Remember that tricky Wall\n\
Kick jump? It's a\n\
technique you'll have to\n\
master in order to reach\n\
high places.\n\
Use it to jump from wall\n\
to wall. Press the\n\
Control Stick in the\n\
direction you want to\n\
bounce to gain momentum.\n\
Practice makes perfect!")

DEFINE_DIALOG(DIALOG_050, NO_SOUND, 4, 30, 200, "\
Hold Ⓩ to crouch and\n\
slide down a slope.\n\
Or press Ⓩ while in the\n\
air to Pound the Ground!\n\
If you stop, crouch, then\n\
jump, you'll do a\n\
Backward Somersault!\n\
Got that?\n\
There's more. Crouch and\n\
then jump to do a\n\
Long Jump! Or crouch and\n\
walk to...never mind.")

DEFINE_DIALOG(DIALOG_051, NO_SOUND, 6, 30, 200, "\
Climbing's easy! When you\n\
jump at trees, poles or\n\
pillars, you'll grab them\n\
automatically. Press Ⓐ to\n\
jump off backward.\n\
\n\
To rotate around the\n\
object, press Right or\n\
Left on the Control Stick.\n\
When you reach the top,\n\
press Up to do a\n\
handstand!\n\
Jump off from the\n\
handstand for a high,\n\
stylin' dismount.")

DEFINE_DIALOG(DIALOG_052, NO_SOUND, 5, 30, 200, "\
Stop and press Ⓩ to\n\
crouch, then press Ⓐ\n\
to do a high, Backward\n\
Somersault!\n\
\n\
To perform a Side\n\
Somersault, run, do a\n\
sharp U-turn and jump.\n\
You can catch lots of\n\
air with both jumps.")

DEFINE_DIALOG(DIALOG_053, NO_SOUND, 5, 30, 200, "\
Sometimes, if you pass\n\
through a coin ring or\n\
find a secret point in a\n\
course, a red number will\n\
appear.\n\
If you trigger five red\n\
numbers, a secret Star\n\
will show up.")

DEFINE_DIALOG(DIALOG_054, NO_SOUND, 5, 30, 200, "\
Welcome to the snow\n\
slide! Hop on! To speed\n\
up, press forward on the\n\
Control Stick. To slow\n\
down, pull back.")

DEFINE_DIALOG(DIALOG_055, SOUND_OBJ_BIG_PENGUIN_YELL, 4, 30, 200, "\
Hey-ey, Mario, buddy,\n\
howzit goin'? Step right\n\
up. You look like a fast\n\
sleddin' kind of guy.\n\
I know speed when I see\n\
it, yes siree--I'm the\n\
world champion sledder,\n\
you know. Whaddya say?\n\
How about a race?\n\
Ready...\n\
\n\
\tGo\t\t Don't Go")

DEFINE_DIALOG(DIALOG_056, SOUND_OBJ_BIG_PENGUIN_YELL, 6, 30, 200, "\
You brrrr-oke my record!\n\
Unbelievable! I knew\n\
that you were the coolest.\n\
Now you've proven\n\
that you're also the\n\
fastest!\n\
I can't award you a gold\n\
medal, but here, take this\n\
Star instead. You've\n\
earned it!")

DEFINE_DIALOG(DIALOG_057, SOUND_OBJ_BIG_PENGUIN_YELL, 4, 30, 200, "\
Egad! My baby!! Have you\n\
seen my baby??? She's\n\
the most precious baby in\n\
the whole wide world.\n\
(They say she has my\n\
beak...) I just can't\n\
remember where I left\n\
her.\n\
Let's see...I stopped\n\
for herring and ice cubes,\n\
then I...oohh! I just\n\
don't know!")

DEFINE_DIALOG(DIALOG_058, SOUND_OBJ_BIG_PENGUIN_YELL, 4, 30, 200, "\
You found my precious,\n\
precious baby! Where\n\
have you been? How can\n\
I ever thank you, Mario?\n\
Oh, I do have this...\n\
...Star. Here, take it\n\
with my eternal\n\
gratitude.")

DEFINE_DIALOG(DIALOG_059, SOUND_OBJ_BIG_PENGUIN_YELL, 4, 30, 200, "\
That's not my baby! She\n\
looks nothing like me!\n\
Her parents must be\n\
worried sick!")

DEFINE_DIALOG(DIALOG_060, NO_SOUND, 4, 30, 200, "\
ATTENTION!\n\
Read Before Diving In!\n\
\n\
\n\
If you stay under the\n\
water for too long, you'll\n\
run out of oxygen.\n\
\n\
Return to the surface for\n\
air or find an air bubble\n\
or coins to breathe while\n\
underwater.\n\
Press Ⓐ to swim. Hold Ⓐ\n\
to swim slow and steady.\n\
Tap Ⓐ with smooth timing\n\
to gain speed.\n\
Press Up on the\n\
Control Stick and press Ⓐ\n\
to dive.\n\
\n\
Press Down on the Control\n\
Stick and press Ⓐ to\n\
return to the surface.\n\
\n\
Hold Down and press Ⓐ\n\
while on the surface near\n\
the edge of the water to\n\
jump out.")

DEFINE_DIALOG(DIALOG_061, NO_SOUND, 4, 30, 200, "\
BRRR! Frostbite Danger!\n\
Do not swim here.\n\
I'm serious.\n\
  --The Penguin")

DEFINE_DIALOG(DIALOG_062, NO_SOUND, 3, 30, 200, "\
Hidden inside the green\n\
block is the amazing\n\
Metal Cap.\n\
Wearing it, you won't\n\
catch fire or be hurt\n\
by enemy attacks.\n\
You don't even have to\n\
breathe while wearing it.\n\
\n\
The only problem:\n\
It is too heavy to\n\
swim with.")

DEFINE_DIALOG(DIALOG_063, NO_SOUND, 5, 30, 200, "\
The Vanish Cap is inside\n\
the blue block. Mr. I.\n\
will be surprised, since\n\
you'll be invisible when\n\
you wear it!\n\
Even the Big Boo will be\n\
fooled--and you can walk\n\
through secret walls, too.")

DEFINE_DIALOG(DIALOG_064, NO_SOUND, 5, 30, 200, "\
When you put on the Wing\n\
Cap that comes from a\n\
red block, do the Triple\n\
Jump to soar high into\n\
the sky.\n\
Use the Control Stick to\n\
guide Mario. Pull back to\n\
to fly up, press forward\n\
to nose down, and press Ⓩ\n\
to land.")

DEFINE_DIALOG(DIALOG_065, NO_SOUND, 6, 30, 200, "\
Swimming Lessons!\n\
Tap Ⓐ to do the breast\n\
stroke. If you time the\n\
taps right, you'll swim\n\
fast.\n\
\n\
Press and hold Ⓐ to do a\n\
slow, steady flutter kick.\n\
Press Up on the Control\n\
Stick to dive, and pull\n\
back on the stick to head\n\
for the surface.\n\
To jump out of the water,\n\
hold Down on the Control\n\
Stick, then press Ⓐ.\n\
Easy as pie, right?\n\
\n\
\n\
But remember:\n\
Mario can't breathe under\n\
the water! Return to the\n\
surface for air when the\n\
Power Meter runs low.\n\
\n\
And one last thing: You\n\
can't open doors that\n\
are underwater.")

DEFINE_DIALOG(DIALOG_066, NO_SOUND, 5, 30, 200, "\
Mario, it's Peach!\n\
Please be careful! Bowser\n\
is so wicked! He will try\n\
to burn you with his\n\
horrible flame breath.\n\
Run around behind and\n\
grab him by the tail with\n\
the Ⓑ Button. Once you\n\
grab hold, swing him\n\
around in great circles.\n\
Rotate the Control Stick\n\
to go faster and faster.\n\
The faster you swing him,\n\
the farther he'll fly.\n\
\n\
Use the Ⓒ Buttons to look\n\
around, Mario. You have\n\
to throw Bowser into one\n\
of the bombs in the four\n\
corners.\n\
Aim well, then press Ⓑ\n\
again to launch Bowser.\n\
Good luck, Mario! Our\n\
fate is in your hands.")

DEFINE_DIALOG(DIALOG_067, SOUND_OBJ_BOWSER_LAUGH, 5, 30, 200, "\
Tough luck, Mario!\n\
Princess Toadstool isn't\n\
here...Gwa ha ha!! Go\n\
ahead--just try to grab\n\
me by the tail!\n\
You'll never be able to\n\
swing ME around! A wimp\n\
like you won't throw me\n\
out of here! Never! Ha!")

DEFINE_DIALOG(DIALOG_068, NO_SOUND, 5, 30, 200, "\
It's Lethal Lava Land!\n\
If you catch fire or fall\n\
into a pool of flames,\n\
you'll be hopping mad, but\n\
don't lose your cool.\n\
You can still control\n\
Mario--just try to keep\n\
calm!")

DEFINE_DIALOG(DIALOG_069, NO_SOUND, 6, 30, 200, "\
Sometimes you'll bump into\n\
invisible walls at the\n\
edges of the painting\n\
worlds. If you hit a wall\n\
while flying, you'll bounce\n\
back.")

DEFINE_DIALOG(DIALOG_070, NO_SOUND, 5, 30, 200, "\
You can return to the\n\
castle's main hall at any\n\
time from the painting\n\
worlds where the enemies\n\
live.\n\
Just stop, stand still,\n\
press Start to pause the\n\
game, then select\n\
“Exit Course.”\n\
\n\
You don't have to collect\n\
all Power Stars in one\n\
course before going on to\n\
the next.\n\
\n\
Return later, when you're\n\
more experienced, to pick\n\
up difficult ones.\n\
\n\
\n\
Whenever you find a Star,\n\
a hint for finding the\n\
next one will appear on\n\
the course's start screen.\n\
\n\
You can, however, collect\n\
any of the remaining\n\
Stars next. You don't\n\
have to recover the one\n\
described by the hint.")

DEFINE_DIALOG(DIALOG_071, NO_SOUND, 3, 30, 200, "\
Danger Ahead!\n\
Beware of the strange\n\
cloud! Don't inhale!\n\
If you feel faint, run for\n\
higher ground and fresh\n\
air!\n\
Circle: Shelter\n\
Arrow: Entrance-Exit")

DEFINE_DIALOG(DIALOG_072, NO_SOUND, 5, 30, 200, "\
High winds ahead!\n\
Pull your Cap down tight.\n\
If it blows off, you'll\n\
have to find it on this\n\
mountain.")

DEFINE_DIALOG(DIALOG_073, NO_SOUND, 4, 95, 200, "\
Aarrgh! Ahoy, matey. I\n\
have sunken treasure,\n\
here, I do.\n\
\n\
But to pluck the plunder,\n\
you must open the\n\
Treasure Chests in the\n\
right order.\n\
What order is that,\n\
ye say?\n\
\n\
\n\
I'll never tell!\n\
\n\
\t--The Cap'n")

DEFINE_DIALOG(DIALOG_074, NO_SOUND, 5, 30, 200, "\
You can grab on to the\n\
edge of a cliff or ledge\n\
with your fingertips and\n\
hang down from it.\n\
\n\
To drop from the edge,\n\
either press the Control\n\
Stick in the direction of\n\
Mario's back or press the\n\
Ⓩ Button.\n\
To get up onto the ledge,\n\
either press Up on the\n\
Control Stick or press Ⓐ\n\
as soon as you grab the\n\
ledge to climb up quickly.")

DEFINE_DIALOG(DIALOG_075, NO_SOUND, 5, 30, 200, "\
Mario!! My castle is in\n\
great peril. I know that\n\
Bowser is the cause...and\n\
I know that only you can\n\
stop him!\n\
The doors in the castle\n\
that have been sealed by\n\
Bowser can be opened only\n\
with Star Power.\n\
\n\
But there are secret\n\
paths in the castle,\n\
paths that Bowser hasn't\n\
found.\n\
\n\
One of those paths is in\n\
this room, and it holds\n\
one of the castle's Secret\n\
Stars!\n\
\n\
Find that Secret Star,\n\
Mario! It will help you\n\
on your quest. Please,\n\
Mario, you have to\n\
help us!\n\
Retrieve all of the\n\
Power Stars in the castle\n\
and free us from this\n\
awful prison!\n\
Please!")

DEFINE_DIALOG(DIALOG_076, NO_SOUND, 6, 30, 200, "\
Thanks to the power of\n\
the Stars, life is\n\
returning to the castle.\n\
Please, Mario, you have\n\
to give Bowser the boot!\n\
\n\
Here, let me tell you a\n\
little something about the\n\
castle. In the room with\n\
the mirrors, look carefully\n\
for anything that's not\n\
reflected in the mirror.\n\
And when you go to the\n\
water town, you can flood\n\
it with a high jump into\n\
the painting. Oh, by the\n\
way, look what I found!")

DEFINE_DIALOG(DIALOG_077, NO_SOUND, 2, 150, 200, "\
It is decreed that one\n\
shall pound the pillars.")

DEFINE_DIALOG(DIALOG_078, NO_SOUND, 5, 30, 200, "\
Break open the Blue Coin\n\
Block by Pounding the\n\
Ground with the Ⓩ Button.\n\
One Blue Coin is worth\n\
five Yellow Coins.\n\
But you have to hurry!\n\
The coins will disappear\n\
if you're not quick to\n\
collect them! Too bad.")

DEFINE_DIALOG(DIALOG_079, SOUND_OBJ_UKIKI_CHATTER_LONG, 4, 30, 200, "\
Owwwuu! Let me go!\n\
Uukee-kee! I was only\n\
teasing! Can't you take\n\
a joke?\n\
I'll tell you what, let's\n\
trade. If you let me go,\n\
I'll give you something\n\
really good.\n\
So, how about it?\n\
\n\
\tFree him   Hold on")

DEFINE_DIALOG(DIALOG_080, SOUND_OBJ_UKIKI_CHATTER_LONG, 1, 30, 200, "\
Eeeh hee hee hee!")

DEFINE_DIALOG(DIALOG_081, NO_SOUND, 4, 30, 200, "\
The mystery is of Wet\n\
or Dry.\n\
And where does the\n\
solution lie?\n\
The city welcomes visitors\n\
with the depth they bring\n\
as they enter.")

DEFINE_DIALOG(DIALOG_082, NO_SOUND, 4, 30, 200, "\
Hold on to your hat! If\n\
you lose it, you'll be\n\
injured easily.\n\
\n\
If you do lose your Cap,\n\
you'll have to find it in\n\
the course where you\n\
lost it.\n\
Oh, boy, it's not looking\n\
good for Peach. She's\n\
still trapped somewhere\n\
inside the walls.\n\
Please, Mario, you have\n\
to help her! Did you know\n\
that there are enemy\n\
worlds inside the walls?\n\
Yup. It's true. Bowser's\n\
troops are there, too.\n\
Oh, here, take this. I've\n\
been keeping it for you.")

DEFINE_DIALOG(DIALOG_083, NO_SOUND, 6, 30, 200, "\
There's something strange\n\
about that clock. As you\n\
jump inside, watch the\n\
position of the big hand.\n\
Oh, look what I found!\n\
Here, Mario, catch!")

DEFINE_DIALOG(DIALOG_084, NO_SOUND, 3, 30, 200, "\
Yeeoww! Unhand me,\n\
brute! I'm late, so late,\n\
I must make haste!\n\
This shiny thing? Mine!\n\
It's mine. Finders,\n\
keepers, losers...\n\
Late, late, late...\n\
Ouch! Take it then! A\n\
gift from Bowser, it was.\n\
Now let me be! I have a\n\
date! I cannot be late\n\
for tea!")

DEFINE_DIALOG(DIALOG_085, SOUND_OBJ_BOO_LAUGH_LONG, 5, 30, 200, "\
You don't stand a ghost\n\
of a chance in this house.\n\
If you walk out of here,\n\
you deserve...\n\
...a Ghoul Medal...")

DEFINE_DIALOG(DIALOG_086, NO_SOUND, 3, 30, 200, "\
Running around in circles\n\
makes some bad guys roll\n\
their eyes.")

DEFINE_DIALOG(DIALOG_087, NO_SOUND, 4, 30, 200, "\
Santa Claus isn't the only\n\
one who can go down a\n\
chimney! Come on in!\n\
  --Cabin Proprietor")

DEFINE_DIALOG(DIALOG_088, NO_SOUND, 5, 30, 200, "\
Work Elevator\n\
For those who get off\n\
here: Grab the pole to the\n\
left and slide carefully\n\
down.")

DEFINE_DIALOG(DIALOG_089, NO_SOUND, 5, 95, 200, "\
Both ways fraught with\n\
danger! Watch your feet!\n\
Those who can't do the\n\
Long Jump, tsk, tsk. Make\n\
your way to the right.\n\
Right: Work Elevator\n\
\t   Cloudy Maze\n\
Left: Black Hole\n\
\t  Underground Lake\n\
\n\
Red Circle: Elevator 2\n\
\t\t Underground Lake\n\
Arrow: You are here")

DEFINE_DIALOG(DIALOG_090, SOUND_OBJ_BOWSER_LAUGH, 6, 30, 200, "\
Bwa ha ha ha!\n\
You've stepped right into\n\
my trap, just as I knew\n\
you would! I warn you,\n\
“Friend,” watch your\n\
step!")

DEFINE_DIALOG(DIALOG_091, NO_SOUND, 2, 30, 200, "\
Danger!\n\
Strong Gusts!\n\
But the wind makes a\n\
comfy ride.")

DEFINE_DIALOG(DIALOG_092, SOUND_OBJ_BOWSER_LAUGH, 5, 30, 200, "\
Pestering me again, are\n\
you, Mario? Can't you see\n\
that I'm having a merry\n\
little time, making\n\
mischief with my minions?\n\
Now, return those Stars!\n\
My troops in the walls\n\
need them! Bwa ha ha!")

DEFINE_DIALOG(DIALOG_093, SOUND_OBJ_BOWSER_LAUGH, 5, 30, 200, "\
Mario! You again! Well\n\
that's just fine--I've\n\
been looking for something\n\
to fry with my fire\n\
breath!\n\
Your Star Power is\n\
useless against me!\n\
Your friends are all\n\
trapped within the\n\
walls...\n\
And you'll never see the\n\
Princess again!\n\
Bwa ha ha ha!")

DEFINE_DIALOG(DIALOG_094, NO_SOUND, 4, 30, 200, "\
Get a good run up the\n\
slope! Do you remember\n\
the Long Jump? Run, press\n\
Ⓩ, then jump!")

DEFINE_DIALOG(DIALOG_095, NO_SOUND, 4, 30, 200, "\
To read a sign, stand in\n\
front of it and press Ⓑ,\n\
like you did just now.\n\
\n\
When you want to talk to\n\
a Koopa Troopa or other\n\
animal, stand right in\n\
front of it.\n\
Please recover the Stars\n\
that were stolen by\n\
Bowser in this course.")

DEFINE_DIALOG(DIALOG_096, NO_SOUND, 4, 30, 200, "\
The path is narrow here.\n\
Easy does it! No one is\n\
allowed on top of the\n\
mountain!\n\
And if you know what's\n\
good for you, you won't\n\
wake anyone who's\n\
sleeping!\n\
Move slowly,\n\
tread lightly.")

DEFINE_DIALOG(DIALOG_097, NO_SOUND, 5, 30, 200, "\
Don't be a pushover!\n\
If anyone tries to shove\n\
you around, push back!\n\
It's one-on-one, with a\n\
fiery finish for the loser!")

DEFINE_DIALOG(DIALOG_098, SOUND_OBJ_BOO_LAUGH_LONG, 2, 95, 200, "\
Come on in here...\n\
...heh, heh, heh...")

// unused
DEFINE_DIALOG(DIALOG_099, SOUND_OBJ_BOO_LAUGH_LONG, 5, 95, 200, "\
Eh he he...\n\
You're mine, now, hee hee!\n\
I'll pass right through\n\
this wall. Can you do\n\
that? Heh, heh, heh!")

DEFINE_DIALOG(DIALOG_100, SOUND_OBJ_UKIKI_CHATTER_LONG, 3, 95, 200, "\
Ukkiki...Wakkiki...kee kee!\n\
Ha! I snagged it!\n\
It's mine! Heeheeheeee!")

DEFINE_DIALOG(DIALOG_101, SOUND_OBJ_UKIKI_CHATTER_LONG, 3, 95, 200, "\
Ackk! Let...go...\n\
You're...choking...me...\n\
Cough...I've been framed!\n\
This Cap? Oh, all right,\n\
take it. It's a cool Cap,\n\
but I'll give it back.\n\
I think it looks better on\n\
me than it does on you,\n\
though! Eeeee! Kee keee!")

DEFINE_DIALOG(DIALOG_102, NO_SOUND, 5, 30, 200, "\
Pssst! The Boos are super\n\
shy. If you look them\n\
in the eyes, they fade\n\
away, but if you turn\n\
your back, they reappear.\n\
It's no use trying to hit\n\
them when they're fading\n\
away. Instead, sneak up\n\
behind them and punch.")

DEFINE_DIALOG(DIALOG_103, NO_SOUND, 4, 95, 200, "\
Upon four towers\n\
one must alight...\n\
Then at the peak\n\
shall shine the light...")

DEFINE_DIALOG(DIALOG_104, NO_SOUND, 5, 30, 200, "\
The shadowy star in front\n\
of you is a “Star\n\
Marker.” When you collect\n\
all 8 Red Coins, the Star\n\
will appear here.")

DEFINE_DIALOG(DIALOG_105, SOUND_OBJ_BOBOMB_BUDDY_TALK, 3, 95, 200, "\
Ready for blastoff! Come\n\
on, hop into the cannon!\n\
\n\
You can reach the Star on\n\
the floating island by\n\
using the four cannons.\n\
Use the Control Stick to\n\
aim, then press Ⓐ to fire.\n\
\n\
If you're handy, you can\n\
grab on to trees or poles\n\
to land.")

DEFINE_DIALOG(DIALOG_106, SOUND_OBJ_BOBOMB_BUDDY_TALK, 2, 95, 200, "\
Ready for blastoff! Come\n\
on, hop into the cannon!")

DEFINE_DIALOG(DIALOG_107, SOUND_OBJ_BOO_LAUGH_LONG, 3, 95, 200, "\
Ghosts...\n\
...don't...\n\
...DIE!\n\
Heh, heh, heh!\n\
Can you get out of here...\n\
...alive?")

DEFINE_DIALOG(DIALOG_108, SOUND_OBJ_BOO_LAUGH_LONG, 2, 95, 200, "\
Boooooo-m! Here comes\n\
the master of mischief,\n\
the tower of terror,\n\
the Big Boo!\n\
Ka ha ha ha...")

DEFINE_DIALOG(DIALOG_109, NO_SOUND, 4, 95, 200, "\
Ooooo Nooooo!\n\
Talk about out-of-body\n\
experiences--my body\n\
has melted away!\n\
Have you run in to any\n\
headhunters lately??\n\
I could sure use a new\n\
body!\n\
Brrr! My face might\n\
freeze like this!")

DEFINE_DIALOG(DIALOG_110, NO_SOUND, 5, 95, 200, "\
I need a good head on my\n\
shoulders. Do you know of\n\
anybody in need of a good\n\
body? Please! I'll follow\n\
you if you do!")

DEFINE_DIALOG(DIALOG_111, NO_SOUND, 4, 95, 200, "\
Perfect! What a great\n\
new body! Here--this is a\n\
present for you. It's sure\n\
to warm you up.")

DEFINE_DIALOG(DIALOG_112, NO_SOUND, 4, 30, 200, "\
Collect as many coins as\n\
possible! They'll refill\n\
your Power Meter.\n\
\n\
You can check to see how\n\
many coins you've\n\
collected in each of the\n\
15 enemy worlds.\n\
You can also recover\n\
power by touching the\n\
Spinning Heart.\n\
\n\
The faster you run\n\
through the heart, the\n\
more power you'll recover.")

DEFINE_DIALOG(DIALOG_113, NO_SOUND, 6, 30, 200, "\
There are special Caps in\n\
the red, green and blue\n\
blocks. Step on the\n\
switches in the hidden\n\
courses to activate the\n\
Cap Blocks.")

DEFINE_DIALOG(DIALOG_114, SOUND_OBJ2_BOSS_DIALOG_GRUNT, 5, 95, 200, "\
It makes me so mad! We\n\
build your houses, your\n\
castles. We pave your\n\
roads, and still you\n\
walk all over us.\n\
Do you ever say thank\n\
you? No! Well, you're not\n\
going to wipe your feet\n\
on me! I think I'll crush\n\
you just for fun!\n\
Do you have a problem\n\
with that? Just try to\n\
pound me, wimp! Ha!")

DEFINE_DIALOG(DIALOG_115, SOUND_OBJ2_BOSS_DIALOG_GRUNT, 5, 95, 200, "\
No! Crushed again!\n\
I'm just a stepping stone,\n\
after all. I won't gravel,\n\
er, grovel. Here, you win.\n\
Take this with you!")

DEFINE_DIALOG(DIALOG_116, SOUND_OBJ2_BOSS_DIALOG_GRUNT, 5, 95, 200, "\
Whaaa....Whaaat?\n\
Can it be that a\n\
pipsqueak like you has\n\
defused the Bob-omb\n\
king????\n\
You might be fast enough\n\
to ground me, but you'll\n\
have to pick up the pace\n\
if you want to take King\n\
Bowser by the tail.\n\
Methinks my troops could\n\
learn a lesson from you!\n\
Here is your Star, as I\n\
promised, Mario.\n\
\n\
If you want to see me\n\
again, select this Star\n\
from the menu. For now,\n\
farewell.")

DEFINE_DIALOG(DIALOG_117, SOUND_OBJ2_BOSS_DIALOG_GRUNT, 1, 95, 200, "\
Who...walk...here?\n\
Who...break...seal?\n\
Wake..ancient..ones?\n\
We no like light...\n\
Rrrrummbbble...\n\
We no like...intruders!\n\
Now battle...\n\
...hand...\n\
...to...\n\
...hand!")

DEFINE_DIALOG(DIALOG_118, SOUND_OBJ2_BOSS_DIALOG_GRUNT, 6, 95, 200, "\
Grrrrumbbble!\n\
What...happen?\n\
We...crushed like pebble.\n\
You so strong!\n\
You rule ancient pyramid!\n\
For today...\n\
Now, take Star of Power.\n\
We...sleep...darkness.")

DEFINE_DIALOG(DIALOG_119, NO_SOUND, 6, 30, 200, "\
Grrr! I was a bit\n\
careless. This is not as I\n\
had planned...but I still\n\
hold the power of the\n\
Stars, and I still have\n\
Peach.\n\
Bwa ha ha! You'll get no\n\
more Stars from me! I'm\n\
not finished with you yet,\n\
but I'll let you go for\n\
now. You'll pay for this...\n\
later!")

DEFINE_DIALOG(DIALOG_120, NO_SOUND, 4, 30, 200, "\
Ooowaah! Can it be that\n\
I've lost??? The power of\n\
the Stars has failed me...\n\
this time.\n\
Consider this a draw.\n\
Next time, I'll be in\n\
perfect condition.\n\
\n\
Now, if you want to see\n\
your precious Princess,\n\
come to the top of the\n\
tower.\n\
I'll be waiting!\n\
Gwa ha ha ha!")

DEFINE_DIALOG(DIALOG_121, NO_SOUND, 5, 30, 200, "\
Nooo! It can't be!\n\
You've really beaten me,\n\
Mario?!! I gave those\n\
troops power, but now\n\
it's fading away!\n\
Arrgghh! I can see peace\n\
returning to the world! I\n\
can't stand it! Hmmm...\n\
It's not over yet...\n\
\n\
C'mon troops! Let's watch\n\
the ending together!\n\
Bwa ha ha!")

DEFINE_DIALOG(DIALOG_122, NO_SOUND, 4, 30, 200, "\
The Black Hole\n\
Right: Work Elevator\n\
\t   Cloudy Maze\n\
Left: Underground Lake")

DEFINE_DIALOG(DIALOG_123, NO_SOUND, 4, 30, 200, "\
Metal Cavern\n\
Right: To Waterfall\n\
Left: Metal Cap Switch")

DEFINE_DIALOG(DIALOG_124, NO_SOUND, 4, 30, 200, "\
Work Elevator\n\
Danger!!\n\
Read instructions\n\
thoroughly!\n\
Elevator continues in the\n\
direction of the arrow\n\
activated.")

DEFINE_DIALOG(DIALOG_125, NO_SOUND, 3, 30, 200, "\
Hazy Maze-Exit\n\
Danger! Closed.\n\
Turn back now.")

DEFINE_DIALOG(DIALOG_126, NO_SOUND, 3, 30, 200, "\
Up: Black Hole\n\
Right: Work Elevator\n\
\t   Hazy Maze")

DEFINE_DIALOG(DIALOG_127, NO_SOUND, 4, 30, 200, "\
Underground Lake\n\
Right: Metal Cave\n\
Left: Abandoned Mine\n\
\t  (Closed)\n\
A gentle sea dragon lives\n\
here. Pound on his back to\n\
make him lower his head.\n\
Don't become his lunch.")

DEFINE_DIALOG(DIALOG_128, SOUND_OBJ_KING_BOBOMB_TALK, 4, 95, 200, "\
You must fight with\n\
honor! It is against the\n\
royal rules to throw the\n\
king out of the ring!")

DEFINE_DIALOG(DIALOG_129, NO_SOUND, 5, 30, 200, "\
Welcome to the Vanish\n\
Cap Switch Course! All of\n\
the blue blocks you find\n\
will become solid once you\n\
step on the Cap Switch.\n\
You'll disappear when you\n\
put on the Vanish Cap, so\n\
you'll be able to elude\n\
enemies and walk through\n\
many things. Try it out!")

DEFINE_DIALOG(DIALOG_130, NO_SOUND, 5, 30, 200, "\
Welcome to the Metal Cap\n\
Switch Course! Once you\n\
step on the Cap Switch,\n\
the green blocks will\n\
become solid.\n\
When you turn your body\n\
into metal with the Metal\n\
Cap, you can walk\n\
underwater! Try it!")

DEFINE_DIALOG(DIALOG_131, NO_SOUND, 5, 30, 200, "\
Welcome to the Wing Cap\n\
Course! Step on the red\n\
switch at the top of the\n\
tower, in the center of\n\
the rainbow ring.\n\
When you trigger the\n\
switch, all of the red\n\
blocks you find will\n\
become solid.\n\
\n\
Try out the Wing Cap! Do\n\
the Triple Jump to take\n\
off and press Ⓩ to land.\n\
\n\
\n\
Pull back on the Control\n\
Stick to go up and push\n\
forward to nose down,\n\
just as you would when\n\
flying an airplane.")

DEFINE_DIALOG(DIALOG_132, SOUND_OBJ_BIG_PENGUIN_YELL, 4, 30, 200, "\
Whoa, Mario, pal, you\n\
aren't trying to cheat,\n\
are you? Shortcuts aren't\n\
allowed.\n\
Now, I know that you\n\
know better. You're\n\
disqualified! Next time,\n\
play fair!")

DEFINE_DIALOG(DIALOG_133, NO_SOUND, 6, 30, 200, "\
Am I glad to see you! The\n\
Princess...and I...and,\n\
well, everybody...we're all\n\
trapped inside the castle\n\
walls.\n\
\n\
Bowser has stolen the\n\
castle's Stars, and he's\n\
using their power to\n\
create his own world in\n\
the paintings and walls.\n\
\n\
Please recover the Power\n\
Stars! As you find them,\n\
you can use their power\n\
to open the doors that\n\
Bowser has sealed.\n\
\n\
There are four rooms on\n\
the first floor. Start in\n\
the one with the painting\n\
of Bob-omb inside. It's\n\
the only room that Bowser\n\
hasn't sealed.\n\
When you collect eight\n\
Power Stars, you'll be\n\
able to open the door\n\
with the big star. The\n\
Princess must be inside!")

DEFINE_DIALOG(DIALOG_134, NO_SOUND, 5, 30, 200, "\
The names of the Stars\n\
are also hints for\n\
finding them. They are\n\
displayed at the beginning\n\
of each course.\n\
You can collect the Stars\n\
in any order. You won't\n\
find some Stars, enemies\n\
or items unless you select\n\
a specific Star.\n\
After you collect some\n\
Stars, you can try\n\
another course.\n\
We're all waiting for\n\
your help!")

DEFINE_DIALOG(DIALOG_135, NO_SOUND, 5, 30, 200, "\
It was Bowser who stole\n\
the Stars. I saw him with\n\
my own eyes!\n\
\n\
\n\
He's hidden six Stars in\n\
each course, but you\n\
won't find all of them in\n\
some courses until you\n\
press the Cap Switches.\n\
The Stars you've found\n\
will show on each course's\n\
starting screen.\n\
\n\
\n\
If you want to see some\n\
of the enemies you've\n\
already defeated, select\n\
the Stars you recovered\n\
from them.")

DEFINE_DIALOG(DIALOG_136, NO_SOUND, 6, 30, 200, "\
Wow! You've already\n\
recovered that many\n\
Stars? Way to go, Mario!\n\
I'll bet you'll have us out\n\
of here in no time!\n\
\n\
Be careful, though.\n\
Bowser and his band\n\
wrote the book on “bad.”\n\
Take my advice: When you\n\
need to recover from\n\
injuries, collect coins.\n\
Yellow Coins refill one\n\
piece of the Power Meter,\n\
Red Coins refill two\n\
pieces, and Blue Coins\n\
refill five.\n\
\n\
To make Blue Coins\n\
appear, pound on Blue\n\
Coin Blocks.\n\
\n\
\n\
\n\
Also, if you fall from\n\
high places, you'll\n\
minimize damage if you\n\
Pound the Ground as you\n\
land.")

DEFINE_DIALOG(DIALOG_137, NO_SOUND, 6, 30, 200, "\
Thanks, Mario! The castle\n\
is recovering its energy\n\
as you retrieve Power\n\
Stars, and you've chased\n\
Bowser right out of here,\n\
on to some area ahead.\n\
Oh, by the by, are you\n\
collecting coins? Special\n\
Stars appear when you\n\
collect 100 coins in each\n\
of the 15 courses!")

DEFINE_DIALOG(DIALOG_138, NO_SOUND, 3, 30, 200, "\
Down: Underground Lake\n\
Left: Black Hole\n\
Right: Hazy Maze (Closed)")

DEFINE_DIALOG(DIALOG_139, NO_SOUND, 6, 30, 200, "\
Above: Automatic Elevator\n\
Elevator begins\n\
automatically and follows\n\
pre-set course.\n\
It disappears\n\
automatically, too.")

DEFINE_DIALOG(DIALOG_140, NO_SOUND, 6, 30, 200, "\
Elevator Area\n\
Right: Hazy Maze\n\
\t   Entrance\n\
Left: Black Hole\n\
\t  Elevator 1\n\
Arrow: You are here")

DEFINE_DIALOG(DIALOG_141, NO_SOUND, 5, 150, 200, "\
You've recovered one of\n\
the stolen Power Stars!\n\
Now you can open some of\n\
the sealed doors in the\n\
castle.\n\
Try the Princess's room\n\
on the second floor and\n\
the room with the\n\
painting of Whomp's\n\
Fortress on Floor 1.\n\
Bowser's troops are still\n\
gaining power, so you\n\
can't give up. Save us,\n\
Mario! Keep searching for\n\
Stars!")

DEFINE_DIALOG(DIALOG_142, NO_SOUND, 5, 150, 200, "\
You've recovered three\n\
Power Stars! Now you can\n\
open any door with a 3\n\
on its star.\n\
\n\
You can come and go from\n\
the open courses as you\n\
please. The enemies ahead\n\
are even meaner, so be\n\
careful!")

DEFINE_DIALOG(DIALOG_143, NO_SOUND, 6, 150, 200, "\
You've recovered eight of\n\
the Power Stars! Now you\n\
can open the door with\n\
the big Star! But Bowser\n\
is just ahead...can you\n\
hear the Princess calling?")

DEFINE_DIALOG(DIALOG_144, NO_SOUND, 6, 150, 200, "\
You've recovered 30\n\
Power Stars! Now you can\n\
open the door with the\n\
big Star! But before you\n\
move on, how's it going\n\
otherwise?\n\
Did you pound the two\n\
columns down? You didn't\n\
lose your hat, did you?\n\
If you did, you'll have to\n\
stomp on the condor to\n\
get it back!\n\
They say that Bowser has\n\
sneaked out of the sea\n\
and into the underground.\n\
Have you finally\n\
cornered him?")

DEFINE_DIALOG(DIALOG_145, NO_SOUND, 6, 150, 200, "\
You've recovered 50\n\
Power Stars! Now you can\n\
open the Star Door on the\n\
third floor. Bowser's\n\
there, you know.\n\
\n\
Oh! You've found all of\n\
the Cap Switches, haven't\n\
you? Red, green and blue?\n\
The Caps you get from the\n\
colored blocks are really\n\
helpful.\n\
Hurry along, now. The\n\
third floor is just ahead.")

DEFINE_DIALOG(DIALOG_146, NO_SOUND, 6, 150, 200, "\
You've found 70 Power\n\
Stars! The mystery of the\n\
endless stairs is solved,\n\
thanks to you--and is\n\
Bowser ever upset! Now,\n\
on to the final bout!")

DEFINE_DIALOG(DIALOG_147, NO_SOUND, 5, 30, 200, "\
Are you using the Cap\n\
Blocks? You really should,\n\
you know.\n\
\n\
\n\
To make them solid so you\n\
can break them, you have\n\
to press the colored Cap\n\
Switches in the castle's\n\
hidden courses.\n\
You'll find the hidden\n\
courses only after\n\
regaining some of the\n\
Power Stars.\n\
\n\
The Cap Blocks are a big\n\
help! Red for the Wing\n\
Cap, green for the Metal\n\
Cap, blue for the Vanish\n\
Cap.")

DEFINE_DIALOG(DIALOG_148, NO_SOUND, 6, 30, 200, "\
Snowman Mountain ahead.\n\
Keep out! And don't try\n\
the Triple Jump over the\n\
ice block shooter.\n\
\n\
\n\
If you fall into the\n\
freezing pond, your power\n\
decreases quickly, and\n\
you won't recover\n\
automatically.\n\
\t--The Snowman")

DEFINE_DIALOG(DIALOG_149, NO_SOUND, 3, 30, 200, "\
Welcome to\n\
Princess Toadstool's\n\
secret slide!\n\
There's a Star hidden\n\
here that Bowser couldn't\n\
find.\n\
When you slide, press\n\
forward to speed up,\n\
pull back to slow down.\n\
If you slide really\n\
fast, you'll win the Star!")

DEFINE_DIALOG(DIALOG_150, SOUND_OBJ_WIGGLER_TALK, 5, 30, 200, "\
Waaaa! You've flooded my\n\
house! Wh-why?? Look at\n\
this mess! What am I\n\
going to do now?\n\
\n\
The ceiling's ruined, the\n\
floor is soaked...what to\n\
do, what to do? Huff...\n\
huff...it makes me so...\n\
MAD!!!\n\
Everything's been going\n\
wrong ever since I got\n\
this Star...It's so shiny,\n\
but it makes me feel...\n\
strange...")

DEFINE_DIALOG(DIALOG_151, SOUND_OBJ_WIGGLER_TALK, 4, 30, 200, "\
I can't take this\n\
anymore! First you get\n\
me all wet, then you\n\
stomp on me!\n\
Now I'm really, really,\n\
REALLY mad!\n\
Waaaaaaaaaaaaaaaaa!!!")

DEFINE_DIALOG(DIALOG_152, SOUND_OBJ_WIGGLER_TALK, 3, 30, 200, "\
Owwch! Uncle! Uncle!\n\
Okay, I give up. Take this\n\
Star!\n\
Whew! I feel better now.\n\
I don't really need it\n\
anymore, anyway--\n\
I can see the stars\n\
through my ceiling at\n\
night.\n\
They make me feel...\n\
...peaceful. Please, come\n\
back and visit anytime.")

DEFINE_DIALOG(DIALOG_153, NO_SOUND, 4, 30, 200, "\
Hey! Who's there?\n\
What's climbing on me?\n\
Is it an ice ant?\n\
A snow flea?\n\
Whatever it is, it's\n\
bugging me! I think I'll\n\
blow it away!")

DEFINE_DIALOG(DIALOG_154, NO_SOUND, 5, 30, 200, "\
Hold on to your hat! If\n\
you lose it, you'll be\n\
easily injured. If you\n\
lose it, look for it in the\n\
course where you lost it.\n\
Speaking of lost, the\n\
Princess is still stuck in\n\
the walls somewhere.\n\
Please help, Mario!\n\
\n\
Oh, you know that there\n\
are secret worlds in the\n\
walls as well as in the\n\
paintings, right?")

DEFINE_DIALOG(DIALOG_155, NO_SOUND, 6, 30, 200, "\
Thanks to the power of\n\
the Stars, life is\n\
returning to the castle.\n\
Please, Mario, you have\n\
to give Bowser the boot!\n\
\n\
Here, let me tell you a\n\
little something about the\n\
castle. In the room with\n\
the mirrors, look carefully\n\
for anything that's not\n\
reflected in the mirror.\n\
And when you go to the\n\
water town, you can flood\n\
it with a high jump into\n\
the painting.")

DEFINE_DIALOG(DIALOG_156, NO_SOUND, 5, 30, 200, "\
The world inside the\n\
clock is so strange!\n\
When you jump inside,\n\
watch the position of\n\
the big hand!")

DEFINE_DIALOG(DIALOG_157, NO_SOUND, 5, 30, 200, "\
Watch out! Don't let\n\
yourself be swallowed by\n\
quicksand.\n\
\n\
\n\
If you sink into the sand,\n\
you won't be able to\n\
jump, and if your head\n\
goes under, you'll be\n\
smothered.\n\
The dark areas are\n\
bottomless pits.")

DEFINE_DIALOG(DIALOG_158, NO_SOUND, 6, 30, 200, "\
1. If you jump repeatedly\n\
and time it right, you'll\n\
jump higher and higher.\n\
If you run really fast and\n\
time three jumps right,\n\
you can do a Triple Jump.\n\
2. Jump into a solid wall,\n\
then jump again when you\n\
hit the wall. You can\n\
bounce to a higher level\n\
using this Wall Kick.")

DEFINE_DIALOG(DIALOG_159, NO_SOUND, 6, 30, 200, "\
3. If you stop, press Ⓩ\n\
to crouch, then jump, you\n\
can perform a Backward\n\
Somersault. To do a Long\n\
Jump, run fast, press Ⓩ,\n\
then jump.")

DEFINE_DIALOG(DIALOG_160, NO_SOUND, 4, 30, 200, "\
Press Ⓑ while running\n\
fast to do a Body Slide\n\
attack. To stand while\n\
sliding, press Ⓐ or Ⓑ.")

DEFINE_DIALOG(DIALOG_161, SOUND_GENERAL_YOSHI_TALK, 4, 30, 200, "\
Mario!!!\n\
It that really you???\n\
It has been so long since\n\
our last adventure!\n\
They told me that I might\n\
see you if I waited here,\n\
but I'd just about given\n\
up hope!\n\
Is it true? Have you\n\
really beaten Bowser? And\n\
restored the Stars to the\n\
castle?\n\
And saved the Princess?\n\
I knew you could do it!\n\
Now I have a very special\n\
message for you.\n\
“Thanks for playing Super\n\
Mario 64! This is the\n\
end of the game, but not\n\
the end of the fun.\n\
We want you to keep on\n\
playing, so we have a\n\
little something for you.\n\
We hope that you like it!\n\
Enjoy!!!”\n\
\n\
The Super Mario 64 Team")

DEFINE_DIALOG(DIALOG_162, NO_SOUND, 4, 30, 200, "\
No, no, no! Not you\n\
again! I'm in a great\n\
hurry, can't you see?\n\
\n\
I've no time to squabble\n\
over Stars. Here, have it.\n\
I never meant to hide it\n\
from you...\n\
It's just that I'm in such\n\
a rush. That's it, that's\n\
all. Now, I must be off.\n\
Owww! Let me go!")

DEFINE_DIALOG(DIALOG_163, NO_SOUND, 5, 30, 200, "\
Noooo! You've really\n\
beaten me this time,\n\
Mario! I can't stand\n\
losing to you!\n\
\n\
My troops...worthless!\n\
They've turned over all\n\
the Power Stars! What?!\n\
There are 120 in all???\n\
\n\
Amazing! There were some\n\
in the castle that I\n\
missed??!!\n\
\n\
\n\
Now I see peace\n\
returning to the world...\n\
Oooo! I really hate that!\n\
I can't watch--\n\
I'm outta here!\n\
Just you wait until next\n\
time. Until then, keep\n\
that Control Stick\n\
smokin'!\n\
Buwaa ha ha!")

DEFINE_DIALOG(DIALOG_164, NO_SOUND, 4, 30, 200, "\
Mario! What's up, pal?\n\
I haven't been on the\n\
slide lately, so I'm out\n\
of shape.\n\
Still, I'm always up for a\n\
good race, especially\n\
against an old sleddin'\n\
buddy.\n\
Whaddya say?\n\
Ready...set...\n\
\n\
\tGo\t\t Don't Go")

DEFINE_DIALOG(DIALOG_165, NO_SOUND, 5, 30, 200, "\
I take no responsibility\n\
whatsoever for those who\n\
get dizzy and pass out\n\
from running around\n\
this post.")

DEFINE_DIALOG(DIALOG_166, NO_SOUND, 4, 30, 200, "\
I'll be back soon.\n\
I'm out training now,\n\
so come back later.\n\
\t--Koopa the Quick")

DEFINE_DIALOG(DIALOG_167, NO_SOUND, 4, 30, 200, "\
Princess Toadstool's\n\
castle is just ahead.\n\
\n\
\n\
Press Ⓐ to jump, Ⓩ to\n\
crouch, and Ⓑ to punch,\n\
read a sign, or grab\n\
something.\n\
Press Ⓑ again to throw\n\
something you're holding.")

DEFINE_DIALOG(DIALOG_168, SOUND_OBJ_WIGGLER_TALK, 5, 30, 200, "\
Hey! Knock it off! That's\n\
the second time you've\n\
nailed me. Now you're\n\
asking for it, linguine\n\
breath!")

DEFINE_DIALOG(DIALOG_169, NO_SOUND, 4, 30, 200, "\
Keep out!\n\
That means you!\n\
Arrgghh!\n\
\n\
Anyone entering this cave\n\
without permission will\n\
meet certain disaster.")
