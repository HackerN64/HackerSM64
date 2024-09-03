import sys, os

fb = []

print("#include <PR/os_cont.h>")
print('#include "demo_macros.inc"')
print()

with open(sys.argv[1], "rb") as f:
    fb = f.read()

for i in range(len(fb))[:-4:4]:
    holdcount = fb[i]
    stickx = fb[i + 1]
    sticky = fb[i + 2]
    button = fb[i + 3]
    buttonStr = "press "
    if button & 0x80:
        buttonStr += "A | "
    if button & 0x40:
        buttonStr += "B | "
    if button & 0x20:
        buttonStr += "Z | "
    if button & 0x10:
        buttonStr += "Start | "
    if button & 0x08:
        buttonStr += "C_Up | "
    if button & 0x04:
        buttonStr += "C_Down | "
    if button & 0x02:
        buttonStr += "C_Left | "
    if button & 0x01:
        buttonStr += "C_Right | "

    if button == 0x00:
        buttonStr = "press _"
    else:
        buttonStr = buttonStr[:-3]
    print(f"for {holdcount:3} frames;  stick {stickx:3}, {sticky:3};  {buttonStr}")

print("end_demo")
