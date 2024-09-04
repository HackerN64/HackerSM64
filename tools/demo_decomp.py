import sys, os

fl = []
with open(sys.argv[1]) as f:
    fl = f.readlines()


with open(sys.argv[1], "w+") as f:
    for line in fl:
        if line.startswith("for"):
            tokens = line.split()
            # correct stick positions
            stickx = int(tokens[4][:-1])
            sticky = int(tokens[5][:-1])
            if stickx > 127:
                stickx = -(256 - stickx)
            if sticky > 127:
                sticky = -(256 - sticky)

            buf = f"for {int(tokens[1]):3} frames;  stick {stickx:4}, {sticky:4};  press {tokens[-1]}\n"
            f.write(buf)
        else:
            f.write(line)


