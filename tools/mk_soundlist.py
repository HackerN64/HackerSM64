import re

pattern = re.compile(r'#define (SOUND_\w+)')

def get_sounds():
    with open('include/sounds.h') as fp:
        lines = fp.readlines()
        sounds = []
        start = False
        for line in lines:
            if '#define' in line and 'SOUND_ARG_LOAD(' in line:
                if not start:
                    start = True
                    continue
                match = pattern.match(line)
                if match:
                    arg = match.group(1)
                    sounds.append(arg)
    return sounds

terrains = [
    "DEFAULT",
    "GRASS",
    "WATER",
    "STONE",
    "SPOOKY",
    "SNOW",
    "ICE",
    "SAND",
]

def s_to_str_terrain(s: str):
    for i in range(8):
        ns = f'"{s} ({terrains[i]})",'
        yield "    { " + f'.name = {ns:50} .val = {s} + ({i} << 16)' + " },"

def s_to_str(s: str, idx = 0):
    ns = f'"{s}",'
    return "    { " + f'.name = {ns:50} .val = {s}' + " },"

def main():
    sounds = get_sounds()

    array_list = []
    for s in sounds:
        if 'TERRAIN' in s:
            array_list.extend(s_to_str_terrain(s))
            continue
        array_list.append(s_to_str(s))

    c_data = 'struct SoundPreview gSoundPreviews[] = {\n' + '\n'.join(array_list) + '\n};\n\n'

    with open('src/game/sound_previews.c.in', 'w') as wfp:
        wfp.write(c_data)


if __name__ == "__main__":
    main()
