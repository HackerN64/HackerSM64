#!/usr/bin/env python3
import sys
import os
import json
import subprocess

from tools.detect_baseroms import get_rom_candidates

envmap_table = set([
    "actors/mario/mario_metal.rgba16.png",
    "actors/mario_cap/mario_cap_metal.rgba16.png",
    "actors/star/star_surface.rgba16.png",
    "actors/water_bubble/water_bubble.rgba16.png",
    "actors/water_ring/water_ring.rgba16.png",
    "levels/castle_inside/29.rgba16.png",
    "levels/castle_inside/30.rgba16.png",
    "levels/hmc/7.rgba16.png",
    "levels/castle_inside/16.ia16.png",
    "levels/cotmc/2.rgba16.png"
])

def read_asset_map():
    with open("assets.json") as f:
        ret = json.load(f)
    return ret


def read_local_asset_list(f):
    if f is None:
        return []
    ret = []
    for line in f:
        ret.append(line.strip())
    return ret

def asset_needs_update(asset, version):
    if version <= 7 and asset in envmap_table:
        return True
    if version <= 6 and asset in ["actors/king_bobomb/king_bob-omb_eyes.rgba16.png", "actors/king_bobomb/king_bob-omb_hand.rgba16.png"]:
        return True
    if version <= 5 and asset == "textures/spooky/bbh_textures.00800.rgba16.png":
        return True
    if version <= 4 and asset in ["textures/mountain/ttm_textures.01800.rgba16.png", "textures/mountain/ttm_textures.05800.rgba16.png"]:
        return True
    if version <= 3 and asset == "textures/cave/hmc_textures.01800.rgba16.png":
        return True
    if version <= 2 and asset == "textures/inside/inside_castle_textures.09000.rgba16.png":
        return True
    if version <= 1 and asset.endswith(".m64"):
        return True
    if version <= 0 and asset.endswith(".aiff"):
        return True
    return False


def remove_file(fname):
    os.remove(fname)
    print("deleting", fname)
    try:
        os.removedirs(os.path.dirname(fname))
    except OSError:
        pass


def clean_assets(local_asset_file):
    assets = set(read_asset_map().keys())
    assets.update(read_local_asset_list(local_asset_file))
    for fname in list(assets) + [".assets-local.txt"]:
        if fname.startswith("@"):
            continue
        try:
            remove_file(fname)
        except FileNotFoundError:
            pass


def main():
    # In case we ever need to change formats of generated files, we keep a
    # revision ID in the local asset file.
    new_version = 8

    try:
        local_asset_file = open(".assets-local.txt")
        local_asset_file.readline()
        local_version = int(local_asset_file.readline().strip())
    except Exception:
        local_asset_file = None
        local_version = -1

    langs = sys.argv[1:]
    if langs == ["--clean"]:
        clean_assets(local_asset_file)
        sys.exit(0)

    asset_map = read_asset_map()
    all_assets = []
    any_missing_assets = False
    for asset, data in asset_map.items():
        if asset.startswith("@"):
            continue
        if os.path.isfile(asset):
            all_assets.append((asset, data, True))
        else:
            all_assets.append((asset, data, False))
            if not any_missing_assets and any(lang in data[-1] for lang in langs):
                any_missing_assets = True

    if not any_missing_assets and local_version == new_version:
        # Nothing to do, no need to read a ROM. For efficiency we don't check
        # the list of old assets either.
        return

    romLUT = get_rom_candidates()


    # verify the correct rom
    for lang in langs:
        if lang not in romLUT:
            print("[%s] Error: No %s ROM detected in this folder."
                % (sys.argv[0], lang.upper())
            )
            if len(romLUT.items()) > 0:
                print()
                print("Detected ROMS:")
            for k,v in romLUT.items():
                print("    %s ROM found at: %s" % (k.upper(), v))
            sys.exit(1)

    all_langs = ["jp", "us", "eu", "sh"]
    if not langs or not all(a in all_langs for a in langs):
        langs_str = " ".join("[" + lang + "]" for lang in all_langs)
        print("Usage: " + sys.argv[0] + " " + langs_str)
        print("For each version, its ROM file must exist in this folder")
        sys.exit(1)

    # Late imports (to optimize startup perf)
    import hashlib
    import tempfile
    from collections import defaultdict

    new_assets = {a[0] for a in all_assets}

    previous_assets = read_local_asset_list(local_asset_file)
    if local_version == -1:
        # If we have no local asset file, we assume that files are version
        # controlled and thus up to date.
        local_version = new_version

    # Create work list
    todo = defaultdict(lambda: [])
    for (asset, data, exists) in all_assets:
        # Leave existing assets alone if they have a compatible version.
        if exists and not (local_version == new_version or asset_needs_update(asset, local_version)):
            continue

        meta = data[:-2]
        size, positions = data[-2:]
        for lang, pos in positions.items():
            mio0 = None if len(pos) == 1 else pos[0]
            pos = pos[-1]
            if lang in langs:
                todo[(lang, mio0)].append((asset, pos, size, meta))
                break

    # Load ROMs
    roms = {}
    for lang in langs:
        romname = romLUT[lang]
        try:
            with open(romname, "rb") as f:
                roms[lang] = f.read()
        except Exception as e:
            print("Failed to open " + romname + "! " + str(e))
            sys.exit(1)
        # There used to be an SHA1 check here,
        # but it's unnecessary since we detect the
        # presence of the correct roms automatically

    # Make sure tools exist
    subprocess.check_call(
        ["make", "-s", "-C", "tools/", "n64graphics", "skyconv", "mio0", "aifc_decode"]
    )

    # Go through the assets in roughly alphabetical order (but assets in the same
    # mio0 file still go together).
    keys = sorted(list(todo.keys()), key=lambda k: todo[k][0][0])

    # Import new assets
    for key in keys:

        assets = todo[key]
        lang, mio0 = key
        romname = romLUT[lang]
        if mio0 == "@sound":
            rom = roms[lang]
            args = [
                "python3",
                "tools/disassemble_sound.py",
                romname,
            ]
            def append_args(key):
                size, locs = asset_map["@sound " + key + " " + lang]
                offset = locs[lang][0]
                args.append(str(offset))
                args.append(str(size))
            append_args("ctl")
            append_args("tbl")
            if lang == "sh":
                args.append("--shindou-headers")
                append_args("ctl header")
                append_args("tbl header")
            args.append("--only-samples")
            for (asset, pos, size, meta) in assets:
                print("extracting", asset)
                args.append(asset + ":" + str(pos))
            subprocess.run(args, check=True)
            continue

        if mio0 is not None:
            image = subprocess.run(
                [
                    "./tools/mio0",
                    "-d",
                    "-o",
                    str(mio0),
                    romname,
                    "-",
                ],
                check=True,
                stdout=subprocess.PIPE,
            ).stdout
        else:
            image = roms[lang]

        for (asset, pos, size, meta) in assets:
            print("extracting", asset)
            input = image[pos : pos + size]
            os.makedirs(os.path.dirname(asset), exist_ok=True)
            if asset.endswith(".png"):
                png_file = tempfile.NamedTemporaryFile(prefix="asset", delete=False)
                try:
                    png_file.write(input)
                    png_file.flush()
                    png_file.close()
                    if asset.startswith("textures/skyboxes/") or asset.startswith("levels/ending/cake"):
                        if asset.startswith("textures/skyboxes/"):
                            imagetype = "sky"
                        else:
                            imagetype =  "cake" + ("-eu" if "eu" in asset else "")
                        subprocess.run(
                            [
                                "./tools/skyconv",
                                "--type",
                                imagetype,
                                "--combine",
                                png_file.name,
                                asset,
                            ],
                            check=True,
                        )
                    else:
                        rotate_envmap = "false"
                        if asset in envmap_table:
                            rotate_envmap = "true"
                        w, h = meta
                        fmt = asset.split(".")[-2]
                        subprocess.run(
                            [
                                "./tools/n64graphics",
                                "-e",
                                png_file.name,
                                "-g",
                                asset,
                                "-f",
                                fmt,
                                "-w",
                                str(w),
                                "-h",
                                str(h),
                                "-r",
                                rotate_envmap,
                            ],
                            check=True,
                        )
                finally:
                    png_file.close()
                    os.remove(png_file.name)
            else:
                with open(asset, "wb") as f:
                    f.write(input)

    # Remove old assets
    for asset in previous_assets:
        if asset not in new_assets:
            try:
                remove_file(asset)
            except FileNotFoundError:
                pass

    # Replace the asset list
    output = "\n".join(
        [
            "# This file tracks the assets currently extracted by extract_assets.py.",
            str(new_version),
            *sorted(list(new_assets)),
            "",
        ]
    )
    with open(".assets-local.txt", "w") as f:
        f.write(output)


main()
