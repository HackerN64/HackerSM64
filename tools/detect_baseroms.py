import subprocess
import os
import sys
from hashlib import sha1

XDG_DATA_DIR: str = os.environ.get("XDG_DATA_HOME") or "~/.local/share"
ROMS_DIR: str = os.path.expanduser(os.path.join(XDG_DATA_DIR, "HackerSM64"))

sha1_LUT: dict[str, int] = {
    "eu": "4ac5721683d0e0b6bbb561b58a71740845dceea9",
    "jp": "8a20a5c83d6ceb0f0506cfc9fa20d8f438cafe51",
    "sh": "3f319ae697533a255a1003d09202379d78d5a2e0",
    "us": "9bef1128717f958171a4afac3ed78ee2bb4e86ce",
}

sha1_swapLUT: dict[str, str] = {
    "eu": "d80ee9eeb6454d53a96ceb6ed0aca3ffde045091",
    "jp": "1d2579dd5fb1d8263a4bcc063a651a64acc88921",
    "sh": "2a2b85e94581545ca3c05b8f864b488b141a8a1f",
    "us": "1002dd7b56aa0a59a9103f1fb3d57d6b161f8da7",
}


def usage() -> None:
    """
    Print program usage.
    """
    print(f"Usage: {sys.argv[0]} version (us jp eu sh list)")


def get_rom_candidates() -> dict[str, str]:
    """
    Search the working directory and the global baserom directory for files,
     then compare hashes of those files with known SHA1's. If a file matches a
     swapped SHA1, then invoke `dd` to perform a byteswap to a temp folder.

    Returns a dictionary where the key is a version string,
      and the value is the ROM found for that version
    """
    fileArray: list[str] = [f for f in os.listdir(os.getcwd()) if os.path.isfile(f)]
    if os.path.exists(ROMS_DIR):
        fileArray += [
            os.path.join(ROMS_DIR, f)
            for f in os.listdir(ROMS_DIR)
            if os.path.isfile(os.path.join(ROMS_DIR, f))
        ]

    foundVersions: dict[str, str] = {}

    baseromCandidate: str
    for baseromCandidate in fileArray:
        try:
            sha1sum: str = ""
            with open(baseromCandidate, "rb") as romFile:
                sha1sum = sha1(romFile.read()).hexdigest()

            version: str
            sha: str
            for version, sha in sha1_LUT.items():
                if sha == sha1sum:
                    foundVersions[version] = baseromCandidate

            version: str
            sha: str
            for version, sha in sha1_swapLUT.items():
                if sha == sha1sum:  # the ROM is swapped!
                    if not os.path.isfile(f"/tmp/baserom.{version}.swapped.z64"):
                        # Only swap the ROM if it doesn't exist
                        subprocess.run(
                            [
                                "dd",
                                "conv=swab",
                                f"if={baseromCandidate}",
                                f"of=/tmp/baserom.{version}.swapped.z64",
                            ],
                            stderr=subprocess.PIPE,
                        )
                    foundVersions[version] = f"/tmp/baserom.{version}.swapped.z64"
        except Exception:
            # Just skip the file if anything goes wrong
            continue
    return foundVersions

if __name__ == "__main__":
    if len(sys.argv) != 2:
        usage()
        sys.exit(1)
    gamelist = get_rom_candidates()
    version = sys.argv[1]

    if version == "list":
        # List all found versions
        print(" ".join(gamelist.keys()))
    elif version in gamelist:
        # List the ROM found for a given version, if it exists
        print(gamelist[version])
