#pragma once
//Module name: Floating IPS, header for all frontends
//Author: Alcaro
//Date: See Git history
//Licence: GPL v3.0 or higher

//Preprocessor switch documentation:
//
//FLIPS_WINDOWS
//FLIPS_GTK
//FLIPS_CLI
//  Picks which frontend to use for Flips. You can pick one manually, or let Flips choose
//  automatically depending on the platform (Windows -> FLIPS_WINDOWS, Linux -> FLIPS_GTK, anything
//  else -> FLIPS_CLI). FLIPS_WINDOWS and FLIPS_CLI can be compiled under both C99 and C++98;
//  FLIPS_GTK is only tested under C99.
//  Note that picking the platform native frontend will bring a few advantages even if you only
//  intend to use Flips from the command line; Windows gains access to filenames outside the 8bit
//  charset, and GTK+ will gain the ability to handle files on URIs and not the local file system.
//
//All of these must be defined globally, or Flips will behave erratically.

#if defined(FLIPS_WINDOWS) || defined(FLIPS_GTK) || defined(FLIPS_CLI)
//already picked
#elif defined(_WIN32)
#define FLIPS_WINDOWS
#elif defined(__linux__)
#define FLIPS_GTK
#else
#define FLIPS_CLI
#endif

//#ifdef __cplusplus
//#define EXTERN_C extern "C"
//#else
//#define EXTERN_C
//#endif

#define flipsversion "Floating IPS"


#if defined(FLIPS_WINDOWS)
#define UNICODE
#define _WIN32_WINNT 0x0501
#define _WIN32_IE 0x0600

#define NOMINMAX // this seems automatically on in C++ - crazy.
#ifdef __MINGW32__
#include <stdlib.h>
#undef __USE_MINGW_ANSI_STDIO // must remove this, to avoid a libgcc_s_sjlj-1.dll dependency on 32bit
#endif                        // comments say libstdc++ demands a POSIX printf, but I'm not using that, so I don't care
#include <windows.h>
#include <windowsx.h>
#include <shlobj.h>
#include <wchar.h>
#include <stdio.h>
#include <commctrl.h>
#include <ctype.h>

#define wcsicmp _wcsicmp // wcsicmp deprecated? okay, have a define
#define wcsdup _wcsdup
#define wtoi _wtoi


#else
#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

//Flips uses Windows type names internally, since it's easier to #define them to Linux types than
//defining "const char *" to anything else. I could invent my own typedefs, but the only advantage
//that would bring over Windows types would be not being Windows types, and I don't see that as a
//valid argument.

#define LPCWSTR const char *
#define LPWSTR char *
#define WCHAR char

#define wcscpy strcpy
#define wcscat strcat
#define wcschr strchr
#define wcslen strlen
#define wcsdup strdup
#define wcsrchr strrchr
#define wcscmp strcmp
#define wcsncmp strncmp
#define wcsicmp strcasecmp
//#define wcsnicmp strncasecmp
#define wprintf printf
#define wsprintf sprintf
#define wscanf scanf
#define swscanf sscanf
#define wtoi atoi

#define iswalnum isalnum
#define iswalpha isalpha
#define iswascii isascii
#define iswblank isblank
#define iswcntrl iscntrl
#define iswdigit isdigit
#define iswgraph isgraph
#define iswlower islower
#define iswprint isprint
#define iswpunct ispunct
#define iswspace isspace
#define iswupper isupper
#define iswxdigit isxdigit 

#define TEXT(text) text
//EXTERN_C int strcasecmp(const char *s1, const char *s2);

//some platforms define strdup, some don't.
#define strdup strdup_flips
static inline char* strdup(const char * in)
{
	size_t len=strlen(in);
	char * ret=(char*)malloc(len+1);
	memcpy(ret, in, len+1);
	return ret;
}
#endif

#include "libbps.h"
#include "libips.h"
#include "libups.h"

#ifndef __cplusplus
#include <stdbool.h>//If this file does not exist, remove it and uncomment the following three lines.
//#define bool int
//#define true 1
//#define false 0
#endif


//provided by Flips core
#include "global.h"

enum patchtype {
	ty_null,
	ty_bps,
	ty_ips,
	
	//non-recommended formats
	ty_bps_linear,
	ty_bps_moremem,
	ty_ups,
	
	ty_shut_up_gcc
};

enum errorlevel {
	el_ok,
	el_notice,
	el_unlikelythis,
	el_warning,
	el_notthis,
	el_broken,
	el_shut_up_gcc
};

struct errorinfo {
	enum errorlevel level;
	const char * description;
};

struct manifestinfo {
	bool use;
	bool required;
	LPCWSTR name;
};

class file;
class filewrite;

LPWSTR GetExtension(LPCWSTR fname);
LPWSTR GetBaseName(LPCWSTR fname);
bool shouldRemoveHeader(LPCWSTR romname, size_t romlen);

class config
{
	LPWSTR filename;
	
	size_t numentries;
	LPWSTR * names;
	LPWSTR * values;
	
	//stupid c++, why is there no sane way to get the implementation out of the headers
	void sort();
	
public:
	
	config()
	{
		numentries = 0;
		names = NULL;
		values = NULL;
	}
	
	//This ends up writing a really ugly format on Windows: UTF-16, no BOM, LF endings.
	//This is because Microsoft are rude and refuse to support UTF-8 properly. I'm not rewarding that.
	//I'm catering to their shitty char type, that's way more than enough.
	
	//If the input is invalid, the object will ignore the invalid parts and remain valid.
	//In particular, failure to initialize from a file will still update the file on destruction.
	//Only init once, or it may leak memory or otherwise misbehave.
	void init_file(LPCWSTR filename);
	void init_raw(LPWSTR contents); // Modifies the input string.
	
	//The key may only contain alphanumerics, . and _.
	//The value may not have leading or trailing whitespace, or contain \r or \n.
	void set(LPCWSTR key, LPCWSTR value); // If NULL, the key is removed. This may alter or rearrange unrelated get{name,value}byid values.
	LPCWSTR get(LPCWSTR key, LPCWSTR def = NULL);
	
	void setint(LPCWSTR key, int value) { WCHAR valstr[16]; wsprintf(valstr, TEXT("%i"), value); set(key, valstr); }
	int getint(LPCWSTR key, int def = 0) { LPCWSTR val = get(key); return val ? wtoi(val) : def; }
	
	size_t getcount() { return numentries; }
	LPCWSTR getnamebyid(size_t i) { return names[i]; }
	LPCWSTR getvaluebyid(size_t i) { return values[i]; }
	
	LPWSTR flatten(); // free() this when you're done.
	~config(); // If you used init_file, this saves automatically.
};
extern config cfg;

//TODO: rewrite these
struct mem GetRomList();
void SetRomList(struct mem data);
LPCWSTR FindRomForPatch(file* patch, bool * possibleToFind);
void AddToRomList(file* patch, LPCWSTR path);
void AddConfigToRomList(LPCWSTR key, LPCWSTR value);
void DeleteRomFromList(LPCWSTR path);

LPCWSTR GetEmuFor(LPCWSTR filename); // NULL if none
void SetEmuFor(LPCWSTR filename, LPCWSTR emu);

struct errorinfo ApplyPatchMem2(file* patch, struct mem inrom, bool removeheader, bool verifyinput,
                                LPCWSTR outromname, struct manifestinfo * manifestinfo);
struct errorinfo ApplyPatchMem(file* patch, LPCWSTR inromname, bool verifyinput,
                               LPCWSTR outromname, struct manifestinfo * manifestinfo, bool update_rom_list);
struct errorinfo ApplyPatch(LPCWSTR patchname, LPCWSTR inromname, bool verifyinput,
                            LPCWSTR outromname, struct manifestinfo * manifestinfo, bool update_rom_list);
//struct errorinfo CreatePatchToMem(file* inrom, file* outrom, enum patchtype patchtype,
                                  //struct manifestinfo * manifestinfo, struct mem * patchmem);
//struct errorinfo CreatePatch(file* inrom, file* outrom, enum patchtype patchtype,
                             //struct manifestinfo * manifestinfo, LPCWSTR patchname);
struct errorinfo CreatePatchToMem(LPCWSTR inromname, LPCWSTR outromname, enum patchtype patchtype,
                                  struct manifestinfo * manifestinfo, struct mem * patchmem);
struct errorinfo CreatePatch(LPCWSTR inromname, LPCWSTR outromname, enum patchtype patchtype,
                             struct manifestinfo * manifestinfo, LPCWSTR patchname);

extern char bpsdProgStr[24];
extern int bpsdLastPromille;
bool bpsdeltaGetProgress(size_t done, size_t total);

int flipsmain(int argc, WCHAR * argv[]);
void usage();//does not return


//provided by the OS port
//several functions of file:: and filewrite:: also belong to the OS port

//TODO: delete
struct mem ReadWholeFile(LPCWSTR filename);
bool WriteWholeFile(LPCWSTR filename, struct mem data);
bool WriteWholeFileWithHeader(LPCWSTR filename, struct mem header, struct mem data);
void FreeFileMemory(struct mem mem);

void bpsdeltaBegin();
bool bpsdeltaProgress(void* userdata, size_t done, size_t total);
void bpsdeltaEnd();

int GUIShow(LPCWSTR filename);
void GUILoadConfig();
//LPCWSTR GUIGetFileFor(uint32_t crc32); // use FindRomForPatch instead
#ifdef FLIPS_WINDOWS
void GUIClaimConsole();
#else
#define GUIClaimConsole() // all other platforms have consoles already
#endif

//the OS port is responsible for main()
