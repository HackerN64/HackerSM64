//Module name: Floating IPS, global header
//Author: Alcaro
//Date: See Git history
//Licence: GPL v3.0 or higher

#ifndef struct_mem
#define struct_mem

//the standard library can be assumed to exist
#include <stddef.h>//size_t, SIZE_MAX
#include <stdint.h>//uint8_t

#ifndef SIZE_MAX
#define SIZE_MAX ((size_t)-1)
#endif

struct mem {
	uint8_t * ptr;
	size_t len;
};

#if defined(FLIPS_WINDOWS)
#define LPCWSTR const wchar_t *
#else
#define LPCWSTR const char *
#endif

#ifdef __cplusplus
//used by both Flips core/GUI and the BPS creator
class file {
public:
	static file* create(LPCWSTR filename);
	static file* create_libc(const char * filename);
	static bool exists(LPCWSTR filename);
	static bool exists_libc(const char * filename);
	
	virtual size_t len() = 0;
	virtual bool read(uint8_t* target, size_t start, size_t len) = 0;
	
	//these two add sizeof(WCHAR) 00s after the actual data, so you can cast it to LPCWSTR
	static struct mem read(LPCWSTR filename); // provided by Flips core
	struct mem read(); // provided by Flips core
	
	virtual ~file() {}
};

class filemap {
public:
	static filemap* create(LPCWSTR filename);
	static filemap* create_fallback(LPCWSTR filename);
	
	virtual size_t len() = 0;
	virtual const uint8_t * ptr() = 0;
	struct mem get() { struct mem m = { (uint8_t*)ptr(), len() }; return m; }
	
	virtual ~filemap() {}
};

class filewrite {
public:
	static filewrite* create(LPCWSTR filename);
	static filewrite* create_libc(const char * filename);
	
	virtual bool append(const uint8_t* data, size_t len) = 0;
	
	static bool write(LPCWSTR filename, struct mem data); // provided by Flips core
	
	virtual ~filewrite() {}
};
#endif

#endif
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


#ifndef __cplusplus
#include <stdbool.h>//If this file does not exist, remove it and uncomment the following three lines.
//#define bool int
//#define true 1
//#define false 0
#endif


//provided by Flips core

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
#define GUIClaimConsole()

//the OS port is responsible for main()
//Module name: crc32
//Author: Alcaro
//Date: June 3, 2015
//Licence: GPL v3.0 or higher

#include <stdint.h>
#include <stdlib.h>

uint32_t crc32_update(const uint8_t* data, size_t len, uint32_t crc);
static inline uint32_t crc32(const uint8_t* data, size_t len) { return crc32_update(data, len, 0); }
/*
 * divsufsort.h for libdivsufsort-lite
 * Copyright (c) 2003-2008 Yuta Mori All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef _DIVSUFSORT_H
#define _DIVSUFSORT_H 1

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


/*- Prototypes -*/

/**
 * Constructs the suffix array of a given string.
 * @param T[0..n-1] The input string.
 * @param SA[0..n-1] The output array of suffixes.
 * @param n The length of the given string.
 * @return 0 if no error occurred, -1 or -2 otherwise.
 */
int
divsufsort(const unsigned char *T, int *SA, int n);

/**
 * Constructs the burrows-wheeler transformed string of a given string.
 * @param T[0..n-1] The input string.
 * @param U[0..n-1] The output string. (can be T)
 * @param A[0..n-1] The temporary array. (can be NULL)
 * @param n The length of the given string.
 * @return The primary index if no error occurred, -1 or -2 otherwise.
 */
int
divbwt(const unsigned char *T, unsigned char *U, int *A, int n);


#ifdef __cplusplus
} /* extern "C" */
#endif /* __cplusplus */

#endif /* _DIVSUFSORT_H */
//Module name: libbps
//Author: Alcaro
//Date: November 30, 2015
//Licence: GPL v3.0 or higher

#include <stdint.h>
#include <string.h>

#ifndef __cplusplus
#include <stdbool.h>//bool; if this file does not exist (hi msvc), remove it and uncomment the following three lines.
//#define bool int
//#define true 1
//#define false 0
#endif

#ifdef __cplusplus
extern "C" {
#endif

enum bpserror {
	bps_ok,//Patch applied or created successfully.
	
	bps_to_output,//You attempted to apply a patch to its output.
	bps_not_this, //This is not the intended input file for this patch.
	bps_broken,   //This is not a BPS patch, or it's malformed somehow.
	bps_io,       //The patch could not be read.
	
	bps_identical, //The input files are identical.
	bps_too_big,   //Somehow, you're asking for something a size_t can't represent.
	bps_out_of_mem,//Memory allocation failure.
	bps_canceled,  //The callback returned false.
	
	bps_shut_up_gcc//This one isn't used, it's just to kill a stray comma warning.
};

//Applies the given BPS patch to the given ROM and puts it in 'out'. Metadata, if present and
// requested ('metadata'!=NULL), is also returned. Send both to bps_free when you're done with them.
//If accept_wrong_input is true, it may return bps_to_output or bps_not_this, while putting non-NULL in out/metadata.
enum bpserror bps_apply(struct mem patch, struct mem in, struct mem * out, struct mem * metadata, bool accept_wrong_input);

//Creates a BPS patch that converts source to target and stores it to patch. It is safe to give
//  {NULL,0} as metadata.
enum bpserror bps_create_linear(struct mem source, struct mem target, struct mem metadata, struct mem * patch);

#ifdef __cplusplus // TODO: make this functionality available from C and C-ABI-only languages
//Very similar to bps_create_linear; the difference is that this one takes longer to run, but
//  generates smaller patches.
//Because it can take much longer, a progress meter is supplied; total is guaranteed to be constant
//  between every call until this function returns, done is guaranteed to increase between each
//  call, and done/total is an approximate percentage counter. Anything else is undefined; for
//  example, progress may or may not be called for done=0, progress may or may not be called for
//  done=total, done may or may not increase by the same amount between each call, and the duration
//  between each call may or may not be constant.
//To cancel the patch creation, return false from the callback.
//It is safe to pass in NULL for the progress indicator if you're not interested. If the callback is
//  NULL, it can obviously not be canceled that way (though if it's a CLI program, you can always
//  Ctrl-C it).
//The 'moremem' flag makes it use about twice as much memory (9*(source+target) instead of 5*), but is usually slightly faster.
enum bpserror bps_create_delta(file* source, file* target, struct mem metadata, struct mem * patch,
                               bool (*progress)(void* userdata, size_t done, size_t total), void* userdata,
                               bool moremem);
#endif

//Like the above, but takes struct mem rather than file*. Better use the above if possible, the
//  creator takes 5*(source+target) in addition to whatever the source/target arguments need.
enum bpserror bps_create_delta_inmem(struct mem source, struct mem target, struct mem metadata, struct mem * patch,
                               bool (*progress)(void* userdata, size_t done, size_t total), void* userdata,
                               bool moremem);

//Frees the memory returned in the output parameters of the above. Do not call it twice on the same
//  input, nor on anything you got from anywhere else. bps_free is guaranteed to be equivalent to
//  calling stdlib.h's free() on mem.ptr.
void bps_free(struct mem mem);

#ifdef __cplusplus
struct bpsinfo {
	enum bpserror error; // If this is not bps_ok, all other values are undefined.
	
	size_t size_in;
	size_t size_out;
	
	uint32_t crc_in;
	uint32_t crc_out;
	uint32_t crc_patch;
	
	size_t meta_start;
	size_t meta_size;
	
	//Tells approximately how much of the input ROM is changed compared to the output ROM.
	//It's quite heuristic. The algorithm may change with or without notice.
	//As of writing, I believe this is accurate to 2 significant digits in base 10.
	//It's also more expensive to calculate than the other data, so it's optional.
	//If you don't want it, their values are undefined.
	//The denominator is always guaranteed nonzero, even if something else says it's undefined.
	//Note that this can return success for invalid patches.
	size_t change_num;
	size_t change_denom;
};
struct bpsinfo bps_get_info(file* patch, bool changefrac);
#endif

#include <stdio.h>
void bps_disassemble(struct mem patch, FILE* out);

#ifdef __cplusplus
}
#endif
