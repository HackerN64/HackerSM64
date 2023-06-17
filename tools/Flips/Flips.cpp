//Module name: crc32
//Author: Alcaro
//Date: June 3, 2015
//Licence: GPL v3.0 or higher

#define INLINE __inline

/*
 * divsufsort.h for libdivsufsort
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

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include <inttypes.h>

#ifndef DIVSUFSORT_API
# ifdef DIVSUFSORT_BUILD_DLL
#  define DIVSUFSORT_API 
# else
#  define DIVSUFSORT_API 
# endif
#endif

/*- Datatypes -*/
#ifndef SAUCHAR_T
#define SAUCHAR_T
typedef uint8_t sauchar_t;
#endif /* SAUCHAR_T */
#ifndef SAINT_T
#define SAINT_T
typedef int32_t saint_t;
#endif /* SAINT_T */
#ifndef SAIDX_T
#define SAIDX_T
typedef int32_t saidx_t;
#endif /* SAIDX_T */
#ifndef PRIdSAINT_T
#define PRIdSAINT_T PRId32
#endif /* PRIdSAINT_T */
#ifndef PRIdSAIDX_T
#define PRIdSAIDX_T PRId32
#endif /* PRIdSAIDX_T */


/*- Prototypes -*/

/**
 * Constructs the suffix array of a given string.
 * @param T[0..n-1] The input string.
 * @param SA[0..n-1] The output array of suffixes.
 * @param n The length of the given string.
 * @return 0 if no error occurred, -1 or -2 otherwise.
 */
DIVSUFSORT_API
saint_t
divsufsort(const sauchar_t *T, saidx_t *SA, saidx_t n);

/**
 * Constructs the burrows-wheeler transformed string of a given string.
 * @param T[0..n-1] The input string.
 * @param U[0..n-1] The output string. (can be T)
 * @param A[0..n-1] The temporary array. (can be NULL)
 * @param n The length of the given string.
 * @return The primary index if no error occurred, -1 or -2 otherwise.
 */
DIVSUFSORT_API
saidx_t
divbwt(const sauchar_t *T, sauchar_t *U, saidx_t *A, saidx_t n);

/**
 * Returns the version of the divsufsort library.
 * @return The version number string.
 */
DIVSUFSORT_API
const char *
divsufsort_version(void);


/**
 * Constructs the burrows-wheeler transformed string of a given string and suffix array.
 * @param T[0..n-1] The input string.
 * @param U[0..n-1] The output string. (can be T)
 * @param SA[0..n-1] The suffix array. (can be NULL)
 * @param n The length of the given string.
 * @param idx The output primary index.
 * @return 0 if no error occurred, -1 or -2 otherwise.
 */
DIVSUFSORT_API
saint_t
bw_transform(const sauchar_t *T, sauchar_t *U,
             saidx_t *SA /* can NULL */,
             saidx_t n, saidx_t *idx);

/**
 * Inverse BW-transforms a given BWTed string.
 * @param T[0..n-1] The input string.
 * @param U[0..n-1] The output string. (can be T)
 * @param A[0..n-1] The temporary array. (can be NULL)
 * @param n The length of the given string.
 * @param idx The primary index.
 * @return 0 if no error occurred, -1 or -2 otherwise.
 */
DIVSUFSORT_API
saint_t
inverse_bw_transform(const sauchar_t *T, sauchar_t *U,
                     saidx_t *A /* can NULL */,
                     saidx_t n, saidx_t idx);

/**
 * Checks the correctness of a given suffix array.
 * @param T[0..n-1] The input string.
 * @param SA[0..n-1] The input suffix array.
 * @param n The length of the given string.
 * @param verbose The verbose mode.
 * @return 0 if no error occurred.
 */
DIVSUFSORT_API
saint_t
sufcheck(const sauchar_t *T, const saidx_t *SA, saidx_t n, saint_t verbose);

/**
 * Search for the pattern P in the string T.
 * @param T[0..Tsize-1] The input string.
 * @param Tsize The length of the given string.
 * @param P[0..Psize-1] The input pattern string.
 * @param Psize The length of the given pattern string.
 * @param SA[0..SAsize-1] The input suffix array.
 * @param SAsize The length of the given suffix array.
 * @param idx The output index.
 * @return The count of matches if no error occurred, -1 otherwise.
 */
DIVSUFSORT_API
saidx_t
sa_search(const sauchar_t *T, saidx_t Tsize,
          const sauchar_t *P, saidx_t Psize,
          const saidx_t *SA, saidx_t SAsize,
          saidx_t *left);

/**
 * Search for the character c in the string T.
 * @param T[0..Tsize-1] The input string.
 * @param Tsize The length of the given string.
 * @param SA[0..SAsize-1] The input suffix array.
 * @param SAsize The length of the given suffix array.
 * @param c The input character.
 * @param idx The output index.
 * @return The count of matches if no error occurred, -1 otherwise.
 */
DIVSUFSORT_API
saidx_t
sa_simplesearch(const sauchar_t *T, saidx_t Tsize,
                const saidx_t *SA, saidx_t SAsize,
                saint_t c, saidx_t *left);


#ifdef __cplusplus
} /* extern "C" */
#endif /* __cplusplus */


#include "divsufsort_private.h"

// for divsufsort
#define PROJECT_VERSION_FULL "2.0.0"

#include "crc32.h"

static const uint32_t crctable_4bits[]={
	0x00000000, 0x1DB71064, 0x3B6E20C8, 0x26D930AC, 0x76DC4190, 0x6B6B51F4, 0x4DB26158, 0x5005713C,
	0xEDB88320, 0xF00F9344, 0xD6D6A3E8, 0xCB61B38C, 0x9B64C2B0, 0x86D3D2D4, 0xA00AE278, 0xBDBDF21C,
};
uint32_t crc32_update(const uint8_t* data, size_t len, uint32_t crc)
{
	crc = ~crc;
	for (size_t i=0;i<len;i++)
	{
		crc = crctable_4bits[(crc^ data[i]    )&0x0F] ^ (crc>>4);
		crc = crctable_4bits[(crc^(data[i]>>4))&0x0F] ^ (crc>>4);
	}
	return ~crc;
}
//Module name: Floating IPS, command line frontend
//Author: Alcaro
//Date: See Git history
//Licence: GPL v3.0 or higher

#include "flips.h"

file* file::create(const char * filename) { return file::create_libc(filename); }
bool file::exists(const char * filename) { return file::exists_libc(filename); }
filewrite* filewrite::create(const char * filename) { return filewrite::create_libc(filename); }
filemap* filemap::create(const char * filename) { return filemap::create_fallback(filename); }

int main(int argc, char * argv[])
{
	return flipsmain(argc, argv);
}
//Module name: Floating IPS, shared core for all frontends
//Author: Alcaro
//Date: See Git history
//Licence: GPL v3.0 or higher

#include "flips.h"
#include "crc32.h"


//TODO: source ROM chooser
//given a target, from all known source ROMs with same extension, find the most similar
// read 1MB from each; check how many bytes are same and same location as target
//  (only counting offsets where both source and target bytes are different from previous, to avoid false positives on long runs of 00)
// if one is >80% same (not counting repeated runs), and all others are <20%, use that
//if no match, multiple matches, or one or more half-matches:
// read first 64K from each of the listed sources, create BPS, and see if one is <20% of size of bps with blank source, while others are >70%
//if still nothing unambiguous:
// fail
//this goes on a separate thread

//TODO: more manual GUI
// has three text fields, for patch / source / target, and 'pick' buttons nearby that open file dialogs
// also Create / Apply / ApplyRun buttons at the bottom
//auto selection (only if target field is blank):
// patch -> source (crc32 database)
// target -> source (algorithm above)
// target -> patch (set extension to bps)
// (source&patch) -> target (source extension, name/path from patch; also happens if triggering patch->source)
//all text fields are scrolled to the end on creation
//no auto selection in simple UI
//applying or creating clears patch/target, but keeps source
//ips creation is allowed, but only by changing filetype in picker, or editing text field; it does not keep last used type
// ips application remains unchanged
// multi-patching is allowed, each filename is wrapped in <>; if filename doesn't start or end with right char, it's a single name
//  colon-separated or semicolon-separated are more common, but they're plausible (uncommon, but possible) in filenames
//   (Super Mario World 2: Yoshi's Island, or Marios;Gate, for example), and I don't want to fail on that
// multi-creation is not allowed; technically feasible, but super rare, should probably be CLI instead
// if first file isn't found, it tries using the entire field as filename, in case it actually contains semicolon


//TODO: delete
struct mem ReadWholeFile(LPCWSTR filename)
{
	return file::read(filename);
}

bool WriteWholeFile(LPCWSTR filename, struct mem data)
{
	return filewrite::write(filename, data);
}

bool WriteWholeFileWithHeader(LPCWSTR filename, struct mem header, struct mem data)
{
	filewrite* f = filewrite::create(filename);
	if (!f) return false;
	bool ret = (f->append(header.ptr, 512) && f->append(data.ptr, data.len)); // do not use header.len, that'd prepend the entire file
	delete f;
	return ret;
}

void FreeFileMemory(struct mem mem)
{
	free(mem.ptr);
}





class file_libc : public file {
	size_t size;
	FILE* io;
	
public:
	static file* create(const char * filename)
	{
		FILE* f = fopen(filename, "rb");
		if (!f) return NULL;
		return new file_libc(f);
	}
	
private:
	file_libc(FILE* io) : io(io)
	{
		fseek(io, 0, SEEK_END);
		size = ftell(io);
	}
	
public:
	size_t len() { return size; }
	
	bool read(uint8_t* target, size_t start, size_t len)
	{
		fseek(io, start, SEEK_SET);
		return (fread(target, 1,len, io) == len);
	}
	
	~file_libc() { fclose(io); }
};
file* file::create_libc(const char * filename) { return file_libc::create(filename); }
bool file::exists_libc(const char * filename)
{
	FILE* f = fopen(filename, "rb");
	if (f) fclose(f);
	return (bool)f;
}


class filewrite_libc : public filewrite {
	FILE* io;
	
public:
	static filewrite* create(const char * filename)
	{
		FILE* f = fopen(filename, "wb");
		if (!f) return NULL;
		return new filewrite_libc(f);
	}
	
private:
	filewrite_libc(FILE* io) : io(io) {}
	
public:
	bool append(const uint8_t* data, size_t len)
	{
		return (fwrite(data, 1,len, io)==len);
	}
	
	~filewrite_libc() { fclose(io); }
};

filewrite* filewrite::create_libc(const char * filename) { return filewrite_libc::create(filename); }


class filemap_fallback : public filemap {
public:
	size_t m_len;
	uint8_t* m_ptr;
	
	static filemap* create(file* f)
	{
		if (!f) return NULL;
		
		size_t len = f->len();
		uint8_t* ptr = (uint8_t*)malloc(len);
		if (!ptr) return NULL;
		if (!f->read(ptr, 0, len)) { free(ptr); return NULL; }
		return new filemap_fallback(f, len, ptr);
	}
	
	size_t len() { return m_len; }
	const uint8_t * ptr() { return m_ptr; }
	
	//delete the file early, to avoid file sharing issues on Windows (and because keeping it is useless)
	// https://github.com/Alcaro/Flips/pull/14
	filemap_fallback(file* f, size_t len, uint8_t* ptr) : m_len(len), m_ptr(ptr) { delete f; }
	~filemap_fallback() { free(m_ptr); }
};
filemap* filemap::create_fallback(LPCWSTR filename)
{
	return filemap_fallback::create(file::create(filename));
}





LPWSTR GetExtension(LPCWSTR fname)
{
	LPWSTR ptr1=(LPWSTR)fname;
	LPWSTR ptr2;
	ptr2=wcsrchr(ptr1, '/'); if (ptr2) ptr1=ptr2;
	ptr2=wcsrchr(ptr1, '.'); if (ptr2) ptr1=ptr2;
	if (*ptr1=='.') return ptr1;
	else return wcsrchr(ptr1, '\0');
}

LPWSTR GetBaseName(LPCWSTR fname)
{
	LPWSTR ptr1=(LPWSTR)fname;
	LPWSTR ptr2;
	ptr2=wcsrchr(ptr1, '/'); if (ptr2) ptr1=ptr2+1;
	return ptr1;
}

bool forceKeepHeader=false;



struct mem file::read()
{
	struct mem out;
	out.len = len();
	out.ptr = (uint8_t*)malloc(out.len + sizeof(WCHAR));
	memset(out.ptr + out.len, 0, sizeof(WCHAR));
	if (!read(out.ptr, 0, out.len))
	{
		free(out.ptr);
		struct mem err = {NULL, 0};
		return err;
	}
	return out;
}

struct mem file::read(LPCWSTR filename)
{
	struct mem err = {NULL, 0};
	file* f = file::create(filename);
	if (!f) return err;
	struct mem ret = f->read();
	delete f;
	return ret;
}

bool filewrite::write(LPCWSTR filename, struct mem data)
{
	filewrite* f = filewrite::create(filename);
	if (!f) return false;
	bool ret = f->append(data.ptr, data.len);
	delete f;
	return ret;
}


class fileheader : public file {
	file* child;
	
public:
	fileheader(file* child) : child(child) {}
	
	size_t len() { return child->len()-512; }
	bool read(uint8_t* target, size_t start, size_t len) { return child->read(target, start+512, len); }
	
	~fileheader() { delete child; }
};

class fileheadermap : public filemap {
	filemap* child;
	
public:
	fileheadermap(filemap* child) : child(child) {}
	
	size_t len() { return child->len()-512; }
	//bool read(uint8_t* target, size_t start, size_t len) { return child->read(target, start+512, len); }
	const uint8_t* ptr() { return child->ptr()+512; }
	
	~fileheadermap() { delete child; }
};




const struct errorinfo ipserrors[]={
		{ el_ok, NULL },//ips_ok
		{ el_unlikelythis, "The patch was applied, but is most likely not intended for this ROM." },//ips_notthis
		{ el_unlikelythis, "The patch was applied, but did nothing. You most likely already had the output file of this patch." },//ips_thisout
		{ el_warning, "The patch was applied, but appears scrambled or malformed." },//ips_suspicious
		{ el_broken, "The patch is broken and can't be used." },//ips_invalid
		
		{ el_broken, "The IPS format does not support files larger than 16MB." },//ips_16MB
		{ el_warning, "The files are identical! The patch will do nothing." },//ips_identical
	};

const struct errorinfo bpserrors[]={
		{ el_ok, NULL },//bps_ok,
		{ el_notthis, "That's the output file already." },//bps_to_output
		{ el_notthis, "This patch is not intended for this ROM." },//bps_not_this
		{ el_broken, "This patch is broken and can't be used." },//bps_broken
		{ el_broken, "Couldn't read input patch." },//bps_io
		
		{ el_warning, "The files are identical! The patch will do nothing." },//bps_identical
		{ el_broken, "These files are too big for this program to handle." },//bps_too_big
		{ el_broken, "These files are too big for this program to handle." },//bps_out_of_mem (same message as above, it's accurate for both.)
		{ el_broken, "Patch creation was canceled." },//bps_canceled
	};

LPCWSTR GetManifestName(LPCWSTR romname)
{
	//static WCHAR manifestname[MAX_PATH];
	//wcscpy(manifestname, romname);
	//LPWSTR manifestext=GetExtension(manifestname);
	//if (!manifestext) manifestext=wcschr(manifestname, '\0');
	//wcscpy(manifestext, TEXT(".xml"));
	//return manifestname;
	
	static WCHAR * manifestname=NULL;
	if (manifestname) free(manifestname);
	manifestname=(WCHAR*)malloc((wcslen(romname)+1+4)*sizeof(WCHAR));
	wcscpy(manifestname, romname);
	LPWSTR manifestext=GetExtension(manifestname);
	if (manifestext) wcscpy(manifestext, TEXT(".xml"));
	return manifestname;
}

enum patchtype IdentifyPatch(file* patch)
{
	size_t len = patch->len();
	uint8_t data[16];
	if (len>16) len=16;
	
	patch->read(data, 0, len);
	if (len>=5 && !memcmp(data, "PATCH", 5)) return ty_ips;
	if (len>=4 && !memcmp(data, "BPS1", 4)) return ty_bps;
	if (len>=4 && !memcmp(data, "UPS1", 4)) return ty_ups;
	return ty_null;
}





//this is the most inefficient possible implementation, but since it only needs about 10 entries,
//performance is irrelevant
void config::init_raw(LPWSTR contents)
{
	LPCWSTR header = TEXT("[Flips]\n");
	
	if (wcsncmp(contents, header, wcslen(header)) != 0) return;
	contents += wcslen(header);
	
	//I need to somehow ensure that stepping backwards across whitespace doesn't go before the original string.
	//This can be done with while (iswspace(*contents)) contents++;, but demanding the header above works just as well.
	
	while (true)
	{
		LPWSTR key;
		LPWSTR keyend;
		LPWSTR val;
		LPWSTR valend;
		
		LPWSTR nextline = wcschr(contents, '\n');
		
		if (nextline != NULL) valend = nextline;
		else valend = wcschr(contents, '\0');
		//do not move inside the conditional, it screws up the strchr
		while (iswspace(valend[-1])) valend--;
		*valend = '\0';
		
		LPWSTR sep = wcschr(contents, '=');
		if (sep != NULL)
		{
			key = contents;
			keyend = sep;
			val = sep+1;
			
			while (iswspace(key[0])) key++;
			while (iswspace(keyend[-1])) keyend--;
			*keyend = '\0';
			while (iswspace(val[0])) val++;
			
			if (valend>val && keyend>key && iswalnum(key[0]))
			{
				set(key, val);
			}
		}
		
		if (!nextline) break;
		contents = nextline+1;
		while (contents && iswspace(*contents)) contents++;
	}
	
	for (size_t i=0;i<numentries;i++)
	{
		AddConfigToRomList(names[i], values[i]);
	}
}

void config::init_file(LPCWSTR filename)
{
	struct mem data = file::read(filename);
	if (data.len > 0 && data.len%sizeof(WCHAR) == 0)
	{
		this->init_raw((LPWSTR)(data.ptr));
	}
	free(data.ptr);
	
	this->filename = wcsdup(filename);
}

void config::sort()
{
	//bubble sort, and called for every insertion... super fun
	//but it's easy, it works, and it's fast for numentries=10 and there's no reason to go much higher than that
	for (size_t i=0;i<numentries;i++)
	for (size_t j=i+1;j<numentries;j++)
	{
		if (wcscmp(names[i], names[j]) > 0)
		{
			LPWSTR tmp = names[i];
			names[i] = names[j];
			names[j] = tmp;
			
			tmp = values[i];
			values[i] = values[j];
			values[j] = tmp;
		}
	}
}

void config::set(LPCWSTR name, LPCWSTR value)
{
	for (size_t i=0;i<this->numentries;i++)
	{
		if (!wcscmp(name, this->names[i]))
		{
			if (value == this->values[i])
				return;
			free(this->values[i]);
			if (value!=NULL)
			{
				this->values[i] = wcsdup(value);
			}
			else
			{
				free(this->names[i]);
				
				this->names[i] = this->names[this->numentries-1];
				this->values[i] = this->values[this->numentries-1];
				this->numentries--;
			}
			return;
		}
	}
	
	this->numentries++;
	this->names = (LPWSTR*)realloc(this->names, sizeof(LPWSTR)*this->numentries);
	this->values = (LPWSTR*)realloc(this->values, sizeof(LPWSTR)*this->numentries);
	
	this->names[this->numentries-1] = wcsdup(name);
	this->values[this->numentries-1] = wcsdup(value);
	
	sort();
}

LPCWSTR config::get(LPCWSTR name, LPCWSTR def)
{
	for (size_t i=0;i<this->numentries;i++)
	{
		if (!wcscmp(name, this->names[i]))
		{
			if (this->values[i]) return this->values[i];
			else return def;
		}
	}
	return def;
}

LPWSTR config::flatten()
{
	LPCWSTR header = TEXT("[Flips]\n#Changing this file may void your warranty. Do not report any bugs if you do.\n");
	
	size_t len = wcslen(header);
	for (size_t i=0;i<this->numentries;i++)
	{
		if (this->values[i]!=NULL)
		{
			len += wcslen(this->names[i]) + 1 + wcslen(this->values[i]) + 1;
		}
	}
	
	LPWSTR ret = (LPWSTR)malloc((len+1)*sizeof(WCHAR));
	
	LPWSTR at = ret;
	at += wsprintf(at, TEXT("%s"), header);
	for (size_t i=0;i<this->numentries;i++)
	{
		if (this->values[i]!=NULL)
		{
			at += wsprintf(at, TEXT("%s=%s\n"), this->names[i], this->values[i]);
		}
	}
	
	return ret;
}

config::~config()
{
	if (this->filename)
	{
		LPWSTR data = this->flatten();
//puts(data);
		struct mem m = { (uint8_t*)data, wcslen(data)*sizeof(WCHAR) };
		filewrite::write(this->filename, m);
		free(data);
		free(this->filename);
	}
	
	for (size_t i=0;i<this->numentries;i++)
	{
//printf("#(%s)(%s)\n",this->names[i],this->values[i]);
		free(this->names[i]);
		free(this->values[i]);
	}
	free(this->names);
	free(this->values);
}

config cfg;




static LPWSTR EmuGetKey(LPCWSTR filename)
{
	static WCHAR ret[64];
	wsprintf(ret, TEXT("emu%s"), GetExtension(filename));
	return ret;
}

LPCWSTR GetEmuFor(LPCWSTR filename)
{
	return cfg.get(EmuGetKey(filename));
}

void SetEmuFor(LPCWSTR filename, LPCWSTR emu)
{
	cfg.set(EmuGetKey(filename), emu);
}





enum {
	ch_crc32,
	ch_last
};
static LPCWSTR checkmap_typenames[] = { TEXT("rom.crc32.") };
// sizeof rather than strlen to ensure compile-time evaluation; -1 for NUL
static const int checkmap_typenames_maxlen = sizeof("rom.crc32.")-1;
struct checkmap {
	uint8_t* sum;
	LPWSTR name;
};
static struct checkmap * checkmap[ch_last]={NULL};
static uint32_t checkmap_len[ch_last]={0};
static const uint8_t checkmap_sum_size[]={ 4 };
static const uint8_t checkmap_sum_size_max = 4;

static const int CfgSumNameMaxLen = checkmap_typenames_maxlen + checkmap_sum_size_max*2 + 1;
static void CfgSumName(WCHAR* out, int type, const void* sum)
{
	const uint8_t* sum8 = (uint8_t*)sum;
	wcscpy(out, checkmap_typenames[type]);
	WCHAR* end = out + wcslen(checkmap_typenames[type]);
	for (int i=0;i<checkmap_sum_size[type];i++)
		wsprintf(end+i*2, TEXT("%.2X"), sum8[i]);
}
static bool CfgSumParseName(int* type, void* sum, LPCWSTR in)
{
	if (wcsncmp(in, TEXT("rom."), strlen("rom.")) != 0)
		return false;
	uint8_t* out = (uint8_t*)sum;
	for (int t=0;t<ch_last;t++)
	{
		if (!wcsncmp(in, checkmap_typenames[t], wcslen(checkmap_typenames[t])))
		{
			*type = t;
			LPCWSTR hex = in + wcslen(checkmap_typenames[t]);
			if (wcslen(hex) != checkmap_sum_size[t]*2) return false;
			WCHAR tmp[3];
			unsigned tmpout = -1;
			tmp[2] = '\0';
			for (int i=0;i<checkmap_sum_size[t];i++)
			{
				tmp[0] = hex[i*2+0]; // let non-hex yield garbage, messing with config voids your warranty anyways
				tmp[1] = hex[i*2+1];
				swscanf(tmp, TEXT("%x"), &tmpout);
				out[i] = tmpout; // not %hhx because XP doesn't trust c99
			}
			return true;
		}
	}
	return false;
}

static LPCWSTR FindRomForSum(int type, void* sum)
{
	for (unsigned int i=0;i<checkmap_len[type];i++)
	{
		if (!memcmp(checkmap[type][i].sum, sum, checkmap_sum_size[type]))
		{
			return checkmap[type][i].name;
		}
	}
	return NULL;
}

static void AddRomForSum(int type, void* sum, LPCWSTR filename)
{
	if (FindRomForSum(type, sum)) return;
	
	int ch_pos=(checkmap_len[type]++);
	
	if (!(ch_pos&(ch_pos+1)))
	{
		checkmap[type]=(struct checkmap*)realloc(checkmap[type], sizeof(struct checkmap)*((ch_pos+1)*2));
	}
	
	struct checkmap* item=&checkmap[type][ch_pos];
	item->sum=(uint8_t*)malloc(checkmap_sum_size[type]);
	memcpy(item->sum, sum, checkmap_sum_size[type]);
	item->name=wcsdup(filename);
	
	WCHAR cfgname[CfgSumNameMaxLen];
	CfgSumName(cfgname, type, sum);
	cfg.set(cfgname, filename);
}

struct mem GetRomList()
{
	struct mem out={NULL, 0};
	for (unsigned int type=0;type<ch_last;type++)
	{
		out.len+=sizeof(uint32_t);
		for (uint32_t i=0;i<checkmap_len[type];i++)
		{
			out.len+=sizeof(uint8_t);
			out.len+=sizeof(uint16_t);
			out.len+=checkmap_sum_size[type];
			out.len+=sizeof(WCHAR)*wcslen(checkmap[type][i].name);
		}
	}
	out.ptr=(uint8_t*)malloc(out.len);
	uint8_t* data=out.ptr;
	for (unsigned int type=0;type<ch_last;type++)
	{
#define write(ptr, size) \
	memcpy(data, ptr, size); \
	data+=size
#define write_obj(obj) write(&obj, sizeof(obj))
		write_obj(checkmap_len[type]);
		for (uint32_t i=0;i<checkmap_len[type];i++)
		{
			write_obj(checkmap_sum_size[type]);
			uint16_t len=sizeof(WCHAR)*wcslen(checkmap[type][i].name);
			write_obj(len);
			
			write(checkmap[type][i].sum, checkmap_sum_size[type]);
			write(checkmap[type][i].name, len);
		}
#undef write
#undef write_obj
	}
	return out;
}

void FreeRomList(struct mem data)
{
	free(data.ptr);
}

void SetRomList(struct mem data)
{
	for (int type=0;type<ch_last;type++)
	{
#define read(target, bytes) \
	if (bytes > data.len) return; \
	memcpy(target, data.ptr, bytes); \
	data.ptr += bytes; \
	data.len -= bytes
#define read_discard(bytes) \
	if (bytes > data.len) return; \
	data.ptr += bytes; \
	data.len -= bytes
		uint32_t count;
		read(&count, sizeof(count));
		checkmap[type]=(struct checkmap*)malloc(sizeof(struct checkmap)*count*2);//overallocate so I won't need to round the count
		
		while (count--)
		{
			uint8_t hashlen;
			read(&hashlen, sizeof(hashlen));
			uint16_t strlen;
			read(&strlen, sizeof(strlen));
			if (hashlen==checkmap_sum_size[type])
			{
				if (data.len < hashlen+strlen) return;
				
				struct checkmap* item=&checkmap[type][checkmap_len[type]++];
				item->sum=(uint8_t*)malloc(checkmap_sum_size[type]);
				read(item->sum, hashlen);
				item->name=(WCHAR*)malloc(strlen+sizeof(WCHAR));
				read(item->name, strlen);
				memset((uint8_t*)item->name + strlen, 0, sizeof(WCHAR));
			}
			else
			{
				read_discard(hashlen);
				read_discard(strlen);
			}
		}
#undef read
	}
}

LPCWSTR FindRomForPatch(file* patch, bool * possibleToFind)
{
	if (possibleToFind) *possibleToFind=false;
	enum patchtype patchtype=IdentifyPatch(patch);
	if (patchtype==ty_bps)
	{
		struct bpsinfo info = bps_get_info(patch, false);
		if (info.error) return NULL;
		if (possibleToFind) *possibleToFind=true;
		return FindRomForSum(ch_crc32, &info.crc_in);
	}
	//UPS has checksums too, but screw UPS. Nobody cares.
	return NULL;
}

void AddToRomList(file* patch, LPCWSTR path)
{
	enum patchtype patchtype=IdentifyPatch(patch);
	if (patchtype==ty_bps)
	{
		struct bpsinfo info = bps_get_info(patch, false);
		if (info.error) return;
		AddRomForSum(ch_crc32, &info.crc_in, path);
	}
}

void AddConfigToRomList(LPCWSTR key, LPCWSTR value)
{
	int type;
	uint8_t sum[checkmap_sum_size_max];
	if (CfgSumParseName(&type, sum, key))
		AddRomForSum(type, sum, value);
}

void DeleteRomFromList(LPCWSTR path)
{
	for (unsigned int type=0;type<ch_last;type++)
	{
		for (unsigned int i=0;i<checkmap_len[type];i++)
		{
			if (!wcscmp(checkmap[type][i].name, path))
			{
				free(checkmap[type][i].name);
				free(checkmap[type][i].sum);
				memmove(&checkmap[type][i], &checkmap[type][i+1], sizeof(struct checkmap)*(checkmap_len[type]-1 - i));
				i--;
				checkmap_len[type]--;
			}
		}
	}
}





static struct errorinfo error(errorlevel level, const char * text)
{
	struct errorinfo errinf = { level, text };
	return errinf;
}

struct errorinfo ApplyPatchMem2(file* patch, struct mem inrom, bool verifyinput, bool removeheader,
                                LPCWSTR outromname, struct manifestinfo * manifestinfo)
{
	struct mem patchmem = patch->read(); // There's no real reason to remove this, no patcher knows how to handle these file objects.
	
	enum patchtype patchtype=IdentifyPatch(patch);
	struct errorinfo errinf;
	removeheader=(removeheader && patchtype==ty_bps);
	if (removeheader)
	{
		inrom.ptr+=512;
		inrom.len-=512;
	}
	struct mem outrom={NULL,0};
	struct mem manifest={NULL,0};
	
	errinf=error(el_broken, "Unknown patch format.");
	if (patchtype==ty_bps)
	{
		errinf=bpserrors[bps_apply(patchmem, inrom, &outrom, &manifest, !verifyinput)];
		if (errinf.level==el_notthis && !verifyinput && outrom.ptr)
			errinf = error(el_warning, "This patch is not intended for this ROM (output created anyways)");
		if (errinf.level==el_notthis)
		{
			bpsinfo inf = bps_get_info(patch, false);
			static char errtextbuf[2][256]; // ugly trick to get the nested invocation for the header remover to not screw up the error
			static int errtextid=0; // makes it impossible to save the error strings, but Flips doesn't do that anyways
			char* errtext=errtextbuf[errtextid];
			if (++errtextid == 2) errtextid=0;
			if (inf.size_in != inrom.len)
			{
//http://msdn.microsoft.com/en-us/library/vstudio/tcxf1dw6.aspx says %zX is not supported
//this is true up to and including Windows Vista; 7 adds support for it
//I could define it to "I", but my GCC does not acknowledge its legitimacy and throws bogus warnings
//nor does my GCC accept any supported alternative, so let's just nuke the entire warning.
//it's a poor solution, but it's the best I can find
# define z "z"
				sprintf(errtext, "This patch is not intended for this ROM. Expected file size %" z "u, got %" z "u.", inf.size_in, inrom.len);
				errinf.description=errtext;
			}
			else
			{
				uint32_t crc = crc32(inrom.ptr, inrom.len);
				if (inf.crc_in != crc)
				{
					sprintf(errtext, "This patch is not intended for this ROM. Expected checksum %.8X, got %.8X.", inf.crc_in, crc);
					errinf.description=errtext;
				}
			}
		}
	}
	if (patchtype==ty_ips) errinf=ipserrors[ips_apply(patchmem, inrom, &outrom)];
	if (patchtype==ty_ups) errinf=bpserrors[ups_apply(patchmem, inrom, &outrom)];
	if (errinf.level==el_ok) errinf.description="The patch was applied successfully!";
	
	struct manifestinfo defmanifestinfo={true,false,NULL};
	if (!manifestinfo) manifestinfo=&defmanifestinfo;
	if (manifestinfo->use)
	{
		if (manifest.ptr)
		{
			LPCWSTR manifestname;
			if (manifestinfo->name) manifestname=manifestinfo->name;
			else manifestname=GetManifestName(outromname);
			if (!WriteWholeFile(manifestname, manifest) && manifestinfo->required)
			{
				if (errinf.level==el_ok) errinf=error(el_warning, "The patch was applied, but the manifest could not be created.");
			}
		}
		else if (manifestinfo->required && errinf.level==el_ok)
		{
			errinf=error(el_warning, "The patch was applied, but there was no manifest present.");
		}
	}
	
	if (removeheader)
	{
		inrom.ptr-=512;
		inrom.len+=512;
		if (errinf.level<el_notthis)
		{
			if (!WriteWholeFileWithHeader(outromname, inrom, outrom))
			{
				errinf=error(el_broken, "Couldn't write ROM");
			}
		}
	}
	else if (errinf.level<el_notthis)
	{
		if (!WriteWholeFile(outromname, outrom)) errinf=error(el_broken, "Couldn't write ROM");
	}
	free(outrom.ptr);
	free(patchmem.ptr);
	
	if (errinf.level==el_notthis && removeheader)
	{
		struct errorinfo errinf2=ApplyPatchMem2(patch, inrom, verifyinput, false, outromname, manifestinfo);
		if (errinf2.level < el_notthis)
		{
			if (errinf2.level==el_ok)
			{
				return error(el_warning, "The patch was applied, but it was created from a headered ROM, which may not work for everyone.");
			}
			else return errinf2;
		}
	}
	return errinf;
}

bool shouldRemoveHeader(LPCWSTR romname, size_t romlen)
{
	LPWSTR romext=GetExtension(romname);
	return ((romlen&0x7FFF)==512 &&
	        (!wcsicmp(romext, TEXT(".smc")) || !wcsicmp(romext, TEXT(".sfc"))) &&
	        !forceKeepHeader);
}

struct errorinfo ApplyPatchMem(file* patch, LPCWSTR inromname, bool verifyinput,
                               LPCWSTR outromname, struct manifestinfo * manifestinfo, bool update_rom_list)
{
	filemap* inrom = filemap::create(inromname);
	if (!inrom)
	{
		if (update_rom_list) DeleteRomFromList(inromname);
		return error(el_broken, "Couldn't read ROM");
	}
	struct errorinfo errinf = ApplyPatchMem2(patch, inrom->get(), verifyinput,
	                                         shouldRemoveHeader(inromname, inrom->len()), outromname, manifestinfo);
	if (update_rom_list && errinf.level==el_ok) AddToRomList(patch, inromname);
	delete inrom;
	return errinf;
}

struct errorinfo ApplyPatch(LPCWSTR patchname, LPCWSTR inromname, bool verifyinput,
                            LPCWSTR outromname, struct manifestinfo * manifestinfo, bool update_rom_list)
{
	file* patch = file::create(patchname);
	if (!patch)
	{
		return error(el_broken, "Couldn't read input patch");
	}
	struct errorinfo errinf=ApplyPatchMem(patch, inromname, verifyinput, outromname, manifestinfo, update_rom_list);
	delete patch;
	return errinf;
}


char bpsdProgStr[24];
int bpsdLastPromille=-1;

bool bpsdeltaGetProgress(size_t done, size_t total)
{
	if (total<1000) total=1000;//avoid div by zero
	int promille=done/(total/1000);//don't set this to done*1000/total, it'd just give overflows on huge stuff. 100% is handled later
	if (promille==bpsdLastPromille) return false;
	bpsdLastPromille=promille;
	if (promille>=1000) return false;
	strcpy(bpsdProgStr, "Please wait... ");
	bpsdProgStr[15]='0'+promille/100;
	int digit1=((promille<100)?15:16);
	bpsdProgStr[digit1+0]='0'+promille/10%10;
	bpsdProgStr[digit1+1]='.';
	bpsdProgStr[digit1+2]='0'+promille%10;
	bpsdProgStr[digit1+3]='%';
	bpsdProgStr[digit1+4]='\0';
	return true;
}

bool bpsdeltaProgressCLI(void* userdata, size_t done, size_t total)
{
	if (!bpsdeltaGetProgress(done, total)) return true;
	fputs(bpsdProgStr, stdout);
	putchar('\r');
	fflush(stdout);
	return true;
}

struct errorinfo CreatePatchToMem(LPCWSTR inromname, LPCWSTR outromname, enum patchtype patchtype,
                                  struct manifestinfo * manifestinfo, struct mem * patchmem)
{
	bool usemmap = (patchtype!=ty_bps && patchtype!=ty_bps_moremem);
	
	//pick roms
	filemap* romsmap[2]={NULL, NULL};
	file* roms[2]={NULL, NULL};
	size_t lens[2];
	
	for (int i=0;i<2;i++)
	{
		LPCWSTR romname=((i==0)?inromname:outromname);
		
		if (usemmap)
		{
			romsmap[i] = filemap::create(romname);
			
			if (!romsmap[i])
			{
				if (i==1) delete romsmap[0];
				return error(el_broken, "Couldn't read this ROM.");
			}
			if (shouldRemoveHeader(romname, romsmap[i]->len()) && (patchtype==ty_bps || patchtype==ty_bps_linear || patchtype==ty_bps_moremem))
			{
				romsmap[i] = new fileheadermap(romsmap[i]);
			}
			
			lens[i] = romsmap[i]->len();
		}
		else
		{
			roms[i] = file::create(romname);
			
			if (!roms[i])
			{
				if (i==1) delete roms[0];
				return error(el_broken, "Couldn't read this ROM.");
			}
			lens[i] = roms[i]->len();
			if (shouldRemoveHeader(romname, lens[i]) && (patchtype==ty_bps || patchtype==ty_bps_linear || patchtype==ty_bps_moremem))
			{
				roms[i] = new fileheader(roms[i]);
			}
		}
	}
	
	struct mem manifest={NULL,0};
	struct errorinfo manifesterr={el_ok, NULL};
	struct manifestinfo defmanifestinfo={true,false,NULL};
	if (!manifestinfo) manifestinfo=&defmanifestinfo;
	if (patchtype==ty_bps || patchtype==ty_bps_linear || patchtype==ty_bps_moremem)
	{
		LPCWSTR manifestname;
		if (manifestinfo->name) manifestname=manifestinfo->name;
		else manifestname=GetManifestName(outromname);
		manifest=ReadWholeFile(manifestname);
		if (!manifest.ptr) manifesterr=error(el_warning, "The patch was created, but the manifest could not be read.");
	}
	else manifesterr=error(el_warning, "The patch was created, but this patch format does not support manifests.");
	
	struct errorinfo errinf={ el_broken, "Unknown patch format." };
	if (patchtype==ty_ips)
	{
		errinf=ipserrors[ips_create(romsmap[0]->get(), romsmap[1]->get(), patchmem)];
	}
	if (patchtype==ty_bps || patchtype==ty_bps_moremem)
	{
		{
			errinf=bpserrors[bps_create_delta(roms[0], roms[1], manifest, patchmem, bpsdeltaProgressCLI, NULL, (patchtype==ty_bps_moremem))];
		}
	}
	if (patchtype==ty_bps_linear)
	{
		errinf=bpserrors[bps_create_linear(romsmap[0]->get(), romsmap[1]->get(), manifest, patchmem)];
	}
	FreeFileMemory(manifest);
	if (errinf.level==el_ok) errinf.description="The patch was created successfully!";
	
	if (manifestinfo->required && errinf.level==el_ok && manifesterr.level!=el_ok) errinf=manifesterr;
	
	if (errinf.level==el_ok && lens[0] > lens[1])
	{
		errinf=error(el_warning, "The patch was created, but the input ROM is larger than the "
		                         "output ROM. Double check whether you've gotten them backwards.");
	}
	
	if (usemmap)
	{
		delete romsmap[0];
		delete romsmap[1];
	}
	else
	{
		delete roms[0];
		delete roms[1];
	}
	
	return errinf;
}

struct errorinfo CreatePatch(LPCWSTR inromname, LPCWSTR outromname, enum patchtype patchtype,
                             struct manifestinfo * manifestinfo, LPCWSTR patchname)
{
	struct mem patch={NULL,0};
	struct errorinfo errinf = CreatePatchToMem(inromname, outromname, patchtype, manifestinfo, &patch);
	
	if (errinf.level<el_notthis)
	{
		if (!WriteWholeFile(patchname, patch)) errinf=error(el_broken, "Couldn't write patch.");
	}
	if (patch.ptr) free(patch.ptr);
	return errinf;
}

errorlevel patchinfo(LPCWSTR patchname, struct manifestinfo * manifestinfo)
{
	GUIClaimConsole();
	
	file* patch = file::create(patchname);
	if (!patch)
	{
		puts("Couldn't read file");
		return el_broken;
	}
	
	enum patchtype patchtype=IdentifyPatch(patch);
	if (patchtype==ty_bps)
	{
		struct bpsinfo info = bps_get_info(patch, false);
		if (info.error)
		{
			puts(bpserrors[info.error].description);
			return bpserrors[info.error].level;
		}
		
		struct mem meta = {};
		if (info.meta_size)
		{
			meta.len = info.meta_size;
			meta.ptr = (uint8_t*)malloc(info.meta_size);
			patch->read(meta.ptr, info.meta_start, info.meta_size);
			
			if (manifestinfo->required)
			{
				if (manifestinfo->name)
				{
					filewrite::write(manifestinfo->name, meta);
				}
				else
				{
					fwrite(meta.ptr, 1,meta.len, stdout);
					free(meta.ptr);
					return el_ok;
				}
			}
		}
		
		LPCWSTR inromname = FindRomForPatch(patch, NULL);
		//'z' macro defined above
		printf("Input ROM: %" z "u bytes, CRC32 %.8X", info.size_in, info.crc_in);
		if (inromname) wprintf(TEXT(", %s"), inromname);
		puts("");
		
		printf("Output ROM: %" z "u bytes, CRC32 %.8X\n", info.size_out, info.crc_out);
		//floating point may lose a little precision, but it's easier than dodging overflows, and this
		//is the output of inaccurate heuristics anyways, losing a little more makes no difference.
		//Windows MulDiv could also work, but it's kinda nonportable.
		//printf("Change index: %i / 1000\n", (int)(info.change_num / (float)info.change_denom * 1000));
		
		if (info.meta_size)
		{
			printf("Metadata: %" z "u bytes:\n", info.meta_size);
			char* meta_iter = (char*)meta.ptr;
			char* meta_end = meta_iter + meta.len;
			for (int i=0;i<3;i++)
			{
				int n_chars = meta_end-meta_iter;
				if (n_chars > 75) n_chars = 75;
				char* nextline = (char*)memchr(meta_iter, '\n', n_chars);
				if (nextline && nextline-meta_iter < n_chars) n_chars = nextline-meta_iter;
				if (!nextline && !n_chars) break; // wipe trailing linebreaks
				printf("  %.*s\n", n_chars, meta_iter);
				if (!nextline) break;
				meta_iter = nextline+1;
			}
		}
		
		free(meta.ptr);
		return el_ok;
	}
	puts("No information available for this patch type");
	return el_broken;
}



void usage()
{
	GUIClaimConsole();
	fputs(
	// 12345678901234567890123456789012345678901234567890123456789012345678901234567890
	   flipsversion "\n"
	  "usage:\n"
	  "   "
	     "flips [--apply] [--exact] patch.bps rom.smc [outrom.smc]\n"
	  "or flips [--create] [--exact] [--bps | etc] clean.smc hack.smc [patch.bps]\n"
	  "\n"
	// 12345678901234567890123456789012345678901234567890123456789012345678901234567890
	  "options:\n"
	  "-a --apply: apply IPS, BPS or UPS patch (default if given two arguments)\n"
	  "  if output filename is not given, Flips defaults to patch.smc beside the patch\n"
	  "-c --create: create IPS or BPS patch (default if given three arguments)\n"
	  "-I --info: BPS files contain information about input and output roms, print it\n"
	  //"  also estimates how much of the source file is retained\n"
	  //"  anything under 400 is fine, anything over 600 should be treated with suspicion\n"
	  //(TODO: --info --verbose)
	  "-i --ips, -b -B --bps --bps-delta, --bps-delta-moremem, --bps-linear:\n"
	  "  create this patch format instead of guessing based on file extension\n"
	  "  ignored when applying\n"
	  " bps creation styles:\n"
	  "  delta is the recommended and default one; it's a good balance between creation\n"
	  "    performance and patch size\n"
	  "  delta-moremem is usually slightly (~3%) faster than delta, but uses about\n"
	  "    twice as much memory; it gives identical patches to delta\n"
	  "  linear is the fastest, but tends to give pretty big patches\n"
	  "  all BPS patchers can apply all patch styles, the only difference is file size\n"
	  "    and creation performance\n"
	  "--exact: do not remove SMC headers when applying or creating a BPS patch\n"
	  "    not recommended, may affect patcher compatibility\n"
	  "--ignore-checksum: accept checksum mismatches (BPS only)\n"
	  "-m or --manifest: emit or insert a manifest file as romname.xml (BPS only)\n"
	  "-mfilename or --manifest=filename: emit or insert a manifest file exactly here\n"
	  "-h -? --help: show this information\n"
	  "-v --version: show application version\n"
	  "\n"
	// 12345678901234567890123456789012345678901234567890123456789012345678901234567890
	, stdout);
	exit(0);
}


int error_to_exit(errorlevel level)
{
	return (level >= el_notthis ? EXIT_FAILURE : EXIT_SUCCESS);
}
int flipsmain(int argc, WCHAR * argv[])
{
	enum patchtype patchtype=ty_null;
	enum { a_default, a_apply_filepicker, a_apply_given, a_create, a_info } action=a_default;
	int numargs=0;
	LPCWSTR arg[3]={NULL,NULL,NULL};
	bool hasFlags=false;
	
	bool ignoreChecksum=false;
	
	struct manifestinfo manifestinfo={false, false, NULL};
//	 {
//	bool use;
//	bool required;
//	LPCWSTR name;
//	bool success;
//};
	for (int i=1;i<argc;i++)
	{
		if (argv[i][0]=='-')
		{
			hasFlags=true;
			if(0);
			else if (!wcscmp(argv[i], TEXT("--apply")) || !wcscmp(argv[i], TEXT("-a")))
			{
				if (action==a_default) action=a_apply_given;
				else usage();
			}
			else if (!wcscmp(argv[i], TEXT("--create")) || !wcscmp(argv[i], TEXT("-c")))
			{
				if (action==a_default) action=a_create;
				else usage();
			}
			else if (!wcscmp(argv[i], TEXT("--info")) || !wcscmp(argv[i], TEXT("-I")))
			{
				if (action==a_default) action=a_info;
				else usage();
			}
			else if (!wcscmp(argv[i], TEXT("--ips")) || !wcscmp(argv[i], TEXT("-i")))
			{
				if (patchtype==ty_null) patchtype=ty_ips;
				else usage();
			}
			else if (!wcscmp(argv[i], TEXT("--bps")) || !wcscmp(argv[i], TEXT("--bps-delta")) ||
			         !wcscmp(argv[i], TEXT("-b")) || !wcscmp(argv[i], TEXT("-B")))
			{
				if (patchtype==ty_null) patchtype=ty_bps;
				else usage();
			}
			else if (!wcscmp(argv[i], TEXT("--bps-delta-moremem")))
			{
				if (patchtype==ty_null) patchtype=ty_bps_moremem;
				else usage();
			}
			else if (!wcscmp(argv[i], TEXT("--bps-linear")))
			{
				if (patchtype==ty_null) patchtype=ty_bps_linear;
				else usage();
			}
			else if (!wcscmp(argv[i], TEXT("--exact"))) // no short form
			{
				if (forceKeepHeader) usage();
				forceKeepHeader=true;
			}
			else if (!wcscmp(argv[i], TEXT("--ignore-checksum")))
			{
				if (ignoreChecksum) usage();
				ignoreChecksum=true;
			}
			else if (!wcscmp(argv[i], TEXT("--manifest")) || !wcscmp(argv[i], TEXT("-m")))
			{
				manifestinfo.use=true;
				manifestinfo.required=true;
			}
			else if (!wcsncmp(argv[i], TEXT("--manifest="), wcslen(TEXT("--manifest="))))
			{
				manifestinfo.use=true;
				manifestinfo.required=true;
				manifestinfo.name=argv[i]+wcslen(TEXT("--manifest="));
			}
			else if (!wcsncmp(argv[i], TEXT("-m"), wcslen(TEXT("-m"))))
			{
				manifestinfo.use=true;
				manifestinfo.required=true;
				manifestinfo.name=argv[i]+wcslen(TEXT("-m"));
			}
			else if (!wcscmp(argv[i], TEXT("--version")) || !wcscmp(argv[i], TEXT("-v")))
			{
				GUIClaimConsole();
				puts(flipsversion);
				return 0;
			}
			else if (!wcscmp(argv[i], TEXT("--help")) || !wcscmp(argv[i], TEXT("-h")) || !wcscmp(argv[i], TEXT("-?"))) usage();
			else usage();
		}
		else
		{
			if (numargs==3) usage();
			arg[numargs++]=argv[i];
		}
	}
	if (action==a_default)
	{
		if (numargs==0) action=a_default;
		if (numargs==1) action=a_apply_filepicker;
		if (numargs==2) action=a_apply_given;
		if (numargs==3) action=a_create;
	}
	switch (action)
	{
		case a_default:
		{
			if (numargs!=0 || hasFlags) usage();
			usage();
		}
		case a_apply_filepicker:
		{
			if (numargs!=1 || hasFlags) usage();
			usage();
		}
		case a_apply_given:
		{
			if (numargs!=2 && numargs!=3) usage();
			GUIClaimConsole();
			LPCWSTR outname = arg[2];
			LPWSTR outname_buf = NULL;
			if (!outname)
			{
				WCHAR* base_ext = GetExtension(arg[1]);
				int extlen = wcslen(base_ext);
				outname_buf = (WCHAR*)malloc(sizeof(WCHAR)*(wcslen(arg[0])+extlen));
				wcscpy(outname_buf, arg[0]);
				wcscpy(GetExtension(outname_buf), base_ext);
				outname = outname_buf;
				if (wcscmp(arg[1], outname) != 0 && file::exists(outname))
				{
					wprintf(TEXT("You have requested creation of file %s, but that file already exists.\n"
					             "If you want to overwrite it, supply that filename explicitly; if not, provide another filename.\n"),
					             outname);
					return 1;
				}
			}
			struct errorinfo errinf=ApplyPatch(arg[0], arg[1], !ignoreChecksum, outname, &manifestinfo, false);
			free(outname_buf);
			puts(errinf.description);
			return error_to_exit(errinf.level);
		}
		case a_create:
		{
			if (numargs!=2 && numargs!=3) usage();
			GUIClaimConsole();
			if (!arg[2])
			{
				if (patchtype==ty_null)
				{
					puts("Error: Unknown patch type.");
					return error_to_exit(el_broken);
				}
				LPWSTR arg2=(WCHAR*)malloc(sizeof(WCHAR)*(wcslen(arg[1])+4+1));
				arg[2]=arg2;
				wcscpy(arg2, arg[1]);
				GetExtension(arg2)[0]='\0';
				if (patchtype==ty_ips) wcscat(arg2, TEXT(".ips"));
				if (patchtype==ty_bps) wcscat(arg2, TEXT(".bps"));
				if (patchtype==ty_bps_linear) wcscat(arg2, TEXT(".bps"));
			}
			if (patchtype==ty_null)
			{
				LPCWSTR patchext=GetExtension(arg[2]);
				if (!*patchext)
				{
					puts("Error: Unknown patch type.");
					return el_broken;
				}
				else if (!wcsicmp(patchext, TEXT(".ips"))) patchtype=ty_ips;
				else if (!wcsicmp(patchext, TEXT(".bps"))) patchtype=ty_bps;
				else
				{
					wprintf(TEXT("Error: Unknown patch type (%s)\n"), patchext);
					return error_to_exit(el_broken);
				}
			}
			struct errorinfo errinf=CreatePatch(arg[0], arg[1], patchtype, &manifestinfo, arg[2]);
			puts(errinf.description);
			return error_to_exit(errinf.level);
		}
		case a_info:
		{
			if (numargs!=1) usage();
			return error_to_exit(patchinfo(arg[0], &manifestinfo));
		}
	}
	return 99;//doesn't happen
}
//Module name: Floating IPS, GTK+ frontend
//Author: Alcaro
//Date: See Git history
//Licence: GPL v3.0 or higher

//List of assumptions made whose correctness is not guaranteed by GTK+:
//The character '9' is as wide as the widest of '0' '1' '2' '3' '4' '5' '6' '7' '8' '9'.
// Failure leads to: The BPS delta creation progress window being a little too small.
// Fixable: Not hard, but unlikely to be worth it.
//g_spawn_async does not write to argv or its pointed-to strings.
// Failure leads to: Corrupting the configuration.
// Fixable: Not hard, but unlikely to be worth it.

#include "flips.h"

//Module name: Floating IPS, Windows frontend
//Author: Alcaro
//Date: See Git history
//Licence: GPL v3.0 or higher

#include "flips.h"

//Module name: libbps
//Author: Alcaro
//Date: November 7, 2015
//Licence: GPL v3.0 or higher

#include "libbps.h"

#include <stdlib.h>//malloc, realloc, free
#include <string.h>//memcpy, memset
#include <stdint.h>//uint8_t, uint32_t
#include "crc32.h"//crc32

static uint32_t read32(uint8_t * ptr)
{
	uint32_t out;
	out =ptr[0];
	out|=ptr[1]<<8;
	out|=ptr[2]<<16;
	out|=ptr[3]<<24;
	return out;
}

enum { SourceRead, TargetRead, SourceCopy, TargetCopy };

static bool try_add(size_t& a, size_t b)
{
	if (SIZE_MAX-a < b) return false;
	a+=b;
	return true;
}

static bool try_shift(size_t& a, size_t b)
{
	if (SIZE_MAX>>b < a) return false;
	a<<=b;
	return true;
}

static bool decodenum(const uint8_t*& ptr, size_t& out)
{
	out=0;
	unsigned int shift=0;
	while (true)
	{
		uint8_t next=*ptr++;
		size_t addthis=(next&0x7F);
		if (shift) addthis++;
		if (!try_shift(addthis, shift)) return false;
		// unchecked because if it was shifted, the lowest bit is zero, and if not, it's <=0x7F.
		if (!try_add(out, addthis)) return false;
		if (next&0x80) return true;
		shift+=7;
	}
}

#define error(which) do { error=which; goto exit; } while(0)
#define assert_sum(a,b) do { if (SIZE_MAX-(a)<(b)) error(bps_too_big); } while(0)
#define assert_shift(a,b) do { if (SIZE_MAX>>(b)<(a)) error(bps_too_big); } while(0)
enum bpserror bps_apply(struct mem patch, struct mem in, struct mem * out, struct mem * metadata, bool accept_wrong_input)
{
	enum bpserror error = bps_ok;
	out->len=0;
	out->ptr=NULL;
	if (metadata)
	{
		metadata->len=0;
		metadata->ptr=NULL;
	}
	if (patch.len<4+3+12) return bps_broken;
	
	if (true)
	{
#define read8() (*(patchat++))
#define decodeto(var) \
				do { \
					if (!decodenum(patchat, var)) error(bps_too_big); \
				} while(false)
#define write8(byte) (*(outat++)=byte)
		
		const uint8_t * patchat=patch.ptr;
		const uint8_t * patchend=patch.ptr+patch.len-12;
		
		if (read8()!='B') error(bps_broken);
		if (read8()!='P') error(bps_broken);
		if (read8()!='S') error(bps_broken);
		if (read8()!='1') error(bps_broken);
		
		uint32_t crc_in_e = read32(patch.ptr+patch.len-12);
		uint32_t crc_out_e = read32(patch.ptr+patch.len-8);
		uint32_t crc_patch_e = read32(patch.ptr+patch.len-4);
		
		uint32_t crc_in_a = crc32(in.ptr, in.len);
		uint32_t crc_patch_a = crc32(patch.ptr, patch.len-4);
		
		if (crc_patch_a != crc_patch_e) error(bps_broken);
		
		size_t inlen;
		decodeto(inlen);
		
		size_t outlen;
		decodeto(outlen);
		
		if (inlen!=in.len || crc_in_a!=crc_in_e)
		{
			if (in.len==outlen && crc_in_a==crc_out_e) error=bps_to_output;
			else error=bps_not_this;
			if (!accept_wrong_input) goto exit;
		}
		
		out->len=outlen;
		out->ptr=(uint8_t*)malloc(outlen);
		
		const uint8_t * instart=in.ptr;
		const uint8_t * inreadat=in.ptr;
		const uint8_t * inend=in.ptr+in.len;
		
		uint8_t * outstart=out->ptr;
		uint8_t * outreadat=out->ptr;
		uint8_t * outat=out->ptr;
		uint8_t * outend=out->ptr+out->len;
		
		size_t metadatalen;
		decodeto(metadatalen);
		
		if (metadata && metadatalen)
		{
			metadata->len=metadatalen;
			metadata->ptr=(uint8_t*)malloc(metadatalen+1);
			for (size_t i=0;i<metadatalen;i++) metadata->ptr[i]=read8();
			metadata->ptr[metadatalen]='\0';//just to be on the safe side - that metadata is assumed to be text, might as well terminate it
		}
		else
		{
			for (size_t i=0;i<metadatalen;i++) (void)read8();
		}
		
		while (patchat<patchend)
		{
			size_t thisinstr;
			decodeto(thisinstr);
			size_t length=(thisinstr>>2)+1;
			int action=(thisinstr&3);
			if (outat+length>outend) error(bps_broken);
			
			switch (action)
			{
				case SourceRead:
				{
					if (outat-outstart+length > in.len) error(bps_broken);
					for (size_t i=0;i<length;i++)
					{
						size_t pos = outat-outstart; // don't inline, write8 changes outat
						write8(instart[pos]);
					}
				}
				break;
				case TargetRead:
				{
					if (patchat+length>patchend) error(bps_broken);
					for (size_t i=0;i<length;i++) write8(read8());
				}
				break;
				case SourceCopy:
				{
					size_t encodeddistance;
					decodeto(encodeddistance);
					size_t distance=encodeddistance>>1;
					if ((encodeddistance&1)==0) inreadat+=distance;
					else inreadat-=distance;
					
					if (inreadat<instart || inreadat+length>inend) error(bps_broken);
					for (size_t i=0;i<length;i++) write8(*inreadat++);
				}
				break;
				case TargetCopy:
				{
					size_t encodeddistance;
					decodeto(encodeddistance);
					size_t distance=encodeddistance>>1;
					if ((encodeddistance&1)==0) outreadat+=distance;
					else outreadat-=distance;
					
					if (outreadat<outstart || outreadat>=outat || outreadat+length>outend) error(bps_broken);
					for (size_t i=0;i<length;i++) write8(*outreadat++);
				}
				break;
			}
		}
		if (patchat!=patchend) error(bps_broken);
		if (outat!=outend) error(bps_broken);
		
		uint32_t crc_out_a = crc32(out->ptr, out->len);
		
		if (crc_out_a!=crc_out_e)
		{
			error=bps_not_this;
			if (!accept_wrong_input) goto exit;
		}
		return error;
#undef read8
#undef decodeto
#undef write8
	}
	
exit:
	free(out->ptr);
	out->len=0;
	out->ptr=NULL;
	if (metadata)
	{
		free(metadata->ptr);
		metadata->len=0;
		metadata->ptr=NULL;
	}
	return error;
}



#define write(val) \
			do { \
				out[outlen++]=(val); \
				if (outlen==outbuflen) \
				{ \
					outbuflen*=2; \
					uint8_t* newout=(uint8_t*)realloc(out, outbuflen); \
					if (!newout) { free(out); return bps_out_of_mem; } \
					out=newout; \
				} \
			} while(0)
#define write32(val) \
			do { \
				uint32_t tmp=(val); \
				write(tmp); \
				write(tmp>>8); \
				write(tmp>>16); \
				write(tmp>>24); \
			} while(0)
#define writenum(val) \
			do { \
				size_t tmpval=(val); \
				while (true) \
				{ \
					uint8_t tmpbyte=(tmpval&0x7F); \
					tmpval>>=7; \
					if (!tmpval) \
					{ \
						write(tmpbyte|0x80); \
						break; \
					} \
					write(tmpbyte); \
					tmpval--; \
				} \
			} while(0)

enum bpserror bps_create_linear(struct mem sourcemem, struct mem targetmem, struct mem metadata, struct mem * patchmem)
{
	if (sourcemem.len>=(SIZE_MAX>>2) - 16) return bps_too_big;//the 16 is just to be on the safe side, I don't think it's needed.
	if (targetmem.len>=(SIZE_MAX>>2) - 16) return bps_too_big;
	
	const uint8_t * source=sourcemem.ptr;
	const uint8_t * sourceend=sourcemem.ptr+sourcemem.len;
	if (sourcemem.len>targetmem.len) sourceend=sourcemem.ptr+targetmem.len;
	const uint8_t * targetbegin=targetmem.ptr;
	const uint8_t * target=targetmem.ptr;
	const uint8_t * targetend=targetmem.ptr+targetmem.len;
	
	const uint8_t * targetcopypos=targetbegin;
	
	size_t outbuflen=4096;
	uint8_t * out=(uint8_t*)malloc(outbuflen);
	if (!out) return bps_out_of_mem;
	size_t outlen=0;
	write('B');
	write('P');
	write('S');
	write('1');
	writenum(sourcemem.len);
	writenum(targetmem.len);
	writenum(metadata.len);
	for (size_t i=0;i<metadata.len;i++) write(metadata.ptr[i]);
	
	size_t mainContentPos=outlen;
	
	const uint8_t * lastknownchange=targetbegin;
	while (target<targetend)
	{
		size_t numunchanged=0;
		while (source+numunchanged<sourceend && source[numunchanged]==target[numunchanged]) numunchanged++;
		if (numunchanged>1 || numunchanged == (uintptr_t)(targetend-target))
		{
			//assert_shift((numunchanged-1), 2);
			writenum((numunchanged-1)<<2 | 0);//SourceRead
			source+=numunchanged;
			target+=numunchanged;
		}
		
		size_t numchanged=0;
		if (lastknownchange>target) numchanged=lastknownchange-target;
		while ((source+numchanged>=sourceend ||
		        source[numchanged]!=target[numchanged] ||
		        source[numchanged+1]!=target[numchanged+1] ||
		        source[numchanged+2]!=target[numchanged+2]) &&
		       target+numchanged<targetend)
		{
			numchanged++;
			if (source+numchanged>=sourceend) numchanged=targetend-target;
		}
		lastknownchange=target+numchanged;
		if (numchanged)
		{
			//assert_shift((numchanged-1), 2);
			size_t rle1start=(target==targetbegin);
			while (true)
			{
				if (
					target[rle1start-1]==target[rle1start+0] &&
					target[rle1start+0]==target[rle1start+1] &&
					target[rle1start+1]==target[rle1start+2] &&
					target[rle1start+2]==target[rle1start+3])
				{
					numchanged=rle1start;
					break;
				}
				if (
					target[rle1start-2]==target[rle1start+0] &&
					target[rle1start-1]==target[rle1start+1] &&
					target[rle1start+0]==target[rle1start+2] &&
					target[rle1start+1]==target[rle1start+3] &&
					target[rle1start+2]==target[rle1start+4])
				{
					numchanged=rle1start;
					break;
				}
				if (rle1start+3>=numchanged) break;
				rle1start++;
			}
			if (numchanged)
			{
				writenum((numchanged-1)<<2 | TargetRead);
				for (size_t i=0;i<numchanged;i++)
				{
					write(target[i]);
				}
				source+=numchanged;
				target+=numchanged;
			}
			if (target[-2]==target[0] && target[-1]==target[1] && target[0]==target[2])
			{
				//two-byte RLE
				size_t rlelen=0;
				while (target+rlelen<targetend && target[0]==target[rlelen+0] && target[1]==target[rlelen+1]) rlelen+=2;
				writenum((rlelen-1)<<2 | TargetCopy);
				writenum((target-targetcopypos-2)<<1);
				source+=rlelen;
				target+=rlelen;
				targetcopypos=target-2;
			}
			else if (target[-1]==target[0] && target[0]==target[1])
			{
				//one-byte RLE
				size_t rlelen=0;
				while (target+rlelen<targetend && target[0]==target[rlelen]) rlelen++;
				writenum((rlelen-1)<<2 | TargetCopy);
				writenum((target-targetcopypos-1)<<1);
				source+=rlelen;
				target+=rlelen;
				targetcopypos=target-1;
			}
		}
	}
	
	write32(crc32(sourcemem.ptr, sourcemem.len));
	write32(crc32(targetmem.ptr, targetmem.len));
	write32(crc32(out, outlen));
	
	patchmem->ptr=out;
	patchmem->len=outlen;
	
	//while this may look like it can be fooled by a patch containing exactly one of any other command, it
	//  can't, because the ones that aren't SourceRead requires an argument
	size_t i;
	for (i=mainContentPos;(out[i]&0x80)==0x00;i++) {}
	if (i==outlen-12-1) return bps_identical;
	
	return bps_ok;
}

#undef write_nocrc
#undef write
#undef writenum

void bps_free(struct mem mem)
{
	free(mem.ptr);
}
#undef error



struct bpsinfo bps_get_info(file* patch, bool changefrac)
{
#define error(why) do { ret.error=why; return ret; } while(0)
	struct bpsinfo ret;
	size_t len = patch->len();
	if (len<4+3+12) error(bps_broken);
	
	uint8_t top[256];
	if (!patch->read(top, 0, len>256 ? 256 : len)) error(bps_io);
	if (memcmp(top, "BPS1", 4)) error(bps_broken);
	
	const uint8_t* patchdat=top+4;
	if (!decodenum(patchdat, ret.size_in)) error(bps_too_big);
	if (!decodenum(patchdat, ret.size_out)) error(bps_too_big);
	
	if (!decodenum(patchdat, ret.meta_size)) error(bps_too_big);
	ret.meta_start = patchdat - top;
	
	uint8_t checksums[12];
	if (!patch->read(checksums, len-12, 12)) error(bps_io);
	ret.crc_in  = read32(checksums+0);
	ret.crc_out = read32(checksums+4);
	ret.crc_patch=read32(checksums+8);
	
	if (changefrac && ret.size_in>0)
	{
		//algorithm: each command adds its length to the numerator, unless it's above 32, in which case
		// it adds 32; or if it's SourceRead, in which case it adds 0
		//denominator is just input length
		uint8_t* patchbin=(uint8_t*)malloc(len);
		patch->read(patchbin, 0, len);
		size_t outpos=0; // position in the output file
		size_t changeamt=0; // change score
		const uint8_t* patchat=patchbin+(patchdat-top);
		
		size_t metasize;
		if (!decodenum(patchat, metasize)) error(bps_too_big);
		patchat+=metasize;
		
		const uint8_t* patchend=patchbin+len-12;
		
		while (patchat<patchend && outpos<ret.size_in)
		{
			size_t thisinstr;
			decodenum(patchat, thisinstr);
			size_t length=(thisinstr>>2)+1;
			int action=(thisinstr&3);
			int min_len_32 = (length<32 ? length : 32);
			
			switch (action)
			{
				case SourceRead:
				{
					changeamt+=0;
				}
				break;
				case TargetRead:
				{
					changeamt+=min_len_32;
					patchat+=length;
				}
				break;
				case SourceCopy:
				case TargetCopy:
				{
					changeamt+=min_len_32;
					size_t ignore;
					decodenum(patchat, ignore);
				}
				break;
			}
			outpos+=length;
		}
		if (patchat>patchend || outpos>ret.size_out) error(bps_broken);
		ret.change_num = (changeamt<ret.size_in ? changeamt : ret.size_in);
		ret.change_denom = ret.size_in;
		
		free(patchbin);
	}
	else
	{
		//this also happens if change fraction is not requested, but it's undefined behaviour anyways.
		ret.change_num=1;
		ret.change_denom=1;
	}
	
	ret.error=bps_ok;
	return ret;
}



//Module name: libbps-suf
//Author: Alcaro
//Date: See Git history
//Licence: GPL v3.0 or higher


#include "libbps.h"
#include "crc32.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

//These two give minor performance penalties and will print some random stuff to stdout.
//The former will verify the correctness of the output patch, the latter will print some performance data.
//Can be useful for debugging, but should be disabled for release builds.
#ifdef BPS_STANDALONE
#endif
//#define TEST_CORRECT
//#define TEST_PERF

//If the suffix array of [0, 0, 0, 0] is [3, 2, 1, 0], set to true. If it's [0, 1, 2, 3], this is false.
//If it's [4, 3, 2, 1, 0] or [0, 1, 2, 3, 4], remove the 4 (easily done with some pointer math), and follow the above.
//If it's something else, get a non-broken array calculator.
#define EOF_IS_LAST false

#if defined(TEST_CORRECT) || defined(TEST_PERF)
#include <stdio.h>
#endif

//Algorithm description:
//
//This is heavily built upon suffix sorting; the implementation I use, libdivsufsort, claims
// O(n log n) complexity, so I'll believe that. There is also SA-IS, which claims O(n), but if that
// is true, its constant factors are ridiculously high.
//
//The program starts by taking an equal amount of the source file and target file, concatenates that
// with target first, and suffix sorts it.
//It also calculates a reverse index, such that reverse[sorted[i]]==i.
//
//To find a match, it goes to reverse[outpos], and scans sorted[] up and down for the closest entry
// that either starts before the current output position, or is somewhere in the source file.
//As the source file comes last, the end-of-file marker (whose value is outside the range of a byte)
// is guaranteed to not be in the way for a better match.
//This is called O(n) times, and averages O(1) as at least 50% of sorted[] is in range. However, it
// is worst-case O(n) for sorted inputs, giving a total of O(n^2).
//
//It then checks which of the two candidates are superior, by checking how far they match each
// other, and then checking if the upper one has another correct byte.
//This is potentially O(n), but for each matched byte, another iteration is removed from the outer
// loop, so the sum of all calls is O(n).
//
//When the program approaches the end of the sorted area, it re-sorts twice as much as last time.
// This gives O(log n) calls to the suffix sorter.
//Given O(n log n) for one sorting step, the time taken is O(n/1 log n/1 + n/2 log n/2 +
// n/4 log n/4 + ...), which is strictly less than O(n/1 log n + n/2 log n + n/4 log n + ...), which
// equals O(2n log n), which is O(n log n). (The exact value of that infinite sum is 2n*log(n/2).)
//
//Many details were omitted from the above, but that's the basic setup.
//
//Thus, the program is O(max(n log n, n, n) = n log n) average and O(max(n log n, n^2, n) = n^2)
// worst case.
//
//I conclude that the task of finding, understanding and implementing a sub-O(n^2) algorithm for
// delta patching is resolved.


//Known cases where this function does not emit the optimal encoding:
//If a match in the target file would extend further than target_search_size, it is often skipped.
// Penalty: O(log n), with extremely low constants (it'd require a >256B match to be exactly there).
// Even for big files, the penalty is very likely to remain zero; even hitting double-digit bytes
// would require a file designed exactly for that.
//If multiple matches are equally good, it picks one at random, not the one that's cheaper to encode.
// Penalty: Likely O(n) or O(n log log n), with low constants. I'd guess ~1.4% for my 48MB test file.
//However, due to better heuristics and others' performance optimizations, this one still beats its
// competitors.


//Possible optimizations:
//divsufsort() takes approximately 2/3 of the total time. create_reverse_index() takes roughly a third of the remainder.
//Each iteration takes four times as long as the previous one.
//If each iteration takes 4 times as long as the previous one, then the last one takes 3/4 of the total time.
//Since divsufsort+create_reverse_index doesn't depend on anything else, the last iteration can be split off to its own thread.
//This would split it to
//Search, non-final:   2/9 * 1/4 = 2/36
//Search, final:       2/9 * 3/4 = 6/36
//Sort+rev, non-final: 7/9 * 1/4 = 7/36
//Sort+rev, final:     7/9 * 3/4 = 21/36
//All non-final must be done sequentially. Both Sort Final and non-final must be done before Search Final can start.
//This means the final time, if Sort Final is split off, is
//max(7/36+2/36, 21/36) + 6/36 = 27/36 = 3/4
//of the original time.
//Due to
//- the considerable complexity costs (OpenMP doesn't seem able to represent the "insert a wait in
//   the middle of this while loop" I would need)
//- the added memory use, approximately 25% higher - it's already high enough
//- libdivsufsort already using threads, which would make the gains lower
//   and would increase complexity, as I have to ensure the big one remains threaded -
//    and that the small ones are not, as that'd starve the big one
//I deem a possible 25% boost not worthwhile.


//Both sorting algorithms claim O(1) memory use (in addition to the bytes and the output). In
// addition to that, this algorithm uses (source.len*target.len)*(sizeof(uint8_t)+2*sizeof(off_t))
// bytes of memory, plus the patch (the input/output files are read from disk).
//For most hardware, this is 9*(source.len+target.len), or 5*(source+target) for the slim one.


//I don't need 64bit support, it'd take 20GB RAM and way too long.
//#include "sais.cpp"
//template<typename sais_index_type>
//static void sufsort(sais_index_type* SA, const uint8_t* T, sais_index_type n) {
//  if(n <= 1) { if(n == 1) SA[0] = 0; return; }
//  sais_main<sais_index_type>(T, SA, 0, n, 256);
//}

//According to <https://code.google.com/p/libdivsufsort/wiki/SACA_Benchmarks>, divsufsort achieves
// approximately half the time of SAIS for nearly all files, despite SAIS' promises of linear
// performance (divsufsort claims O(n log n)).

//divsufsort only allocates O(1) for some radix/bucket sorting. SAIS seems constant too.
//I'd prefer to let them allocate from an array I give it, but divsuf doesn't allow that, and there
// are only half a dozen allocations per call anyways.

#include "divsufsort.h"
static void sufsort(int32_t* SA, uint8_t* T, int32_t n)
{
	divsufsort(T, SA, n);
}

#ifdef USE_DIVSUFSORT64
#include "divsufsort64.h"

static void sufsort(int64_t* SA, uint8_t* T, int64_t n)
{
	divsufsort(T, SA, n);
}
#endif



template<typename T> static T min(T a, T b) { return a<b ? a : b; }
template<typename T> static T max(T a, T b) { return a<b ? b : a; }



namespace {
struct bps_creator {
	uint8_t* out;
	size_t outlen;
	size_t outbuflen;
	
	void reserve(size_t len)
	{
		if (outlen+len > outbuflen)
		{
			if (!outbuflen) outbuflen = 128;
			while (outlen+len > outbuflen) outbuflen *= 2;
			out = (uint8_t*)realloc(out, outbuflen);
		}
	}
	
	void append(const uint8_t * data, size_t len)
	{
		reserve(len);
		memcpy(out+outlen, data, len);
		outlen+=len;
	}
	
	void appendnum(size_t num)
	{
#ifdef TEST_CORRECT
		if (num > 1000000000)
			printf("ERROR: Attempt to write %.8lX\n",(unsigned long)num),abort();
#endif
		reserve(sizeof(size_t)*8/7+1);
		
		while (num >= 128)
		{
			out[outlen++]=(num&0x7F);
			num>>=7;
			num--;
		}
		out[outlen++]=num|0x80;
	}
	
	void appendnum32(uint32_t num)
	{
		reserve(4);
		out[outlen++] = num>>0;
		out[outlen++] = num>>8;
		out[outlen++] = num>>16;
		out[outlen++] = num>>24;
	}
	
	static size_t maxsize()
	{
		return SIZE_MAX>>2; // can be reduced to SIZE_MAX>>1 by amending append_cmd, but the mallocs overflow at that point anyways.
	}
	
	size_t sourcelen;
	size_t targetlen;
	const uint8_t* targetmem;
	
	enum bpscmd { SourceRead, TargetRead, SourceCopy, TargetCopy };
	
	size_t outpos;
	
	size_t sourcecopypos;
	size_t targetcopypos;
	
	size_t numtargetread;
	
	bps_creator(file* source, file* target, struct mem metadata)
	{
		outlen = 0;
		outbuflen = 128;
		out = (uint8_t*)malloc(outbuflen);
		
		outpos = 0;
		
		sourcelen = source->len();
		targetlen = target->len();
		
		sourcecopypos = 0;
		targetcopypos = 0;
		
		numtargetread = 0;
		
		append((const uint8_t*)"BPS1", 4);
		appendnum(sourcelen);
		appendnum(targetlen);
		appendnum(metadata.len);
		append(metadata.ptr, metadata.len);
		
		setProgress(NULL, NULL);
	}
	
	
	void move_target(const uint8_t* ptr)
	{
		targetmem = ptr;
	}
	
	size_t encode_delta(size_t prev, size_t next)
	{
		bool negative = (next<prev);
		size_t offset = negative ? prev-next : next-prev;
		return (negative?1:0) | (offset<<1);
	}
	
	void append_delta(size_t prev, size_t next)
	{
		appendnum(encode_delta(prev, next));
	}
	
	void append_cmd(bpscmd command, size_t count)
	{
		appendnum((count-1)<<2 | command);
	}
	
	void flush_target_read()
	{
		if (!numtargetread) return;
		append_cmd(TargetRead, numtargetread);
		append(targetmem+outpos-numtargetread, numtargetread);
		numtargetread = 0;
	}
	
	size_t emit_source_copy(size_t location, size_t count)
	{
		if (location == outpos) return emit_source_read(location, count);
		flush_target_read();
		append_cmd(SourceCopy, count);
		append_delta(sourcecopypos, location);
		sourcecopypos = location+count;
		outpos += count;
		return count;
	}
	
	size_t emit_source_read(size_t location, size_t count)
	{
		flush_target_read();
#ifdef TEST_CORRECT
		if (location != outpos)
			puts("ERROR: SourceRead not from source pointer"),abort();
#endif
		append_cmd(SourceRead, count);
		outpos+=count;
		return count;
	}
	
	size_t emit_target_copy(size_t location, size_t count)
	{
		flush_target_read();
		append_cmd(TargetCopy, count);
		append_delta(targetcopypos, location);
		targetcopypos = location+count;
		outpos += count;
		return count;
	}
	
	size_t emit_target_read()
	{
		numtargetread++;
		outpos++;
		return 1;
	}
	
	
	size_t abs_diff(size_t a, size_t b)
	{
		return (b<a) ? (a-b) : (b-a);
	}
	size_t num_cost(size_t num)
	{
		if (num<128) return 1;
		if (num<128*128) return 2; // 32KB
		if (num<128*128*128) return 3; // 2MB
		if (num<128*128*128*128) return 4; // 256MB
		return 5; // 128^5 is 32GB, let's just assume the sizes don't go any higher...
	}
	
	bool use_match(bool hastargetread, size_t cost, size_t len)
	{
		//numbers calculated via trial and error; checking for each cost, optimizing 'len' for each, and checking what happens
		//then a pattern was identified and used
		//yes, it looks weird
		return len >= 1+cost+hastargetread+(len==1);
	}
	
	
	//Return value is how many bytes were used. If you believe the given one sucks, use TargetRead and return 1.
	size_t match(bool is_target, size_t pos, size_t len)
	{
		if (!use_match(
		     numtargetread,
		     (!is_target && pos==outpos) ? 1 : // SourceRead
		         (num_cost(abs_diff(pos, (is_target ? targetcopypos : sourcecopypos)))+1),
		     len
		   ))
		{
			return emit_target_read();
		}
		
		if (is_target) return emit_target_copy(pos, len);
		else return emit_source_copy(pos, len);
	}
	
	
	bool (*prog_func)(void* userdata, size_t done, size_t total);
	void* prog_dat;
	
	static bool prog_func_null(void* userdata, size_t done, size_t total) { return true; }
	
	void setProgress(bool (*progress)(void* userdata, size_t done, size_t total), void* userdata)
	{
		if (!progress) progress = prog_func_null;
		
		prog_func=progress;
		prog_dat=userdata;
	}
	
	bool progress(size_t done, size_t total)
	{
		return prog_func(prog_dat, done, total);
	}
	
	
	void finish(const uint8_t* source, const uint8_t* target)
	{
		flush_target_read();
#ifdef TEST_CORRECT
		if (outpos != targetlen)
			puts("ERROR: patch creates wrong ROM size"),abort();
#endif
		
		appendnum32(crc32(source, sourcelen));
		appendnum32(crc32(target, targetlen));
		appendnum32(crc32(out, outlen));
	}
	
	struct mem getpatch()
	{
		struct mem ret = { out, outlen };
		out = NULL;
		return ret;
	}
	
	~bps_creator() { free(out); }
};
}



#ifdef TEST_PERF
static int match_len_n=0;
static int match_len_tot=0;
#endif

template<typename off_t>
static off_t match_len(const uint8_t* a, const uint8_t* b, off_t len)
{
	off_t i;
	for (i=0;i<len && a[i]==b[i];i++) {}
#ifdef TEST_PERF
	match_len_n++;
	match_len_tot+=i;
#endif
	return i;
}

//This one assumes that the longest common prefix of 'a' and 'b' is shared also by 'search'.
//In practice, lexographically, a < search < b, which is a stronger guarantee.
template<typename off_t>
static off_t pick_best_of_two(const uint8_t* search, off_t searchlen,
                              const uint8_t* data, off_t datalen,
                              off_t a, off_t b,
                              off_t* bestlen)
{
	off_t commonlen = match_len(data+a, data+b, min(datalen-a, datalen-b));
	if (commonlen>=searchlen)
	{
		*bestlen=searchlen;
		return a;
	}
	
	if (a+commonlen<datalen && search[commonlen]==data[a+commonlen])
	{
		// a is better
		*bestlen = commonlen + match_len(search+commonlen, data+a+commonlen, min(searchlen, datalen-a)-commonlen);
		return a;
	}
	else
	{
		// b is better, or they're equal
		*bestlen = commonlen + match_len(search+commonlen, data+b+commonlen, min(searchlen, datalen-b)-commonlen);
		return b;
	}
}

//This one takes a match, which is assumed optimal, and looks for the lexographically closest one
// that either starts before 'maxstart', or starts at or after 'minstart'.
template<typename off_t>
static off_t adjust_match(off_t match, const uint8_t* search, off_t searchlen,
                          const uint8_t* data,off_t datalen, off_t maxstart,off_t minstart,
                          const off_t* sorted, off_t sortedlen,
                          off_t* bestlen)
{
	off_t match_up = match;
	off_t match_dn = match;
	while (match_up>=0        && sorted[match_up]>=maxstart && sorted[match_up]<minstart) match_up--;
	while (match_dn<sortedlen && sorted[match_dn]>=maxstart && sorted[match_dn]<minstart) match_dn++;
	if (match_up<0 || match_dn>=sortedlen)
	{
		if (match_up<0 && match_dn>=sortedlen)
		{
			*bestlen=0;
			return 0;
		}
		off_t pos = sorted[match_up<0 ? match_dn : match_up];
		*bestlen = match_len(search, data+pos, min(searchlen, datalen-pos));
		return pos;
	}
	
	return pick_best_of_two(search,searchlen, data,datalen, sorted[match_up],sorted[match_dn], bestlen);
}



static uint16_t read2_uc(const uint8_t* data)
{
	return data[0]<<8 | data[1];
}

template<typename off_t>
static uint16_t read2(const uint8_t* data, off_t len)
{
	if (len>=2) return read2_uc(data);
	else
	{
		uint16_t out = (EOF_IS_LAST ? 0xFFFF : 0x0000);
		if (len==1) out = (data[0]<<8) | (out&0x00FF);
		return out;
	}
}

template<typename off_t>
static void create_buckets(const uint8_t* data, off_t* index, off_t len, off_t* buckets)
{
	off_t low = 0;
	off_t high;
	
	for (int n=0;n<65536;n++)
	{
		//'low' remains from the previous iteration and is a known minimum
		high = low+(len/131072)+1; // optimal value: slightly above a third of the distance to the next one
		while (true)
		{
			if (high > len-1) break;
			
			off_t pos = index[high];
			uint16_t here = read2(data+pos, len-pos);
			
			if (here >= n) break;
			else
			{
				off_t diff = high-low;
				low = high;
				high = high+diff*2;
			}
		}
		if (high > len-1) high = len-1;
		
		
		while (low < high)
		{
			off_t mid = low + (high-low)/2;
			off_t midpos = index[mid];
			
			uint16_t here = read2(data+midpos, len-midpos);
			if (here < n) low = mid+1;
			else high = mid;
		}
		buckets[n] = low;
	}
	
	buckets[65536] = len;
	
#ifdef TEST_CORRECT
	if (buckets[0]!=0)
	{
		printf("e: buckets suck, [0]=%i\n", buckets[0]);
		abort();
	}
	for (int n=0;n<65536;n++)
	{
		off_t low = buckets[n];
		off_t high = buckets[n+1];
		for (off_t i=low;i<high;i++)
		{
			if (read2(data+index[i], len-index[i])!=n)
			{
				printf("e: buckets suck, %i != (%i)[%i]%i [%i-%i]", n, i,index[i],read2(data+index[i],len-index[i]),low,high);
				abort();
			}
		}
		//printf("%i:[%i]%i\n",n,low,read2(data+index[low],len-low));
	}
#endif
}

template<typename off_t>
static off_t find_index(off_t pos, const uint8_t* data, off_t datalen, const off_t* index, const off_t* reverse, off_t* buckets)
{
	if (reverse) return reverse[pos];
	
	uint16_t bucket = read2(data+pos, datalen-pos);
//printf("p=%i b=%i\n",pos,bucket);
	
	off_t low = buckets[bucket];
	off_t high = buckets[bucket+1]-1;
	
	off_t lowmatch = 2;
	off_t highmatch = 2;
	
//printf("b=%i r=%i(%i)-%i(%i)\n",bucket,low,read2(data+index[low],datalen-index[low]),high,read2(data+index[high],datalen-index[high]));
//fflush(stdout);
	while (true)
	{
		off_t mid = low + (high-low)/2;
		off_t midpos = index[mid];
		if (midpos == pos) return mid;
//printf("r=[%i]%i-%i \n",high-low,low,high,);
//fflush(stdout);
#ifdef TEST_CORRECT
		if (low >= high)
		{
			printf("E: [%i](%i): stuck at %i(%i)-%i(%i)\n", pos, read2_uc(data+pos),
				low, read2_uc(data+index[low]), high, read2_uc(data+index[high]));
			int n=0;
			while (index[n]!=pos) n++;
			printf("correct one is %i(%i)\n",n, read2_uc(data+index[n]));
			abort();
		}
#endif
		
		off_t matchlenstart = min(lowmatch, highmatch);
		
		off_t len = datalen - max(pos, midpos) - matchlenstart;
		
		const uint8_t* search = data+pos+matchlenstart;
		const uint8_t* here = data+midpos+matchlenstart;
		
		while (len>0 && *search==*here)
		{
			search++;
			here++;
			len--;
		}
		
		off_t matchlen = search-data-pos;
		
		bool less;
		if (len > 0) less = (*here<*search);
		else less = (here > search) ^ EOF_IS_LAST;
		
		if (less)
		{
			low = mid+1;
			lowmatch = matchlen;
		}
		else
		{
			high = mid-1;
			highmatch = matchlen;
		}
		
		if (low+256 > high)
		{
			off_t i=low;
			while (true)
			{
				if (index[i]==pos) return i;
				i++;
			}
		}
	}
}


template<typename off_t>
static void create_reverse_index(off_t* index, off_t* reverse, off_t len)
{
//testcase: linux 3.18.14 -> 4.0.4 .xz
//without: real23.544 user32.930
//with:    real22.636 user40.168
//'user' jumps up quite a lot, while 'real' only moves a short bit
//I'm not sure why the tradeoff is so bad (do the cachelines bounce THAT badly?), but I deem it not worth it.
//#pragma omp parallel for
	for (off_t i=0;i<len;i++) reverse[index[i]]=i;
}

template<typename off_t>
static off_t nextsize(off_t outpos, off_t sortedsize, off_t targetlen)
{
	while (outpos >= sortedsize-256 && sortedsize < targetlen)
		sortedsize = min(sortedsize*4+3, targetlen);
	return sortedsize;
}

template<typename off_t>
off_t lerp(off_t x, off_t y, float frac)
{
	return x + (y-x)*frac;
}

template<typename off_t>
static bpserror bps_create_suf_core(file* source, file* target, bool moremem, struct bps_creator * out)
{
#define error(which) do { err = which; goto error; } while(0)
	bpserror err;
	
	size_t realsourcelen = source->len();
	size_t realtargetlen = target->len();
	
	size_t overflowtest = realsourcelen + realtargetlen;
	
	//source+target length is bigger than size_t (how did that manage to get allocated?)
	if (overflowtest < realsourcelen) return bps_too_big;
	
	//source+target doesn't fit in unsigned off_t
	if ((size_t)(off_t)overflowtest != overflowtest) return bps_too_big;
	
	//source+target doesn't fit in signed off_t
	if ((off_t)overflowtest < 0) return bps_too_big;
	
	//the mallocs would overflow
	if (realsourcelen+realtargetlen >= SIZE_MAX/sizeof(off_t)) return bps_too_big;
	
	if (realsourcelen+realtargetlen >= out->maxsize()) return bps_too_big;
	
	
	off_t sourcelen = realsourcelen;
	off_t targetlen = realtargetlen;
	
	uint8_t* mem_joined = (uint8_t*)malloc(sizeof(uint8_t)*(realsourcelen+realtargetlen));
	
	off_t* sorted = (off_t*)malloc(sizeof(off_t)*(realsourcelen+realtargetlen));
	
	off_t* sorted_inverse = NULL;
	if (moremem) sorted_inverse = (off_t*)malloc(sizeof(off_t)*(realsourcelen+realtargetlen));
	
	off_t* buckets = NULL;
	if (!sorted_inverse) buckets = (off_t*)malloc(sizeof(off_t)*65537);
	
	if (!sorted || !mem_joined || (!sorted_inverse && !buckets))
	{
		free(mem_joined);
		free(sorted);
		free(sorted_inverse);
		free(buckets);
		return bps_out_of_mem;
	}
	
	//sortedsize is how much of the target file is sorted
	off_t sortedsize = targetlen;
	//divide by 4 for each iteration, to avoid sorting 50% of the file (the sorter is slow)
	while (sortedsize/4 > sourcelen && sortedsize > 1024) sortedsize >>= 2;
	
	off_t prevsortedsize = 0;
	off_t outpos = 0;
	
	goto reindex; // jump into the middle so I won't need a special case to enter it
	
	while (outpos < targetlen)
	{
		if (outpos >= sortedsize-256 && sortedsize < targetlen)
		{
			sortedsize = nextsize(outpos, sortedsize, targetlen);
			
		reindex:
			
			//this isn't an exact science
			const float percSort = sorted_inverse ? 0.67 : 0.50;
			const float percInv = sorted_inverse ? 0.11 : 0.10;
			//const float percFind = sorted_inverse ? 0.22 : 0.40; // unused
			
			const size_t progPreSort = lerp(prevsortedsize, sortedsize, 0);
			const size_t progPreInv = lerp(prevsortedsize, sortedsize, percSort);
			const size_t progPreFind = lerp(prevsortedsize, sortedsize, percSort+percInv);
			
			prevsortedsize = sortedsize;
			
			if (!out->progress(progPreSort, targetlen)) error(bps_canceled);
			
			if (!target->read(mem_joined, 0, sortedsize)) error(bps_io);
			if (!source->read(mem_joined+sortedsize, 0, sourcelen)) error(bps_io);
			out->move_target(mem_joined);
			sufsort(sorted, mem_joined, sortedsize+sourcelen);
			
			if (!out->progress(progPreInv, targetlen)) error(bps_canceled);
			
			if (sorted_inverse)
				create_reverse_index(sorted, sorted_inverse, sortedsize+sourcelen);
			else
				create_buckets(mem_joined, sorted, sortedsize+sourcelen, buckets);
			
			if (!out->progress(progPreFind, targetlen)) error(bps_canceled);
		}
		
		off_t matchlen = 0;
		off_t matchpos = adjust_match(find_index(outpos, mem_joined, sortedsize+sourcelen, sorted, sorted_inverse, buckets),
		                              mem_joined+outpos, sortedsize-outpos,
		                              mem_joined,sortedsize+sourcelen, outpos,sortedsize,
		                              sorted, sortedsize+sourcelen,
		                              &matchlen);
		
#ifdef TEST_CORRECT
		if (matchlen && matchpos >= outpos && matchpos < sortedsize) puts("ERROR: found match in invalid location"),abort();
		if (memcmp(mem_joined+matchpos, mem_joined+outpos, matchlen)) puts("ERROR: found match doesn't match"),abort();
#endif
		
		off_t taken;
		if (matchpos >= sortedsize) taken = out->match(false, matchpos-sortedsize, matchlen);
		else taken = out->match(true, matchpos, matchlen);
#ifdef TEST_CORRECT
		if (taken < 0) puts("ERROR: match() returned negative"),abort();
		if (matchlen >= 7 && taken < matchlen) printf("ERROR: match() took %i bytes, offered %i\n", taken, matchlen),abort();
#endif
		outpos += taken;
	}
	
	out->finish(mem_joined+sortedsize, mem_joined);
	
	err = bps_ok;
	
error:
	free(buckets);
	free(sorted_inverse);
	free(sorted);
	free(mem_joined);
	
	return err;
}


//template<typename T> static bpserror bps_create_suf_pick(file* source, file* target, bool moremem, struct bps_creator * bps);
//template<> bpserror bps_create_suf_pick<uint32_t>(file* source, file* target, bool moremem, struct bps_creator * bps)
//{
//	return bps_create_suf_core<int32_t>(source, target, moremem, bps);
//}
//template<> bpserror bps_create_suf_pick<uint64_t>(file* source, file* target, bool moremem, struct bps_creator * bps)
//{
//	bpserror err = bps_create_suf_core<int32_t>(source, target, moremem, bps);
//	if (err==bps_too_big) err = bps_create_suf_core<int64_t>(source, target, moremem, bps);
//	return err;
//}

//This one picks a function based on 32-bit integers if that fits. This halves memory use for common inputs.
//It also handles some stuff related to the BPS headers and footers.
bpserror bps_create_delta(file* source, file* target, struct mem metadata, struct mem * patchmem,
                          bool (*progress)(void* userdata, size_t done, size_t total), void* userdata, bool moremem)
{
	bps_creator bps(source, target, metadata);
	bps.setProgress(progress, userdata);
	
	size_t maindata = bps.outlen;
	
	//off_t must be signed
	bpserror err = bps_create_suf_core<int32_t>(source, target, moremem, &bps);
	if (err!=bps_ok) return err;
	
	*patchmem = bps.getpatch();
	
	while ((patchmem->ptr[maindata]&0x80) == 0x00) maindata++;
	if (maindata==patchmem->len-12-1) return bps_identical;
	return bps_ok;
}

enum bpserror bps_create_delta_inmem(struct mem source, struct mem target, struct mem metadata, struct mem * patch,
                               bool (*progress)(void* userdata, size_t done, size_t total), void* userdata,
                               bool moremem)
{
	class memfile : public file {
	public:
		const uint8_t * m_ptr;
		size_t m_len;
		
		size_t len() { return m_len; }
		bool read(uint8_t* target, size_t start, size_t len) { memcpy(target, m_ptr+start, len); return true; }
		
		memfile(const uint8_t * ptr, size_t len) : m_ptr(ptr), m_len(len) {}
	};
	
	memfile sourcef(source.ptr, source.len);
	memfile targetf(target.ptr, target.len);
	
	return bps_create_delta(&sourcef, &targetf, metadata, patch, progress, userdata, moremem);
}




#ifdef BPS_STANDALONE
#include <stdio.h>
static struct mem ReadWholeFile(const char * filename)
{
	struct mem null = {NULL, 0};
	
	FILE * file=fopen(filename, "rb");
	if (!file) return null;
	fseek(file, 0, SEEK_END);
	size_t len=ftell(file);
	fseek(file, 0, SEEK_SET);
	unsigned char * data=(unsigned char*)malloc(len);
	size_t truelen=fread(data, 1,len, file);
	fclose(file);
	if (len!=truelen)
	{
		free(data);
		return null;
	}
	
	struct mem ret = { (unsigned char*)data, len };
	return ret;
}
static bool WriteWholeFile(const char * filename, struct mem data)
{
	FILE * file=fopen(filename, "wb");
	if (!file) return false;
	unsigned int truelen=fwrite(data.ptr, 1,data.len, file);
	fclose(file);
	return (truelen==data.len);
}
int main(int argc, char * argv[])
{
//struct mem out = ReadWholeFile(argv[2]);
//printf("check=%.8X\n",crc32(out.ptr, out.len));

struct mem in = ReadWholeFile(argv[1]);
struct mem out = ReadWholeFile(argv[2]);
struct mem null = {NULL, 0};
struct mem p={NULL,0};
//int n=50;
//for(int i=0;i<n;i++)
//printf("%i/%i\n",i,n),
bps_create_delta(in,out,null,&p, NULL,NULL);
printf("len=%lu        \n",p.len);
printf("check=%.8X\n",*(uint32_t*)(p.ptr+p.len-4));
WriteWholeFile(argv[3], p);
free(in.ptr);
free(out.ptr);
free(p.ptr);

#ifdef TEST_PERF
printf("%i/%i=%f\n",match_len_tot,match_len_n,(float)match_len_tot/match_len_n);
#endif
}
#endif
//Module name: libips
//Author: Alcaro
//Date: July 11, 2013
//Licence: GPL v3.0 or higher

#ifndef __cplusplus
#include <stdbool.h> //bool; if this does not exist, remove it and uncomment the following three lines.
//#define bool int
//#define true 1
//#define false 0
#endif
#include <stdlib.h> //malloc, realloc, free
#include <string.h> //memcpy, memset

#include "libips.h"

typedef unsigned char byte;

#define min(a, b) ((a)<(b) ? (a) : (b))
#define max(a, b) ((a)>(b) ? (a) : (b))
#define clamp(a, b, c) max(a, min(b, c))

struct ipsstudy {
	enum ipserror error;
	unsigned int outlen_min;
	unsigned int outlen_max;
	unsigned int outlen_min_mem;
};

enum ipserror ips_study(struct mem patch, struct ipsstudy * study)
{
	study->error = ips_invalid;
	if (patch.len < 8) return ips_invalid;
	const unsigned char * patchat = patch.ptr;
	const unsigned char * patchend = patchat + patch.len;
#define read8() ((patchat < patchend) ? (*patchat++) : 0)
#define read16() ((patchat+1 < patchend) ? (patchat += 2,(                      (patchat[-2] << 8) | patchat[-1])) : 0)
#define read24() ((patchat+2 < patchend) ? (patchat += 3,((patchat[-3] << 16) | (patchat[-2] << 8) | patchat[-1])) : 0)
	if (read8() != 'P' ||
	    read8() != 'A' ||
	    read8() != 'T' ||
	    read8() != 'C' ||
	    read8() != 'H')
	{
		return ips_invalid;
	}
	
	unsigned int offset = read24();
	unsigned int outlen = 0;
	unsigned int thisout = 0;
	//unsigned int lastoffset = 0;
	bool w_scrambled = false;
	while (offset != 0x454F46) // 454F46=EOF
	{
		unsigned int size = read16();
		if (size == 0)
		{
			size = read16();
			if (!size) return ips_invalid; // don't know what this means (65536 bytes? something else?),
			thisout = offset + size;       //   better reject it until I find out
			read8();
		}
		else
		{
			thisout = offset + size;
			patchat += size;
		}
		//turns out this messes up manually created patches. https://github.com/Alcaro/Flips/issues/13
		//if (offset < lastoffset) w_scrambled = true;
		//lastoffset = offset;
		if (thisout > outlen) outlen = thisout;
		if (patchat >= patchend) return ips_invalid;
		offset = read24();
	}
	study->outlen_min_mem = outlen;
	study->outlen_max = 0xFFFFFFFF;
	if (patchat+3 == patchend)
	{
		unsigned int truncate = read24();
		study->outlen_max = truncate;
		if (outlen > truncate)
		{
			outlen = truncate;
			w_scrambled = true;
		}
	}
	if (patchat != patchend) return ips_invalid;
	study->outlen_min = outlen;
#undef read8
#undef read16
#undef read24
	study->error = ips_ok;
	if (w_scrambled) study->error = ips_scrambled;
	return study->error;
}

enum ipserror ips_apply_study(struct mem patch, struct ipsstudy * study, struct mem in, struct mem * out)
{
	out->ptr = NULL;
	out->len = 0;
	if (study->error == ips_invalid) return study->error;
#define read8() (*patchat++)//guaranteed to not overflow at this point, we already checked the patch
#define read16() (patchat += 2,(                      (patchat[-2] << 8) | patchat[-1]))
#define read24() (patchat += 3,((patchat[-3] << 16) | (patchat[-2] << 8) | patchat[-1]))
	unsigned int outlen = clamp(study->outlen_min, in.len, study->outlen_max);
	out->ptr = (byte*)malloc(max(outlen, study->outlen_min_mem));
	out->len = outlen;
	
	bool anychanges = false;
	if (outlen != in.len) anychanges = true;
	
	if (out->len > in.len)
	{
		memcpy(out->ptr, in.ptr, in.len);
		memset(out->ptr + in.len, 0, out->len - in.len);
	}
	else memcpy(out->ptr, in.ptr, outlen);
	
	const unsigned char * patchat = patch.ptr+5;
	unsigned int offset = read24();
	while (offset != 0x454F46)
	{
		unsigned int size = read16();
		if (size == 0)
		{
			size = read16();
			//if (!size) return ips_invalid; // rejected in ips_study
			unsigned char b = read8();
			
			if (!anychanges && size && (out->ptr[offset] != b || memcmp(out->ptr + offset, out->ptr + offset, size - 1)))
				anychanges = true;
			
			memset(out->ptr + offset, b, size);
		}
		else
		{
			if (!anychanges && memcmp(out->ptr + offset, patchat, size))
				anychanges = true;
			
			memcpy(out->ptr + offset, patchat, size);
			patchat += size;
		}
		offset = read24();
	}
#undef read8
#undef read16
#undef read24
	
	//truncate data without this being needed is a poor idea
	if (study->outlen_max != 0xFFFFFFFF && in.len <= study->outlen_max)
		study->error = ips_notthis;
	
	if (!anychanges)
		study->error = ips_thisout;
	return study->error;
}

enum ipserror ips_apply(struct mem patch, struct mem in, struct mem * out)
{
	struct ipsstudy study;
	ips_study(patch, &study);
	return ips_apply_study(patch, &study, in, out);
}

//Known situations where this function does not generate an optimal patch:
//In:  80 80 80 80 80 80 80 80 80 80 80 80 80 80 80 80 80 80 80 80 80 80 80 80
//Out: FF FF FF FF FF FF FF FF 00 01 02 03 04 05 06 07 FF FF FF FF FF FF FF FF
//IPS: [         RLE         ] [        Copy         ] [         RLE         ]
//Possible improvement: RLE across the entire file, copy on top of that.
//Rationale: It would be a huge pain to create such a multi-pass tool if it should support writing a byte
//  more than twice, and I don't like half-assing stuff. It's also unlikely to apply to anything.


//Known improvements over LIPS:
//In:  00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F
//Out: FF 01 02 03 04 05 FF FF FF FF FF FF FF FF FF FF
//LIPS:[      Copy     ] [            RLE            ]
//Mine:[] [ Unchanged  ] [            RLE            ]
//Rationale: While LIPS can break early if it finds something RLEable in the middle of a block, it's not
//  smart enough to back off if there's something unchanged between the changed area and the RLEable spot.

//In:  FF FF FF FF FF FF FF
//Out: 00 00 00 00 01 02 03
//LIPS:[   RLE   ] [ Copy ]
//Mine:[       Copy       ]
//Rationale: Mistuned heuristics in LIPS.

//It is also known that I win in some other situations. I didn't bother checking which, though.

//There are no known cases where LIPS wins over libips.

enum ipserror ips_create(struct mem sourcemem, struct mem targetmem, struct mem * patchmem)
{
	unsigned int sourcelen = sourcemem.len;
	unsigned int targetlen = targetmem.len;
	const unsigned char * source = sourcemem.ptr;
	const unsigned char * target = targetmem.ptr;
	
	patchmem->ptr = NULL;
	patchmem->len = 0;
	
	if (targetlen > 16777216) return ips_16MB;
	if (targetlen >= 16777216 && sourcelen > targetlen) return ips_16MB; // can't truncate to exactly 16MB
	
	unsigned int offset = 0;
	unsigned int outbuflen = 4096;
	unsigned char * out = (byte*)malloc(outbuflen);
	unsigned int outlen = 0;
#define write8(val) do { \
		out[outlen++] = (val); \
		if (outlen == outbuflen) { outbuflen *= 2; out = (byte*)realloc(out, outbuflen); } \
		} while(0)
#define write16(val) do {                      write8((val) >> 8); write8((val)); } while(0)
#define write24(val) do { write8((val) >> 16); write8((val) >> 8); write8((val)); } while(0)
	write8('P');
	write8('A');
	write8('T');
	write8('C');
	write8('H');
	int lastknownchange = 0;
	//int forcewrite = (targetlen > sourcelen ? 1 : 0);
	while (offset < targetlen)
	{
		while (offset < sourcelen && (offset < sourcelen ? source[offset] : 0) == target[offset])
			offset++;
		//check how much we need to edit until it starts getting similar
		int thislen = 0;
		int consecutiveunchanged = 0;
		thislen = lastknownchange - offset;
		if (thislen < 0) thislen = 0;
		while (true)
		{
			unsigned int thisbyte = offset + thislen + consecutiveunchanged;
			if (thisbyte < sourcelen && (thisbyte < sourcelen ? source[thisbyte] : 0) == target[thisbyte])
			{
				consecutiveunchanged++;
			}
			else
			{
				thislen += consecutiveunchanged + 1;
				consecutiveunchanged = 0;
			}
			if (consecutiveunchanged >= 6 || thislen >= 65536) break;
		}
		
		//avoid premature EOF
		if (offset == 0x454F46)
		{
			offset--;
			thislen++;
		}
		
		lastknownchange = offset + thislen;
		if (thislen > 65535) thislen = 65535;
		if (offset + thislen > targetlen) thislen = targetlen - offset;
		if (offset == targetlen) continue;
		
		//check if RLE here is worthwhile
		int byteshere;
		for (byteshere=0; byteshere<thislen && target[offset] == target[offset+byteshere]; byteshere++) {}
		if (byteshere == thislen)
		{
			int thisbyte = target[offset];
			int i = 0;
			while (true)
			{
				unsigned int pos = offset + byteshere + i - 1;
				if (pos >= targetlen || target[pos] != thisbyte || byteshere + i > 65535)
					break;
				if (pos >= sourcelen || (pos < sourcelen ? source[pos] : 0) != thisbyte)
				{
					byteshere += i;
					thislen += i;
					i = 0;
				}
				i++;
			}
		}
		if ((byteshere > 8-5 && byteshere == thislen) || byteshere > 8)
		{
			write24(offset);
			write16(0);
			write16(byteshere);
			write8(target[offset]);
			offset += byteshere;
		}
		else
		{
			//check if we'd gain anything from ending the block early and switching to RLE
			int byteshere = 0;
			int stopat = 0;
			while (stopat + byteshere < thislen)
			{
				if (target[offset + stopat] == target[offset + stopat + byteshere])
				{
					byteshere++;
				}
				else
				{
					stopat += byteshere;
					byteshere = 0;
				}
				if (byteshere > 8+5 || //rle-worthy despite two ips headers
				   (byteshere > 8 && stopat + byteshere == thislen) || //rle-worthy at end of data
				   (byteshere > 8 && !memcmp(&target[offset +stopat + byteshere], //rle-worthy before another rle-worthy
				                             &target[offset +stopat + byteshere + 1],
				                             9-1)))
				{
					if (stopat) thislen = stopat;
					break; //we don't scan the entire block if we know we'll want to RLE, that'd gain nothing.
				}
			}
			//don't write unchanged bytes at the end of a block if we want to RLE the next couple of bytes
			if (offset + thislen != targetlen)
			{
				while (offset + thislen - 1 < sourcelen &&
				       target[offset + thislen - 1] == (offset + thislen - 1 < sourcelen ? source[offset + thislen - 1] : 0))
				{
					thislen--;
				}
			}
			
			if (thislen > 3 && !memcmp(&target[offset], &target[offset + 1], thislen - 1)) //still worth it?
			{
				write24(offset);
				write16(0);
				write16(thislen);
				write8(target[offset]);
			}
			else
			{
				write24(offset);
				write16(thislen);
				int i;
				for (i=0; i<thislen; i++)
					write8(target[offset + i]);
			}
			offset += thislen;
		}
	}
	write8('E');
	write8('O');
	write8('F');
	if (sourcelen > targetlen) write24(targetlen);
#undef write
	patchmem->ptr = out;
	patchmem->len = outlen;
	if (outlen == 8)
		return ips_identical;
	return ips_ok;
}

void ips_free(struct mem mem)
{
	free(mem.ptr);
}

#if 0
#warning Disable this in release versions.
#include <stdio.h>

//Congratulations, you found the undocumented feature! I don't think it's useful for anything except debugging libips, though.
void ips_dump(struct mem patch)
{
	if (patch.len < 8)
	{
		puts("Invalid");
		return;
	}
	const unsigned char * patchat = patch.ptr;
	const unsigned char * patchend = patchat + patch.len;
#define read8() ((patchat < patchend) ? (*patchat++) : 0)
#define read16() ((patchat+1 < patchend) ? (patchat += 2, (                      (patchat[-2] << 8) | patchat[-1])) : 0)
#define read24() ((patchat+2 < patchend) ? (patchat += 3, ((patchat[-3] << 16) | (patchat[-2] << 8) | patchat[-1])) : 0)
	if (read8() != 'P' ||
			read8() != 'A' ||
			read8() != 'T' ||
			read8() != 'C' ||
			read8() != 'H')
	{
		puts("Invalid");
		return;
	}
	int blockstart = patchat - patch.ptr;
	int offset = read24();
	int outlen = 0;
	int thisout = 0;
	while (offset != 0x454F46)
	{
		int size = read16();
		if (size == 0)
		{
			int rlelen = read16();
			thisout = offset + rlelen;
			printf("[%X] %X: %i (RLE)\n", blockstart, offset, rlelen);
			read8();
		}
		else
		{
			thisout = offset + size;
			printf("[%X] %X: %i\n", blockstart, offset, size);
			patchat += size;
		}
		if (thisout > outlen) outlen = thisout;
		if (patchat >= patchend)
		{
			puts("Invalid");
			return;
		}
		blockstart = patchat - patch.ptr;
		offset = read24();
	}
	printf("Expand to 0x%X\n", outlen);
	if (patchat + 3 == patchend)
	{
		int truncate = read24();
		printf("Truncate to 0x%X\n", truncate);
	}
	if (patchat != patchend) puts("Invalid");
#undef read8
#undef read16
#undef read24
}
#endif
//Module name: libups
//Author: Alcaro
//Date: April 4, 2013
//Licence: GPL v3.0 or higher

#include "libups.h"

#ifndef __cplusplus
#include <stdbool.h>//bool; if this file does not exist (hi msvc), remove it and uncomment the following three lines.
//#define bool int
//#define true 1
//#define false 0
#endif
#include <stdint.h>//uint8_t, uint32_t
#include <stdlib.h>//malloc, realloc, free
#include <string.h>//memcpy, memset
#include "crc32.h"


#define error(which) do { error=which; goto exit; } while(0)
#define assert_sum(a,b) do { if (SIZE_MAX-(a)<(b)) error(ups_too_big); } while(0)
#define assert_shift(a,b) do { if (SIZE_MAX>>(b)<(a)) error(ups_too_big); } while(0)
enum upserror ups_apply(struct mem patch, struct mem in, struct mem * out)
{
	enum upserror error;
	out->len=0;
	out->ptr=NULL;
	if (patch.len<4+2+12) return ups_broken;
	
	if (true)
	{
#define readpatch8() (*(patchat++))
#define readin8() (*(inat++))
#define writeout8(byte) (*(outat++)=byte)
		
#define decodeto(var) \
				do { \
					var=0; \
					unsigned int shift=0; \
					while (true) \
					{ \
						uint8_t next=readpatch8(); \
						assert_shift(next&0x7F, shift); \
						size_t addthis=(next&0x7F)<<shift; \
						assert_sum(var, addthis); \
						var+=addthis; \
						if (next&0x80) break; \
						shift+=7; \
						assert_sum(var, 1U<<shift); \
						var+=1<<shift; \
					} \
				} while(false)
		
		bool backwards=false;
		
		uint8_t * patchat=patch.ptr;
		uint8_t * patchend=patch.ptr+patch.len-12;
		
		if (readpatch8()!='U') error(ups_broken);
		if (readpatch8()!='P') error(ups_broken);
		if (readpatch8()!='S') error(ups_broken);
		if (readpatch8()!='1') error(ups_broken);
		
		size_t inlen;
		size_t outlen;
		decodeto(inlen);
		decodeto(outlen);
		if (inlen!=in.len)
		{
			size_t tmp=inlen;
			inlen=outlen;
			outlen=tmp;
			backwards=true;
		}
		if (inlen!=in.len) error(ups_not_this);
		
		out->len=outlen;
		out->ptr=(uint8_t*)malloc(outlen);
		memset(out->ptr, 0, outlen);
		
		//uint8_t * instart=in.ptr;
		uint8_t * inat=in.ptr;
		uint8_t * inend=in.ptr+in.len;
		
		//uint8_t * outstart=out->ptr;
		uint8_t * outat=out->ptr;
		uint8_t * outend=out->ptr+out->len;
		
		while (patchat<patchend)
		{
			size_t skip;
			decodeto(skip);
			while (skip>0)
			{
				uint8_t out;
				if (inat>=inend) out=0;
				else out=readin8();
				if (outat<outend) writeout8(out);
				skip--;
			}
			uint8_t tmp;
			do
			{
				tmp=readpatch8();
				uint8_t out;
				if (inat>=inend) out=0;
				else out=readin8();
				if (outat<outend) writeout8(out^tmp);
			}
			while (tmp);
		}
		if (patchat!=patchend) error(ups_broken);
		while (outat<outend && inat<inend)
		{
			writeout8(readin8());
		}
		while (outat<outend) writeout8(0);
		while (inat<inend) (void)readin8();
		
		uint32_t crc_in_expected=read32(patchat);
		uint32_t crc_out_expected=read32(patchat+4);
		uint32_t crc_patch_expected=read32(patchat+8);
		
		uint32_t crc_in=crc32(in.ptr, in.len);
		uint32_t crc_out=crc32(out->ptr, out->len);
		uint32_t crc_patch=crc32(patch.ptr, patch.len-4);
		
		if (inlen==outlen)
		{
			if ((crc_in!=crc_in_expected || crc_out!=crc_out_expected) && (crc_in!=crc_out_expected || crc_out!=crc_in_expected)) error(ups_not_this);
		}
		else
		{
			if (!backwards)
			{
				if (crc_in!=crc_in_expected) error(ups_not_this);
				if (crc_out!=crc_out_expected) error(ups_not_this);
			}
			else
			{
				if (crc_in!=crc_out_expected) error(ups_not_this);
				if (crc_out!=crc_in_expected) error(ups_not_this);
			}
		}
		if (crc_patch!=crc_patch_expected) error(ups_broken);
		return ups_ok;
#undef read8
#undef decodeto
#undef write8
	}
	
exit:
	free(out->ptr);
	out->len=0;
	out->ptr=NULL;
	return error;
}

enum upserror ups_create(struct mem sourcemem, struct mem targetmem, struct mem * patchmem)
{
	patchmem->ptr=NULL;
	patchmem->len=0;
	return ups_broken;//unimplemented, just pick a random error
}

void ups_free(struct mem mem)
{
	free(mem.ptr);
}

#if 0
//Sorry, no undocumented features here. The only thing that can change an UPS patch is swapping the two sizes and checksums, and I don't create anyways.
#endif
/*
 * divsufsort.c for libdivsufsort
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

#ifdef _OPENMP
# include <omp.h>
#endif


/*- Private Functions -*/

/* Sorts suffixes of type B*. */
static
saidx_t
sort_typeBstar(const sauchar_t *T, saidx_t *SA,
               saidx_t *bucket_A, saidx_t *bucket_B,
               saidx_t n) {
  saidx_t *PAb, *ISAb, *buf;
#ifdef _OPENMP
  saidx_t *curbuf;
  saidx_t l;
#endif
  saidx_t i, j, k, t, m, bufsize;
  saint_t c0, c1;
#ifdef _OPENMP
  saint_t d0, d1;
  int tmp;
#endif

  /* Initialize bucket arrays. */
  for(i = 0; i < BUCKET_A_SIZE; ++i) { bucket_A[i] = 0; }
  for(i = 0; i < BUCKET_B_SIZE; ++i) { bucket_B[i] = 0; }

  /* Count the number of occurrences of the first one or two characters of each
     type A, B and B* suffix. Moreover, store the beginning position of all
     type B* suffixes into the array SA. */
  for(i = n - 1, m = n, c0 = T[n - 1]; 0 <= i;) {
    /* type A suffix. */
    do { ++BUCKET_A(c1 = c0); } while((0 <= --i) && ((c0 = T[i]) >= c1));
    if(0 <= i) {
      /* type B* suffix. */
      ++BUCKET_BSTAR(c0, c1);
      SA[--m] = i;
      /* type B suffix. */
      for(--i, c1 = c0; (0 <= i) && ((c0 = T[i]) <= c1); --i, c1 = c0) {
        ++BUCKET_B(c0, c1);
      }
    }
  }
  m = n - m;
/*
note:
  A type B* suffix is lexicographically smaller than a type B suffix that
  begins with the same first two characters.
*/

  /* Calculate the index of start/end point of each bucket. */
  for(c0 = 0, i = 0, j = 0; c0 < ALPHABET_SIZE; ++c0) {
    t = i + BUCKET_A(c0);
    BUCKET_A(c0) = i + j; /* start point */
    i = t + BUCKET_B(c0, c0);
    for(c1 = c0 + 1; c1 < ALPHABET_SIZE; ++c1) {
      j += BUCKET_BSTAR(c0, c1);
      BUCKET_BSTAR(c0, c1) = j; /* end point */
      i += BUCKET_B(c0, c1);
    }
  }

  if(0 < m) {
    /* Sort the type B* suffixes by their first two characters. */
    PAb = SA + n - m; ISAb = SA + m;
    for(i = m - 2; 0 <= i; --i) {
      t = PAb[i], c0 = T[t], c1 = T[t + 1];
      SA[--BUCKET_BSTAR(c0, c1)] = i;
    }
    t = PAb[m - 1], c0 = T[t], c1 = T[t + 1];
    SA[--BUCKET_BSTAR(c0, c1)] = m - 1;

    /* Sort the type B* substrings using sssort. */
#ifdef _OPENMP
    tmp = omp_get_max_threads();
    buf = SA + m, bufsize = (n - (2 * m)) / tmp;
    c0 = ALPHABET_SIZE - 2, c1 = ALPHABET_SIZE - 1, j = m;
#pragma omp parallel default(shared) private(curbuf, k, l, d0, d1, tmp)
    {
      tmp = omp_get_thread_num();
      curbuf = buf + tmp * bufsize;
      k = 0;
      for(;;) {
        #pragma omp critical(sssort_lock)
        {
          if(0 < (l = j)) {
            d0 = c0, d1 = c1;
            do {
              k = BUCKET_BSTAR(d0, d1);
              if(--d1 <= d0) {
                d1 = ALPHABET_SIZE - 1;
                if(--d0 < 0) { break; }
              }
            } while(((l - k) <= 1) && (0 < (l = k)));
            c0 = d0, c1 = d1, j = k;
          }
        }
        if(l == 0) { break; }
        sssort(T, PAb, SA + k, SA + l,
               curbuf, bufsize, 2, n, *(SA + k) == (m - 1));
      }
    }
#else
    buf = SA + m, bufsize = n - (2 * m);
    for(c0 = ALPHABET_SIZE - 2, j = m; 0 < j; --c0) {
      for(c1 = ALPHABET_SIZE - 1; c0 < c1; j = i, --c1) {
        i = BUCKET_BSTAR(c0, c1);
        if(1 < (j - i)) {
          sssort(T, PAb, SA + i, SA + j,
                 buf, bufsize, 2, n, *(SA + i) == (m - 1));
        }
      }
    }
#endif

    /* Compute ranks of type B* substrings. */
    for(i = m - 1; 0 <= i; --i) {
      if(0 <= SA[i]) {
        j = i;
        do { ISAb[SA[i]] = i; } while((0 <= --i) && (0 <= SA[i]));
        SA[i + 1] = i - j;
        if(i <= 0) { break; }
      }
      j = i;
      do { ISAb[SA[i] = ~SA[i]] = j; } while(SA[--i] < 0);
      ISAb[SA[i]] = j;
    }

    /* Construct the inverse suffix array of type B* suffixes using trsort. */
    trsort(ISAb, SA, m, 1);

    /* Set the sorted order of tyoe B* suffixes. */
    for(i = n - 1, j = m, c0 = T[n - 1]; 0 <= i;) {
      for(--i, c1 = c0; (0 <= i) && ((c0 = T[i]) >= c1); --i, c1 = c0) { }
      if(0 <= i) {
        t = i;
        for(--i, c1 = c0; (0 <= i) && ((c0 = T[i]) <= c1); --i, c1 = c0) { }
        SA[ISAb[--j]] = ((t == 0) || (1 < (t - i))) ? t : ~t;
      }
    }

    /* Calculate the index of start/end point of each bucket. */
    BUCKET_B(ALPHABET_SIZE - 1, ALPHABET_SIZE - 1) = n; /* end point */
    for(c0 = ALPHABET_SIZE - 2, k = m - 1; 0 <= c0; --c0) {
      i = BUCKET_A(c0 + 1) - 1;
      for(c1 = ALPHABET_SIZE - 1; c0 < c1; --c1) {
        t = i - BUCKET_B(c0, c1);
        BUCKET_B(c0, c1) = i; /* end point */

        /* Move all type B* suffixes to the correct position. */
        for(i = t, j = BUCKET_BSTAR(c0, c1);
            j <= k;
            --i, --k) { SA[i] = SA[k]; }
      }
      BUCKET_BSTAR(c0, c0 + 1) = i - BUCKET_B(c0, c0) + 1; /* start point */
      BUCKET_B(c0, c0) = i; /* end point */
    }
  }

  return m;
}

/* Constructs the suffix array by using the sorted order of type B* suffixes. */
static
void
construct_SA(const sauchar_t *T, saidx_t *SA,
             saidx_t *bucket_A, saidx_t *bucket_B,
             saidx_t n, saidx_t m) {
  saidx_t *i, *j, *k;
  saidx_t s;
  saint_t c0, c1, c2;

  if(0 < m) {
    /* Construct the sorted order of type B suffixes by using
       the sorted order of type B* suffixes. */
    for(c1 = ALPHABET_SIZE - 2; 0 <= c1; --c1) {
      /* Scan the suffix array from right to left. */
      for(i = SA + BUCKET_BSTAR(c1, c1 + 1),
          j = SA + BUCKET_A(c1 + 1) - 1, k = NULL, c2 = -1;
          i <= j;
          --j) {
        if(0 < (s = *j)) {
          assert(T[s] == c1);
          assert(((s + 1) < n) && (T[s] <= T[s + 1]));
          assert(T[s - 1] <= T[s]);
          *j = ~s;
          c0 = T[--s];
          if((0 < s) && (T[s - 1] > c0)) { s = ~s; }
          if(c0 != c2) {
            if(0 <= c2) { BUCKET_B(c2, c1) = k - SA; }
            k = SA + BUCKET_B(c2 = c0, c1);
          }
          assert(k < j);
          *k-- = s;
        } else {
          assert(((s == 0) && (T[s] == c1)) || (s < 0));
          *j = ~s;
        }
      }
    }
  }

  /* Construct the suffix array by using
     the sorted order of type B suffixes. */
  k = SA + BUCKET_A(c2 = T[n - 1]);
  *k++ = (T[n - 2] < c2) ? ~(n - 1) : (n - 1);
  /* Scan the suffix array from left to right. */
  for(i = SA, j = SA + n; i < j; ++i) {
    if(0 < (s = *i)) {
      assert(T[s - 1] >= T[s]);
      c0 = T[--s];
      if((s == 0) || (T[s - 1] < c0)) { s = ~s; }
      if(c0 != c2) {
        BUCKET_A(c2) = k - SA;
        k = SA + BUCKET_A(c2 = c0);
      }
      assert(i < k);
      *k++ = s;
    } else {
      assert(s < 0);
      *i = ~s;
    }
  }
}

/* Constructs the burrows-wheeler transformed string directly
   by using the sorted order of type B* suffixes. */
static
saidx_t
construct_BWT(const sauchar_t *T, saidx_t *SA,
              saidx_t *bucket_A, saidx_t *bucket_B,
              saidx_t n, saidx_t m) {
  saidx_t *i, *j, *k, *orig;
  saidx_t s;
  saint_t c0, c1, c2;

  if(0 < m) {
    /* Construct the sorted order of type B suffixes by using
       the sorted order of type B* suffixes. */
    for(c1 = ALPHABET_SIZE - 2; 0 <= c1; --c1) {
      /* Scan the suffix array from right to left. */
      for(i = SA + BUCKET_BSTAR(c1, c1 + 1),
          j = SA + BUCKET_A(c1 + 1) - 1, k = NULL, c2 = -1;
          i <= j;
          --j) {
        if(0 < (s = *j)) {
          assert(T[s] == c1);
          assert(((s + 1) < n) && (T[s] <= T[s + 1]));
          assert(T[s - 1] <= T[s]);
          c0 = T[--s];
          *j = ~((saidx_t)c0);
          if((0 < s) && (T[s - 1] > c0)) { s = ~s; }
          if(c0 != c2) {
            if(0 <= c2) { BUCKET_B(c2, c1) = k - SA; }
            k = SA + BUCKET_B(c2 = c0, c1);
          }
          assert(k < j);
          *k-- = s;
        } else if(s != 0) {
          *j = ~s;
#ifndef NDEBUG
        } else {
          assert(T[s] == c1);
#endif
        }
      }
    }
  }

  /* Construct the BWTed string by using
     the sorted order of type B suffixes. */
  k = SA + BUCKET_A(c2 = T[n - 1]);
  *k++ = (T[n - 2] < c2) ? ~((saidx_t)T[n - 2]) : (n - 1);
  /* Scan the suffix array from left to right. */
  for(i = SA, j = SA + n, orig = SA; i < j; ++i) {
    if(0 < (s = *i)) {
      assert(T[s - 1] >= T[s]);
      c0 = T[--s];
      *i = c0;
      if((0 < s) && (T[s - 1] < c0)) { s = ~((saidx_t)T[s - 1]); }
      if(c0 != c2) {
        BUCKET_A(c2) = k - SA;
        k = SA + BUCKET_A(c2 = c0);
      }
      assert(i < k);
      *k++ = s;
    } else if(s != 0) {
      *i = ~s;
    } else {
      orig = i;
    }
  }

  return orig - SA;
}


/*---------------------------------------------------------------------------*/

/*- Function -*/

saint_t
divsufsort(const sauchar_t *T, saidx_t *SA, saidx_t n) {
  saidx_t *bucket_A, *bucket_B;
  saidx_t m;
  saint_t err = 0;

  /* Check arguments. */
  if((T == NULL) || (SA == NULL) || (n < 0)) { return -1; }
  else if(n == 0) { return 0; }
  else if(n == 1) { SA[0] = 0; return 0; }
  else if(n == 2) { m = (T[0] < T[1]); SA[m ^ 1] = 0, SA[m] = 1; return 0; }

  bucket_A = (saidx_t *)malloc(BUCKET_A_SIZE * sizeof(saidx_t));
  bucket_B = (saidx_t *)malloc(BUCKET_B_SIZE * sizeof(saidx_t));

  /* Suffixsort. */
  if((bucket_A != NULL) && (bucket_B != NULL)) {
    m = sort_typeBstar(T, SA, bucket_A, bucket_B, n);
    construct_SA(T, SA, bucket_A, bucket_B, n, m);
  } else {
    err = -2;
  }

  free(bucket_B);
  free(bucket_A);

  return err;
}

saidx_t
divbwt(const sauchar_t *T, sauchar_t *U, saidx_t *A, saidx_t n) {
  saidx_t *B;
  saidx_t *bucket_A, *bucket_B;
  saidx_t m, pidx, i;

  /* Check arguments. */
  if((T == NULL) || (U == NULL) || (n < 0)) { return -1; }
  else if(n <= 1) { if(n == 1) { U[0] = T[0]; } return n; }

  if((B = A) == NULL) { B = (saidx_t *)malloc((size_t)(n + 1) * sizeof(saidx_t)); }
  bucket_A = (saidx_t *)malloc(BUCKET_A_SIZE * sizeof(saidx_t));
  bucket_B = (saidx_t *)malloc(BUCKET_B_SIZE * sizeof(saidx_t));

  /* Burrows-Wheeler Transform. */
  if((B != NULL) && (bucket_A != NULL) && (bucket_B != NULL)) {
    m = sort_typeBstar(T, B, bucket_A, bucket_B, n);
    pidx = construct_BWT(T, B, bucket_A, bucket_B, n, m);

    /* Copy to output string. */
    U[0] = T[n - 1];
    for(i = 0; i < pidx; ++i) { U[i + 1] = (sauchar_t)B[i]; }
    for(i += 1; i < n; ++i) { U[i] = (sauchar_t)B[i]; }
    pidx += 1;
  } else {
    pidx = -2;
  }

  free(bucket_B);
  free(bucket_A);
  if(A == NULL) { free(B); }

  return pidx;
}

const char *
divsufsort_version(void) {
  return PROJECT_VERSION_FULL;
}
/*
 * sssort.c for libdivsufsort
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




/*- Private Functions -*/

static const saint_t lg_table[256]= {
 -1,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
  5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
  6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
  6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
};

#if (SS_BLOCKSIZE == 0) || (SS_INSERTIONSORT_THRESHOLD < SS_BLOCKSIZE)

static INLINE
saint_t
ss_ilg(saidx_t n) {
#if SS_BLOCKSIZE == 0
# if defined(BUILD_DIVSUFSORT64)
  return (n >> 32) ?
          ((n >> 48) ?
            ((n >> 56) ?
              56 + lg_table[(n >> 56) & 0xff] :
              48 + lg_table[(n >> 48) & 0xff]) :
            ((n >> 40) ?
              40 + lg_table[(n >> 40) & 0xff] :
              32 + lg_table[(n >> 32) & 0xff])) :
          ((n & 0xffff0000) ?
            ((n & 0xff000000) ?
              24 + lg_table[(n >> 24) & 0xff] :
              16 + lg_table[(n >> 16) & 0xff]) :
            ((n & 0x0000ff00) ?
               8 + lg_table[(n >>  8) & 0xff] :
               0 + lg_table[(n >>  0) & 0xff]));
# else
  return (n & 0xffff0000) ?
          ((n & 0xff000000) ?
            24 + lg_table[(n >> 24) & 0xff] :
            16 + lg_table[(n >> 16) & 0xff]) :
          ((n & 0x0000ff00) ?
             8 + lg_table[(n >>  8) & 0xff] :
             0 + lg_table[(n >>  0) & 0xff]);
# endif
#elif SS_BLOCKSIZE < 256
  return lg_table[n];
#else
  return (n & 0xff00) ?
          8 + lg_table[(n >> 8) & 0xff] :
          0 + lg_table[(n >> 0) & 0xff];
#endif
}

#endif /* (SS_BLOCKSIZE == 0) || (SS_INSERTIONSORT_THRESHOLD < SS_BLOCKSIZE) */

#if SS_BLOCKSIZE != 0

static const saint_t sqq_table[256] = {
  0,  16,  22,  27,  32,  35,  39,  42,  45,  48,  50,  53,  55,  57,  59,  61,
 64,  65,  67,  69,  71,  73,  75,  76,  78,  80,  81,  83,  84,  86,  87,  89,
 90,  91,  93,  94,  96,  97,  98,  99, 101, 102, 103, 104, 106, 107, 108, 109,
110, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126,
128, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142,
143, 144, 144, 145, 146, 147, 148, 149, 150, 150, 151, 152, 153, 154, 155, 155,
156, 157, 158, 159, 160, 160, 161, 162, 163, 163, 164, 165, 166, 167, 167, 168,
169, 170, 170, 171, 172, 173, 173, 174, 175, 176, 176, 177, 178, 178, 179, 180,
181, 181, 182, 183, 183, 184, 185, 185, 186, 187, 187, 188, 189, 189, 190, 191,
192, 192, 193, 193, 194, 195, 195, 196, 197, 197, 198, 199, 199, 200, 201, 201,
202, 203, 203, 204, 204, 205, 206, 206, 207, 208, 208, 209, 209, 210, 211, 211,
212, 212, 213, 214, 214, 215, 215, 216, 217, 217, 218, 218, 219, 219, 220, 221,
221, 222, 222, 223, 224, 224, 225, 225, 226, 226, 227, 227, 228, 229, 229, 230,
230, 231, 231, 232, 232, 233, 234, 234, 235, 235, 236, 236, 237, 237, 238, 238,
239, 240, 240, 241, 241, 242, 242, 243, 243, 244, 244, 245, 245, 246, 246, 247,
247, 248, 248, 249, 249, 250, 250, 251, 251, 252, 252, 253, 253, 254, 254, 255
};

static INLINE
saidx_t
ss_isqrt(saidx_t x) {
  saidx_t y, e;

  if(x >= (SS_BLOCKSIZE * SS_BLOCKSIZE)) { return SS_BLOCKSIZE; }
  e = (x & 0xffff0000) ?
        ((x & 0xff000000) ?
          24 + lg_table[(x >> 24) & 0xff] :
          16 + lg_table[(x >> 16) & 0xff]) :
        ((x & 0x0000ff00) ?
           8 + lg_table[(x >>  8) & 0xff] :
           0 + lg_table[(x >>  0) & 0xff]);

  if(e >= 16) {
    y = sqq_table[x >> ((e - 6) - (e & 1))] << ((e >> 1) - 7);
    if(e >= 24) { y = (y + 1 + x / y) >> 1; }
    y = (y + 1 + x / y) >> 1;
  } else if(e >= 8) {
    y = (sqq_table[x >> ((e - 6) - (e & 1))] >> (7 - (e >> 1))) + 1;
  } else {
    return sqq_table[x] >> 4;
  }

  return (x < (y * y)) ? y - 1 : y;
}

#endif /* SS_BLOCKSIZE != 0 */


/*---------------------------------------------------------------------------*/

/* Compares two suffixes. */
static INLINE
saint_t
ss_compare(const sauchar_t *T,
           const saidx_t *p1, const saidx_t *p2,
           saidx_t depth) {
  const sauchar_t *U1, *U2, *U1n, *U2n;

  for(U1 = T + depth + *p1,
      U2 = T + depth + *p2,
      U1n = T + *(p1 + 1) + 2,
      U2n = T + *(p2 + 1) + 2;
      (U1 < U1n) && (U2 < U2n) && (*U1 == *U2);
      ++U1, ++U2) {
  }

  return U1 < U1n ?
        (U2 < U2n ? *U1 - *U2 : 1) :
        (U2 < U2n ? -1 : 0);
}


/*---------------------------------------------------------------------------*/

#if (SS_BLOCKSIZE != 1) && (SS_INSERTIONSORT_THRESHOLD != 1)

/* Insertionsort for small size groups */
static
void
ss_insertionsort(const sauchar_t *T, const saidx_t *PA,
                 saidx_t *first, saidx_t *last, saidx_t depth) {
  saidx_t *i, *j;
  saidx_t t;
  saint_t r;

  for(i = last - 2; first <= i; --i) {
    for(t = *i, j = i + 1; 0 < (r = ss_compare(T, PA + t, PA + *j, depth));) {
      do { *(j - 1) = *j; } while((++j < last) && (*j < 0));
      if(last <= j) { break; }
    }
    if(r == 0) { *j = ~*j; }
    *(j - 1) = t;
  }
}

#endif /* (SS_BLOCKSIZE != 1) && (SS_INSERTIONSORT_THRESHOLD != 1) */


/*---------------------------------------------------------------------------*/

#if (SS_BLOCKSIZE == 0) || (SS_INSERTIONSORT_THRESHOLD < SS_BLOCKSIZE)

static INLINE
void
ss_fixdown(const sauchar_t *Td, const saidx_t *PA,
           saidx_t *SA, saidx_t i, saidx_t size) {
  saidx_t j, k;
  saidx_t v;
  saint_t c, d, e;

  for(v = SA[i], c = Td[PA[v]]; (j = 2 * i + 1) < size; SA[i] = SA[k], i = k) {
    d = Td[PA[SA[k = j++]]];
    if(d < (e = Td[PA[SA[j]]])) { k = j; d = e; }
    if(d <= c) { break; }
  }
  SA[i] = v;
}

/* Simple top-down heapsort. */
static
void
ss_heapsort(const sauchar_t *Td, const saidx_t *PA, saidx_t *SA, saidx_t size) {
  saidx_t i, m;
  saidx_t t;

  m = size;
  if((size % 2) == 0) {
    m--;
    if(Td[PA[SA[m / 2]]] < Td[PA[SA[m]]]) { SWAP(SA[m], SA[m / 2]); }
  }

  for(i = m / 2 - 1; 0 <= i; --i) { ss_fixdown(Td, PA, SA, i, m); }
  if((size % 2) == 0) { SWAP(SA[0], SA[m]); ss_fixdown(Td, PA, SA, 0, m); }
  for(i = m - 1; 0 < i; --i) {
    t = SA[0], SA[0] = SA[i];
    ss_fixdown(Td, PA, SA, 0, i);
    SA[i] = t;
  }
}


/*---------------------------------------------------------------------------*/

/* Returns the median of three elements. */
static INLINE
saidx_t *
ss_median3(const sauchar_t *Td, const saidx_t *PA,
           saidx_t *v1, saidx_t *v2, saidx_t *v3) {
  saidx_t *t;
  if(Td[PA[*v1]] > Td[PA[*v2]]) { SWAP(v1, v2); }
  if(Td[PA[*v2]] > Td[PA[*v3]]) {
    if(Td[PA[*v1]] > Td[PA[*v3]]) { return v1; }
    else { return v3; }
  }
  return v2;
}

/* Returns the median of five elements. */
static INLINE
saidx_t *
ss_median5(const sauchar_t *Td, const saidx_t *PA,
           saidx_t *v1, saidx_t *v2, saidx_t *v3, saidx_t *v4, saidx_t *v5) {
  saidx_t *t;
  if(Td[PA[*v2]] > Td[PA[*v3]]) { SWAP(v2, v3); }
  if(Td[PA[*v4]] > Td[PA[*v5]]) { SWAP(v4, v5); }
  if(Td[PA[*v2]] > Td[PA[*v4]]) { SWAP(v2, v4); SWAP(v3, v5); }
  if(Td[PA[*v1]] > Td[PA[*v3]]) { SWAP(v1, v3); }
  if(Td[PA[*v1]] > Td[PA[*v4]]) { SWAP(v1, v4); SWAP(v3, v5); }
  if(Td[PA[*v3]] > Td[PA[*v4]]) { return v4; }
  return v3;
}

/* Returns the pivot element. */
static INLINE
saidx_t *
ss_pivot(const sauchar_t *Td, const saidx_t *PA, saidx_t *first, saidx_t *last) {
  saidx_t *middle;
  saidx_t t;

  t = last - first;
  middle = first + t / 2;

  if(t <= 512) {
    if(t <= 32) {
      return ss_median3(Td, PA, first, middle, last - 1);
    } else {
      t >>= 2;
      return ss_median5(Td, PA, first, first + t, middle, last - 1 - t, last - 1);
    }
  }
  t >>= 3;
  first  = ss_median3(Td, PA, first, first + t, first + (t << 1));
  middle = ss_median3(Td, PA, middle - t, middle, middle + t);
  last   = ss_median3(Td, PA, last - 1 - (t << 1), last - 1 - t, last - 1);
  return ss_median3(Td, PA, first, middle, last);
}


/*---------------------------------------------------------------------------*/

/* Binary partition for substrings. */
static INLINE
saidx_t *
ss_partition(const saidx_t *PA,
                    saidx_t *first, saidx_t *last, saidx_t depth) {
  saidx_t *a, *b;
  saidx_t t;
  for(a = first - 1, b = last;;) {
    for(; (++a < b) && ((PA[*a] + depth) >= (PA[*a + 1] + 1));) { *a = ~*a; }
    for(; (a < --b) && ((PA[*b] + depth) <  (PA[*b + 1] + 1));) { }
    if(b <= a) { break; }
    t = ~*b;
    *b = *a;
    *a = t;
  }
  if(first < a) { *first = ~*first; }
  return a;
}

/* Multikey introsort for medium size groups. */
static
void
ss_mintrosort(const sauchar_t *T, const saidx_t *PA,
              saidx_t *first, saidx_t *last,
              saidx_t depth) {
#define STACK_SIZE SS_MISORT_STACKSIZE
  struct { saidx_t *a, *b, c; saint_t d; } stack[STACK_SIZE];
  const sauchar_t *Td;
  saidx_t *a, *b, *c, *d, *e, *f;
  saidx_t s, t;
  saint_t ssize;
  saint_t limit;
  saint_t v, x = 0;

  for(ssize = 0, limit = ss_ilg(last - first);;) {

    if((last - first) <= SS_INSERTIONSORT_THRESHOLD) {
#if 1 < SS_INSERTIONSORT_THRESHOLD
      if(1 < (last - first)) { ss_insertionsort(T, PA, first, last, depth); }
#endif
      STACK_POP(first, last, depth, limit);
      continue;
    }

    Td = T + depth;
    if(limit-- == 0) { ss_heapsort(Td, PA, first, last - first); }
    if(limit < 0) {
      for(a = first + 1, v = Td[PA[*first]]; a < last; ++a) {
        if((x = Td[PA[*a]]) != v) {
          if(1 < (a - first)) { break; }
          v = x;
          first = a;
        }
      }
      if(Td[PA[*first] - 1] < v) {
        first = ss_partition(PA, first, a, depth);
      }
      if((a - first) <= (last - a)) {
        if(1 < (a - first)) {
          STACK_PUSH(a, last, depth, -1);
          last = a, depth += 1, limit = ss_ilg(a - first);
        } else {
          first = a, limit = -1;
        }
      } else {
        if(1 < (last - a)) {
          STACK_PUSH(first, a, depth + 1, ss_ilg(a - first));
          first = a, limit = -1;
        } else {
          last = a, depth += 1, limit = ss_ilg(a - first);
        }
      }
      continue;
    }

    /* choose pivot */
    a = ss_pivot(Td, PA, first, last);
    v = Td[PA[*a]];
    SWAP(*first, *a);

    /* partition */
    for(b = first; (++b < last) && ((x = Td[PA[*b]]) == v);) { }
    if(((a = b) < last) && (x < v)) {
      for(; (++b < last) && ((x = Td[PA[*b]]) <= v);) {
        if(x == v) { SWAP(*b, *a); ++a; }
      }
    }
    for(c = last; (b < --c) && ((x = Td[PA[*c]]) == v);) { }
    if((b < (d = c)) && (x > v)) {
      for(; (b < --c) && ((x = Td[PA[*c]]) >= v);) {
        if(x == v) { SWAP(*c, *d); --d; }
      }
    }
    for(; b < c;) {
      SWAP(*b, *c);
      for(; (++b < c) && ((x = Td[PA[*b]]) <= v);) {
        if(x == v) { SWAP(*b, *a); ++a; }
      }
      for(; (b < --c) && ((x = Td[PA[*c]]) >= v);) {
        if(x == v) { SWAP(*c, *d); --d; }
      }
    }

    if(a <= d) {
      c = b - 1;

      if((s = a - first) > (t = b - a)) { s = t; }
      for(e = first, f = b - s; 0 < s; --s, ++e, ++f) { SWAP(*e, *f); }
      if((s = d - c) > (t = last - d - 1)) { s = t; }
      for(e = b, f = last - s; 0 < s; --s, ++e, ++f) { SWAP(*e, *f); }

      a = first + (b - a), c = last - (d - c);
      b = (v <= Td[PA[*a] - 1]) ? a : ss_partition(PA, a, c, depth);

      if((a - first) <= (last - c)) {
        if((last - c) <= (c - b)) {
          STACK_PUSH(b, c, depth + 1, ss_ilg(c - b));
          STACK_PUSH(c, last, depth, limit);
          last = a;
        } else if((a - first) <= (c - b)) {
          STACK_PUSH(c, last, depth, limit);
          STACK_PUSH(b, c, depth + 1, ss_ilg(c - b));
          last = a;
        } else {
          STACK_PUSH(c, last, depth, limit);
          STACK_PUSH(first, a, depth, limit);
          first = b, last = c, depth += 1, limit = ss_ilg(c - b);
        }
      } else {
        if((a - first) <= (c - b)) {
          STACK_PUSH(b, c, depth + 1, ss_ilg(c - b));
          STACK_PUSH(first, a, depth, limit);
          first = c;
        } else if((last - c) <= (c - b)) {
          STACK_PUSH(first, a, depth, limit);
          STACK_PUSH(b, c, depth + 1, ss_ilg(c - b));
          first = c;
        } else {
          STACK_PUSH(first, a, depth, limit);
          STACK_PUSH(c, last, depth, limit);
          first = b, last = c, depth += 1, limit = ss_ilg(c - b);
        }
      }
    } else {
      limit += 1;
      if(Td[PA[*first] - 1] < v) {
        first = ss_partition(PA, first, last, depth);
        limit = ss_ilg(last - first);
      }
      depth += 1;
    }
  }
#undef STACK_SIZE
}

#endif /* (SS_BLOCKSIZE == 0) || (SS_INSERTIONSORT_THRESHOLD < SS_BLOCKSIZE) */


/*---------------------------------------------------------------------------*/

#if SS_BLOCKSIZE != 0

static INLINE
void
ss_blockswap(saidx_t *a, saidx_t *b, saidx_t n) {
  saidx_t t;
  for(; 0 < n; --n, ++a, ++b) {
    t = *a, *a = *b, *b = t;
  }
}

static INLINE
void
ss_rotate(saidx_t *first, saidx_t *middle, saidx_t *last) {
  saidx_t *a, *b, t;
  saidx_t l, r;
  l = middle - first, r = last - middle;
  for(; (0 < l) && (0 < r);) {
    if(l == r) { ss_blockswap(first, middle, l); break; }
    if(l < r) {
      a = last - 1, b = middle - 1;
      t = *a;
      do {
        *a-- = *b, *b-- = *a;
        if(b < first) {
          *a = t;
          last = a;
          if((r -= l + 1) <= l) { break; }
          a -= 1, b = middle - 1;
          t = *a;
        }
      } while(1);
    } else {
      a = first, b = middle;
      t = *a;
      do {
        *a++ = *b, *b++ = *a;
        if(last <= b) {
          *a = t;
          first = a + 1;
          if((l -= r + 1) <= r) { break; }
          a += 1, b = middle;
          t = *a;
        }
      } while(1);
    }
  }
}


/*---------------------------------------------------------------------------*/

static
void
ss_inplacemerge(const sauchar_t *T, const saidx_t *PA,
                saidx_t *first, saidx_t *middle, saidx_t *last,
                saidx_t depth) {
  const saidx_t *p;
  saidx_t *a, *b;
  saidx_t len, half;
  saint_t q, r;
  saint_t x;

  for(;;) {
    if(*(last - 1) < 0) { x = 1; p = PA + ~*(last - 1); }
    else                { x = 0; p = PA +  *(last - 1); }
    for(a = first, len = middle - first, half = len >> 1, r = -1;
        0 < len;
        len = half, half >>= 1) {
      b = a + half;
      q = ss_compare(T, PA + ((0 <= *b) ? *b : ~*b), p, depth);
      if(q < 0) {
        a = b + 1;
        half -= (len & 1) ^ 1;
      } else {
        r = q;
      }
    }
    if(a < middle) {
      if(r == 0) { *a = ~*a; }
      ss_rotate(a, middle, last);
      last -= middle - a;
      middle = a;
      if(first == middle) { break; }
    }
    --last;
    if(x != 0) { while(*--last < 0) { } }
    if(middle == last) { break; }
  }
}


/*---------------------------------------------------------------------------*/

/* Merge-forward with internal buffer. */
static
void
ss_mergeforward(const sauchar_t *T, const saidx_t *PA,
                saidx_t *first, saidx_t *middle, saidx_t *last,
                saidx_t *buf, saidx_t depth) {
  saidx_t *a, *b, *c, *bufend;
  saidx_t t;
  saint_t r;

  bufend = buf + (middle - first) - 1;
  ss_blockswap(buf, first, middle - first);

  for(t = *(a = first), b = buf, c = middle;;) {
    r = ss_compare(T, PA + *b, PA + *c, depth);
    if(r < 0) {
      do {
        *a++ = *b;
        if(bufend <= b) { *bufend = t; return; }
        *b++ = *a;
      } while(*b < 0);
    } else if(r > 0) {
      do {
        *a++ = *c, *c++ = *a;
        if(last <= c) {
          while(b < bufend) { *a++ = *b, *b++ = *a; }
          *a = *b, *b = t;
          return;
        }
      } while(*c < 0);
    } else {
      *c = ~*c;
      do {
        *a++ = *b;
        if(bufend <= b) { *bufend = t; return; }
        *b++ = *a;
      } while(*b < 0);

      do {
        *a++ = *c, *c++ = *a;
        if(last <= c) {
          while(b < bufend) { *a++ = *b, *b++ = *a; }
          *a = *b, *b = t;
          return;
        }
      } while(*c < 0);
    }
  }
}

/* Merge-backward with internal buffer. */
static
void
ss_mergebackward(const sauchar_t *T, const saidx_t *PA,
                 saidx_t *first, saidx_t *middle, saidx_t *last,
                 saidx_t *buf, saidx_t depth) {
  const saidx_t *p1, *p2;
  saidx_t *a, *b, *c, *bufend;
  saidx_t t;
  saint_t r;
  saint_t x;

  bufend = buf + (last - middle) - 1;
  ss_blockswap(buf, middle, last - middle);

  x = 0;
  if(*bufend < 0)       { p1 = PA + ~*bufend; x |= 1; }
  else                  { p1 = PA +  *bufend; }
  if(*(middle - 1) < 0) { p2 = PA + ~*(middle - 1); x |= 2; }
  else                  { p2 = PA +  *(middle - 1); }
  for(t = *(a = last - 1), b = bufend, c = middle - 1;;) {
    r = ss_compare(T, p1, p2, depth);
    if(0 < r) {
      if(x & 1) { do { *a-- = *b, *b-- = *a; } while(*b < 0); x ^= 1; }
      *a-- = *b;
      if(b <= buf) { *buf = t; break; }
      *b-- = *a;
      if(*b < 0) { p1 = PA + ~*b; x |= 1; }
      else       { p1 = PA +  *b; }
    } else if(r < 0) {
      if(x & 2) { do { *a-- = *c, *c-- = *a; } while(*c < 0); x ^= 2; }
      *a-- = *c, *c-- = *a;
      if(c < first) {
        while(buf < b) { *a-- = *b, *b-- = *a; }
        *a = *b, *b = t;
        break;
      }
      if(*c < 0) { p2 = PA + ~*c; x |= 2; }
      else       { p2 = PA +  *c; }
    } else {
      if(x & 1) { do { *a-- = *b, *b-- = *a; } while(*b < 0); x ^= 1; }
      *a-- = ~*b;
      if(b <= buf) { *buf = t; break; }
      *b-- = *a;
      if(x & 2) { do { *a-- = *c, *c-- = *a; } while(*c < 0); x ^= 2; }
      *a-- = *c, *c-- = *a;
      if(c < first) {
        while(buf < b) { *a-- = *b, *b-- = *a; }
        *a = *b, *b = t;
        break;
      }
      if(*b < 0) { p1 = PA + ~*b; x |= 1; }
      else       { p1 = PA +  *b; }
      if(*c < 0) { p2 = PA + ~*c; x |= 2; }
      else       { p2 = PA +  *c; }
    }
  }
}

/* D&C based merge. */
static
void
ss_swapmerge(const sauchar_t *T, const saidx_t *PA,
             saidx_t *first, saidx_t *middle, saidx_t *last,
             saidx_t *buf, saidx_t bufsize, saidx_t depth) {
#define STACK_SIZE SS_SMERGE_STACKSIZE
#define GETIDX(a) ((0 <= (a)) ? (a) : (~(a)))
#define MERGE_CHECK(a, b, c)\
  do {\
    if(((c) & 1) ||\
       (((c) & 2) && (ss_compare(T, PA + GETIDX(*((a) - 1)), PA + *(a), depth) == 0))) {\
      *(a) = ~*(a);\
    }\
    if(((c) & 4) && ((ss_compare(T, PA + GETIDX(*((b) - 1)), PA + *(b), depth) == 0))) {\
      *(b) = ~*(b);\
    }\
  } while(0)
  struct { saidx_t *a, *b, *c; saint_t d; } stack[STACK_SIZE];
  saidx_t *l, *r, *lm, *rm;
  saidx_t m, len, half;
  saint_t ssize;
  saint_t check, next;

  for(check = 0, ssize = 0;;) {
    if((last - middle) <= bufsize) {
      if((first < middle) && (middle < last)) {
        ss_mergebackward(T, PA, first, middle, last, buf, depth);
      }
      MERGE_CHECK(first, last, check);
      STACK_POP(first, middle, last, check);
      continue;
    }

    if((middle - first) <= bufsize) {
      if(first < middle) {
        ss_mergeforward(T, PA, first, middle, last, buf, depth);
      }
      MERGE_CHECK(first, last, check);
      STACK_POP(first, middle, last, check);
      continue;
    }

    for(m = 0, len = MIN(middle - first, last - middle), half = len >> 1;
        0 < len;
        len = half, half >>= 1) {
      if(ss_compare(T, PA + GETIDX(*(middle + m + half)),
                       PA + GETIDX(*(middle - m - half - 1)), depth) < 0) {
        m += half + 1;
        half -= (len & 1) ^ 1;
      }
    }

    if(0 < m) {
      lm = middle - m, rm = middle + m;
      ss_blockswap(lm, middle, m);
      l = r = middle, next = 0;
      if(rm < last) {
        if(*rm < 0) {
          *rm = ~*rm;
          if(first < lm) { for(; *--l < 0;) { } next |= 4; }
          next |= 1;
        } else if(first < lm) {
          for(; *r < 0; ++r) { }
          next |= 2;
        }
      }

      if((l - first) <= (last - r)) {
        STACK_PUSH(r, rm, last, (next & 3) | (check & 4));
        middle = lm, last = l, check = (check & 3) | (next & 4);
      } else {
        if((next & 2) && (r == middle)) { next ^= 6; }
        STACK_PUSH(first, lm, l, (check & 3) | (next & 4));
        first = r, middle = rm, check = (next & 3) | (check & 4);
      }
    } else {
      if(ss_compare(T, PA + GETIDX(*(middle - 1)), PA + *middle, depth) == 0) {
        *middle = ~*middle;
      }
      MERGE_CHECK(first, last, check);
      STACK_POP(first, middle, last, check);
    }
  }
#undef STACK_SIZE
}

#endif /* SS_BLOCKSIZE != 0 */


/*---------------------------------------------------------------------------*/

/*- Function -*/

/* Substring sort */
void
sssort(const sauchar_t *T, const saidx_t *PA,
       saidx_t *first, saidx_t *last,
       saidx_t *buf, saidx_t bufsize,
       saidx_t depth, saidx_t n, saint_t lastsuffix) {
  saidx_t *a;
#if SS_BLOCKSIZE != 0
  saidx_t *b, *middle, *curbuf;
  saidx_t j, k, curbufsize, limit;
#endif
  saidx_t i;

  if(lastsuffix != 0) { ++first; }

#if SS_BLOCKSIZE == 0
  ss_mintrosort(T, PA, first, last, depth);
#else
  if((bufsize < SS_BLOCKSIZE) &&
      (bufsize < (last - first)) &&
      (bufsize < (limit = ss_isqrt(last - first)))) {
    if(SS_BLOCKSIZE < limit) { limit = SS_BLOCKSIZE; }
    buf = middle = last - limit, bufsize = limit;
  } else {
    middle = last, limit = 0;
  }
  for(a = first, i = 0; SS_BLOCKSIZE < (middle - a); a += SS_BLOCKSIZE, ++i) {
#if SS_INSERTIONSORT_THRESHOLD < SS_BLOCKSIZE
    ss_mintrosort(T, PA, a, a + SS_BLOCKSIZE, depth);
#elif 1 < SS_BLOCKSIZE
    ss_insertionsort(T, PA, a, a + SS_BLOCKSIZE, depth);
#endif
    curbufsize = last - (a + SS_BLOCKSIZE);
    curbuf = a + SS_BLOCKSIZE;
    if(curbufsize <= bufsize) { curbufsize = bufsize, curbuf = buf; }
    for(b = a, k = SS_BLOCKSIZE, j = i; j & 1; b -= k, k <<= 1, j >>= 1) {
      ss_swapmerge(T, PA, b - k, b, b + k, curbuf, curbufsize, depth);
    }
  }
#if SS_INSERTIONSORT_THRESHOLD < SS_BLOCKSIZE
  ss_mintrosort(T, PA, a, middle, depth);
#elif 1 < SS_BLOCKSIZE
  ss_insertionsort(T, PA, a, middle, depth);
#endif
  for(k = SS_BLOCKSIZE; i != 0; k <<= 1, i >>= 1) {
    if(i & 1) {
      ss_swapmerge(T, PA, a - k, a, middle, buf, bufsize, depth);
      a -= k;
    }
  }
  if(limit != 0) {
#if SS_INSERTIONSORT_THRESHOLD < SS_BLOCKSIZE
    ss_mintrosort(T, PA, middle, last, depth);
#elif 1 < SS_BLOCKSIZE
    ss_insertionsort(T, PA, middle, last, depth);
#endif
    ss_inplacemerge(T, PA, first, middle, last, depth);
  }
#endif

  if(lastsuffix != 0) {
    /* Insert last type B* suffix. */
    saidx_t PAi[2]; PAi[0] = PA[*(first - 1)], PAi[1] = n - 2;
    for(a = first, i = *(first - 1);
        (a < last) && ((*a < 0) || (0 < ss_compare(T, &(PAi[0]), PA + *a, depth)));
        ++a) {
      *(a - 1) = *a;
    }
    *(a - 1) = i;
  }
}
/*
 * trsort.c for libdivsufsort
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



/*- Private Functions -*/

static INLINE
saint_t
tr_ilg(saidx_t n) {
#if defined(BUILD_DIVSUFSORT64)
  return (n >> 32) ?
          ((n >> 48) ?
            ((n >> 56) ?
              56 + lg_table[(n >> 56) & 0xff] :
              48 + lg_table[(n >> 48) & 0xff]) :
            ((n >> 40) ?
              40 + lg_table[(n >> 40) & 0xff] :
              32 + lg_table[(n >> 32) & 0xff])) :
          ((n & 0xffff0000) ?
            ((n & 0xff000000) ?
              24 + lg_table[(n >> 24) & 0xff] :
              16 + lg_table[(n >> 16) & 0xff]) :
            ((n & 0x0000ff00) ?
               8 + lg_table[(n >>  8) & 0xff] :
               0 + lg_table[(n >>  0) & 0xff]));
#else
  return (n & 0xffff0000) ?
          ((n & 0xff000000) ?
            24 + lg_table[(n >> 24) & 0xff] :
            16 + lg_table[(n >> 16) & 0xff]) :
          ((n & 0x0000ff00) ?
             8 + lg_table[(n >>  8) & 0xff] :
             0 + lg_table[(n >>  0) & 0xff]);
#endif
}


/*---------------------------------------------------------------------------*/

/* Simple insertionsort for small size groups. */
static
void
tr_insertionsort(const saidx_t *ISAd, saidx_t *first, saidx_t *last) {
  saidx_t *a, *b;
  saidx_t t, r;

  for(a = first + 1; a < last; ++a) {
    for(t = *a, b = a - 1; 0 > (r = ISAd[t] - ISAd[*b]);) {
      do { *(b + 1) = *b; } while((first <= --b) && (*b < 0));
      if(b < first) { break; }
    }
    if(r == 0) { *b = ~*b; }
    *(b + 1) = t;
  }
}


/*---------------------------------------------------------------------------*/

static INLINE
void
tr_fixdown(const saidx_t *ISAd, saidx_t *SA, saidx_t i, saidx_t size) {
  saidx_t j, k;
  saidx_t v;
  saidx_t c, d, e;

  for(v = SA[i], c = ISAd[v]; (j = 2 * i + 1) < size; SA[i] = SA[k], i = k) {
    d = ISAd[SA[k = j++]];
    if(d < (e = ISAd[SA[j]])) { k = j; d = e; }
    if(d <= c) { break; }
  }
  SA[i] = v;
}

/* Simple top-down heapsort. */
static
void
tr_heapsort(const saidx_t *ISAd, saidx_t *SA, saidx_t size) {
  saidx_t i, m;
  saidx_t t;

  m = size;
  if((size % 2) == 0) {
    m--;
    if(ISAd[SA[m / 2]] < ISAd[SA[m]]) { SWAP(SA[m], SA[m / 2]); }
  }

  for(i = m / 2 - 1; 0 <= i; --i) { tr_fixdown(ISAd, SA, i, m); }
  if((size % 2) == 0) { SWAP(SA[0], SA[m]); tr_fixdown(ISAd, SA, 0, m); }
  for(i = m - 1; 0 < i; --i) {
    t = SA[0], SA[0] = SA[i];
    tr_fixdown(ISAd, SA, 0, i);
    SA[i] = t;
  }
}


/*---------------------------------------------------------------------------*/

/* Returns the median of three elements. */
static INLINE
saidx_t *
tr_median3(const saidx_t *ISAd, saidx_t *v1, saidx_t *v2, saidx_t *v3) {
  saidx_t *t;
  if(ISAd[*v1] > ISAd[*v2]) { SWAP(v1, v2); }
  if(ISAd[*v2] > ISAd[*v3]) {
    if(ISAd[*v1] > ISAd[*v3]) { return v1; }
    else { return v3; }
  }
  return v2;
}

/* Returns the median of five elements. */
static INLINE
saidx_t *
tr_median5(const saidx_t *ISAd,
           saidx_t *v1, saidx_t *v2, saidx_t *v3, saidx_t *v4, saidx_t *v5) {
  saidx_t *t;
  if(ISAd[*v2] > ISAd[*v3]) { SWAP(v2, v3); }
  if(ISAd[*v4] > ISAd[*v5]) { SWAP(v4, v5); }
  if(ISAd[*v2] > ISAd[*v4]) { SWAP(v2, v4); SWAP(v3, v5); }
  if(ISAd[*v1] > ISAd[*v3]) { SWAP(v1, v3); }
  if(ISAd[*v1] > ISAd[*v4]) { SWAP(v1, v4); SWAP(v3, v5); }
  if(ISAd[*v3] > ISAd[*v4]) { return v4; }
  return v3;
}

/* Returns the pivot element. */
static INLINE
saidx_t *
tr_pivot(const saidx_t *ISAd, saidx_t *first, saidx_t *last) {
  saidx_t *middle;
  saidx_t t;

  t = last - first;
  middle = first + t / 2;

  if(t <= 512) {
    if(t <= 32) {
      return tr_median3(ISAd, first, middle, last - 1);
    } else {
      t >>= 2;
      return tr_median5(ISAd, first, first + t, middle, last - 1 - t, last - 1);
    }
  }
  t >>= 3;
  first  = tr_median3(ISAd, first, first + t, first + (t << 1));
  middle = tr_median3(ISAd, middle - t, middle, middle + t);
  last   = tr_median3(ISAd, last - 1 - (t << 1), last - 1 - t, last - 1);
  return tr_median3(ISAd, first, middle, last);
}


/*---------------------------------------------------------------------------*/

typedef struct _trbudget_t trbudget_t;
struct _trbudget_t {
  saidx_t chance;
  saidx_t remain;
  saidx_t incval;
  saidx_t count;
};

static INLINE
void
trbudget_init(trbudget_t *budget, saidx_t chance, saidx_t incval) {
  budget->chance = chance;
  budget->remain = budget->incval = incval;
}

static INLINE
saint_t
trbudget_check(trbudget_t *budget, saidx_t size) {
  if(size <= budget->remain) { budget->remain -= size; return 1; }
  if(budget->chance == 0) { budget->count += size; return 0; }
  budget->remain += budget->incval - size;
  budget->chance -= 1;
  return 1;
}


/*---------------------------------------------------------------------------*/

static INLINE
void
tr_partition(const saidx_t *ISAd,
             saidx_t *first, saidx_t *middle, saidx_t *last,
             saidx_t **pa, saidx_t **pb, saidx_t v) {
  saidx_t *a, *b, *c, *d, *e, *f;
  saidx_t t, s;
  saidx_t x = 0;

  for(b = middle - 1; (++b < last) && ((x = ISAd[*b]) == v);) { }
  if(((a = b) < last) && (x < v)) {
    for(; (++b < last) && ((x = ISAd[*b]) <= v);) {
      if(x == v) { SWAP(*b, *a); ++a; }
    }
  }
  for(c = last; (b < --c) && ((x = ISAd[*c]) == v);) { }
  if((b < (d = c)) && (x > v)) {
    for(; (b < --c) && ((x = ISAd[*c]) >= v);) {
      if(x == v) { SWAP(*c, *d); --d; }
    }
  }
  for(; b < c;) {
    SWAP(*b, *c);
    for(; (++b < c) && ((x = ISAd[*b]) <= v);) {
      if(x == v) { SWAP(*b, *a); ++a; }
    }
    for(; (b < --c) && ((x = ISAd[*c]) >= v);) {
      if(x == v) { SWAP(*c, *d); --d; }
    }
  }

  if(a <= d) {
    c = b - 1;
    if((s = a - first) > (t = b - a)) { s = t; }
    for(e = first, f = b - s; 0 < s; --s, ++e, ++f) { SWAP(*e, *f); }
    if((s = d - c) > (t = last - d - 1)) { s = t; }
    for(e = b, f = last - s; 0 < s; --s, ++e, ++f) { SWAP(*e, *f); }
    first += (b - a), last -= (d - c);
  }
  *pa = first, *pb = last;
}

static
void
tr_copy(saidx_t *ISA, const saidx_t *SA,
        saidx_t *first, saidx_t *a, saidx_t *b, saidx_t *last,
        saidx_t depth) {
  /* sort suffixes of middle partition
     by using sorted order of suffixes of left and right partition. */
  saidx_t *c, *d, *e;
  saidx_t s, v;

  v = b - SA - 1;
  for(c = first, d = a - 1; c <= d; ++c) {
    if((0 <= (s = *c - depth)) && (ISA[s] == v)) {
      *++d = s;
      ISA[s] = d - SA;
    }
  }
  for(c = last - 1, e = d + 1, d = b; e < d; --c) {
    if((0 <= (s = *c - depth)) && (ISA[s] == v)) {
      *--d = s;
      ISA[s] = d - SA;
    }
  }
}

static
void
tr_partialcopy(saidx_t *ISA, const saidx_t *SA,
               saidx_t *first, saidx_t *a, saidx_t *b, saidx_t *last,
               saidx_t depth) {
  saidx_t *c, *d, *e;
  saidx_t s, v;
  saidx_t rank, lastrank, newrank = -1;

  v = b - SA - 1;
  lastrank = -1;
  for(c = first, d = a - 1; c <= d; ++c) {
    if((0 <= (s = *c - depth)) && (ISA[s] == v)) {
      *++d = s;
      rank = ISA[s + depth];
      if(lastrank != rank) { lastrank = rank; newrank = d - SA; }
      ISA[s] = newrank;
    }
  }

  lastrank = -1;
  for(e = d; first <= e; --e) {
    rank = ISA[*e];
    if(lastrank != rank) { lastrank = rank; newrank = e - SA; }
    if(newrank != rank) { ISA[*e] = newrank; }
  }

  lastrank = -1;
  for(c = last - 1, e = d + 1, d = b; e < d; --c) {
    if((0 <= (s = *c - depth)) && (ISA[s] == v)) {
      *--d = s;
      rank = ISA[s + depth];
      if(lastrank != rank) { lastrank = rank; newrank = d - SA; }
      ISA[s] = newrank;
    }
  }
}

static
void
tr_introsort(saidx_t *ISA, const saidx_t *ISAd,
             saidx_t *SA, saidx_t *first, saidx_t *last,
             trbudget_t *budget) {
#define STACK_SIZE TR_STACKSIZE
  struct { const saidx_t *a; saidx_t *b, *c; saint_t d, e; }stack[STACK_SIZE];
  saidx_t *a, *b, *c;
  saidx_t t;
  saidx_t v, x = 0;
  saidx_t incr = ISAd - ISA;
  saint_t limit, next;
  saint_t ssize, trlink = -1;

  for(ssize = 0, limit = tr_ilg(last - first);;) {

    if(limit < 0) {
      if(limit == -1) {
        /* tandem repeat partition */
        tr_partition(ISAd - incr, first, first, last, &a, &b, last - SA - 1);

        /* update ranks */
        if(a < last) {
          for(c = first, v = a - SA - 1; c < a; ++c) { ISA[*c] = v; }
        }
        if(b < last) {
          for(c = a, v = b - SA - 1; c < b; ++c) { ISA[*c] = v; }
        }

        /* push */
        if(1 < (b - a)) {
          STACK_PUSH5(NULL, a, b, 0, 0);
          STACK_PUSH5(ISAd - incr, first, last, -2, trlink);
          trlink = ssize - 2;
        }
        if((a - first) <= (last - b)) {
          if(1 < (a - first)) {
            STACK_PUSH5(ISAd, b, last, tr_ilg(last - b), trlink);
            last = a, limit = tr_ilg(a - first);
          } else if(1 < (last - b)) {
            first = b, limit = tr_ilg(last - b);
          } else {
            STACK_POP5(ISAd, first, last, limit, trlink);
          }
        } else {
          if(1 < (last - b)) {
            STACK_PUSH5(ISAd, first, a, tr_ilg(a - first), trlink);
            first = b, limit = tr_ilg(last - b);
          } else if(1 < (a - first)) {
            last = a, limit = tr_ilg(a - first);
          } else {
            STACK_POP5(ISAd, first, last, limit, trlink);
          }
        }
      } else if(limit == -2) {
        /* tandem repeat copy */
        a = stack[--ssize].b, b = stack[ssize].c;
        if(stack[ssize].d == 0) {
          tr_copy(ISA, SA, first, a, b, last, ISAd - ISA);
        } else {
          if(0 <= trlink) { stack[trlink].d = -1; }
          tr_partialcopy(ISA, SA, first, a, b, last, ISAd - ISA);
        }
        STACK_POP5(ISAd, first, last, limit, trlink);
      } else {
        /* sorted partition */
        if(0 <= *first) {
          a = first;
          do { ISA[*a] = a - SA; } while((++a < last) && (0 <= *a));
          first = a;
        }
        if(first < last) {
          a = first; do { *a = ~*a; } while(*++a < 0);
          next = (ISA[*a] != ISAd[*a]) ? tr_ilg(a - first + 1) : -1;
          if(++a < last) { for(b = first, v = a - SA - 1; b < a; ++b) { ISA[*b] = v; } }

          /* push */
          if(trbudget_check(budget, a - first)) {
            if((a - first) <= (last - a)) {
              STACK_PUSH5(ISAd, a, last, -3, trlink);
              ISAd += incr, last = a, limit = next;
            } else {
              if(1 < (last - a)) {
                STACK_PUSH5(ISAd + incr, first, a, next, trlink);
                first = a, limit = -3;
              } else {
                ISAd += incr, last = a, limit = next;
              }
            }
          } else {
            if(0 <= trlink) { stack[trlink].d = -1; }
            if(1 < (last - a)) {
              first = a, limit = -3;
            } else {
              STACK_POP5(ISAd, first, last, limit, trlink);
            }
          }
        } else {
          STACK_POP5(ISAd, first, last, limit, trlink);
        }
      }
      continue;
    }

    if((last - first) <= TR_INSERTIONSORT_THRESHOLD) {
      tr_insertionsort(ISAd, first, last);
      limit = -3;
      continue;
    }

    if(limit-- == 0) {
      tr_heapsort(ISAd, first, last - first);
      for(a = last - 1; first < a; a = b) {
        for(x = ISAd[*a], b = a - 1; (first <= b) && (ISAd[*b] == x); --b) { *b = ~*b; }
      }
      limit = -3;
      continue;
    }

    /* choose pivot */
    a = tr_pivot(ISAd, first, last);
    SWAP(*first, *a);
    v = ISAd[*first];

    /* partition */
    tr_partition(ISAd, first, first + 1, last, &a, &b, v);
    if((last - first) != (b - a)) {
      next = (ISA[*a] != v) ? tr_ilg(b - a) : -1;

      /* update ranks */
      for(c = first, v = a - SA - 1; c < a; ++c) { ISA[*c] = v; }
      if(b < last) { for(c = a, v = b - SA - 1; c < b; ++c) { ISA[*c] = v; } }

      /* push */
      if((1 < (b - a)) && (trbudget_check(budget, b - a))) {
        if((a - first) <= (last - b)) {
          if((last - b) <= (b - a)) {
            if(1 < (a - first)) {
              STACK_PUSH5(ISAd + incr, a, b, next, trlink);
              STACK_PUSH5(ISAd, b, last, limit, trlink);
              last = a;
            } else if(1 < (last - b)) {
              STACK_PUSH5(ISAd + incr, a, b, next, trlink);
              first = b;
            } else {
              ISAd += incr, first = a, last = b, limit = next;
            }
          } else if((a - first) <= (b - a)) {
            if(1 < (a - first)) {
              STACK_PUSH5(ISAd, b, last, limit, trlink);
              STACK_PUSH5(ISAd + incr, a, b, next, trlink);
              last = a;
            } else {
              STACK_PUSH5(ISAd, b, last, limit, trlink);
              ISAd += incr, first = a, last = b, limit = next;
            }
          } else {
            STACK_PUSH5(ISAd, b, last, limit, trlink);
            STACK_PUSH5(ISAd, first, a, limit, trlink);
            ISAd += incr, first = a, last = b, limit = next;
          }
        } else {
          if((a - first) <= (b - a)) {
            if(1 < (last - b)) {
              STACK_PUSH5(ISAd + incr, a, b, next, trlink);
              STACK_PUSH5(ISAd, first, a, limit, trlink);
              first = b;
            } else if(1 < (a - first)) {
              STACK_PUSH5(ISAd + incr, a, b, next, trlink);
              last = a;
            } else {
              ISAd += incr, first = a, last = b, limit = next;
            }
          } else if((last - b) <= (b - a)) {
            if(1 < (last - b)) {
              STACK_PUSH5(ISAd, first, a, limit, trlink);
              STACK_PUSH5(ISAd + incr, a, b, next, trlink);
              first = b;
            } else {
              STACK_PUSH5(ISAd, first, a, limit, trlink);
              ISAd += incr, first = a, last = b, limit = next;
            }
          } else {
            STACK_PUSH5(ISAd, first, a, limit, trlink);
            STACK_PUSH5(ISAd, b, last, limit, trlink);
            ISAd += incr, first = a, last = b, limit = next;
          }
        }
      } else {
        if((1 < (b - a)) && (0 <= trlink)) { stack[trlink].d = -1; }
        if((a - first) <= (last - b)) {
          if(1 < (a - first)) {
            STACK_PUSH5(ISAd, b, last, limit, trlink);
            last = a;
          } else if(1 < (last - b)) {
            first = b;
          } else {
            STACK_POP5(ISAd, first, last, limit, trlink);
          }
        } else {
          if(1 < (last - b)) {
            STACK_PUSH5(ISAd, first, a, limit, trlink);
            first = b;
          } else if(1 < (a - first)) {
            last = a;
          } else {
            STACK_POP5(ISAd, first, last, limit, trlink);
          }
        }
      }
    } else {
      if(trbudget_check(budget, last - first)) {
        limit = tr_ilg(last - first), ISAd += incr;
      } else {
        if(0 <= trlink) { stack[trlink].d = -1; }
        STACK_POP5(ISAd, first, last, limit, trlink);
      }
    }
  }
#undef STACK_SIZE
}



/*---------------------------------------------------------------------------*/

/*- Function -*/

/* Tandem repeat sort */
void
trsort(saidx_t *ISA, saidx_t *SA, saidx_t n, saidx_t depth) {
  saidx_t *ISAd;
  saidx_t *first, *last;
  trbudget_t budget;
  saidx_t t, skip, unsorted;

  trbudget_init(&budget, tr_ilg(n) * 2 / 3, n);
/*  trbudget_init(&budget, tr_ilg(n) * 3 / 4, n); */
  for(ISAd = ISA + depth; -n < *SA; ISAd += ISAd - ISA) {
    first = SA;
    skip = 0;
    unsorted = 0;
    do {
      if((t = *first) < 0) { first -= t; skip += t; }
      else {
        if(skip != 0) { *(first + skip) = skip; skip = 0; }
        last = SA + ISA[t] + 1;
        if(1 < (last - first)) {
          budget.count = 0;
          tr_introsort(ISA, ISAd, SA, first, last, &budget);
          if(budget.count != 0) { unsorted += budget.count; }
          else { skip = first - last; }
        } else if((last - first) == 1) {
          skip = -1;
        }
        first = last;
      }
    } while(first < (SA + n));
    if(skip != 0) { *(first + skip) = skip; }
    if(unsorted == 0) { break; }
  }
}
