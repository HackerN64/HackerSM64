#pragma once

//Module name: libbps
//Author: Alcaro
//Date: November 30, 2015
//Licence: GPL v3.0 or higher

#include "global.h"
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

#ifdef __cplusplus
}
#endif
