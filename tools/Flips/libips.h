#pragma once
//Module name: libips
//Author: Alcaro
//Date: March 8, 2013
//Licence: GPL v3.0 or higher

#include "global.h"

#ifdef __cplusplus
extern "C" {
#endif

enum ipserror {
	ips_ok,//Patch applied or created successfully.
	
	ips_notthis,//The patch is most likely not intended for this ROM.
	ips_thisout,//You most likely applied the patch on the output ROM.
	ips_scrambled,//The patch is technically valid, but seems scrambled or malformed.
	ips_invalid,//The patch is invalid.
	
	ips_16MB,//One or both files is bigger than 16MB. The IPS format doesn't support that. The created
	         //patch contains only the differences to that point.
	ips_identical,//The input buffers are identical.
	
	ips_shut_up_gcc//This one isn't used, it's just to kill a stray comma warning.
};

//Applies the IPS patch in [patch, patchlen] to [in, inlen] and stores it to [out, outlen]. Send the
//  return value in out to ips_free when you're done with it.
enum ipserror ips_apply(struct mem patch, struct mem in, struct mem * out);

//Creates an IPS patch that converts source to target and stores it to patch.
enum ipserror ips_create(struct mem source, struct mem target, struct mem * patch);

//Frees the memory returned in the output parameters of the above. Do not call it twice on the same
//  input, nor on anything you got from anywhere else. ips_free is guaranteed to be equivalent to
//  calling stdlib.h's free() on mem.ptr.
void ips_free(struct mem mem);

//ips_study allows you to detect most patching errors without applying it to a ROM, or even a ROM to
//  apply it to. ips_apply calls ips_study and ips_apply_study, so if you call ips_study yourself,
//  it's recommended to call ips_apply_study to not redo the calculation. ips_free is still
//  required.
struct ipsstudy;
enum ipserror ips_study(struct mem patch, struct ipsstudy * study);
enum ipserror ips_apply_study(struct mem patch, struct ipsstudy * study, struct mem in, struct mem * out);

#ifdef __cplusplus
}
#endif
