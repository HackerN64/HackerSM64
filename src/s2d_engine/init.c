#include <ultra64.h>
#include <PR/gs2dex.h>
#include "s2d_config.h"
#include "init.h"
#include "debug.h"
#include "s2d_error.h"

void s2d_init(void) {
	s2d_error_y = TEX_HEIGHT;
	if (gIsEmulator)
		gSPLoadUcode(gdl_head++, s2d_text, s2d_data);
}

void s2d_stop(void) {
	if (gIsEmulator) {
		gSPLoadUcode(gdl_head++, zex_text, zex_data);
		if deinit_cond {
			my_rdp_init();
			my_rsp_init();
		}
	}
	s2d_reset_defer_index();
}

