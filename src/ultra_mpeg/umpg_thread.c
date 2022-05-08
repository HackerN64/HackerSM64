// custom code implementation of umpeg on its own thread

#include "umpg.h"
#include "mem.h"
#include "buffers/buffers.h"
#include "game/game_init.h"

extern u8 umpg_heap[];
u64 umpg_stack[0x2000];
OSThread umpg_thread;
OSMesgQueue umpg_syncqueue;

u8 umpg_enabled = 0;

OSMesgQueue umpg_videoqueue;
OSMesg      umpg_videomesg;
static struct umpg_t *mpg;

// hooks onto the gfx SPTask in the scheduler;
//  hopefully that doesnt break anything
void umpg_proc(void *arg) {
    osViSetEvent(&umpg_videoqueue, 0, 1);

    while (1) {
        osRecvMesg(&umpg_videoqueue, 0, OS_MESG_BLOCK);

        select_gfx_pool();
        select_framebuffer();
        u8 result = umpg_update(mpg, &gDisplayListHead);
        end_master_display_list();

        // osViSwapBuffer[]

        create_gfx_task_structure();

        if (result == 1) {
            umpg_free(mpg);
            osSendMesg(&umpg_syncqueue, 0, OS_MESG_BLOCK);
        }
    }
}

void umpg_create_thread(int x, int y, int wd, int ht, void *videostart, void *videoend) {
    mem_init();

    mem_link(umpg_heap, umpg_heap + 0x800000);
    mpg = umpg_init(x, y, wd * 4, ht * 4, videostart, videoend);
// void osCreateThread(OSThread *t, OSId id, void (*entry)(void *),
//                 void *arg, void *sp, OSPri pri);
    osCreateThread(&umpg_thread, 7, umpg_proc, 0, umpg_stack + (0x2000 / 8), 11);
    // osStartThread(&umpg_thread);
}



