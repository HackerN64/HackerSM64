// custom code implementation of umpeg on its own thread

#include "umpg.h"
#include "mem.h"
#include "buffers/buffers.h"
#include "buffers/gfx_output_buffer.h"
#include "game/game_init.h"

extern u8 umpg_heap[];
extern u16 sRenderedFramebuffer;
OSThread umpg_thread;
u64 umpg_stack[0x4000/sizeof(u64)];
OSMesgQueue umpg_syncqueue;
OSMesg umpg_syncmesg;

u8 umpg_enabled = 0;

OSMesgQueue umpg_videoqueue;
OSMesg      umpg_videomesg;

OSMesgQueue umpg_spqueue;
OSMesg      umpg_spmesg;

OSMesgQueue umpg_dpqueue;
OSMesg      umpg_dpmesg;

static struct umpg_t *mpg;

OSTask  umpg_task;

// hooks onto the gfx SPTask in the scheduler;
//  hopefully that doesnt break anything

Gfx dpBuf[3][25];
Gfx *gp;
int pickFB = 0;

void umpg_maketask(void) {
    s32 entries = gp - dpBuf[pickFB];

    umpg_task.t.type = M_GFXTASK;
    umpg_task.t.ucode_boot = rspbootTextStart;
    umpg_task.t.ucode_boot_size = ((u8 *) rspbootTextEnd - (u8 *) rspbootTextStart);

    umpg_task.t.flags = 0x0;

    umpg_task.t.ucode = gspF3DZEX2_PosLight_fifoTextStart;
    umpg_task.t.ucode_data = gspF3DZEX2_PosLight_fifoDataStart;
    umpg_task.t.ucode_size = ((u8 *) gspF3DZEX2_PosLight_fifoTextEnd - (u8 *) gspF3DZEX2_PosLight_fifoTextStart);
    umpg_task.t.ucode_data_size = ((u8 *) gspF3DZEX2_PosLight_fifoDataEnd - (u8 *) gspF3DZEX2_PosLight_fifoDataStart);

    umpg_task.t.dram_stack = (u64 *) gGfxSPTaskStack;
    umpg_task.t.dram_stack_size = SP_DRAM_STACK_SIZE8;
    umpg_task.t.output_buff = gGfxSPTaskOutputBuffer;
    umpg_task.t.output_buff_size =
        (u64 *)((u8 *) gGfxSPTaskOutputBuffer + sizeof(gGfxSPTaskOutputBuffer));
    umpg_task.t.data_ptr = (u64 *) dpBuf[pickFB];
    // umpg_task.t.data_size = entries * sizeof(Gfx);
    umpg_task.t.data_size = 0;
    umpg_task.t.yield_data_ptr = (u64 *) gGfxSPTaskYieldBuffer;
    umpg_task.t.yield_data_size = OS_YIELD_DATA_SIZE;
}

void umpg_selFB(void) {
    gDPPipeSync(gp++);

    gDPSetCycleType(gp++, G_CYC_1CYCLE);
    gDPSetColorImage(gp++, G_IM_FMT_RGBA, G_IM_SIZ_16b, SCREEN_WIDTH,
                     gPhysicalFramebuffers[sRenderingFramebuffer]);
    gDPSetScissor(gp++, G_SC_NON_INTERLACE, 0, gBorderHeight, SCREEN_WIDTH,
                  SCREEN_HEIGHT - gBorderHeight);
}

void umpg_scissor(void) {
    gDPPipeSync(gp++);

    gDPSetScissor(gp++, G_SC_NON_INTERLACE, 0, 0, SCREEN_WIDTH, SCREEN_HEIGHT);
    // gDPSetRenderMode(gp++, G_RM_OPA_SURF, G_RM_OPA_SURF2);
    // gDPSetCycleType(gp++, G_CYC_FILL);

    // gDPSetFillColor(gp++, GPACK_RGBA5551(0, 0, 0, 0) << 16 | GPACK_RGBA5551(0, 0, 0, 0));

    // if (gBorderHeight) {
    //     gDPFillRectangle(gp++, 0, 0,
    //                     SCREEN_WIDTH - 1, gBorderHeight - 1);
    //     gDPFillRectangle(gp++,
    //                     0, SCREEN_HEIGHT - gBorderHeight,
    //                     SCREEN_WIDTH - 1, SCREEN_HEIGHT - 1);
    // }
}


void umpg_proc(UNUSED void *arg) {
    osCreateMesgQueue(&umpg_videoqueue, &umpg_videomesg, 1);
    osViSetEvent(&umpg_videoqueue, 0, 1);

    osCreateMesgQueue(&umpg_spqueue, &umpg_spmesg, 1);
    osSetEventMesg(OS_EVENT_SP, &umpg_spqueue, 0);

    osCreateMesgQueue(&umpg_dpqueue, &umpg_dpmesg, 1);
    osSetEventMesg(OS_EVENT_DP, &umpg_dpqueue, 0);

    while (1) {
        osRecvMesg(&umpg_videoqueue, 0, OS_MESG_BLOCK);
        osRecvMesg(&umpg_videoqueue, 0, OS_MESG_BLOCK);

        gp = dpBuf[pickFB];

        umpg_selFB();
        
        u8 result = umpg_update(mpg, &gp);

        umpg_scissor();
        gDPFullSync(gp++);
        gSPEndDisplayList(gp++);

        umpg_maketask();

        osWritebackDCache(dpBuf, sizeof(dpBuf));
        osSpTaskStart(&umpg_task);

        osRecvMesg(&umpg_spqueue, 0, OS_MESG_BLOCK);
        osRecvMesg(&umpg_dpqueue, 0, OS_MESG_BLOCK);
        osViSwapBuffer((void *) PHYSICAL_TO_VIRTUAL(gPhysicalFramebuffers[pickFB]));

        if (++pickFB == 3) {
            pickFB = 0;
        }

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
    osCreateMesgQueue(&umpg_syncqueue, &umpg_syncmesg, 1);
    osSetEventMesg(OS_EVENT_SP, NULL, 0);
    osSetEventMesg(OS_EVENT_DP, NULL, 0);
// void osCreateThread(OSThread *t, OSId id, void (*entry)(void *),
//                 void *arg, void *sp, OSPri pri);
    osCreateThread(&umpg_thread, 1000, umpg_proc, 0, umpg_stack + (0x2000 / sizeof(u64)), 11);
    osStartThread(&umpg_thread);
}



