#pragma once

#include "crash_screen/util/registers.h"


// -- Video Interface (VI) --
// https://n64brew.dev/wiki/Video_Interface


static const char* sRegDesc_VI[] = {
    [REGID_VI_STATUS ] = "VI Status/Control",
    [REGID_VI_ORIGIN ] = "FB origin (bytes)",
    [REGID_VI_WIDTH  ] = "FB line width (pixels)",
    [REGID_VI_INTR   ] = "Vertical interrupt",
    [REGID_VI_CURRENT] = "Current vertical line",
    [REGID_VI_BURST  ] = "Video timing",
    [REGID_VI_V_SYNC ] = "Vertical sync",
    [REGID_VI_H_SYNC ] = "Horizontal sync",
    [REGID_VI_LEAP   ] = "Horizontal sync leap",
    [REGID_VI_H_START] = "Horizontal video",
    [REGID_VI_V_START] = "Vertical video",
    [REGID_VI_V_BURST] = "Vertical burst",
    [REGID_VI_X_SCALE] = "X scale",
    [REGID_VI_Y_SCALE] = "Y scale",
};
ALIGNED32 static const RegisterInfo sRegInfo_VI[] = {
    [REGID_VI_STATUS ] = DEF_IREG(VI_STATUS_REG,  "STATUS/CONTROL",         REGID_VI_STATUS ),
    [REGID_VI_ORIGIN ] = DEF_IREG(VI_ORIGIN_REG,  "ORIGIN/DRAM_ADDR",       REGID_VI_ORIGIN ),
    [REGID_VI_WIDTH  ] = DEF_IREG(VI_WIDTH_REG,   "WIDTH/H_WIDTH",          REGID_VI_WIDTH  ),
    [REGID_VI_INTR   ] = DEF_IREG(VI_INTR_REG,    "INTERRUPT/V_INTERRUPT",  REGID_VI_INTR   ),
    [REGID_VI_CURRENT] = DEF_IREG(VI_CURRENT_REG, "CURRENT/V_CURRENT_LINE", REGID_VI_CURRENT),
    [REGID_VI_BURST  ] = DEF_IREG(VI_BURST_REG,   "BURST/TIMING",           REGID_VI_BURST  ),
    [REGID_VI_V_SYNC ] = DEF_IREG(VI_V_SYNC_REG,  "V_SYNC",                 REGID_VI_V_SYNC ),
    [REGID_VI_H_SYNC ] = DEF_IREG(VI_H_SYNC_REG,  "H_SYNC",                 REGID_VI_H_SYNC ),
    [REGID_VI_LEAP   ] = DEF_IREG(VI_LEAP_REG,    "LEAP/H_SYNC_LEAP",       REGID_VI_LEAP   ),
    [REGID_VI_H_START] = DEF_IREG(VI_H_START_REG, "H_START/H_VIDEO",        REGID_VI_H_START),
    [REGID_VI_V_START] = DEF_IREG(VI_V_START_REG, "V_START/V_VIDEO",        REGID_VI_V_START),
    [REGID_VI_V_BURST] = DEF_IREG(VI_V_BURST_REG, "V_BURST",                REGID_VI_V_BURST),
    [REGID_VI_X_SCALE] = DEF_IREG(VI_X_SCALE_REG, "X_SCALE",                REGID_VI_X_SCALE),
    [REGID_VI_Y_SCALE] = DEF_IREG(VI_Y_SCALE_REG, "Y_SCALE",                REGID_VI_Y_SCALE),
};
static const RegisterSource sRegisters_VI = DEF_REG_LIST_INTERFACE(
    "VI",
    "Video Interface",
    sRegDesc_VI,
    sRegInfo_VI
);
