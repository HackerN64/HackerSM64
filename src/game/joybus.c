#include <ultra64.h>
#include <PR/os_internal.h>
#include "config.h"
#include "string.h"
#include "joybus.h"
#include "engine/math_util.h"
#include "game_init.h"
#include "input.h"
#include "rumble.h"

void __osSiGetAccess(void);
void __osSiRelAccess(void);

////////////////////
// contreaddata.c //
////////////////////

// Default N64 Controller Input Poll command:
static __OSContReadFormat sN64WriteFormat = {
    .size.tx            = sizeof(sN64WriteFormat.send),
    .size.rx            = sizeof(sN64WriteFormat.recv),
    .send.cmdID         = CONT_CMD_READ_BUTTON,
    .recv.raw.u8        = { [0 ... (sizeof(sN64WriteFormat.recv.raw.u8) - 1)] = PIF_CMD_NOP }, // 4 bytes of PIF_CMD_NOP (0xFF).
};

// Default GCN Controller Input Short Poll command:
static __OSContGCNShortPollFormat sGCNWriteFormatShort = {
    .size.tx            = sizeof(sGCNWriteFormatShort.send),
    .size.rx            = sizeof(sGCNWriteFormatShort.recv),
    .send.cmdID         = CONT_CMD_GCN_SHORT_POLL,
    .send.analog_mode   = GCN_MODE_3_220,
    .send.rumble        = MOTOR_STOP,
    .recv.raw.u8        = { [0 ... (sizeof(sGCNWriteFormatShort.recv.raw.u8) - 1)] = PIF_CMD_NOP }, // 8 bytes of PIF_CMD_NOP (0xFF).
};

// Default GCN Controller Read Origin command:
static __OSContGCNReadOriginFormat sGCNReadOriginFormat = {
    .size.tx            = sizeof(sGCNReadOriginFormat.send),
    .size.rx            = sizeof(sGCNReadOriginFormat.recv),
    .send.cmdID         = CONT_CMD_GCN_READ_ORIGIN,
    .recv.raw.u8        = { [0 ... (sizeof(sGCNReadOriginFormat.recv.raw.u8) - 1)] = PIF_CMD_NOP }, // 10 bytes of PIF_CMD_NOP (0xFF).
};

// Default GCN Controller Input Calibrate command:
static __OSContGCNCalibrateFormat sGCNCalibrateFormat = {
    .size.tx            = sizeof(sGCNCalibrateFormat.send),
    .size.rx            = sizeof(sGCNCalibrateFormat.recv),
    .send.cmdID         = CONT_CMD_GCN_CALIBRATE,
    .send.analog_mode   = GCN_MODE_3_220,
    .send.rumble        = MOTOR_STOP,
    .recv.raw.u8        = { [0 ... (sizeof(sGCNCalibrateFormat.recv.raw.u8) - 1)] = PIF_CMD_NOP }, // 10 bytes of PIF_CMD_NOP (0xFF).
};

// Default GCN Controller Input Long Poll command:
static __OSContGCNLongPollFormat sGCNWriteFormatLong = {
    .size.tx            = sizeof(sGCNWriteFormatLong.send),
    .size.rx            = sizeof(sGCNWriteFormatLong.recv),
    .send.cmdID         = CONT_CMD_GCN_LONG_POLL,
    .send.analog_mode   = GCN_MODE_3_220,
    .send.rumble        = MOTOR_STOP,
    .recv.raw.u8        = { [0 ... (sizeof(sGCNWriteFormatLong.recv.raw.u8) - 1)] = PIF_CMD_NOP }, // 10 bytes of PIF_CMD_NOP (0xFF).
};

static void __osPackRead_impl(u8 cmdID);

/**
 * @brief Implementation for PIF pack handlers.
 *
 * @param[in] mq    The SI event message queue.
 * @param[in] cmdID The command ID to run (see enum OSContCmds).
 * @returns Error status: -1 = busy, 0 = success.
 */
s32 osStartRead_impl(OSMesgQueue* mq, u8 cmdID) {
    s32 ret = 0;

    __osSiGetAccess();

    // If this was called twice in a row, there is no need to write the command again.
    if (__osContLastCmd != cmdID) {
        // Run the pack command.
        __osPackRead_impl(cmdID);

        // Write __osContPifRam to the PIF RAM.
        ret = __osSiRawStartDma(OS_WRITE, &__osContPifRam);

        // Wait for the command to execute.
        osRecvMesg(mq, NULL, OS_MESG_BLOCK);
    }

    // Read the resulting __osContPifRam from the PIF RAM.
    ret = __osSiRawStartDma(OS_READ, &__osContPifRam);

    __osContLastCmd = cmdID;

    __osSiRelAccess();

    return ret;
}

#define WRITE_PIF_CMD(dst, src) {   \
    (*(typeof(src)*)(dst)) = (src); \
    (dst) += sizeof(src);           \
}

#define WRITE_PIF_CMD_WITH_GCN_RUMBLE(dst, src, port) {                 \
    (*(typeof(src)*)(dst)) = (src);                                     \
    (*(typeof(src)*)(dst)).send.rumble = gRumbleInfos[port].motorState; \
    (dst) += sizeof(src);                                               \
}

/**
 * @brief Writes PIF commands to poll controller inputs depending on the command.
 * Called by osContStartReadData and osStartRead_impl.
 *
 * @param[in] cmdID The command ID to run (see enum OSContCmds).
 */
static void __osPackRead_impl(u8 cmdID) {
    u8* ptr = (u8*)__osContPifRam.ramarray;

    bzero(__osContPifRam.ramarray, sizeof(__osContPifRam.ramarray));
    __osContPifRam.pifstatus = PIF_STATUS_EXE;

    for (int port = 0; port < __osMaxControllers; port++) {
        OSContPadEx* pad = &gControllerPads[port];
        u16 type = gControllerStatuses[port].type;

        // Make sure this port has a controller plugged in, and if not status repolling, only poll assigned ports.
        if ((type != CONT_NONE) && (gContStatusPolling || (pad->playerNum != 0))) {
            _Bool isGCN = (type & CONT_CONSOLE_GCN);

            switch (cmdID) {
                case CONT_CMD_READ_BUTTON: // Instead of running these commands separately, run one or the other depending on the connected controller type for each port.
                case CONT_CMD_GCN_SHORT_POLL:
                    if (isGCN) {
                        WRITE_PIF_CMD_WITH_GCN_RUMBLE(ptr, sGCNWriteFormatShort, port);
                    } else {
                        WRITE_PIF_CMD(ptr, sN64WriteFormat);
                    }
                    break;

                case CONT_CMD_GCN_READ_ORIGIN:
                    if (isGCN && !pad->origins.initialized) {
                        WRITE_PIF_CMD(ptr, sGCNReadOriginFormat);
                    } else {
                        ptr++; // Not a GCN controller, or doesn't need origins updated, so leave a PIF_CMD_SKIP_CHNL (0x00) byte to tell the PIF to skip it.
                    }
                    break;

                case CONT_CMD_GCN_CALIBRATE:
                    if (isGCN && !pad->origins.initialized) {
                        WRITE_PIF_CMD_WITH_GCN_RUMBLE(ptr, sGCNCalibrateFormat, port);
                    } else {
                        ptr++; // Not a GCN controller, or doesn't need origins updated, so leave a PIF_CMD_SKIP_CHNL (0x00) byte to tell the PIF to skip it.
                    }
                    break;

                case CONT_CMD_GCN_LONG_POLL:
                    if (isGCN) {
                        WRITE_PIF_CMD_WITH_GCN_RUMBLE(ptr, sGCNWriteFormatLong, port);
                    } else {
                        WRITE_PIF_CMD(ptr, sN64WriteFormat);
                    }
                    break;

                default:
                    osSyncPrintf("__osPackRead_impl error: Unimplemented input poll command: %.02X (port %d)\n", cmdID, port);
                    *ptr = PIF_CMD_END;
                    return;
            }
        } else {
            ptr++; // Empty channel/port, so leave a PIF_CMD_SKIP_CHNL (0x00) byte to tell the PIF to skip it.
        }
    }

    *ptr = PIF_CMD_END;
}

/**
 * @brief Updates the analog origins data for a GCN controller pad.
 *
 * @param[in,out] origins The origins struct to modify.
 * @param[in    ] stick   The raw GCN analog stick data.
 * @param[in    ] c_stick The raw GCN C-stick data.
 * @param[in    ] trig    The raw GCN analog triggers data.
 */
static void set_gcn_origins(OSContOrigins* origins, Analog_u8 stick, Analog_u8 c_stick, Analog_u8 trig) {
    origins->initialized = TRUE;
    origins->stick       = stick;
    origins->c_stick     = c_stick;
    origins->trig        = trig;
}

/**
 * @brief Writes controller data to OSContPadEx and stores the controller center on first run.
 * Called by osContGetReadDataEx.
 *
 * @param[out] pad     The controller pad to write data to.
 * @param[in ] gcn     The raw GCN button data.
 * @param[in ] stick   The raw GCN analog stick data.
 * @param[in ] c_stick The raw GCN C-stick data.
 * @param[in ] trig    The raw GCN analog triggers data.
 */
static void __osContReadGCNInputData(OSContPadEx* pad, GCNButtons gcn, Analog_u8 stick, Analog_u8 c_stick, Analog_u8 trig) {
    OSContOrigins* origins = &pad->origins;
    OSContButtons buttons = { .raw = 0x0000 };
    Analog_u8 origins_stick   = ANALOG_U8_ZERO;
    Analog_u8 origins_c_stick = ANALOG_U8_ZERO;
    Analog_u8 origins_trig    = ANALOG_U8_ZERO;

    // If the GET_ORIGIN bit is set, that means the controller has new analog origins data and either CONT_CMD_GCN_READ_ORIGIN or CONT_CMD_GCN_CALIBRATE needs to be run to get the new data.
    // The first frame after boot will use the above origins until the proper command runs later in the frame.
    if (gcn.standard.GET_ORIGIN) {
        origins->initialized = FALSE;
    }

    // If the USE_ORIGIN bit is set, use the provided origins. Otherwise assume the origins are all zero.
    if (gcn.standard.USE_ORIGIN) {
        origins_stick   = origins->stick;
        origins_c_stick = origins->c_stick;
        origins_trig    = origins->trig;
    }

    // Write the analog data.
    pad->stick   = ANALOG_S8_CENTER(stick,   origins_stick  );
    pad->c_stick = ANALOG_S8_CENTER(c_stick, origins_c_stick);
    pad->trig    = ANALOG_U8_CENTER(trig,    origins_trig   );

    // Map GCN button bits to N64 button bits.
    buttons.A       = gcn.standard.A;
    buttons.B       = gcn.standard.B;
    buttons.Z       = (gcn.standard.L || (trig.l > GCN_TRIGGER_THRESHOLD)); // Swap L and Z.
    buttons.START   = gcn.standard.START;
    buttons.D.UP    = gcn.standard.D.UP;
    buttons.D.DOWN  = gcn.standard.D.DOWN;
    buttons.D.LEFT  = gcn.standard.D.LEFT;
    buttons.D.RIGHT = gcn.standard.D.RIGHT;
    buttons.X       = gcn.standard.X; // This bit was previously set when L+R+START was pressed on a standard N64 controller to recalibrate the analog stick (which also unsets the START bit).
    buttons.Y       = gcn.standard.Y; // This bit was unused by the N64 controller.
    buttons.L       = gcn.standard.Z; // Swap L and Z.
    buttons.R       = gcn.standard.R;
    buttons.C.UP    = (pad->c_stick.y >  GCN_C_STICK_THRESHOLD);
    buttons.C.DOWN  = (pad->c_stick.y < -GCN_C_STICK_THRESHOLD);
    buttons.C.LEFT  = (pad->c_stick.x < -GCN_C_STICK_THRESHOLD);
    buttons.C.RIGHT = (pad->c_stick.x >  GCN_C_STICK_THRESHOLD);

    // Write the button data.
    pad->button.raw = buttons.raw;
    pad->rawContButtons = gcn.raw;

    // Write the non-button data.
    pad->ex.gcn.standard.ERRSTAT    = gcn.standard.ERRSTAT;
    pad->ex.gcn.standard.ERRLATCH   = gcn.standard.ERRLATCH;
    pad->ex.gcn.standard.GET_ORIGIN = gcn.standard.GET_ORIGIN;
    pad->ex.gcn.standard.USE_ORIGIN = gcn.standard.USE_ORIGIN;
}

/**
 * @brief Reads PIF command result written by __osPackRead_impl and converts it into OSContPadEx data.
 * Modified from vanilla libultra to handle GameCube controllers, skip empty/unassigned ports,
 *   and trigger status polling if an active controller is unplugged.
 * Called by poll_controller_inputs.
 *
 * @param[in,out] pad A pointer to the data for the 4 controller pads.
 */
void osContGetReadDataEx(OSContPadEx* pad) {
    u8* ptr = (u8*)__osContPifRam.ramarray;
    __OSContGenericFormat *readformatptr = NULL;
    N64InputData n64Input;
    GCNInputData gcnInput;

    while (*ptr != PIF_CMD_END) {
        if (
            *ptr == PIF_CMD_SKIP_CHNL ||
            *ptr == PIF_CMD_RESET_CHNL
        ) {
            // Skip empty channels/ports.
            pad++;
            ptr++;
            continue;
        }
        if (
            *ptr == PIF_CMD_NOP
        ) {
            // Skip bytes that are PIF_CMD_NOP (0xFF).
            ptr++;
            continue;
        }

        // Not a special byte, so read a poll command:
        readformatptr = (__OSContGenericFormat*)ptr;
        pad->errno = CHNL_ERR(readformatptr->size);

        // If the controller being read was unplugged, start status polling on all 4 ports.
        if (pad->errno == (CHNL_ERR_NORESP >> 4)) {
            start_controller_status_polling(FALSE);
            return;
        }

        OSContOrigins* origins = &pad->origins;

        // Handle different types of poll commands:
        switch (readformatptr->send.cmdID) {
            case CONT_CMD_READ_BUTTON:
                if (pad->errno == (CHNL_ERR_SUCCESS >> 4)) {
                    n64Input = (*(__OSContReadFormat*)ptr).recv.input;

                    pad->rawContButtons = n64Input.buttons.raw;

                    pad->button.raw = (n64Input.buttons.raw & ~(CONT_RESET | CONT_UNUSED)); // These two bits are repurposed to X and Y on the virtual controller, so make sure the game doesn't read an X button press when resetting.
                    // Allow the game to read the start button press that happens when resetting the analog stick.
                    if (n64Input.buttons.standard.RESET) {
                        pad->button.START = TRUE;
                    }
                    // Move these two bits to the other struct.
                    pad->ex.n64.standard.RESET  = n64Input.buttons.standard.RESET;
                    pad->ex.n64.standard.unused = n64Input.buttons.standard.unused;
                    // Standard N64 controller only has one analog stick.
                    pad->stick                  = n64Input.stick;
                    pad->c_stick                = ANALOG_S8_ZERO;
                    pad->trig                   = ANALOG_U8_ZERO;
                }

                ptr += sizeof(__OSContReadFormat);
                break;

            case CONT_CMD_GCN_SHORT_POLL:
                if (pad->errno == (CHNL_ERR_SUCCESS >> 4)) {
                    gcnInput = (*(__OSContGCNShortPollFormat*)ptr).recv.input;
                    u8 analog_mode = (*(__OSContGCNShortPollFormat*)ptr).send.analog_mode;
                    Analog_u8 c_stick, trig;

                    // The GameCube controller has various modes for returning the lower analog bits (4 bits per axis vs. 8 bits per axis).
                    switch (analog_mode) {
                        default: // GCN_MODE_0_211, GCN_MODE_5_211, GCN_MODE_6_211, GCN_MODE_7_211
                            c_stick = gcnInput.m0.c_stick;
                            trig    = ANALOG_U4_TO_U8(gcnInput.m0.trig);
                            break;
                        case GCN_MODE_1_121:
                            c_stick = ANALOG_U4_TO_U8(gcnInput.m1.c_stick);
                            trig    = gcnInput.m1.trig;
                            break;
                        case GCN_MODE_2_112:
                            c_stick = ANALOG_U4_TO_U8(gcnInput.m2.c_stick);
                            trig    = ANALOG_U4_TO_U8(gcnInput.m2.trig);
                            break;
                        case GCN_MODE_3_220:
                            c_stick = gcnInput.m3.c_stick;
                            trig    = gcnInput.m3.trig;
                            break;
                        case GCN_MODE_4_202:
                            c_stick = gcnInput.m3.c_stick;
                            trig    = ANALOG_U8_ZERO;
                            break;
                    }

                    __osContReadGCNInputData(pad, gcnInput.buttons, gcnInput.stick, c_stick, trig);
                } else {
                    origins->initialized = FALSE;
                }

                ptr += sizeof(__OSContGCNShortPollFormat);
                break;

            case CONT_CMD_GCN_READ_ORIGIN:
                if (pad->errno == (CHNL_ERR_SUCCESS >> 4)) {
                    gcnInput = (*(__OSContGCNReadOriginFormat*)ptr).recv.origins;

                    set_gcn_origins(origins, gcnInput.stick, gcnInput.c_stick, gcnInput.trig);
                } else {
                    origins->initialized = FALSE;
                }

                ptr += sizeof(__OSContGCNReadOriginFormat);
                break;

            case CONT_CMD_GCN_CALIBRATE:
                if (pad->errno == (CHNL_ERR_SUCCESS >> 4)) {
                    gcnInput = (*(__OSContGCNCalibrateFormat*)ptr).recv.origins;

                    set_gcn_origins(origins, gcnInput.stick, gcnInput.c_stick, gcnInput.trig);
                } else {
                    origins->initialized = FALSE;
                }

                ptr += sizeof(__OSContGCNCalibrateFormat);
                break;

            case CONT_CMD_GCN_LONG_POLL:
                if (pad->errno == (CHNL_ERR_SUCCESS >> 4)) {
                    gcnInput = (*(__OSContGCNLongPollFormat*)ptr).recv.input;

                    // Long poll returns 8 bits for all analog axes (equivalent to mode 3 but with 2 more bytes for the usually unused analog buttons (1 byte each)).
                    __osContReadGCNInputData(pad, gcnInput.buttons, gcnInput.stick, gcnInput.c_stick, gcnInput.trig);
                } else {
                    origins->initialized = FALSE;
                }

                ptr += sizeof(__OSContGCNLongPollFormat);
                break;

            default:
                osSyncPrintf("osContGetReadDataEx error: Unimplemented input poll command: %.02X\n", readformatptr->send.cmdID);
                return;
        }

        pad++;
    }
}

/////////////////
// contquery.c //
/////////////////

void __osContGetInitData(u8* pattern, OSContStatus* status);

/**
 * @brief Read status query data written by osContStartQuery.
 * odified from vanilla libultra to return the bitpattern returned by __osContGetInitData, similar to osContInit.
 * Called by poll_controller_statuses.
 *
 * @param[out] bitpattern The first 4 bits correspond to which 4 ports have controllers plugged in (low-high).
 * @param[out] status     A pointer to the 4 controller statuses.
 */
void osContGetQueryEx(u8* bitpattern, OSContStatus* status) {
    __osContGetInitData(bitpattern, status);
}

/////////////
// motor.c //
/////////////

// A buffer to hold separate rumble commands for each port.
ALIGNED64 static OSPifRamEx __MotorDataBuf[MAXCONTROLLERS];

/**
 * @brief Turns controller rumble on or off.
 * Modified from vanilla libultra to handle GameCube controller rumble.
 * Called by osMotorStart, osMotorStop, and osMotorStopHard via macro.
 *
 * @param[in] pfs        A pointer to a buffer for the controller pak (AKA rumble pak) file system.
 * @param[in] motorState MOTOR_STOP = stop motor, MOTOR_START = start motor, MOTOR_STOP_HARD (GCN only) = motor brake.
 * @returns PIF error status.
 */
s32 __osMotorAccessEx(OSPfs* pfs, s32 motorState) {
    s32 err = PFS_ERR_SUCCESS;
    int channel = pfs->channel;
    u8* ptr = (u8*)&__MotorDataBuf[channel];

    // Make sure the rumble pak pfs was initialized.
    if (!(pfs->status & PFS_MOTOR_INITIALIZED)) {
        return PFS_ERR_INVALID;
    }

    // Check whether the controller is a GCN controller.
    if (gControllerStatuses[channel].type & CONT_CONSOLE_GCN) {
        // GCN rumble is set in the input poll command by motorState in gRumbleInfos.

        // Change the last command ID so that input poll command (which includes the rumble byte) gets written again next frame.
        __osContLastCmd = PIF_CMD_END;
    } else { // N64 Controllers.
        // N64 rumble pak can only use MOTOR_STOP or MOTOR_START.
        motorState &= MOTOR_MASK_N64;

        __osSiGetAccess();

        // Set the PIF to be ready to run a command.
        __MotorDataBuf[channel].pifstatus = PIF_STATUS_EXE;

        // Leave a PIF_CMD_SKIP_CHNL (0x00) byte in __MotorDataBuf for each skipped channel.
        ptr += channel;

        __OSContRamWriteFormatAligned* readformat = (__OSContRamWriteFormatAligned*)ptr;

        // Set the entire block to either MOTOR_STOP or MOTOR_START.
        memset(readformat->fmt.send.data, motorState, sizeof(readformat->fmt.send.data));

        __osContLastCmd = PIF_CMD_END;

        // Write __MotorDataBuf to the PIF RAM and then wait for the command to execute.
        __osSiRawStartDma(OS_WRITE, &__MotorDataBuf[channel]);
        osRecvMesg(pfs->queue, NULL, OS_MESG_BLOCK);

        // Read the resulting __MotorDataBuf from the PIF RAM and then wait for the command to execute.
        __osSiRawStartDma(OS_READ, &__MotorDataBuf[channel]);
        osRecvMesg(pfs->queue, NULL, OS_MESG_BLOCK);

        // Check for errors.
        err = CHNL_ERR(readformat->fmt.size);
        if (err == (CHNL_ERR_SUCCESS >> 4)) {
            if (motorState == MOTOR_STOP) {
                if (readformat->fmt.recv.datacrc != 0x00) { // 0xFF = Disconnected.
                    err = PFS_ERR_CONTRFAIL; // "Controller pack communication error"
                }
            } else { // MOTOR_START
                if (readformat->fmt.recv.datacrc != 0xEB) { // 0x14 = Uninitialized.
                    err = PFS_ERR_CONTRFAIL; // "Controller pack communication error"
                }
            }
        }

        __osSiRelAccess();
    }

    return err;
}

u8 __osContAddressCrc(u16 addr);
s32 __osPfsSelectBank(OSPfs* pfs, u8 bank);
s32 __osContRamRead(OSMesgQueue* mq, int channel, u16 address, u8* buffer);

/**
 * @brief Writes PIF commands to control the rumble pak.
 * Unmodified from vanilla libultra.
 * Called by osMotorInit and osMotorInitEx.
 *
 * @param[in] channel The port ID to operate on.
 * @param[in] mdata   A pointer to a buffer for the PIF RAM command data.
 */
static void _MakeMotorData(int channel, OSPifRamEx* mdata) {
    u8* ptr = (u8*)mdata->ramarray;
    __OSContRamWriteFormatAligned ramwriteformat;
    int i;

    ramwriteformat.align0          = PIF_CMD_NOP;
    ramwriteformat.fmt.size.tx     = sizeof(ramwriteformat.fmt.send);
    ramwriteformat.fmt.size.rx     = sizeof(ramwriteformat.fmt.recv);
    ramwriteformat.fmt.send.cmdID  = CONT_CMD_WRITE_MEMPAK;
    ramwriteformat.fmt.send.addr.h = (CONT_BLOCK_RUMBLE >> 3);
    ramwriteformat.fmt.send.addr.l = (u8)(__osContAddressCrc(CONT_BLOCK_RUMBLE) | (CONT_BLOCK_RUMBLE << 5));

    // Leave a PIF_CMD_SKIP_CHNL (0x00) byte in mdata->ramarray for each skipped channel.
    if (channel != 0) {
        for (i = 0; i < channel; i++) {
            *ptr++ = PIF_CMD_SKIP_CHNL;
        }
    }

    *(__OSContRamWriteFormatAligned*)ptr = ramwriteformat;
    ptr += sizeof(__OSContRamWriteFormatAligned);
    *ptr = PIF_CMD_END;
}

/**
 * @brief Initializes the Rumble Pak.
 * Modified from vanilla libultra to ignore GameCube controllers.
 * Called by thread6_rumble_loop and cancel_rumble.
 *
 * @param[in ] mq      The SI event message queue.
 * @param[out] pfs     A pointer to a buffer for the controller pak (AKA rumble pak) file system.
 * @param[in ] channel The port ID to operate on.
 * @returns PFS error status.
 */
s32 osMotorInitEx(OSMesgQueue* mq, OSPfs* pfs, int channel) {
    s32 err = PFS_ERR_SUCCESS;
    u8 data[BLOCKSIZE];

    pfs->status     = PFS_STATUS_NONE;
    pfs->queue      = mq;
    pfs->channel    = channel;
    pfs->activebank = ACCESSORY_ID_NULL;

    // Make sure the controller is not a GCN controller.
    if (!(gControllerStatuses[channel].type & CONT_CONSOLE_GCN)) {
        // Write probe value (ensure Transfer Pak is turned off).
        err = __osPfsSelectBank(pfs, ACCESSORY_ID_TRANSFER_OFF);
        if (err == PFS_ERR_NEW_PACK) {
            // Write probe value (Rumble bank).
            err = __osPfsSelectBank(pfs, ACCESSORY_ID_RUMBLE);
        }
        if (err != PFS_ERR_SUCCESS) {
            return err;
        }

        // Read probe value (1).
        err = __osContRamRead(mq, channel, CONT_BLOCK_DETECT, data);
        if (err == PFS_ERR_NEW_PACK) {
            err = PFS_ERR_CONTRFAIL; // "Controller pack communication error"
        }
        if (err != PFS_ERR_SUCCESS) {
            return err;
        }

        // Ensure the accessory is not a turned off Transfer Pak.
        if (data[BLOCKSIZE - 1] == ACCESSORY_ID_TRANSFER_OFF) {
            return PFS_ERR_DEVICE; // Wrong device.
        }

        // Write probe value (Rumble bank).
        err = __osPfsSelectBank(pfs, ACCESSORY_ID_RUMBLE);
        if (err == PFS_ERR_NEW_PACK) {
            err = PFS_ERR_CONTRFAIL; // "Controller pack communication error"
        }
        if (err != PFS_ERR_SUCCESS) {
            return err;
        }

        // Read probe value (2).
        err = __osContRamRead(mq, channel, CONT_BLOCK_DETECT, data);
        if (err == PFS_ERR_NEW_PACK) {
            err = PFS_ERR_CONTRFAIL; // "Controller pack communication error"
        }
        if (err != PFS_ERR_SUCCESS) {
            return err;
        }

        // Ensure the accessory is a Rumble Pak.
        if (data[BLOCKSIZE - 1] != ACCESSORY_ID_RUMBLE) {
            return PFS_ERR_DEVICE; // Wrong device.
        }

        // Write the PIF command.
        if (!(pfs->status & PFS_MOTOR_INITIALIZED)) {
            _MakeMotorData(channel, &__MotorDataBuf[channel]);
        }
    }

    pfs->status = PFS_MOTOR_INITIALIZED;

    return PFS_ERR_SUCCESS;
}
