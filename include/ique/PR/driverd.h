/*
 * driverd.h: driver daemon declarations & struct definitions.
 *
 * Copyright 1995, Silicon Graphics, Inc.
 * All Rights Reserved.
 *
 * This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;
 * the contents of this file may not be disclosed to third parties, copied or
 * duplicated in any form, in whole or in part, without the prior written
 * permission of Silicon Graphics, Inc.
 *
 * RESTRICTED RIGHTS LEGEND:
 * Use, duplication or disclosure by the Government is subject to restrictions
 * as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data
 * and Computer Software clause at DFARS 252.227-7013, and/or in similar or
 * successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -
 * rights reserved under the Copyright Laws of the United States.
 *
 */

/**************************************************************************
 *
 *  $Revision: 1.1.1.2 $
 *  $Date: 2002/10/29 08:06:34 $
 *  $Source: /home/routefree/bb/depot/rf/sw/bbplayer/include/driverd.h,v $
 *
 **************************************************************************/


#define MAXCLIENTS	5

/*
 * Same numbers as videod, bumped up by one each.
 */
#define DRIVERD_TCP_PORT 8001
#define DRIVERD_RPC_PORT 391012

/*
 * Daemon must keep track of each client which has attached; it does so by
 * maintaining an array of 'em.
 */

typedef struct _Client *ClientPtr;

typedef struct _Client {

    int fd;	/* File descriptor of private socket between daemon/client */
    id_t id;	/* Process ID of the client (so that we can signal them) */
    /*
     * Event range (6 bit numbers) that this client is interested in; when
     * the game sends the daemon an event via a socket (that the emulator
     * opens with the daemon), the daemon checks to see which clients are
     * interested in this event, then writes to their sockets to indicate
     * that the event came in.  The clients then wake up, realize an event
     * has occurred, & query the daemon to find out what happened (just as
     * they'll have to query the device driver via ioctl to find out what
     * happened).
     */
    int min_event;
    int max_event;
    int index;
} ClientRec;

/*
 * We need a special one byte header on all writes to the private socket
 * between daemon & client to differentiate between the three actions we
 * expect the daemon to perform:
 *
 * Session registration (new clients identify themselves & pass pid to daemon)
 *
 * Events from Game (emulator writes events on private socket)
 *
 * Commands to Game (daemon receives commands from host side clients, 
 * arbitrates for control of ramrom, writes the command into ramrom, 
 * sends a signal to the game to tell it to read the ramrom arena, then
 * waits for an acknowledge to come back from the game indicating that the
 * command has been accepted (so that the daemon could begin a new command
 * arbitration if needed).
 */

#define DRIVERD_REGISTER_SESSION	101
#define DRIVERD_REGISTER_EMULATOR	102
#define DRIVERD_EVENT_FROM_GAME		103
#define DRIVERD_IOCTL_REQUEST		104

typedef struct _registerSession *registerSessionPtr;

typedef struct _registerSession {

    unsigned int dd_header; /* One of the three possible actions for daemon*/

    id_t id;	/* Process ID of the client (so that we can signal them) */
    /*
     * Event range (6 bit numbers) that this client is interested in; when
     * the game sends the daemon an event via a socket (that the emulator
     * opens with the daemon), the daemon checks to see which clients are
     * interested in this event, then writes to their sockets to indicate
     * that the event came in.  The clients then wake up, realize an event
     * has occurred, & query the daemon to find out what happened (just as
     * they'll have to query the device driver via ioctl to find out what
     * happened).
     */
} RegisterSession;

typedef struct _registerEmulator {

    unsigned int dd_header; /* One of the possible actions for daemon*/

    id_t id;	/* Process ID of emulate (so that we can signal them) */
    int shmkey; /* Shared memory key */
} RegisterEmulator;

typedef struct _eventFromGame *eventFromGamePtr;

typedef struct _eventFromGame {
    unsigned int dd_header; /* One of the possible actions for daemon*/
    int event;		    /* Signed value */
} EventFromGame;

typedef struct _commandToGame *commandToGamePtr;

typedef struct _commandToGame {
    unsigned int dd_header; /* One of the possible actions for daemon*/
    /*
     * XXX
     */
} CommandToGame;

typedef struct _ioctlRequest {
    unsigned int dd_header; /* One of the possible actions for daemon*/
    int request;
} IoctlRequest;

extern int MakeDriverDConnection(char *phostname, int iserver, int retries, int *familyp);		/* RETURN */
