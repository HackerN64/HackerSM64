/*
 * ==========================================================================
 * Copyright (c) 1993, Silicon Graphics, Inc.  All Rights Reserved.
 *
 * This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;
 * the contents of this file may not be disclosed to third parties, copied
 * or duplicated in any form, in whole or in part, without the prior written
 * permission of Silicon Graphics, Inc. handleSelectExceptions
 *
 * RESTRICTED RIGHTS LEGEND:
 * Use, duplication or disclosure by the Government is subject to restrictions
 * as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data
 * and Computer Software clause at DFARS 252.227-7013, and/or in similar or
 * successor clauses in the FAR, DOD or NASA FAR Supplement.  Unpublished
 * rights reserved under the Copyright Laws of the United States.
 * ==========================================================================
 */

/**************************************************************************
 *
 *  $Revision: 1.1.1.2 $
 *  $Date: 2002/10/29 08:06:34 $
 *  $Source: /home/routefree/bb/depot/rf/sw/bbplayer/include/dbgproto.h,v $
 *
 **************************************************************************/

#ifndef dbgproto_h
#define dbgproto_h

#ifdef _LANGUAGE_C_PLUS_PLUS
extern "C" {
#endif

#include <dbgdefs.h>

#ident "$Revision: 1.1.1.2 $"

/*
 * NOTE: If you change this file, remember increment the revision below.
 */
#define TV_DBGPROTO_REV 2


#define TV_DBG_MAXREQ_SIZE 1024

/*
 * Higher resolution time information
 */
typedef struct __KKTimeStruct {
    long seconds;
    long nanoseconds;
} KKTimeStruct;

#define MAGIC_PORT 2727 /* clients connect here */

typedef struct __KKObjsStruct {
    TVushort number;        /* number of objects */
    TVid     objects[1];    /* variabel length array of object ids */
} KKObjs;

typedef struct __KKFaultInfoStruct {
    unsigned int  addr;      /* Faulting address on a watchpoint */
    short         major;     /* major classification of fault */
    short         minor;     /* minor classification of fault */
} KKFaultInfo;

typedef struct __KKThreadStatusStruct {
    int            flags;    /* flags */
    short          why;      /* reason for stop */
    short	   what;     /* system call number when why == KK_SYSENTRY */
			     /*  or why == KK_SYSEXIT */
    TVid           tid;      /* thread id */
    TVid           pid;      /* process id */
    unsigned int   instr;    /* current instruction */
    KKFaultInfo    info;     /* info associated with fault */
    int            priority; /* scheduling priority */
    int		   start;    /* used to name the thread */
    int		   rv;	     /* return value from last system call */
    unsigned	   args[6];  /* args to system call */
} KKThreadStatus;

/* values for flags - Thread States */
#define KK_STOPPED      0x1     /* thread is stopped */
#define KK_RUNNABLE     0x2     /* thread is waiting for the cpu */
#define KK_RUNNING      0x4     /* thread is executing in the cpu */
#define KK_WAITING      0x8     /* thread is waiting for non-cpu resource */

/* values for why - reason for being stopped, mutually exclusive */
#define KK_REQUESTED    0x1     /* stopped as a result of stop directive */
#define KK_FAULTED      0x2     /* stopped on incurring a hardware fault */
#define KK_SYSENTRY	0x3	/* stopped on entry to a system call */
#define KK_SYSEXIT	0x4	/* stopped on exit from a system call */
#define KK_NWHY 	0x4	/* number of values for why */

typedef struct __KKRunStruct {
    int          flags;    /* flags */
    unsigned int vaddr;    /* Virtual address at which to resume */
} KKRun;

/* values for flags */
#define KK_STEP        0x1      /* single step thread */
#define KK_VADDR       0x2      /* set PC on resumption from vaddr */
#define KK_ABORT       0x4      /* thread should kill itself when resumed */

typedef struct __KKFaultStruct {
    short majorMask;    /* bit mask giving major fault */
    short minorMask;    /* bit mask giving minor fault */
} KKFault;

/* values for major_ mask */
#define KK_FLTMEM        0x1       /* Memory fault */
#define KK_FLTINSTR      0x2       /* Instruction fault */
#define KK_FLTINTX       0x4       /* Integer exception fault */
#define KK_FLTFPE        0x8       /* Floating point exception fault */

/* types of memory faults */
#define KK_FLTALIGN      0x1       /* Alignment error */
#define KK_FLTACCESS     0x2       /* Memory access violation */
#define KK_FLTBOUNDS     0x4       /* Memory bounds violation */
#define KK_FLTWATCH      0x8       /* User watchpoint */

/* types of instruction faults */
#define KK_FLTILL        0x1       /* Illegal instruction */
#define KK_FLTPRIV       0x2       /* Privileged instruction */
#define KK_FLTTRACE      0x4       /* Trace or breakpoint instruction */

/* types of exception faults - integer and floating point */
#define KK_FLTOVF       0x1       /* overflow */
#define KK_FLTZDIV      0x2       /* zero-divide */

/* types of exceptions - only floating point */
#define KK_FLTUDF      0x4       /* underflow */
#define KK_FLTINXCT    0x8       /* inexact result */
#define KK_FLTINVAL    0xf       /* invalid operand */

typedef struct __KKRegionStruct {
    unsigned int vaddr;		/* Starting virtual address of region */
    unsigned int size;		/* size of region */
    short        flags;		/* access flags */
    unsigned int paddr;		/* physical address of first page in region */
} KKRegion;

/* values for flags - access permissions */
#define KK_READ        0x1      /* read permission */
#define KK_WRITE       0x2      /* write permission */
#define KK_EXEC        0x4      /* execute permission */

#define KK_NGREGS 37
#define KK_NSREGS 40

typedef struct __KKGregSetStruct {
    unsigned int gregs[KK_NGREGS];
} KKGregSet;

typedef struct __KKFPregSetStruct {
    union {
        double       dregs[16];      /* 16 64-bit registers */
        float        fregs[32];      /* 32 32-bit registers */
        unsigned int regs[32];       /* 32 64-bit registers */
    } fpregs;
    unsigned int     fpcsr;
    unsigned int     fppad;
} KKFPregSet;

typedef struct __KKRegisterContextStruct {
    KKGregSet        gregs;          /* general register set */
    KKFPregSet       fpregs;         /* floating point register set */
} KKRegisterContext;

#define	KK_VREGS	32
#define KK_SREGS	40	    /* 32 + 8 special registers */

/*
	sregs[0..31] = scalar registers

	Special Registers (all 32 bits in size):

	sregs[32] = dramDMA
	sregs[33] = memDMA
	sregs[34] = readDMA
	sregs[35] = pc
	sregs[36] = writeDMA
	sregs[37] = status
	sregs[38] = DMAfull
	sregs[39] = DMAbusy
*/
	


typedef struct __KKCpScalarSetStruct {
    unsigned int sregs[KK_SREGS];
} KKCpScalarRegSet;

typedef struct __KKCpVectorSetStruct {
    unsigned int vregs[4 * KK_VREGS];
} KKCpVectorRegSet;

typedef struct __KKCpRegisterContextStruct {
    KKCpScalarRegSet	sregs;
    KKCpVectorRegSet	vregs;
} KKCpRegisterContext;

typedef struct __KKUsageStruct {
    KKTimeStruct tstamp;	/* Wall clock time */
    KKTimeStruct created;	/* Create time */
    KKTimeStruct states[10];
} KKUsage;

/* values for how in __KKSetCommRequestStruct */
#define KK_TTY	0
#define KK_TCP	1
#define KK_NOP	2

/* specify a range of log events to enable or disable */
typedef struct __KKLogEventSetStruct {
    int		from;
    int		to;
} KKLogEventSet;

/* define where log data is flushed */
#define	KK_LOG_FLUSH_TO_FILE	0
#define	KK_LOG_FLUSH_TO_FIFO	1

/* logger status */
typedef struct __KKLogStatusStruct {
    int		bufferType;
    int		bufferSize;
    int		dataSize;
    int		logState;
    int		flushLimit;
} KKLogStatus;

/* process usage data */
typedef struct __KKProcUsageStruct {
    TVid		pid;	
    KKTimeStruct	states[10];
} KKProcUsage;

/*
 * Request codes
 */

#define _KK_REQ_LDXEQ           0
#define _KK_REQ_LISTPROCS       1
#define _KK_REQ_GETXEQNAME      2
#define _KK_REQ_LSTTHREADS      3
#define _KK_REQ_STATUS          4
#define _KK_REQ_STOPPROC        5
#define _KK_REQ_STOPTHREAD      6
#define _KK_REQ_RUNTHREAD       7
#define _KK_REQ_RUNPROC         8
#define _KK_REQ_KILLPROC        9
#define _KK_REQ_SETFAULT       10
#define _KK_REQ_GETFAULT       11
#define _KK_REQ_NUMREGIONS     12
#define _KK_REQ_GETREGIONS     13
#define _KK_REQ_GETGREGSET     14
#define _KK_REQ_SETGREGSET     15
#define _KK_REQ_GETFPREGSET    16
#define _KK_REQ_SETFPREGSET    17
#define _KK_REQ_READMEM        18
#define _KK_REQ_WRITEMEM       19
#define _KK_REQ_SETBKPT        20
#define _KK_REQ_CLRBKPT        21
#define _KK_REQ_LISTBKPTS      22
#define _KK_REQ_NUMWATCH       23
#define _KK_REQ_SETWATCH       24
#define _KK_REQ_GETWATCH       25
#define _KK_REQ_USAGE          26
#define _KK_REQ_GETPATHLIST    27
#define _KK_REQ_GETFRAME       28
#define _KK_REQ_REBOOT	       29
#define _KK_REQ_SETCOMM        30
#define _KK_REQ_SETSYSENTRY    31
#define _KK_REQ_GETSYSENTRY    32
#define _KK_REQ_SETSYSEXIT     33
#define _KK_REQ_GETSYSEXIT     34
#define _KK_REQ_ENABLELOG      35
#define _KK_REQ_DISABLELOG     36
#define _KK_REQ_FLUSHLOG       37
#define _KK_REQ_NEWLOG         38
#define _KK_REQ_GETMASK	       39
#define _KK_REQ_AMCMD 	       40
#define _KK_REQ_REMOTE 	       41
#define _KK_REQ_LOGCONTROL     42
#define _KK_REQ_LOGSTATUS      43
#define _KK_REQ_GETLOGFILE     44
#define _KK_REQ_GETSYSUSAGE    45
#define _KK_REQ_SETTRPT        46
#define _KK_REQ_CLRTRPT        47
#define _KK_REQ_LISTTRPTS      48
#define _KK_REQ_GETSREGSET     49
#define _KK_REQ_SETSREGSET     50
#define _KK_REQ_GETVREGSET     51
#define _KK_REQ_SETVREGSET     52
#define _KK_REQ_COUNT          53             /* must be last */

/* for the type field in KKHeader */
#define _KK_REQUEST		0
#define _KK_REPLY		1
#define _KK_EXCEPTION		2
#define _KK_THREAD_EXIT		3
#define _KK_PROCESS_EXIT	4
#define _KK_CONSOLE		5

typedef struct __KKHeaderStruct {
    int		length;
    char	code;
    char	type;
    short	error;
    char	rev;
    char	method;		/* 0 - cpu,  1 - RCP */
    short	notused2;
} KKHeader;

/*
 *   Request Structures
 */
typedef struct __KKGetFrameRequestStruct {
    KKHeader header;
    int height, width;
    int which;
} KKGetFrameRequest;

typedef struct __KKLoadExecutableRequestStruct {
    KKHeader header;
    char     str[1];
} KKLoadExecutableRequest;

typedef struct __KKObjectRequestStruct {
    KKHeader header;
    TVid     object;
} KKObjectRequest;

typedef struct __KKRunThreadRequestStruct {
    KKHeader header;
    TVid     tid;
    KKRun    actions;
} KKRunThreadRequest;

typedef struct __KKFaultRequestStruct {
    KKHeader header;
    TVid     tid;
    KKFault  fault;
    TVuchar  stopAllThreads;
} KKFaultRequest;

typedef struct __KKGRegsetRequestStruct {
    KKHeader  header;
    TVid      tid;
    KKGregSet registers;
} KKGRegsetRequest;

typedef struct __KKFPRegsetRequestStruct {
    KKHeader   header;
    TVid       tid;
    KKFPregSet registers;
} KKFPRegsetRequest;

typedef struct __KKCpScalarRegsetRequestStruct {
    KKHeader   		header;
    TVid       		tid;
    KKCpScalarRegSet	registers;
} KKCpScalarRegsetRequest;

typedef struct __KKCpVectorRegsetRequestStruct {
    KKHeader   		header;
    TVid       		tid;
    KKCpVectorRegSet	registers;
} KKCpVectorRegsetRequest;

typedef struct __KKReadRequestStruct {
    KKHeader     header;
    TVid         object;
    unsigned int addr;
    unsigned int nbytes;
} KKReadRequest;

typedef struct __KKWriteHeaderStruct {
    KKHeader     header;
    TVid         object;
    unsigned int addr;
    unsigned int nbytes;
} KKWriteHeader;

typedef struct __KKWriteRequestStruct {
    KKWriteHeader	writeHeader;
    char         	buffer[1];
} KKWriteRequest;

typedef struct __KKSetBkptRequestStruct {
    KKHeader     header;
    TVid         object;
    unsigned     int addr;
} KKSetBkptRequest;

typedef struct __KKClrBkptRequestStruct {
    KKHeader     header;
    TVid         object;
    TVid         bp;
} KKClrBkptRequest;

typedef struct __KKWatchRequestStruct {
    KKHeader     header;
    TVid         object;
    unsigned int number;
    KKRegion     wp[1];
} KKWatchRequest;

typedef struct __KKSetCommRequestStruct {
    KKHeader     header;
    unsigned int how;
    unsigned int addr;
} KKSetCommRequest;

typedef struct __KKSysCallSetStruct {
    unsigned int word[8];
} KKSysCallSet;

typedef struct __KKSysCallRequestStruct {
    KKHeader	 header;
    TVid	 pid;
    KKSysCallSet syscallset;
} KKSysCallRequest;

typedef struct __KKLogEventRequestStruct {
    KKHeader     	header;
    unsigned int 	numSet;
    KKLogEventSet	set[1];
} KKLogEventRequest;

typedef struct __KKNewLogRequestStruct {
    KKHeader header;
    char     str[1];
} KKNewLogRequest;

typedef struct __KKRemoteRequestStruct {
    KKHeader header;
    int      len;
    char     buf[16];
} KKRemoteRequest;

typedef struct __KKFlushLogRequestStruct {
    KKHeader 	header;
    int		no_block;
} KKFlushLogRequest;

typedef struct __KKLogControlRequestStruct {
    KKHeader 	header;
    int		command;
    int		arg;
} KKLogControlRequest;

typedef struct __KKSetTrptRequestStruct {
    KKSetBkptRequest	bpRequest;
    unsigned short	code;
} KKSetTrptRequest;

typedef KKClrBkptRequest KKClrTrptRequest;

/*
 * Macros for manipulating sets of flags.
 * sp must be a pointer to KKSysCallSet. 
 * flag must be a member of the enumeration corresponding to *sp.
 */

/* turn on all flags in set */
#define TVFILLSET(sp) \
	{ int _i_ = sizeof(*(sp))/sizeof(unsigned int); \
		while(_i_) ((unsigned int*)(sp))[--_i_] = 0xFFFFFFFF; }

/* turn off all flags in set */
#define TVEMPTYSET(sp) \
	{ int _i_ = sizeof(*(sp))/sizeof(unsigned int); \
		while(_i_) ((unsigned int*)(sp))[--_i_] = 0; }

/* is this set empty? */
#define TVISEMPTYSET(set) \
	(setisempty(set, sizeof(*(set))/sizeof(unsigned int)))

/* turn on specified flag in set */
#define TVADDSET(sp, flag) \
	(((unsigned)((flag)) < 32*sizeof(*(sp))/sizeof(unsigned int)) \
	&& (((unsigned int*)(sp))[((flag))/32] |= (1<<(((flag))%32))))

/* turn off specified flag in set */
#define TVDELSET(sp, flag) \
	(((unsigned)((flag)) < 32*sizeof(*(sp))/sizeof(unsigned int)) \
	&& (((unsigned int*)(sp))[((flag))/32] &= ~(1<<(((flag))%32))))

/* query: != 0 iff flag is turned on in set */
#define TVISMEMBER(sp, flag) \
	(((unsigned)((flag)) < 32*sizeof(*(sp))/sizeof(unsigned int)) \
	&& (((unsigned int*)(sp))[((flag))/32] & (1<<(((flag))%32))))


/*
 *  Event Structures
 */

typedef struct __KKObjectEventStruct {
    KKHeader     header;
    TVid         object;
} KKObjectEvent;

typedef struct __KKObjsEventStruct {
    KKHeader     header;
    TVid         object;
    KKObjs       objs;
} KKObjsEvent;

typedef struct __KKBufferEventStruct {
    KKHeader     header;
    TVid         object;
    char         buffer[1];
} KKBufferEvent;

typedef struct __KKStatusEventStruct {
    KKHeader       header;
    KKThreadStatus status;
} KKStatusEvent;

typedef struct __KKFaultEventStruct {
    KKHeader     header;
    TVid         tid;
    KKFault      fault;
    TVuchar      stopAllThreads;
} KKFaultEvent;

typedef struct __KKNumberEventStruct {
    KKHeader     header;
    TVid         object;
    unsigned int number;
} KKNumberEvent;

typedef struct __KKRegionEventStruct {
    KKHeader     header;
    TVid         object;
    unsigned int number;
    KKRegion     regions[1];
} KKRegionEvent;

typedef struct __KKGregEventStruct {
    KKHeader     header;
    TVid         tid;
    KKGregSet    registers;
} KKGregEvent;

typedef struct __KKFPregEventStruct {
    KKHeader     header;
    TVid         tid;
    KKFPregSet   registers;
} KKFPregEvent;

typedef struct __KKCpSregEventStruct {
    KKHeader     	header;
    TVid         	tid;
    KKCpScalarRegSet	registers;
} KKCpSregEvent;

typedef struct __KKCpVregEventStruct {
    KKHeader     	header;
    TVid         	tid;
    KKCpVectorRegSet	registers;
} KKCpVregEvent;

typedef struct __KKBkptEventStruct {
    KKHeader     header;
    TVid         object;
    TVid         bp;
    unsigned int instruction;
} KKBkptEvent;

typedef struct __KKUsageEventStruct {
    KKHeader     header;
    TVid         tid;
    KKUsage      usage;
} KKUsageEvent;

typedef struct __KKFlushEventStruct {
    KKHeader     header;
    int		 bufferType;
    char         buffer[1];
} KKFlushEvent;

typedef struct __KKLogStatusEventStruct {
    KKHeader     header;
    KKLogStatus  status;
} KKLogStatusEvent;

typedef struct __KKSysUsageEventStruct {
    KKHeader 		header;
    KKTimeStruct	tstamp;
    int			numProc;
    KKProcUsage		usage[1];
} KKSysUsageEvent;

typedef struct __KKStringEventStruct {
    KKHeader 	header;
    char     	str[1];
} KKStringEvent;

typedef KKSysCallRequest KKSysCallEvent;

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif

#endif /* dbgproto_h */
