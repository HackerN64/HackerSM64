
/*
 * rsp_ipc.h
 *
 * Mon Mar 13 10:24:35 PST 1995
 *
 *
 */

/**************************************************************************
 *
 *  $Revision: 1.1.1.2 $
 *  $Date: 2002/10/29 08:06:34 $
 *  $Source: /home/routefree/bb/depot/rf/sw/bbplayer/include/rsp_ipc.h,v $
 *
 **************************************************************************/

#ifndef _RSP_IPC_H_
#define _RSP_IPC_H_ 1

#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)

typedef enum
{
    CMD_NULL,
    CMD_CMD,
    CMD_QUIT,
    CMD_DMA_READ,
    CMD_DMA_WRITE,
    CMD_READ_WORD,
    CMD_WRITE_WORD,
    CMD_LAST
} rsp_shm_cmd_t;

typedef struct
{
    unsigned long int	dram[(2048 * 1024)>>2];	/* share DRAM */
    /*
     * howardc - Keep buff right after the dram to keep it 64b aligned. This
     * is the fake DMA IPC buffer to communicate to rspsim. If 64b aligned, the
     * _emAlign64b (64bit alignment checking code) will properly check for
     * alignment.
     */
    unsigned long int	buff[1024];	/* could be IMEM or DMEM */
    char		string[256];	/* pass command string */
    rsp_shm_cmd_t	cmd;		/* what to do */
    int			reg;		/* which register */
    unsigned long int	addr;		/* RSP memory address for copy */
    unsigned long int	length;		/* length of data to be copied */
} rsp_shm_buf_t;


/*
 * simple semaphore interface, stolen from Stevens' Networking book.
 *
 * Tue Feb  7 13:38:49 PST 1995
 *
 */
extern int	sem_create(key_t key, int initval);
extern int	sem_open(key_t key);
extern int	sem_rm(int id);
extern int	sem_close(int id);
extern int	sem_wait(int id);
extern int	sem_signal(int id);
extern int	sem_op(int id, int value);

#endif

#endif
