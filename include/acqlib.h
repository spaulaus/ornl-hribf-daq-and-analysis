#ifndef  ACQLIB_H_
#define  ACQLIB_H_

#include "acqshm.h"
#define  ACQ
#include "ipcdefs.h"

#define  O_WRITE 0
#define  O_READ  1

/*
*    Function Prototypes
*/

void open_acq_ipc_(char *,int *,int *,int );
void open_acq_ipc__(char *,int *,int *,int );
void open_acq_log_(char *,int *,int);
void open_acq_log__(char *,int *,int);
void remove_acq_ipc_(char *,int *,int );
void remove_acq_ipc__(char *,int *,int );
void create_acq_ipc_(char *,int *,int *,int );
void create_acq_ipc__(char *,int *,int *,int );
void write_shm_(unsigned char *,int *,int *,int *);
void write_shm__(unsigned char *,int *,int *,int *);
void read_shm_(unsigned char *,int *,int *,unsigned int *,int *,unsigned int *,
                                                                  int *,int *);
void read_shm__(unsigned char *,int *,int *,unsigned int *,int *,unsigned int *,
                                                                  int *,int *);
void log_msg_(int *, char *, char *, int *, int *);
void log_msg__(int *, char *, char *, int *, int *);
void acq_error_(int *,char *,int );
void acq_error__(int *,char *,int );
void close_shm_(int *);
void close_shm__(int *);
void acq_status_(void);
void acq_status__(void);

void acq_error(int *,char *,int );
int get_acq_key(char *,int,struct acq_keys *,char ***);
struct shm_buf *write_shm(struct shm_buf *,int *);
struct shm_buf *read_shm(int ,struct shm_buf *,int *,int *);
void init_shm(void);
 int open_shm(int,int *);
void close_shm(int );
 int free_read_buf(int,struct shm_buf *);
 int free_write_buf(struct shm_buf *);
void acq_shm_spy(void);
 int zbuf_shm(void);

void openipc_(char *, char *, int *, int *);
void closeipc_();
void readipc_( unsigned char *, int  *, int  *, int  *, int  *);
void writeipc_( unsigned char *, int  *, int  *, int  *);
void shmread_(char **,int *,int *,int *);
void shmrelease_(int *, char **);
void default_vme_(char *,int);

void mt_openro_(char *, int *);
void mt_openrw_(char *, int *);
void mt_close_(int *);
void mt_rew_( int *, int *);
void mt_where_( int *, int *);
void mt_resetpos_( int *, int *);
void mt_rewul_( int *, int *);
void mt_cse_( int *, int *);
void mt_fr_( int *, int *, int *);
void mt_br_( int *, int *, int *);
void mt_ff_( int *, int *, int *);
void mt_bf_( int *, int *, int *);
void mt_weof_( int *, int *);
void mt_getstatus_( int *, int *t, int *, int *, int *, int *);
void dev_getstatus_( int *, unsigned *, unsigned *, int *, int *);

void  acq_tape_ctrl_(char *,int *,int);
void  acq_tape_error(int *,char *,int);
void  acq_tape_error_(int *,char *,int);

/*
*     Global variables
*/

extern struct acq_ids  Ids;      /*  IPC Ids                                */
extern struct shm_use *Shm;      /*  Pointer to shared memory               */

#endif       /* end  ACQLIB_H_  */
