#ifndef  IPCDEFS_H_
#define  IPCDEFS_H_

#include <sys/types.h>
/*
*   Error codes from ipclib routines
*/
#define  NODEVICE    1
#define  NOTBLSPACE  2
#define  NOMEMORY    3
#define  TOOLARGE    4
#define  NOPERM      5
#define  NOTOWNER    6
#define  NOEXIST     7
#define  TOOMANY     8
#define  TOOFEW      9

/*
*   ipclib prototypes
*/
int create_shm(key_t ,int ,int *);
int attach_shm(key_t ,int *,int *);
int create_msg(key_t ,int *);
int attach_msg(key_t ,int *);
int create_sem(key_t ,int,int *);
int attach_sem(key_t ,int,int *);
int remove_msg(key_t ,int *);
int remove_sem(key_t ,int *);
void *map_shm(int,int *);

#ifdef  HIS
#define  HISTOGRAM  0x02
#endif

#ifdef  ACQ
struct acq_keys {
        key_t dat_shm;
        key_t dat_sem;
        key_t log_msg;
        key_t tape_msg;
};

struct acq_ids {
        int dat_shm;
        int dat_sem;
        int log_msg;
        int tape_msg;
};

struct acq_ipcs {
        char *name;
        struct acq_keys keyval;
};

#endif

/*
*        RMS data structures
*/
#ifdef  RMS
struct rms_keys {
        key_t dat_shm;
        key_t log_msg;
};

struct rms_ids {
        int dat_shm;
        int log_msg;
};

struct rms_ipcs {
        char *name;
        struct rms_keys keyval;
};

#endif

#endif      /* end IPCDEFS_H_   */
