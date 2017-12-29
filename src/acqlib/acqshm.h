#ifndef  ACQSHM_H_
#define  ACQSHM_H_

#define  NUMPROC 10
#define  NUMBUF  16
#define  MAXBUFSIZE 32768
#define  BUFSIZE 8192
#define  LOGINTERVAL 300

#define  NUMSEM  (NUMPROC+2) 
#define  SEMLOCK  0
#define  WRWAKE   1
#define  RDWAKE   2

struct shm_use {
         int log_interval;   /* Log interval in seconds                       */
         int max_buf_size;   /* Max buffer size in bytes                      */
         int buf_size;       /* Current buffer size in bytes                  */
         int num_buf;        /* Number of buffers in use                      */
         int num_proc;       /* Number of read processes allowed              */
         int WRpid;          /* PID of the write process                      */
         int WRwait;         /* nonzero means waiting for write buffer        */
         int WRbuf;          /* Number of current write buffer                */
unsigned int WRuse;          /* Write buffers in use                          */
         int BitMsk[NUMBUF];
         int RDwait;         /* nonzero means waiting for a read buffer       */
unsigned int RDavail[NUMPROC]; /* Read buffers avail                          */
unsigned int RDuse[NUMPROC]; /* Read buffers in use                           */
         int RDpids[NUMPROC];  /* PIDs of read processes                      */
         int pad[10];
};

struct shm_buf {
unsigned int event_num;  /* Event number of last event + 1                    */
         int events;     /* Number of events in this buffer                   */
         int size;       /* Buffer size in bytes                              */
unsigned int buf_num;    /* Buffer number                                     */
unsigned short data[MAXBUFSIZE/2];  /* data part                                 */
         char pad[1500]; /* room for 1 Ether packet                           */
};

#endif     /* end ACQSHM_H_  */
