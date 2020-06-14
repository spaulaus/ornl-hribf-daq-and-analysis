#include <stdio.h>
#include <stdlib.h>

#include  "../Dshm/ipcdefs.h"

#ifdef  ACQ

#define  IPC_ACQ  ((0x2<<24) + 45568)

static struct acq_ipcs acq_dev[21] = {
          "vme1",{IPC_ACQ + 1*20,
                  IPC_ACQ + 1*20,
                  IPC_ACQ + 1*20 + 1,
                  IPC_ACQ + 1*20 + 2 },
          "vme2",{IPC_ACQ + 2*20,
                  IPC_ACQ + 2*20,
                  IPC_ACQ + 2*20 + 1,
                  IPC_ACQ + 2*20 + 2 },
          "vme3",{IPC_ACQ + 3*20,
                  IPC_ACQ + 3*20,
                  IPC_ACQ + 3*20 + 1,
                  IPC_ACQ + 3*20 + 2 },
          "vme4",{IPC_ACQ + 4*20,
                  IPC_ACQ + 4*20,
                  IPC_ACQ + 4*20 + 1,
                  IPC_ACQ + 4*20 + 2 },
          "vme5",{IPC_ACQ + 5*20,
                  IPC_ACQ + 5*20,
                  IPC_ACQ + 5*20 + 1,
                  IPC_ACQ + 5*20 + 2 },
          "vme6",{IPC_ACQ + 6*20,
                  IPC_ACQ + 6*20,
                  IPC_ACQ + 6*20 + 1,
                  IPC_ACQ + 6*20 + 2 },
          "vme7",{IPC_ACQ + 7*20,
                  IPC_ACQ + 7*20,
                  IPC_ACQ + 7*20 + 1,
                  IPC_ACQ + 7*20 + 2 },
          "vme8",{IPC_ACQ + 8*20,
                  IPC_ACQ + 8*20,
                  IPC_ACQ + 8*20 + 1,
                  IPC_ACQ + 8*20 + 2 },
          "vme9",{IPC_ACQ + 9*20,
                  IPC_ACQ + 9*20,
                  IPC_ACQ + 9*20 + 1,
                  IPC_ACQ + 9*20 + 2 },
         "vme10",{IPC_ACQ + 10*20,
                  IPC_ACQ + 10*20,
                  IPC_ACQ + 10*20 + 1,
                  IPC_ACQ + 10*20 + 2 },
         "vme11",{IPC_ACQ + 11*20,
                  IPC_ACQ + 11*20,
                  IPC_ACQ + 11*20 + 1,
                  IPC_ACQ + 11*20 + 2 },
         "vme12",{IPC_ACQ + 12*20,
                  IPC_ACQ + 12*20,
                  IPC_ACQ + 12*20 + 1,
                  IPC_ACQ + 12*20 + 2 },
          "vme20",{IPC_ACQ + 20*20,
                   IPC_ACQ + 20*20,
                   IPC_ACQ + 20*20 + 1,
                   IPC_ACQ + 20*20 + 2 },
          "vme21",{IPC_ACQ + 21*20,
                   IPC_ACQ + 21*20,
                   IPC_ACQ + 21*20 + 1,
                   IPC_ACQ + 21*20 + 2 },
          "vme22",{IPC_ACQ + 22*20,
                   IPC_ACQ + 22*20,
                   IPC_ACQ + 22*20 + 1,
                   IPC_ACQ + 22*20 + 2 },
        "stream",{IPC_ACQ + 8*20,
                  IPC_ACQ + 8*20,
                  0,
                  0 },
       "stream0",{IPC_ACQ + 9*20,
                  IPC_ACQ + 9*20,
                  0,
                  0 },
       "stream1",{IPC_ACQ + 10*20,
                  IPC_ACQ + 10*20,
                  0,
                  0 },
       "stream2",{IPC_ACQ + 11*20,
                  IPC_ACQ + 11*20,
                  0,
                  0 },
       "stream3",{IPC_ACQ + 12*20,
                  IPC_ACQ + 12*20,
                  0,
                  0 },
            NULL,{0,0,0,0}
};

static char *acq_res[5] = {
        "Shared Memory: ",
        "Semaphore: ",
        "Log Msg Queue: ",
        "Tape Msg Queue: ",
        NULL
};
#endif

#ifdef  RMS

#define  IPC_RMS  ((0x3<<24) + 68352)

static struct rms_ipcs rms_dev[3] = {
        "rms1", {IPC_RMS + 0,
                 IPC_RMS + 1 },
        "rms2", {IPC_RMS + 20,
                 IPC_RMS + 21 },
         NULL,  { 0, 0}
};

static char *rms_res[3] = {
        "Shared Memory: ",
        "Log Msg Queue: ",
        NULL
};
#endif
