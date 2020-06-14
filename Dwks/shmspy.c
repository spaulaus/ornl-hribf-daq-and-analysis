#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#define ACQ
#include "../Dshm/ipcdefs.h"
#include "acqshm.h"
#include "acqlib.h"

extern struct shm_use *Shm;

main(int argc, char *argv[])
{
   int size,err;
   char mess[60];
   static unsigned short readall[NUMSEM];

   if (argc < 2) open_acq_ipc_("vme",&size,&err,6);
   else  open_acq_ipc_(argv[1],&size,&err,(int)strlen(argv[1]));
   if (err != 0)
     {
       acq_error(&err,mess,sizeof(mess));
       printf("%s\n",mess);
     }
   acq_shm_spy();
}
