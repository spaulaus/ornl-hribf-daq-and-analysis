#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
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
   int  lu,size,err;
   char mess[60];
   int  i,abort = 0x20202020;

   if (argc < 2) open_acq_ipc_("vme1",&size,&err,6);
   else  open_acq_ipc_(argv[1],&size,&err,(int)strlen(argv[1]));
   if (err != 0)
     {
       acq_error(&err,mess,sizeof(mess));
       printf("%s\n",mess);
       exit(99);
     }
   lu = open_shm(O_READ,&err);
   if (err != 0)
     {
       acq_error(&err,mess,sizeof(mess));
       printf("%s\n",mess);
       exit(99);
     }
   for (i=0; i < 15; i++)
     {
       read_shm(lu,NULL,&err,&abort);   
       if (err != 0)
         {
           acq_error(&err,mess,sizeof(mess));
           printf("%s\n",mess);
           exit(99);
         }
     }
   while(i) sleep(10);
}
