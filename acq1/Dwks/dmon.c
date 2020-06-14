#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include "../Dacq/pkt_io.h"
#include "../Dlan/orph_pf.h"


/*   Global variables       */

char server[12] = "vme";
struct Vmed xbuf,rbuf;

/****************************************************************************
****************************************************************************/
void swap(char *cptr,int len)
{
  char temp;
  while(len)
    {
      temp = *cptr;
      *cptr = *(cptr+1);
      *(cptr+1) = temp;
      cptr += 2;
      len -= 2;
    }
}
/****************************************************************************
****************************************************************************/
main()
{
  int  i,status,len;
           char *vptr;
  unsigned short *sptr;
  unsigned short *lan_buf = (unsigned short *)rbuf.buf;

   if ((vptr = getenv("VME")) != NULL) strcpy(server,vptr);
   xbuf.len = 0;
   rbuf.len = sizeof(struct Vmed) - sizeof(struct Vmed_hdr);
   pkt_open(&xbuf,&rbuf,DATA,60);
   pkt_send(&xbuf);
   while(1)
     {
       pkt_recv(&rbuf);
       len = rbuf.len;
       if (rbuf.error)
         {
           if (rbuf.error == ETHER_OPEN)
                                 printf("Ethernet open failure.\n",rbuf.error);
           else printf("Unknown error code: %i\n",rbuf.error);
           exit(99);
         }
       if (rbuf.len == 0)
         {
           printf("Timeout -- no data in 60 secs on device '%s'\n",server);
           continue;
         }
       sptr = lan_buf;
       printf ("%4.4x%4.4x  ",*(sptr+1),*sptr);
       sptr += 2;
       for(i=4; i < len; i += 2)
         {
           if(i%32 == 0) printf("\n");
           printf("%4.4x ",*sptr++);
         }
       printf("\n");
       fflush(stdout);
printf("Byte count = %i\n",rbuf.len);
/*
       write(1,lan_buf,len);
*/
     }
   return(0);
}
