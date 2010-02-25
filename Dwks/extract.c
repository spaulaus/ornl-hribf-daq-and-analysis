#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../Dlan/orph_pf.h"
#include <sys/types.h>
#include <sys/time.h>
#include <signal.h>


/*   Global variables       */

char server[] = "vme";
int pfd;                                 /* file handle sent in en_open  */
struct Packet_Header xmit_hdr,rec_hdr;   /* Xmit and receive packet hdrs */

unsigned char lan_buf[1600];
volatile int stopflg;

int pkt_recv(unsigned char *, int *);
void swap(unsigned char *,int );
void INTsignal(int);

/****************************************************************************
****************************************************************************/
main(int argc, char *argv[])
{
  int  i,status,len,sav=0;
  unsigned short *sptr;
  char *vptr;


  if ((vptr = getenv("VME")) != NULL) strcpy(server,vptr);

/*
*   Open the packetfilter interface.
*   Note this returns an Ether header, to be passed on.
*/

   pfd = en_open(server, &xmit_hdr, DATA, SERVER);
   if (pfd == FAILURE) 
     {
       printf("Can't open device '%s'\n",server);
       exit (1);
     }

   signal(SIGINT,INTsignal);

#ifdef __ultrix
   en_tmo(pfd,10);
#endif

   stopflg = 1;
   while(stopflg)
    {
      status = pkt_recv(lan_buf,&len);
      if (status == TIMEOUT) continue;
      if (status == FAILURE) break;
      sptr = (unsigned short *)lan_buf;
      sptr += 4;
      len -= 8;
      i = len/4;
      for (; i > 0; i-=2)
        {
          if (sav)
            {
              if (*sptr == 0xffff && *(sptr+1) == 0xff00) sav = 0;
              printf("  %4.4x\n",*sptr);
              printf("  %4.4x\n",*(sptr+1));
              if (!sav) printf("\n");
            }
          else if (*sptr == 0xffff && *(sptr+1) > 0xff00)
            {
              if (*(sptr+1) != 0xffff)
                {
                  printf(" %4.4x%4.4x\n",*sptr,*(sptr+1));
                  sav = 1;
                }
            }
          sptr += 2;
        }
    }
   return(0);
}
/****************************************************************************
  receives packets from the server.  
  Syntax:
       status=pkt_recv(unsigned char *buffer, int *buflen)

       status = FAILURE means an error occurred
       status = SUCCESS means all is ok
       status = TIMEOUT means the connection failed (reliable only)
****************************************************************************/

int pkt_recv(unsigned char *buffer,   /* Information to send   */
             int *buflen)              /* Number of bytes sent  */
{
     static struct Packet_Header rcv_hdr;

    *buflen=en_read(pfd,&rcv_hdr, buffer);     /* read reply reliably */
    if (*buflen==FAILURE) {
       return FAILURE;
    }
    else if (*buflen == TIMEOUT) {
       return TIMEOUT;
    }

    /*   return  with info.*/
    return SUCCESS;
}
/****************************************************************************
****************************************************************************/
void INTsignal(int sig)
{
   stopflg = 0;
}
