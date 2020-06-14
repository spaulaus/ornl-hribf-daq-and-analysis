#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../Dacq/pkt_io.h"
#include "../Dlan/orph_pf.h"
#include <sys/types.h>
#include <sys/time.h>
#include <signal.h>


/*   Global variables       */

struct Vmed xbuf,rbuf;
int param,minval,maxval,count,total_evts,evts;
int minfound,maxfound;
volatile int stopflg;

void swap(unsigned char *,int );
void INTsignal(int);

/****************************************************************************
****************************************************************************/
main(int argc, char *argv[])
{
  int  i,status,len,tmp,num,maxnum=0,dups = 0;
  int  loopcnt = 0;
  unsigned short *sptr;
  unsigned short *lan_buf = (unsigned short *)rbuf.buf;
  char *vptr;

  minval = 1;
  maxval = 8191;
  minfound = 65535;
  maxfound = 0;
  param = 0x8001;
  count = 0;
  total_evts = 0;
  
  if (argc < 2) 
   {
     printf("Need parameter number!\n");
     exit(1);
   }
  if (sscanf(argv[1],"%i",&tmp) != 0) param = 0x8000 + tmp;
  else
    {
      printf("Invalid parameter number\n");
      exit(1);
    }
  if (argc >= 3)
   {
     if (argv[2] != NULL && sscanf(argv[2],"%i",&tmp) != 0) minval = tmp;
   }
  if (argc == 4)
   {
     if (argv[3] != NULL && sscanf(argv[3],"%i",&tmp) != 0) maxval = tmp;
   }
  printf("Search Param = %#x, min value = %d, max value = %d\n",
                                                  param,minval,maxval);

  xbuf.len = 0;
  rbuf.len = sizeof(struct Vmed) - sizeof(struct Vmed_hdr);
  pkt_open(&xbuf,&rbuf,DATA,5);
  pkt_send(&xbuf);

   signal(SIGINT,INTsignal);

   stopflg = 1;
   while(stopflg)
    {
      pkt_recv(&rbuf);
      status = rbuf.error;
      if (status != 0) 
        {
          if (status == ETHER_OPEN) printf("Ethernet open failure.\n");
          else printf("Unknown error code.\n");
          exit(99);
        }
/************
printf("Loop counter = %i\n",++loopcnt);
************/
      sptr = lan_buf;
      sptr += 4;
      len = rbuf.len;
/************
printf("Len = %i\n",len);
************/
      if (len == 0) continue;
      len -= 8;
      i = len/4;
      num=0;
      for (; i > 0; i--)
        {
          if (*sptr == 0xffff)
            {
              total_evts++;
              if (num > maxnum) maxnum=num;
              if (num > 1) dups++;
              num=0;
            }
          if (*sptr++ == param)
            {
              num++;
              if (*sptr >= minval && *sptr <= maxval) count++;
              if (*sptr < minfound) minfound = *sptr;
              if (*sptr > maxfound) maxfound = *sptr;
              ++evts;
            }
          sptr++;
        }
      if (stopflg == 0) break;
    }
   printf("\nSearch Param = %#x, min value = %d, max value = %d\n",
                                                  param,minval,maxval);
   printf("min value = %d, max value = %d, total events seen = %d\n",
                                                 minfound,maxfound,total_evts);
   printf("Events matching ID           = %d\n",evts);
   printf("Events match ID and range    = %d\n",count);
   printf("Num param per event          = %d\n",maxnum);
   printf("Events with duplicate params = %d\n",dups);
   return(0);
}
/****************************************************************************
****************************************************************************/
void INTsignal(int sig)
{
   stopflg = 0;
}
