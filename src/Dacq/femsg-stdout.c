#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include "spkt_io_udp.h"
#include "orphmsg.h"


/*   Global variables       */

char server[12] = "";
struct UDP_Packet rbuf;
static struct sockaddr_in cli_addr;
struct VMEmsg *femess = (struct VMEmsg *)&rbuf.Data[0];

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
int main()
{
  int  i,status,len;
           char *vptr;
  unsigned short *sptr;


  status = spkt_open(server, FEMSG);
  if (status) {
    perror("Femsg open socket");
    return (-1);
  }

  while(1) {
    
    status = spkt_recv(&rbuf); 
    if (status) {
      perror(" Femsg - read");
      exit(99);
    }
    len = rbuf.DataSize;
    printf("%s: %s\n",femess->sender,femess->text);
  }
  return(0);
}
