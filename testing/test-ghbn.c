#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <netdb.h>

extern int h_errno;

union u_addr {
   int addr;
   char caddr[4];
} myaddr;

struct hostent *host;


int main(int argc, char **argv)
{

  myaddr.caddr[0]=160;
  myaddr.caddr[1]=91;
  myaddr.caddr[2]=72;
  myaddr.caddr[3]=12;

  printf("Hostaddr: %x; ",myaddr.addr);

  h_errno=0;
  host = gethostbyaddr((const char *) &myaddr.addr, 4, AF_INET);
  if (host != NULL) {
     printf("Found host name %s\n", host->h_name);
     return(0);
  }
  else {
     switch (h_errno) {
       case (HOST_NOT_FOUND):
           printf("Host not found\n");
           break;
       case (NO_ADDRESS):
           printf("No address for the host\n");
           break;
       case (NO_RECOVERY):
           printf("Unrecoverable lookup error\n");
           break;
       case (TRY_AGAIN):
           printf("Try again later, the nameserver is busy\n");
           break;
       default:
           printf("I have no idea what just happened; code=%x\n");
           break;
     }
  }
  return(0);
}

