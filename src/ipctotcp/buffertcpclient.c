/* Buffertcpclient - debugging tool for ipctotcp.
 * This program just receives data from ipctotcp and either displays it or 
 * dumps it, keeping statistics.
 *
 * RL Varner Nov, 2017 at NSCL
 * */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <signal.h>
#include <ctype.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/wait.h>
#include <errno.h>
#include <unistd.h>
#define  ACQ
#include "../include/ipcdefs.h"
#include "../include/acqlib.h"


/*   Global variables       */

struct shm_buf buffer;
int   msgf = 0x20202020;
char  mess[80];

void INTsignal (int);
/* Returns the byte offset to the end of the last event */
int FindLastEvent(unsigned short *buffer, int bufferlen)
{
   int *bufstr = (int *)buffer;
   int *buf = (int *)(buffer + bufferlen - 4);

   while (buf >= bufstr)
     {
       if (*buf != 0xffffffff) break;
       --buf;
     }
   ++buf;
   return(((char *)buf - (char *)buffer)/2);
}

/****************************************************************************
****************************************************************************/
int main (int argc, char *argv[])
{
   int   i, len, size, tmp, ierr, ipass, lu;
   unsigned short *sptr;
   char *cptr;
   static char server[80] = "127.0.0.0";
   static unsigned short ipPort = 51031;
   static char help1[] = {"Usage:  buffertcpclient "};
   static char help2[] = {"hostip [port number] "};

   
   if (argc < 2) {
     printf ("%s%s\n",help1,help2);
     exit (1);
   }
   else if (argc <= 3) {
     if (isalnum(*argv[1])) {
       strcpy(server,argv[1]);
       printf("Network destination=%s\n",server);
     }
   }

   /*Setup the client socket connection */

   int sockfd;
   unsigned long status;
   struct sockaddr_in  servaddr;
      
   sockfd = socket(AF_INET, SOCK_STREAM, 0);
   if (sockfd == -1) {
     perror("orpastcpclient - socket error");
     exit(1);
   }

   bzero(&servaddr, sizeof(servaddr));
   servaddr.sin_family = AF_INET;
   servaddr.sin_port = htons(ipPort);
   inet_pton(AF_INET, server, &servaddr.sin_addr);
   
   status = connect(sockfd, (struct sockaddr *) &servaddr, sizeof(servaddr));
   if (status) {
     perror("buffertcpclient - error in connect");
     exit(1);
   }
   
   //Catching this signal seems to cause troubles, because the handler is
   //not reposting the signals.  This needs looking into.
   //signal (SIGINT, INTsignal);
   
   ssize_t rdsize=0;
   ssize_t requestSize=sizeof(buffer);
   /* Loop until forced to stop */
   for (;;) {

     printf("-----------------\n");
     /* Get a buffer from the connection */
     rdsize = read(sockfd, (void *)&buffer, requestSize);
     printf("We read %li bytes from the socket\n", rdsize);
     printf("Buffer size is %i\n", buffer.size);
     if (rdsize == -1) { //error checking
       perror("buffertcpclient - error reading");
       close(sockfd);
       exit(1);
     } else if (rdsize == 0) {
       fprintf(stderr, "No data returned by read, stopping.\n");
       close(sockfd);
       return (1);
     } else if (rdsize < buffer.size+16) {
       //read until the entire buffer arrives
       requestSize = buffer.size+16;
       while (rdsize < requestSize) {
	 ssize_t trdsize = read(sockfd, (void *) (&buffer)+rdsize, requestSize-rdsize);
	 rdsize = rdsize + trdsize;
	 printf("We read %li bytes from the socket for a total of %li\n", trdsize, rdsize);
       }
     } else if (rdsize > requestSize) {
     }

     /* Set the size of future requests, based on what the sender thinks */
     requestSize = buffer.size + 16;
     
     /* what is in the buffer? */
     printf("Event Number of first event: %i\n", buffer.event_num);
     printf("Number of buffer events: %i\n", buffer.events);
     printf("Buffer size in bytes: %i\n", buffer.size);
     printf("Buffer number: %i\n", buffer.buf_num);
     int i=0;
     int j=0;
     printf("Data buffer - First Event\n");
     for (j=0; j<16; j+=8) {
       for (i=0; i<8; i+=2) {
	 printf("%i - %x      ", buffer.data[i+j]-0x8000, buffer.data[i+j+1]);
       }
       printf("\n");
     }
     int offset = FindLastEvent(buffer.data, buffer.size/2);
     printf("Data buffer - Last Event\n");
     for (j=offset-16; j<offset; j+=8) {
       for (i=0; i<8; i+=2) {
	 printf("%i - %x      ", buffer.data[i+j]-0x8000, buffer.data[i+j+1]);
       }
       printf("\n");
     }
  }
   close(sockfd);
   return (0);
}
/****************************************************************************
****************************************************************************/
void INTsignal (int sig)
{
   msgf = 0;
}
