/*
 * ORNL Physics Division; 
 * A server to send orphas ipc buffers to external clients, using TCP/IP.
 * Based on McConnell's ipcmon and routines udptoipc, Stevens examples of 
 * TCP clients
 * RLV 14 Nov 2017
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <signal.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/wait.h>
#include <ctype.h>
#include <errno.h>
#include <unistd.h>
#define  ACQ
#include "../include/ipcdefs.h"
#include "../include/acqlib.h"

static unsigned short ipPort=51031;
#define BACKLOG 1
#define DEBUG 1

/*   Global variables       */

int   seen, count, total_evts, evts;
unsigned int   total, sum, evtfirst, evtnext,bufnum; /*event counters */

int   msgf = 0x20202020; /* Interrupt handlers set this to non-blank */
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
  static struct shm_buf *ibuf=NULL;
  int  luin=-1;
  int   i;
  int ierr;  //error return value from acqipc routines
  unsigned short *sptr;
  char *cptr;
  static char device[80] = {"vme"};
  static char help1[] = {"Usage:  ipctotcp "};
  static char help2[] = {"[device] [port]"};

  count = 0;
  total_evts = 0;
  
  /* Process any arguments */
  if ((cptr = getenv("VME")) != NULL) 
    strcpy(device,cptr);
  
  i = 1;
  if (argc > 3) {
    printf ("%s%s\n",help1,help2);
    exit (1);
  }
  if (argc >= 2) {
    if (isalpha(*argv[1]))
      strcpy(device,argv[1]);
  }
  if (argc == 3) {
    if (isdigit(*argv[2]))
      ;
      //      strcpy(ipPort,argv[2]);
  }
  

  /***** Connect to the ipc resources assigned to this VME device */
  int ipcsize = 0; //size of the buffers, returned from call
  open_acq_ipc__ (device, &ipcsize, &ierr,5); //"5" is the length of device str
  if (ierr != 0) {
    acq_error(&ierr,mess,sizeof(mess));
    printf ("%s - %s\n", device,mess);
    exit (1);
  }
#ifdef DEBUG
  printf("Size of IPC Data = %i\n", ipcsize);
#endif   
 
  /**open the shared memory ring buffer to read data */
  luin = open_shm(O_READ, &ierr);
  if (ierr) {
    acq_error(&ierr,mess,sizeof(mess));
    fprintf(stderr,"ipctotcp - open_shm error on %s - %s", device, mess);
    exit(1);
  }
  
  /* Catch SIGINT to detect that we should stop?*/
  //signal (SIGINT, INTsignal);
  
  
   /************************************************************************/
   /****** Setup the TCP listener - looking for requests, setup connection */
   /* Code snatched from Stevens book Unix Network Programming */
   
   int                 listenfd, connfd;
   socklen_t           len;
   struct sockaddr_in  servaddr, cliaddr;
   int                 status;
   
#ifdef DEBUG
    fprintf(stderr, "ipctotcp - opening socket\n");
#endif   
   listenfd = socket(AF_INET, SOCK_STREAM, 0);
   if (listenfd == -1) {
     perror("ipctotcp - socket error");
     exit(1);
   }
   
#ifdef DEBUG
    fprintf(stderr,"Listening port is: %i\n",ipPort);
#endif   
   bzero(&servaddr, sizeof(servaddr));
   servaddr.sin_family      = AF_INET;
   servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
   servaddr.sin_port        = htons(ipPort); //ORPAS Event buffer server
   
#ifdef DEBUG
    fprintf(stderr, "ipctotcp - binding socket\n");
#endif   
   status = bind(listenfd, (struct sockaddr *) &servaddr, (socklen_t) sizeof(servaddr));
   if (status) {
     perror("ipctotcp - bind error");
     exit(1);
   }
   
#ifdef DEBUG
    fprintf(stderr, "ipctotcp - listen\n");
#endif   
   status = listen(listenfd, BACKLOG);
   if (status) {
     perror("ipctotcp - listen error");
     exit(1);
   }
   
   /*Wait for connection requests */
   len = sizeof(cliaddr);

#ifdef DEBUG
    fprintf(stderr, "ipctotcp - accepting connections\n");
#endif   
   for (;;) {
      connfd = accept(listenfd, (struct sockaddr *) &cliaddr, &len);
#ifdef DEBUG
       fprintf(stderr, "ipctotcp - accepting connections returns %i\n",
              connfd);
#endif   
      if (connfd == -1) {
         if (errno == ECONNABORTED) {
           fprintf(stderr, "ipctotcp - TCP connection broken\n");;
         }
         if (errno == EINTR) {
           perror("ipctotcp - accept interrupted");
           exit(1);
         }      
         perror("ipctotcp - accept error");
         exit(1);
      } else 
        break;
   } 

   /* At this point a connection is established */
   /* Print some details */
#ifdef DEBUG
   char strbuffer[INET_ADDRSTRLEN];
   printf("Connection from %s, port %d\n",
	  inet_ntop(AF_INET, &cliaddr.sin_addr, strbuffer, sizeof(strbuffer)),
	  ntohs(cliaddr.sin_port));
#endif   
      
   /* Finished setting up server-client connection */
   /************************************************************************/



   /* Read loop over the ipc buffers */
   unsigned int nbuffers=0;

   /* Catch SIGINT to detect that we should stop?*/
   signal (SIGINT, INTsignal);

   /* executes */
   for (;;) {

     /*Get a pointer (ibuf) to an element of the ring buffer */
#ifdef DEBUG      
     printf("Before read: ibuf pointer = %i\n", ibuf);
#endif
     ibuf = read_shm (luin, ibuf, &ierr, &msgf);
#ifdef DEBUG     
     printf("After read: ibuf pointer = %i\n", ibuf);
#endif
     if (ierr != 0) {
       fprintf(stderr, "ipctotcp - shared memory read error\n");
       break;
     }
     if (msgf == 0) break;

#ifdef DEBUG
     printf("Buffers since starting: %i\n", nbuffers++);
     printf("Event Number of first event: %i\n", ibuf->event_num);
     printf("Number of buffer events: %i\n", ibuf->events);
     printf("Buffer size in bytes: %i\n", ibuf->size);
     printf("Buffer number: %i\n", ibuf->buf_num);

     int i=0;
     int j=0;
     
     printf("Data buffer - First Event\n");
     for (j=0; j<16; j+=8) {
       for (i=0; i<8; i+=2) {
	 printf("%i - %x      ", ibuf->data[i+j]-0x8000, ibuf->data[i+j+1]);
       }
       printf("\n");
     }
     printf("Data buffer - Last Event\n");
     int offset = FindLastEvent(ibuf->data, ibuf->size/2);
     printf("Offset to last event = %i\n",offset);
     for (j=offset-16; j<offset; j+=8) {
       for (i=0; i<8; i+=2) {
	 printf("%i - %x      ", ibuf->data[i+j]-0x8000, ibuf->data[i+j+1]);
       }
       printf("\n");
     }
#endif     

     /* Copy the buffer to the tcp connection socket*/
     /* The length includes the header and the length of data in the buffer*/
     //     size_t len_ibuf=16+ipcsize;
     size_t len_ibuf=16+ibuf->size;
     const void * tibuf = (const void *)ibuf;
     status = 0;
     while (status < ibuf->size+16) {
       status =  write(connfd, tibuf, len_ibuf);
#ifdef DEBUG
       printf("Number of bytes written = %i\n", status);
#endif
       if (status == -1) {
	 if (errno == EINTR) {
	   continue;  // loop and try again
	 } else {
	   perror("ipctotcp - tcp socket write error");
	 }
       } else if (status < len_ibuf) { //Wrote only part of the buffer
	 len_ibuf = len_ibuf - status;
	 tibuf = tibuf + status;
	 continue;
       }
     }
     
     
   }
   
   /* End of the server */

   /* Close open connections */
   close_shm(luin); /*Shared files */
   status = close(connfd);   /*TCP connection */
   if (status) {
     perror("ipctotcp - close error");
   }

   return (0);
}
/****************************************************************************
****************************************************************************/
void INTsignal (int sig)
{
   msgf = 0;
}
