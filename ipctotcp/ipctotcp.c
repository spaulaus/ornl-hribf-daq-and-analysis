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
#include <sys/ipc.h>
#include <sys/msg.h>
#include <ctype.h>
#include <errno.h>
#include <unistd.h>
#define  ACQ
#include "../include/ipcdefs.h"
#include "../include/acqlib.h"
#include "../include/orphmsg.h"

static unsigned short ipPort=51031;
#define BACKLOG 1
//#define DEBUG 1
#define PACMAN 1

/*   Global variables       */
#ifdef PACMAN
static struct orphmsg msg_ipctotcp={MSG_INFORM,"ipctotcp   "};
#endif

int   msgf = 0x20202020; /* Interrupt handlers set this to non-blank */
char  mess[80];

void cleanup_connections(int luin, int connfd);
void PrintBufferStat(int nbuffers, struct shm_buf *ibuf);
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
  /* Pointer to shared memory buffer data */
  static struct shm_buf *ibuf=NULL;

  int  luin=-1;
  int   i;
  int ierr;  //error return value from acqipc routines
  unsigned short *sptr;
  char *cptr;
  static char device[80] = {"vme"};
  static char help1[] = {"Usage:  ipctotcp "};
  static char help2[] = {"[device] [port]"};

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
  

  /**Shared memory buffer setup************************************/

  /***** Connect to the ipc resources assigned to this VME device */
  int ipcsize = 0; //size of the buffers, returned from call
  open_acq_ipc__ (device, &ipcsize, &ierr,5); //"5" is the length of device str
  if (ierr != 0) {
    acq_error(&ierr,mess,sizeof(mess));
#ifdef PACMAN
    sprintf(&msg_ipctotcp.text[9],"%s - %s\n", device,mess);
    msgsnd(Ids.log_msg, &msg_ipctotcp, sizeof(struct orphmsg), IPC_NOWAIT);
#else
    fprintf(stderr,"%s - %s\n", device,mess);
#endif
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
    /* Allocate a socket for listening */
   listenfd = socket(AF_INET, SOCK_STREAM, 0);
   if (listenfd == -1) {
     perror("ipctotcp - socket error");
     exit(1);
   }
   
    /* set up the address */
   bzero(&servaddr, sizeof(servaddr));
   servaddr.sin_family      = AF_INET;
   servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
   servaddr.sin_port        = htons(ipPort); //ORPAS Event buffer server
 
   int optval = 1;
   int optlen = sizeof(int);
   status = setsockopt( listenfd, 
			SOL_SOCKET, 
			SO_REUSEADDR,
			&optval,
			optlen); 
   if (status) {
     perror("ipctotcp - setsockopt error");
     cleanup_connections(luin, listenfd);
     return(1);
   }

    /* Bind the socket to the IP address */
   status = bind(listenfd, 
		 (struct sockaddr *) &servaddr, 
		 (socklen_t) sizeof(servaddr));
   if (status) {
     perror("ipctotcp - bind error");
     exit(1);
   }
   
   /* Configure this socket for listening */   
   status = listen(listenfd, BACKLOG);
   if (status) {
     perror("ipctotcp - listen error");
     return(1);
   }
   
      
   /* Finished preparing to set up server-client connection */
   /************************************************************************/

   /* Read loop over the ipc buffers */
   unsigned int nbuffers=0;

   /* Catch SIGINT to detect that we should stop?*/
   signal (SIGINT, INTsignal);
   signal (SIGPIPE, SIG_IGN);
   signal(SIGTSTP, SIG_IGN);
   signal(SIGCONT, SIG_IGN);
   signal(SIGUSR1, SIG_IGN);
   signal(SIGUSR2, SIG_IGN);
   signal(SIGQUIT, SIG_IGN);

    /* Loop forever to accept connections */
   len = sizeof(cliaddr);
   for (;;) {
#ifdef PACMAN
   sprintf(&msg_ipctotcp.text[9],"ipctotcp - Listening for a new connection\n");
   msgsnd(Ids.log_msg, &msg_ipctotcp, sizeof(struct orphmsg), IPC_NOWAIT);
#else
     printf("ipctotcp - Listening for a new connection\n");
#endif
      connfd = accept(listenfd, (struct sockaddr *) &cliaddr, &len);
      if (connfd == -1) {
        if (errno == EINTR) {
           perror("ipctotcp - accept interrupted");
	   close_shm(luin);
           return(1);
         }      
         perror("ipctotcp - accept error");
         exit(1);
      }

      char strbuffer[INET_ADDRSTRLEN];
#ifdef PACMAN
      sprintf(&msg_ipctotcp.text[9],"ipctotcp - Connection from %s, port %d\n",
	     inet_ntop(AF_INET, &cliaddr.sin_addr, 
		       strbuffer, 
		       sizeof(strbuffer)),
	     ntohs(cliaddr.sin_port));
      msgsnd(Ids.log_msg, &msg_ipctotcp, sizeof(struct orphmsg), IPC_NOWAIT);
#else
      printf("ipctotcp - Connection from %s, port %d\n",
	     inet_ntop(AF_INET, &cliaddr.sin_addr, 
		       strbuffer, 
		       sizeof(strbuffer)),
	     ntohs(cliaddr.sin_port));
#endif   

      char breconnect=0;
      /* Loop forever, getting buffers and writing data to the client */
      for (;;) {
	
	/*Get a pointer (ibuf) to an element of the ring buffer */
	
	ibuf = read_shm (luin, ibuf, &ierr, &msgf);
	if (ierr != 0) {
	  fprintf(stderr, "ipctotcp - shared memory read error\n");
	  cleanup_connections(luin, connfd);
	  return(1);
	}
	/* A keyboard interrupt was sent */
	if (msgf == 0) {
	  cleanup_connections(luin, connfd);
	  return(1);
	}
	
#ifdef DEBUG
	PrintBufferStat(nbuffers, ibuf);
#endif     
	
	/* Copy the buffer to the tcp connection socket*/
	/* The length includes the header and the length of data in the buffer*/
	
	size_t len_ibuf=16+ibuf->size;

	const void * tmpibuf = (const void *)ibuf;
	status = 0;

	while (status < len_ibuf) {
	  status =  write(connfd, tmpibuf, len_ibuf);	  
	  if (status == -1) {
	    if (errno == EINTR) {
	      continue;  // loop and try again
	    } else if ((errno == ECONNABORTED) || (errno == EPIPE)) {
#ifdef PACMAN
   sprintf(&msg_ipctotcp.text[9],"ipctotcp - TCP connection broken\n");
   msgsnd(Ids.log_msg, &msg_ipctotcp, sizeof(struct orphmsg), IPC_NOWAIT);
#else
	      fprintf(stderr, "ipctotcp - TCP connection broken\n");
#endif
	      breconnect = 1;
	      break;
	    } else {
#ifdef PACMAN
   sprintf(&msg_ipctotcp.text[9],"tcp socket write error:%s\n",
	strerror(errno));
   msgsnd(Ids.log_msg, &msg_ipctotcp, sizeof(struct orphmsg), IPC_NOWAIT);
#else
	      perror("ipctotcp - tcp socket write error");
#endif

	    }
	  } else if (status < len_ibuf) { //Wrote only part of the buffer
	    len_ibuf = len_ibuf - status;
	    tmpibuf = tmpibuf + status;
	    continue;
	  }
	  // A keyboard interrupt requests we exit
	  if (msgf == 0) {
	    cleanup_connections(luin, connfd);
	    return(1);
	  }
	} //End of the tcp write while loop

	// We need to reconnect
	if (breconnect)
	  break;

      }  //End of the read - write infinite loop

   } // End of the connect loop

   /* End of the server */

   cleanup_connections(luin, connfd);

   return (0);
}
/****************************************************************************
****************************************************************************/
void cleanup_connections(int luin, int connfd)
{
  int status=0;
  /* Close open connections */
  close_shm(luin); /*Shared files */
  status = close(connfd);   /*TCP connection */
  if (status) 
    perror("ipctotcp - close error");
}

void INTsignal (int sig)
{
   msgf = 0;
}

void PrintBufferStat(int nbuffers, struct shm_buf *ibuf)
{
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
}
