/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1996-2003
*
*
*
*                         IMPORTANT NOTICE TO USER
*
*     All statements, technical information and recommendations contained
*     herein are based on tests we believe to be reliable, but the accuracy
*     or completeness thereof is not guaranteed, and the following  is
*     made in lieu of all warranties expressed or implied:
*
*     Our only obligation shall be to correct or replace, at our convenience,
*     any part of the product proven to be defective.  We shall not be liable
*     for any loss or damage, direct or consequential, arising out of the
*     use or the inability to use the product.  Before using, the USER shall
*     determine the suitability of the product for his or her intended use,
*     and the USER assumes all risk and liability whatsoever in connection
*     therewith.
*
******************************************************************************
*
*    Environment:  VME Acquisition for Linux
*
*    File:         /usr/users/mcsq/Dlinux/Dlan/vmedII.c
*
*    Description:  
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    7/19/02    MCSQ         Original
*
*    3/19/03    MCSQ         Added timeout to struct Vme_start.  Change
*                            server to use timeout specified by the
*                            workstation code.
*
*    7/14/04    MCSQ         In the routine CLDsignal(), I had used wait3()
*                            in a loop until no more children needed to be
*                            waited on.  This appears to be bad since vmedII
*                            master process  hangs with a defunct child and
*                            will not execute new connections.  Best guess
*                            is that a child process can get in a state where
*                            it's death is near but it must deliver the
*                            SIGCLD signal before wait3() can returnwith the
*                            child PID.  In this case, the SIGCLD is blocked
*                            because we are in the signal handler routine.
*
*                            I changed the routine CLDsignal() to use wait()
*                            and removed the loop so that only one call to
*                            wait() is made for each call to CLDsignal().
*
*                            It was still possible to get defunct vmedII
*                            processes but vmedII is not hung and can
*                            continue to execute new connections.
*                            A new routine defunct() is used to purge
*                            these defunct processes.  It is almost identical
*                            to the original CLDsignal() routine.  However,
*                            it is called after each new connection is
*                            executed and the SIGCLD is not blocked at this
*                            time.
*
*    3/1/06     MCSQ         Changed routine defunct().  Add message to log
*                            on which pid was returned by wait3().  Change
*                            text for message when a connection is closed here.
*                            Now calls wait3() only once per call to
*                            defunct().
*
*   12/9/07     RLV          Remove the need for the HHRIF define.
*                            Replace signal with sigaction, "more reliable
*                            signal handling"
*
*   12/10/07    RLV          Set SIGCHLD to be ignored, which allows child 
*                            processes to die quietly.  Remove handler,
*                            defunct() and tables of connections.
*****************************************************************************/
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <net/if.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <arpa/inet.h>
#include <signal.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <pwd.h>
#include <sys/wait.h>
#include "orph_pf.h"
#include "vmedII.h"

/*
*     Function Prototypes
*/
void TermHandle(int);
void loadchk(void);
int readn(int,unsigned char *,int);
int writen(int,unsigned char *,int);
int  WR_io(unsigned char *,int ,unsigned char *,short *);
int  WO_io(unsigned char *,int );
int  RO_io(int,struct Vmed *);
void DEBUG_io(int,struct Vmed *);
void WRserve(int);
void prnthdr(struct Packet_Header);
int sumarray(char *, int);

/*
*    Global variables
*/

struct Vme_start initbuf;
           char  in_line[257];

/***************************************************************************
*
***************************************************************************/
main(int argc, char *argv[])
{
   char msg[32];
   int sockfd,newsockfd,clilen;
   int  status,childpid,fd,fdmax,port;
   int  mypid;
   time_t tod, prevtod;
   static struct sockaddr_in cli_addr,serv_addr;
   char node[16];
   sigset_t blksig;
   static struct sigaction my_sa;

   static char logfile[80] = "/usr/acq/log/vmedII.log";

/*
*  If we were started by the process init, bypass the following.
*/
   if (getppid() != 1)
     {
/*
*   Check for a copy of vmedII running.  We allow only one at startup.
* RLV actually we only allow one process with the string vmedII in the name.
*/
       loadchk();

       /* Set signal handlers to keep the daemon from being interrupted 
          by terminal control signals
       */
       my_sa.sa_handler=SIG_IGN;
       sigemptyset(&my_sa.sa_mask);
       my_sa.sa_flags=0;
       sigaction(SIGTSTP,&my_sa, NULL); 
       sigaction(SIGTTIN,&my_sa, NULL); 
       sigaction(SIGTTOU,&my_sa, NULL); 

       /* This forks so that the parent can die and leave the child without 
          a terminal.
       */
       if ((childpid = fork()) < 0)
         {
           perror("vmedII first fork error");
           exit(0);
         }
       else if (childpid > 0) exit(0);

       /* Now we run in the context of the daemon child */
/*
*   Make ourself a process group leader and get rid of the controlling
*   terminal.
*/
       mypid=getpid();
       setpgid(0,getpid());
       if ((fd = open("/dev/tty",O_RDWR)) >= 0)
         {
           ioctl(fd, TIOCNOTTY, (char *)NULL);
           close(fd);
         }
     }
   
   if ((fd = open(logfile,O_CREAT | O_WRONLY | O_APPEND, 0644)) < 0)
     {
       perror("Log open failure");
       fprintf(stderr,"Log file: %s\n",logfile);
       exit(99);
     }
/*
*  assign the log file to stdout and stderr
*/
   dup2(fd,1);
   dup2(fd,2);
   if ((fd = open("/dev/null",0,0)) < 0)
     {
       perror("Open failure - /dev/null");
       exit(99);
     }
   dup2(fd,0);
/*
*  Close all other open files.
*/
   fdmax = getdtablesize();
   for (fd = 3; fd < fdmax; fd++) close(fd);
   errno = 0;
   umask(0);

/*
*   Setup to ignore most signals
*/
   my_sa.sa_handler=SIG_IGN;
   sigemptyset(&my_sa.sa_mask);
   my_sa.sa_flags=0;
   sigaction(SIGINT, &my_sa, NULL); 
   sigaction(SIGCONT, &my_sa, NULL); 
   sigaction(SIGUSR1, &my_sa, NULL); 
   sigaction(SIGUSR2, &my_sa, NULL); 
   sigaction(SIGQUIT, &my_sa, NULL); 
/*
*   Accept SIGTERM signal to exit
*/
   my_sa.sa_handler=TermHandle;
   sigemptyset(&my_sa.sa_mask);
   my_sa.sa_flags=0;
   sigaction(SIGTERM, &my_sa, NULL); 

   /* Anounce our startup */ 
   time(&tod);
   strftime(msg,21,"%d-%b-%y %H:%M:%S  ",localtime(&tod));
   fprintf(stderr,"%s********** VME server, PID=%i,  Started    *********\n",msg,mypid);

   /* Create and bind a socket for the client connections */
   sockfd = socket(AF_INET,SOCK_STREAM,0);
   if (sockfd == -1) {perror("socket error"); exit(1);}
   bzero((char *) &serv_addr,sizeof(serv_addr));
   serv_addr.sin_family = AF_INET;
   serv_addr.sin_port = htons(6996);
   serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
   status = bind(sockfd,(struct sockaddr *)&serv_addr,sizeof(serv_addr));
   if (status == -1) {perror("bind"); exit(1);}

   listen(sockfd,5);

   /* This sets the SIGCHLD to be ignored and children die without being
      zombified
    */
   my_sa.sa_handler=SIG_IGN;
   sigemptyset(&my_sa.sa_mask);
   my_sa.sa_flags=0;
   sigaction(SIGCHLD, &my_sa, NULL); 

   /* Loop forever, accepting connections and forking child processes to 
      handle the requests.
   */
   prevtod=0;  // This helps measure the interval of logging messages */
   while(1)
    {
      clilen = sizeof(cli_addr);
      do
        {
          newsockfd = accept(sockfd,(struct sockaddr *) &cli_addr, &clilen);
        } while (newsockfd < 0 && errno == EINTR);
      if (newsockfd < 0)
        {
          perror("accept");
          exit(0);
        }
      time(&tod);
      strftime(msg,21,"%d-%b-%y %H:%M:%S  ",localtime(&tod));
      strcpy(node,inet_ntoa(cli_addr.sin_addr));

      if ((childpid = fork()) == 0)
        {
          close(sockfd);
          WRserve(newsockfd);
          exit(0);
        }
      else if (childpid == -1) perror("fork failure");
      else if (childpid > 0)
        {
	  if ((tod-prevtod) > 600) {
	    prevtod=tod;
	    fprintf(stderr,"%s** VMEDII still alive: last PID spawned = %i\n",msg,childpid);
	  }
	}
      close(newsockfd);
    }
   return(0);
}
/******************************************************************************
******************************************************************************/
void WRserve(int sockpfd)
{
   int status, hdr = sizeof(struct Vmed_hdr);
   static union PKT {
          struct Vmed lbuf;
            char cbuf[1536];
   } Pkt;

   status = readn(sockpfd,(unsigned char *)&initbuf,sizeof(struct Vme_start));
   if (status <= 0)
    {
      close(sockpfd);
      return;
    }

#ifdef DEBUG
printf("\nSpawning WRserve\n");
printf("\nReceived %i bytes - start header\n",status);
printf("vme = %s\n",initbuf.server);
printf("proto = %x\n",initbuf.proto);
printf("timeout = %x\n",initbuf.timeout);
printf("cmd len = %i\n",initbuf.cmd_len);
printf("rpy len = %i\n",initbuf.rpy_len);
#endif

   if (initbuf.proto == SOFT) DEBUG_io(sockpfd,&Pkt.lbuf);
   while(1)
     {
       status = readn(sockpfd,(unsigned char *)&Pkt.lbuf,initbuf.cmd_len);
       if (status <= 0)
        {
          close(sockpfd);
          return;
        }
#ifdef DEBUG
printf("received %i bytes from client - send command to VME\n",status);
printf("HDR=%i, initbuf.cmd_len=%i, initbuf.rpy_len=%i\n",hdr, initbuf.cmd_len, 
        initbuf.rpy_len);
#endif
       if (initbuf.cmd_len > hdr && initbuf.rpy_len > hdr)
         {
           status = WR_io(Pkt.lbuf.buf,Pkt.lbuf.len,Pkt.lbuf.buf,&Pkt.lbuf.len);
         }
       else if (initbuf.cmd_len > hdr && initbuf.rpy_len <= hdr)
         {
           status = WO_io(Pkt.lbuf.buf,Pkt.lbuf.len);
           Pkt.lbuf.len = 0;
         }
       else if (initbuf.cmd_len <= hdr && initbuf.rpy_len > hdr)
         {
           status = RO_io(sockpfd, &Pkt.lbuf);
           Pkt.lbuf.len = 0;
         }
       else status = -99;
       Pkt.lbuf.error = status;
#ifdef DEBUG
printf("Ethernet status %i after WR_io\n",status);
#endif
       status = writen(sockpfd,(unsigned char *)&Pkt.lbuf,initbuf.rpy_len);
       if (status <= 0)
        {
          close(sockpfd);
          return;
        }
#ifdef DEBUG
printf("sent %i bytes to client - reply from VME \n\n",status);
#endif
     }
}
/******************************************************************************
******************************************************************************/
int readn(int fd,unsigned char *ptr,int nbytes)
{
   int nleft,nread;

   nleft = nbytes;
   while(nleft > 0)
     {
       nread = read(fd,ptr,nleft);
       if (nread < 0)  return(nread);       /* read error  */
       else if (nread == 0) break;          /* EOF         */
       nleft -= nread;
       ptr += nread;
     }
   return(nbytes - nleft);
}
/******************************************************************************
******************************************************************************/
int writen(int fd,unsigned char *ptr,int nbytes)
{
   int nleft,nwritten;

   nleft = nbytes;
   while(nleft > 0)
     {
       nwritten = write(fd,ptr,nleft);
       if (nwritten <= 0)  return(nwritten);   /* write error  */
       nleft -= nwritten;
       ptr += nwritten;
     }
   return(nbytes - nleft);
}

/*****************************************************************************
*   Send command packet and receive reply from the VME processor.
*
*  Return:  0  -  OK
*          ETHER_RECEIVE -  No reply from the VME processor
*          ETHER_TRANSMIT -  Packet transmission error
*          ETHER_OPEN -  Packet filter open error
*
*  Most communications with the VME processor fall in this case.  The WS
*  sends a command and the VME responds with a reply.  Protocols for this
*  type are:
*
*       REQUEST_BOOT    FECNTRL      RMSSIO
*       FASTBUS         VMEIO
*       CNAF            LNFILL       CODE (most of the time)
*****************************************************************************/
int  WR_io(unsigned char *xbuf, int xlen, unsigned char *rbuf, short *rlen)
{
#ifdef DEBUG
  static int dsum=0,tsum=0;
#endif
  int i,j;

  static int pfd = -1;                   /* file handle set in en_open */
#ifdef DEBUG
  static char dummy[4096];
#endif
  static struct Packet_Header xmit_hdr;
  static struct Packet_Header recv_hdr;

  int   buflen, status;

#ifdef DEBUG
  printf("\n**********************Enter WR_io\n");
  printf("PFD=%i\n",pfd);
  printf("xmit_hdr, start WR_io\n:");
  prnthdr(xmit_hdr);
  printf("recv_hdr, start WR_io\n:");
  prnthdr(recv_hdr);
printf("xlen = %i\n",xlen);
#endif

  if(pfd == -1)
    {
#ifdef DEBUG
      for (i=0;i<4096;i++) /* debug me */
	dummy[i]=1;
      dsum=sumarray(dummy,4096);
      printf("Setting dsum=%i\n",dsum);
#endif

      pfd = en_open(initbuf.server, &xmit_hdr, initbuf.proto, CLIENT);
      if (pfd == FAILURE) return (ETHER_OPEN);
    }
  *rlen = 0;
  status = en_write(pfd,&xmit_hdr, xbuf, xlen);
  if (status == FAILURE || status == TIMEOUT) return (ETHER_TRANSMIT);
  /*
   *      wait for the reply
   */
#ifdef DEBUG
  printf("dummy sum is: %i before en_read.\n",sumarray(dummy,4096));
#endif
  do
    {
      en_tmo(pfd,initbuf.timeout);;
      buflen=en_read(pfd,&recv_hdr, rbuf);
      if (buflen == FAILURE || buflen == TIMEOUT) return (ETHER_RECEIVE);
#ifdef DEBUG
printf("buflen = %i\n",buflen);
#endif
    } while (xmit_hdr.Order != recv_hdr.Order);
  *rlen = buflen;
  
#ifdef DEBUG
  printf("\nxmit_hdr, end WR_io\n:");
  prnthdr(xmit_hdr);
  printf("\nrecv_hdr, end WR_io\n:");
  prnthdr(recv_hdr);
  tsum=sumarray(dummy,4096);
  if (dsum != tsum){
    printf("Something messed with dummy! %i\n",tsum);
    for (i=0;i<64;i++) {
      printf("\n%i: ",i);
      for (j=0;j<64;j++) {
	printf("%x",dummy[j+i*(64)]);
      }
    }
    printf("\n");
  }
#endif
  return (0);
}
/*****************************************************************************
*   Send command packet and return only status to the WS
*
*  Return:  0  -  OK
*          ETHER_TRANSMIT -  Packet transmission error
*          ETHER_OPEN -  Packet filter open error
*
*  This routine used for protocol CODE for initial load at boot time.
*
*****************************************************************************/
int  WO_io(unsigned char *xbuf, int xlen)
{

     static int pfd = -1;                   /* file handle set in en_open */
     static struct Packet_Header xmit_hdr,recv_hdr;
     int    status;

     if(pfd == -1)
      {
        pfd = en_open(initbuf.server, &xmit_hdr, initbuf.proto, CLIENT);
        if (pfd == FAILURE) return (ETHER_OPEN);
      }

#ifdef DEBUG
printf("************ enter WO_io\n");
printf("xlen = %i\n",xlen);
#endif

     status = en_write(pfd,&xmit_hdr, xbuf, xlen);
     if (status == FAILURE || status == TIMEOUT) return (ETHER_TRANSMIT);
#ifdef DEBUG
printf("************ exit WO_io\n");
#endif

     return (0);
}
/*****************************************************************************
*   Receive packets from the VME and send to the WS
*
*  Return:  0  -  OK
*          ETHER_RECEIVE -  No reply from the VME processor
*          ETHER_OPEN -  Packet filter open error
*
*   Two protocols - DATA and FEMSG - are read only from the VME processor.
*   This routine handles those.
*****************************************************************************/
int  RO_io(int sockpfd, struct Vmed *lbuf)
{

     unsigned char *rbuf = lbuf->buf;
     static int pfd = -1;                   /* file handle set in en_open */
     static struct Packet_Header xmit_hdr,recv_hdr;
     int   buflen,status;

     if(pfd == -1)
      {
        pfd = en_open(initbuf.server, &xmit_hdr, initbuf.proto, SERVER);
        if (pfd == FAILURE) return (ETHER_OPEN);
      }
     while(1)
       {
         en_tmo(pfd,initbuf.timeout);
         buflen = en_read(pfd,&recv_hdr, rbuf);
         if (buflen == FAILURE) return (ETHER_RECEIVE);
         lbuf->error = 0;
         if (buflen == TIMEOUT) lbuf->len = 0;
         else  lbuf->len = buflen;
         status = writen(sockpfd,(unsigned char *)lbuf,initbuf.rpy_len);
         if (status <= 0)
           {
             close(sockpfd);
             exit(99);
           }
       }
     return (0);
}
/******************************************************************************
*
*  This routine if for the debugger.  The WS sends commands to the VME.
*  The VME responds with multiple packets which are sent to the WS.
*
*  This uses the protocol SOFT
******************************************************************************/
void DEBUG_io(int sockpfd,struct Vmed *lbuf)
{
  int pfd;                               /* file handle returned by en_open  */
  int buflen,status;
  struct Packet_Header xmit_hdr,recv_hdr; /* Xmit and receive packet hdrs   */
  fd_set readfds,errfds;
  struct timeval timeout;

/*
*   Open the packetfilter interface.
*/

   pfd = en_open(initbuf.server, &xmit_hdr, SOFT, CLIENT);
   if (pfd == FAILURE) 
     {
       lbuf->len = 0;
       lbuf->error = ETHER_OPEN;
       status = writen(sockpfd,(unsigned char *)lbuf,initbuf.rpy_len);
       close(sockpfd);
       exit (99);
     }

#ifdef DEBUG
printf("vme = %s\n",initbuf.server);
printf("sockpfd = %i\n",sockpfd);
printf(" pfd = %i\n",pfd);
#endif

   FD_ZERO(&errfds);
   FD_ZERO(&readfds);
   while(1)
   {
     timeout.tv_sec = 60;
     timeout.tv_usec = 0;
     FD_SET(pfd,&readfds);
     FD_SET(sockpfd,&readfds);
     FD_SET(sockpfd,&errfds);
     if ((status=select(6,&readfds,(fd_set *)0,&errfds,&timeout)) < 0)
                                                                      exit(99);
     if (FD_ISSET(sockpfd,&errfds))
      {
        close(sockpfd);
        exit(99);
      }
     if (FD_ISSET(sockpfd,&readfds))
      {
       status = readn(sockpfd,(unsigned char *)lbuf,initbuf.cmd_len);
#ifdef DEBUG
printf("Receive packet %i bytes\n",status);
#endif
       if (status <= 0)
        {
          close(sockpfd);
          exit(99);
        }
#ifdef DEBUG
printf("Send private %i bytes\n",lbuf->len);
#endif
       status = en_write(pfd,&xmit_hdr, lbuf->buf, lbuf->len);
       if (status == FAILURE || status == TIMEOUT) lbuf->error = ETHER_TRANSMIT;
       else  lbuf->error = 0;
       lbuf->len = 0;
       status = writen(sockpfd,(unsigned char *)lbuf,initbuf.rpy_len);
       if (status <= 0)
        {
          close(sockpfd);
          exit(99);
        }
#ifdef DEBUG
printf("Send packet\n");
#endif
      }
     if (FD_ISSET(pfd,&readfds))
      {
       en_tmo(pfd,1);
       buflen=en_read(pfd,&recv_hdr,lbuf->buf);
#ifdef DEBUG
printf("Receive %i bytes - private\n",buflen);
#endif
       if (buflen == FAILURE || buflen == TIMEOUT) lbuf->error = ETHER_RECEIVE;
       else  lbuf->error = 0;
       lbuf->len = buflen;

       status = writen(sockpfd,(unsigned char *)lbuf,initbuf.rpy_len);
       if (status <= 0)
        {
          close(sockpfd);
          exit(99);
        }
      }
    }
}
/****************************************************************************
*
*   Catch signal SIGTERM.  Just cleanup and exit.
*
****************************************************************************/
void TermHandle(int sig)
{
   char msg[32];
   time_t tod;

   time(&tod);
   strftime(msg,21, "%d-%b-%y %H:%M:%S  ", localtime(&tod));
   fprintf(stderr, "%s***** VME server, PID=%i, Terminated *****\n",
	   msg, getpid());
   fflush(stderr);
   fflush(stdout);
   exit(EXIT_SUCCESS);
}
/***************************************************************************
*   See if vmedII is already running.  If it is, just exit.
***************************************************************************/
void loadchk(void)
{
   int  numln = 0,status = 0,pid,fd;
   FILE *infile;
   char tmpfilename[L_tmpnam];
   static char vmedII[] = "/vmedII";

   static char *ps[] = {"/bin/ps",
                             "auxw",
                              NULL };

/*
*   Run ps auxw to get the processes running
*/
   strcpy(tmpfilename,P_tmpdir);
   strcat(tmpfilename,"/VMEDXXXXXX");
   fd = mkstemp(tmpfilename);

   if ((pid = fork()) == 0)
    {
      freopen(tmpfilename,"w",stdout);
      execvp(ps[0],ps);
      perror(ps[0]);
      _exit(0);
    }
   else if (pid <= 0) {printf("vmedII loadchk fork failure \n"); exit(99);}
   while(wait(&status) != pid);
   close(fd);
/*
*   Open the input file
*/
   if ((infile = fopen(tmpfilename,"r")) == (FILE *)NULL)
     {
       fprintf(stdout,"vmedII loadchk - Cannot open input file\n");
       exit(2);
     }
/*
*  See if vmedII is already running.
*/
   while (fgets(in_line,sizeof(in_line),infile) != NULL)
    {
      if (strstr(in_line,vmedII)  != NULL) numln++;
    }
  fclose(infile);
  unlink(tmpfilename);
/*
*  If vmedII is running, just exit.
*/
  if (numln > 1)
    {
      printf("VmedII already running\n");
      exit(99);
    }
}
/*
 *  Print contents for packet_header structure for debugging 
*/
void prnthdr(struct Packet_Header pkt)
{
  int i;
  printf("\nPKT header information\n");
  printf("destination: ");
  printf("%x",pkt.Destination[0]);
  for (i=1;i<6;i++)
    printf(":%x",pkt.Destination[i]);
  printf("\n");

  printf("source: ");
  printf("%x",pkt.Source[0]);
  for (i=1;i<6;i++)
    printf(":%x",pkt.Source[i]);
  printf("\n");

  printf("protocol: ");
  printf("%x-%x",pkt.Protocol[0],pkt.Protocol[1]);
  printf("\n");

  printf("Ack flag: ");
  printf("%x", pkt.Ack);
  printf("\n");

  printf("Order number: ");
  printf("%i", pkt.Order);
  printf("\n");

  printf("Request number: ");
  printf("%i", pkt.Request_Number);
  printf("\n");

  return;
}

/*
 * sum a test array for diagnostics
 */
int sumarray(char *array, int size)
{
  int s = 0;
  int i;

  for (i=0;i<size;i++)
    s+=array[i];

  return(s);
}
