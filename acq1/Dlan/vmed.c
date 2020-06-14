/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1996-2002
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
*    Environment:  VME Acquisition for Linux - Alpha server
*
*    File:         /usr/users/mcsq/Dlan/vmed.c
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
#include "vmed.h"

/*
*   When you compile for hhirf directory, define HHIRF in this code
*   or use 'makeall hhirf'
#define  HHIRF
*/

/*
*     Function Prototypes
*/
void TermHandle(int);
void loadchk(void);
int readn(int,unsigned char *,int);
int writen(int,unsigned char *,int);
int  WR_io(unsigned char *,int ,unsigned char *,short *);
int  WO_io(unsigned char *,int );
int  RO_io(int);
void DEBUG_io(int,struct Vmed *);
void CLDsignal(int);
void WRserve(int);

#define  ETHER_RECEIVE  -32
#define  ETHER_TRANSMIT -33
#define  ETHER_OPEN     -34

/*
*    Global variables
*/

struct Vme_start initbuf;
           char  in_line[257];

#define CLIENTS  20
struct connects {
             pid_t  lpid;
struct sockaddr_in  raddr;
}  conns[CLIENTS];

/***************************************************************************
*
***************************************************************************/
main(int argc, char *argv[])
{
   char msg[32];
   int sockfd,newsockfd,clilen;
   int  status,childpid,fd,fdmax,port;
   time_t tod;
   static struct sockaddr_in cli_addr,serv_addr;
   char node[16];
   sigset_t blksig;

   static char logfile[80] = "/usr/acq/log/vmed.log";

/*
*  If we were started by the process init, bypass the following.
*/
   if (getppid() != 1)
     {
/*
*   Check for a copy of vmed running.  We allow only one at startup.
*/
       loadchk();
       signal(SIGTSTP, SIG_IGN);
       signal(SIGTTIN, SIG_IGN);
       signal(SIGTTOU, SIG_IGN);
       if ((childpid = fork()) < 0)
         {
           perror("vmed first fork error");
           exit(0);
         }
       else if (childpid > 0) exit(0);

/*
*   Make ourself a process group leader and get rid of the controlling
*   terminal.
*/
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
*   Set the default directory so it can core dump.  Shouldn't ever happen.
*   HA! HA!
*/

/*
*   Setup to ignore most signals
*/
   signal(SIGINT, SIG_IGN);
   signal(SIGCONT, SIG_IGN);
   signal(SIGUSR1, SIG_IGN);
   signal(SIGUSR2, SIG_IGN);
   signal(SIGQUIT, SIG_IGN);
/*
*   Accept SIGTERM signal to exit
*/
   signal(SIGTERM, TermHandle);

   time(&tod);
   strftime(msg,21,"%d-%b-%y %H:%M:%S  ",localtime(&tod));
   fprintf(stderr,"%s********** VME server Started    *********\n",msg);

   sockfd = socket(AF_INET,SOCK_STREAM,0);
   if (sockfd == -1) {perror("socket error"); exit(1);}
   bzero((char *) &serv_addr,sizeof(serv_addr));
   serv_addr.sin_family = AF_INET;
   serv_addr.sin_port = htons(6996);
   serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
   status = bind(sockfd,(struct sockaddr *)&serv_addr,sizeof(serv_addr));
   if (status == -1) {perror("bind"); exit(1);}
   listen(sockfd,5);
   CLDsignal(0);

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

      sigemptyset(&blksig);
      sigaddset(&blksig,SIGCLD);
      sigprocmask(SIG_BLOCK,&blksig,NULL);

      if ((childpid = fork()) == 0)
        {
          close(sockfd);
          WRserve(newsockfd);
          exit(0);
        }
      else if (childpid == -1) perror("fork failure");
      else if (childpid > 0)
        {
          int i;

          port = ntohs(cli_addr.sin_port);
          fprintf(stderr,"%s** Open connection to %s.%i  PID = %i\n",msg,
                                   inet_ntoa(cli_addr.sin_addr),port,childpid);
          for(i=0; i < CLIENTS; i++)
           {
             if (conns[i].lpid == 0)
               {
                 conns[i].lpid = childpid;
                 conns[i].raddr = cli_addr;
                 break;
               }
           }
          if (i == CLIENTS)
           {
             fprintf(stderr,"%s-- Client table full\n",msg);
             for (i=0; i < CLIENTS; i++)
              {
                port = ntohs(conns[i].raddr.sin_port);
                fprintf(stderr,"$$$$ Table: pid = %i %s.%i\n",conns[i].lpid,
                                      inet_ntoa(conns[i].raddr.sin_addr),port);
                conns[i].lpid = 0;

              }
             conns[0].lpid = childpid;
             conns[0].raddr = cli_addr;
           }
        }
      sigprocmask(SIG_UNBLOCK,&blksig,NULL);

      close(newsockfd);
    }
   return(0);
}
/******************************************************************************
******************************************************************************/
void WRserve(int sockpfd)
{
   int status, hdr = sizeof(struct Vmed_hdr);
   char env[64] = "VME=";
   static struct Vmed lbuf;

   status = readn(sockpfd,(unsigned char *)&initbuf,sizeof(struct Vme_start));
   if (status <= 0)
    {
      close(sockpfd);
      return;
    }

#ifdef DEBUG
printf("received %i bytes - start header\n",status);
printf("vme = %s\n",initbuf.server);
printf("proto = %x\n",initbuf.proto);
printf("cmd len = %i\n",initbuf.cmd_len);
printf("rpy len = %i\n",initbuf.rpy_len);
#endif

   if (initbuf.proto == SOFT) DEBUG_io(sockpfd,&lbuf);
   while(1)
     {
       status = readn(sockpfd,(unsigned char *)&lbuf,initbuf.cmd_len);
       if (status <= 0)
        {
          close(sockpfd);
          return;
        }
#ifdef DEBUG
printf("received %i bytes - command\n",status);
#endif
       if (initbuf.cmd_len > hdr && initbuf.rpy_len > hdr)
         {
           status = WR_io(lbuf.buf,lbuf.len,lbuf.buf,&lbuf.len);
         }
       else if (initbuf.cmd_len > hdr && initbuf.rpy_len <= hdr)
         {
           status = WO_io(lbuf.buf,lbuf.len);
           lbuf.len = 0;
         }
       else if (initbuf.cmd_len <= hdr && initbuf.rpy_len > hdr)
         {
           status = RO_io(sockpfd);
           lbuf.len = 0;
         }
       else status = -99;
       lbuf.error = status;
#ifdef DEBUG
printf("ethernet status %i\n",status);
#endif
       status = writen(sockpfd,(unsigned char *)&lbuf,initbuf.rpy_len);
       if (status <= 0)
        {
          close(sockpfd);
          return;
        }
#ifdef DEBUG
printf("sent %i bytes - reply\n",status);
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
*****************************************************************************/
int  WR_io(unsigned char *xbuf, int xlen, unsigned char *rbuf, short *rlen)
{

     static int pfd = -1;                   /* file handle set in en_open */
     static struct Packet_Header xmit_hdr,recv_hdr;
     int   buflen,status;

     if(pfd == -1)
      {
        pfd = en_open(initbuf.server, &xmit_hdr, initbuf.proto, CLIENT);
        if (pfd == FAILURE) return (ETHER_OPEN);
      }
     *rlen = 0;
     status = en_write(pfd,&xmit_hdr, xbuf, xlen);
     if (status == FAILURE || status == TIMEOUT) return (ETHER_TRANSMIT);
/*
*      wait for the reply
*/
     do
       {
         en_tmo(pfd,10);
         buflen=en_read(pfd,&recv_hdr, rbuf);
         en_tmo(pfd,0);
         if (buflen == FAILURE || buflen == TIMEOUT) return (ETHER_RECEIVE);
        } while (xmit_hdr.Order != recv_hdr.Order);
     *rlen = buflen;
     return (0);
}
/*****************************************************************************
*   Send command packet and receive reply from the VME processor.
*
*  Return:  0  -  OK
*          ETHER_RECEIVE -  No reply from the VME processor
*          ETHER_TRANSMIT -  Packet transmission error
*          ETHER_OPEN -  Packet filter open error
*****************************************************************************/
int  WO_io(unsigned char *xbuf, int xlen)
{

     static int pfd = -1;                   /* file handle set in en_open */
     static struct Packet_Header xmit_hdr,recv_hdr;
     int   buflen,status;

     if(pfd == -1)
      {
        pfd = en_open(initbuf.server, &xmit_hdr, initbuf.proto, CLIENT);
        if (pfd == FAILURE) return (ETHER_OPEN);
      }
     status = en_write(pfd,&xmit_hdr, xbuf, xlen);
     if (status == FAILURE || status == TIMEOUT) return (ETHER_TRANSMIT);
     return (0);
}
/*****************************************************************************
*   Send command packet and receive reply from the VME processor.
*
*  Return:  0  -  OK
*          ETHER_RECEIVE -  No reply from the VME processor
*          ETHER_TRANSMIT -  Packet transmission error
*          ETHER_OPEN -  Packet filter open error
*****************************************************************************/
int  RO_io(int sockpfd)
{

     static unsigned char buffer[1536];
     struct Vmed *lbuf = (struct Vmed *)buffer;
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
         en_tmo(pfd,60);
         buflen=en_read(pfd,&recv_hdr, rbuf);
         en_tmo(pfd,0);
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
       printf("Can't open device '%s'\n",initbuf.server);
       lbuf->len = sizeof(struct Vmed_hdr);
       lbuf->error = ETHER_OPEN;
       status = writen(sockpfd,(unsigned char *)lbuf,initbuf.rpy_len);
       if (status <= 0)
        {
          close(sockpfd);
        }
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
     timeout.tv_sec =20;
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
       lbuf->len = sizeof(struct Vmed_hdr);
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
       en_tmo(pfd,5);
       buflen=en_read(pfd,&recv_hdr,lbuf->buf);
#ifdef DEBUG
printf("Receive %i bytes - private\n",buflen);
#endif
       en_tmo(pfd,0);
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
   strftime(msg,21,"%d-%b-%y %H:%M:%S  ",localtime(&tod));
   fprintf(stderr,"%s********** VME server Terminated *********\n",msg);
   fflush(stderr);
   fflush(stdout);
   exit(EXIT_SUCCESS);
}
/****************************************************************************
*
*   Catch signal SIGCLD.
*
****************************************************************************/
void CLDsignal(int sig)
{
  int i,status,port;
  pid_t pid,cpid;
  char msg[32];
  time_t tod;

  if(sig != 0)
    {
/*
*   Loop till there are no more children or no other child is
*   exiting.
*/
      while(pid = wait3(&status,WNOHANG,NULL))
       {
         if (pid == -1 && errno == EINTR) continue;
         if (pid <= 0) break;
         for (i=0; i < CLIENTS; i++)
          {
            if ((cpid = conns[i].lpid) == pid)
              {
                conns[i].lpid = 0;
                break;
              }
          }
         time(&tod);
         strftime(msg,21,"%d-%b-%y %H:%M:%S  ",localtime(&tod));
         if (i >= CLIENTS)
           {
             fprintf(stderr,"%s ** Close connection to unknown host. PID = %i\n",
                                                                       msg,pid);
             for (i=0; i < CLIENTS; i++)
              {
                port = ntohs(conns[i].raddr.sin_port);
                fprintf(stderr,"$$$$ Table: pid = %i %s.%i\n",conns[i].lpid,
                                      inet_ntoa(conns[i].raddr.sin_addr),port);
                conns[i].lpid = 0;

              }
           }
         else
           {
             port = ntohs(conns[i].raddr.sin_port);
             fprintf(stderr,"%s** Close connection to %s.%i pid = %i\n",msg,
                                 inet_ntoa(conns[i].raddr.sin_addr),port,cpid);
           }
        }
    }
  signal(SIGCLD,CLDsignal);
  return;
}
/***************************************************************************
*   See if vmed is already running.  If it is, just exit.
***************************************************************************/
void loadchk(void)
{
   int  numln = 0,status = 0,pid;
   FILE *infile;
   char tmpfilename[L_tmpnam];
   static char vmed[] = "vmed";

   static char *ps[] = {"/bin/ps",
                             "auxw",
                              NULL };

/*
*   Run ps auxw to get the processes running
*/
   tmpnam(tmpfilename);

   if ((pid = fork()) == 0)
    {
      freopen(tmpfilename,"w",stdout);
      execvp(ps[0],ps);
      perror(ps[0]);
      _exit(0);
    }
   else if (pid <= 0) {printf("vmed loadchk fork failure \n"); exit(99);}
   while(wait(&status) != pid);
/*
*   Open the input file
*/
   if ((infile = fopen(tmpfilename,"r")) == (FILE *)NULL)
     {
       fprintf(stdout,"vmed loadchk - Cannot open input file\n");
       exit(2);
     }
/*
*  See if vmed is already running.
*/
   while (fgets(in_line,sizeof(in_line),infile) != NULL)
    {
      if (strstr(in_line,vmed)  != NULL) numln++;
    }
  fclose(infile);
  unlink(tmpfilename);
/*
*  If vmed is running, just exit.
*/
  if (numln > 1)
    {
      printf("Vmed already running\n");
      exit(99);
    }
}
