/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 2005
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
*    Environment:  VME based Data Acquisition System
*
*    File:         /tera/mcsq/Drtems/Dcnaf/cnafxx.c
*
*    Description:  A RTEMS task to do CAMAC cycles for the host.
*                  Does a list of CNAFs supplied over ethernet from the
*                  host.  Returns X, Q, Crate On-line and nonexistent crate
*                  status flags and for read functions the data.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    1/03/03    MCSQ         
*
*    3/16/05    RLV & MCSQ   RTEMS version with UDP packets
*
*****************************************************************************/
#include "../include/vme_sys.h"
#include "../include/ksc.h"
#include "../include/orph_udp.h"
#include "../include/cnaf.h"
#include "../include/devices.h"

#include <bsp.h>
#include <rtems/rtems_bsdnet.h>
#include <rtems/error.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

 struct camac {
     char  c;
     char  n;
     char  a;
     char  f;
     unsigned short  d[2];
      };

 struct reply {
     char  x;
     char  q;
     char  cok;
     char  online;
     unsigned short  d[2];
      };

/*    Function Prototypes        */
static void ksc_init(void);
static int ksc_camio(struct camac *);
static int ksc_online(int c);
static void byte_swap(unsigned char *,int);
static unsigned int  word_swap(unsigned short *,int);

/*      Global data              */
extern struct devices DEVTBL;

static struct KSC_VME *ksc1_ptr = (struct KSC_VME *)KSC1;
static struct KSC_VME *ksc2_ptr = (struct KSC_VME *)KSC2;
static struct KSC_VME noxksc;


/*****************************************************************************
*****************************************************************************/

/**********
#define DEBUG
**********/

void cnafxx()
{
  static struct sockaddr_in cli_addr,serv_addr;
  int   clilen,sockfd;
  int    err,i,size,status;
  static struct UDP_Packet in_buf,out_buf;
  struct cnaf_buffer *cmds;
  struct camac *cam;
  struct reply *rpy;

  ksc_init();

  if (!DEVTBL.ksc2917a) ksc1_ptr = &noxksc;
  if (!DEVTBL.ksc2917b) ksc2_ptr = &noxksc;

  sockfd = socket(AF_INET,SOCK_DGRAM,0);
  if (sockfd == -1) {perror("cnafxx - socket error"); exit(1);}
  memset((char *) &serv_addr,0,sizeof(serv_addr));
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_port = htons(45000+PROTO_CNAF);
  serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  status = bind(sockfd,(struct sockaddr *)&serv_addr,sizeof(serv_addr));
  if (status == -1) {perror("cnafxx - bind"); exit(1);}

  clilen = sizeof(cli_addr);
  while (1)
    {
     size = recvfrom(sockfd,&in_buf,sizeof(in_buf),0,
                                   (struct sockaddr *)&cli_addr,(socklen_t *)&clilen);
     if (size < 0) { 
         fprintf(stderr,"\nEthernet read error\n"); 
         exit(0);
     }
#ifdef DEBUG
fprintf(stderr,"Sockfd = %x\n",sockfd);
fprintf(stderr,"Family = %x\n",cli_addr.sin_family);
fprintf(stderr,"Port   = %x\n",cli_addr.sin_port);
fprintf(stderr,"IP addr = %lx\n",cli_addr.sin_addr.s_addr);
fprintf(stderr," Received %d bytes\n",size);
fprintf(stderr," Sequence = %x\n",in_buf.Sequence);
fprintf(stderr," DataSize = %x\n",in_buf.DataSize);
#endif

     cmds = (struct cnaf_buffer *)in_buf.Data;
     rpy = (struct reply *)out_buf.Data;
     byte_swap((unsigned char *)&cmds->count,4);
     i = word_swap((unsigned short *)&cmds->count,2);
#ifdef DEBUG
fprintf(stderr," CAMAC count = %i\n",i);
#endif
     if(i > MAX_CNAF)
       {
         fprintf(stderr,"Packet data error: %x \n",i);
         i = 0; 
         cmds->count = 0;
       }
     cam = (struct camac *)&cmds->cnafs[0];
     while (i)
      {
        byte_swap((unsigned char *)&cam->d[0],4);
        err = 0;
        status = ksc_camio(cam);
        if (status & KSC_CSR_TMO)
         {
           rpy->q = 0;
           rpy->x = 0;
           rpy->online = 0;
           if (status & KSC_CSR_RST) rpy->cok = 0;
           else  rpy->cok = 1;
         }
        else
         {
          rpy->cok = 1;
          rpy->online = 1;
          rpy->q = 1;
          rpy->x = 1;
          if (status & KSC_CSR_NOX) rpy->x = 0;
          if (status & KSC_CSR_NOQ) rpy->q = 0;;
         }
        rpy->d[0] = cam->d[0];
        rpy->d[1] = cam->d[1] & 0xff;
        byte_swap((unsigned char *)rpy->d,4);
        i--;
        cam++;
        rpy++;
      }
     i = (cmds->count * 8);
     out_buf.DataSize = i;
     word_swap((unsigned short *)&out_buf.DataSize,2);
     byte_swap((unsigned char *)&out_buf.DataSize,4);
     out_buf.Sequence = in_buf.Sequence;
     i = i + PKTHDRLEN;
     if(i != 0) {
        status=sendto(sockfd,&out_buf,i,0,(struct sockaddr *)&cli_addr,clilen);
        if (status < 0) {
          perror("cnafxx - error at sendto");
          fprintf(stderr,"sockfd=%i, i=%i, clilen=%i\n",sockfd,i,clilen);
        }
#ifdef DEBUG
fprintf(stderr,"sockfd = %i, clilen = %i\n",sockfd,clilen);
fprintf(stderr," Sent %d bytes\n",i);
#endif
      }


    }
}
/****************************************************************************
****************************************************************************/
static unsigned int word_swap(unsigned short *buf,int count)
{
    register unsigned short tmp1;

    while(count > 0)
     {
       tmp1 = *buf;
       *buf = *(buf+1);
       buf++;
       *buf++ = tmp1;
       count -= 2;
     }
    return (*((int *)(buf - 2)));
}
/****************************************************************************
****************************************************************************/
static void byte_swap(unsigned char *buf,int count)
{
    register unsigned char tmp1;

    while(count > 0)
     {
       tmp1 = *buf;
       *buf = *(buf+1);
       buf++;
       *buf++ = tmp1;
       count -= 2;
     }
    return;
}
/****************************************************************************
*   Initialize KSC 2917 Module
*
*   Call:   No arguments
*   Return: None
*
*   If the DMA channel is active or the command list is active, we do
*   a power-up reset to the module.  Always clear all DMA flags (operation
*   complete and all error flags), set the DMA device operation control
*   register and initialize the upper address byte and address modifier.
*
****************************************************************************/
static void ksc_init(void)
{
  register struct KSC_VME *ksc;
  int  i;
  rtems_interrupt_level level;

  rtems_interrupt_disable(level);
/*
*   Initialize the first KSC 2917
*/
  if (DEVTBL.ksc2917a)
    {
      ksc = ksc1_ptr;
      eieio();
      if((ksc->cse & KSC_CSE_ACT) || (ksc->csr & KSC_CSR_GO))
                              ksc->csr = KSC_CSR_RST; /* Reset 2917 interface */
      ksc->cse = KSC_CSE_COC | KSC_CSE_ABT | KSC_CSE_ERR; /* Clear DMA error and
                                                                   done flags */
      ksc->doc = KSC_DOC_CAM_RD;
      ksc->amr = KSC_AMR;
      ksc->cma = 0;
      for (i=0; i < 8192; i++) ksc->cmr = HALT;
      eieio();
    }
/*
*   Initialize the second KSC 2917
*/
  if (DEVTBL.ksc2917b)
    {
      ksc = ksc2_ptr;
      eieio();
      if((ksc->cse & KSC_CSE_ACT) || (ksc->csr & KSC_CSR_GO))
                              ksc->csr = KSC_CSR_RST; /* Reset 2917 interface */
      ksc->cse = KSC_CSE_COC | KSC_CSE_ABT | KSC_CSE_ERR; /* Clear DMA error and
                                                                   done flags */
      ksc->doc = KSC_DOC_CAM_RD;
      ksc->amr = KSC_AMR;
      ksc->cma = 0;
      for (i=0; i < 8192; i++) ksc->cmr = HALT;
      eieio();
    }
  rtems_interrupt_enable(level);
  return;
}
/****************************************************************************
*
*   Test for crate on-line.
*
*   CALL:   crate  -  Crate number to check.
*
*   Return: Returns 1 if crate is on-line.
*                   0 if crate if off-line.
*                  -1 if crate does not exist.
*
****************************************************************************/
static int ksc_online(int crate)
{
  register struct KSC_VME *ksc = ksc1_ptr;
  int  status = 1,tmo = 15;
  rtems_interrupt_level level;

  if (crate <= 7) ksc = ksc1_ptr;
  if (crate >= 10)
    {
      ksc = ksc2_ptr;
      crate -= 10;
    }
  rtems_interrupt_disable(level);
  ksc->cma = 0;
  ksc->cmr = CAM(crate,WS16,A_DIS);
  ksc->cmr = NAF(30,0,1);
  ksc->cmr = HALT;
  ksc->cma = 0;
  ksc->csr = KSC_CSR_GO;
  eieio();
  while((ksc->csr & (KSC_CSR_DONE | KSC_CSR_RDY)) == 0 && tmo )tmo--;
  if ((ksc->csr & KSC_CSR_TMO) | !tmo)
   {
     status = -1;
     if (!tmo) ksc->csr = KSC_CSR_RST;
   }
  else
   {
     if (ksc->dlr & 0x2000) status = 0;;
   }
  while((ksc->csr & KSC_CSR_DONE) == 0 && !tmo) tmo--;
  rtems_interrupt_enable(level);
  return(status);
}

/****************************************************************************
*
*   Routine to do one CAMAC CNAF.  The CAMAC operation is described in the
*   structure camac.  A software timeout is used to prevent hanging when
*   the addressed crate is switched off-line.  The acquisition interrupt
*   is disabled while the KSC 2917 is in use.
*
*  Call:   pointer to struct camac
*
*  Return: KSC 2917 status register.
*
****************************************************************************/
static int ksc_camio(struct camac *cambuf)
{
   rtems_interrupt_level level = 0;
   register struct KSC_VME *ksc = ksc1_ptr;
   register int wd0,wd1;
   register unsigned short status;
   register int tmo = 15;	/* Timeout is approx. 1.5*tmo microseconds */


   if (cambuf->c <= 7) ksc = ksc1_ptr;
   else if (cambuf->c >= 10)
     {
       ksc = ksc2_ptr;
       cambuf->c -= 10;
     }
/*
*   Build the command words for KSC 2917 interface from the data in struct
*   camac.
*/
   wd0 = CAM(cambuf->c,WS24,A_DIS);
   wd1 = NAF(cambuf->n,cambuf->a,cambuf->f);

   switch  (cambuf->f & 0x18)
     {
/*
*    CAMAC non-data transfer functions - F(8) thru F(15) & F(24) thru F(31)
*/
       case  0x8:
       case  0x18:
         cambuf->d[0] = 0;
         cambuf->d[1] = 0;
         rtems_interrupt_disable(level);  /* disable acq intrrupts */
         ksc->cma = 0;
         ksc->cmr = wd0;
         ksc->cmr = wd1;
         ksc->cmr = HALT;
         ksc->cma = 0;
         ksc->csr = KSC_CSR_GO;
         eieio();
         break;
/*
*    CAMAC read functions - F(0) thru F(7)
*/
       case  0x0:
         rtems_interrupt_disable(level);  /* disable acq intrrupts */
         ksc->cma = 0;
         ksc->cmr = wd0;
         ksc->cmr = wd1;
         ksc->cmr = HALT;
         ksc->cma = 0;
         ksc->csr = KSC_CSR_GO;
         eieio();
         do
          {
            if(ksc->csr & KSC_CSR_RDY)
             {
              cambuf->d[0] = ksc->dlr;
              cambuf->d[1] = ksc->dhr;
              break;
             }
            tmo--;
          } while(!(ksc->csr & KSC_CSR_DONE) && tmo);
         break;
/*
*   CAMAC write functions - F(16) thru F(23)
*/
       case  0x10:
         rtems_interrupt_disable(level);  /* disable acq intrrupts */
         ksc->cma = 0;
         ksc->cmr = wd0;
         ksc->cmr = wd1;
         ksc->cmr = HALT;
         ksc->cma = 0;
         ksc->csr = KSC_CSR_CAM_WR | KSC_CSR_GO;
         eieio();
         do
          {
            if(ksc->csr & KSC_CSR_RDY)
             {
              ksc->dlr = cambuf->d[0];
              ksc->dhr = cambuf->d[1];
              break;
             }
            tmo--;
          } while(!(ksc->csr & KSC_CSR_DONE) && tmo);
         break;
     }
/*
*   If the CAMAC operation was executed, we return the KSC 2917 status
*   register.  On a timeout (i.e. tmo == 0), clear the run flag in the
*   KSC 2917 and return timeout status.  NOTE: The caller cannot 
*   distinguish between a crate off-line and a nonexistent crate!!
*/
   eieio();
   do
     {
       status = ksc->csr;
     } while (tmo-- && !(status & KSC_CSR_DONE));
   if (tmo < 0)
     {
       status = KSC_CSR_TMO | KSC_CSR_RST;
       ksc->csr = KSC_CSR_RST;
     }
/*********************
{
volatile unsigned short tmp[16];
int i;

printf("wd0 = %x,  wd1 = %x\n",wd0,wd1);
for(i=0; i < 4; i++)
{
  ksc->cma = i;
  eieio();
  tmp[i] =  ksc->cmr;
}
for(i=0; i < 4; i++)
{
  printf("addr = %i, data = %x\n",i,tmp[i]);
}
}
*********************/
   rtems_interrupt_enable(level);  /* restore interrupt level */

   return (status);
}
