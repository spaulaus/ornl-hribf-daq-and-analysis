/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                         Copyright(C) 2001-2005
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
*    File:         acq2/Myriad/rtems/vmeacq/vmexx.c
*
*    Description:  A RTEMS task to do setup for VME modules
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    8/08/01    MCSQ         
*
*   10/31/01    MCSQ        Set Common mode(start or stop) in caen_tdc_write
*                           routine.
*
*    3/19/05    MCSQ        RTEMS version
*               MCSQ        Add SIS820 scaler
*               RLV         Add more 785, 775 and 792 modules
*    7/31/2015  RLV         Add MyRIAD module from ANL/DGS
*****************************************************************************/
#include "../include/vme_sys.h"
#include "../include/orph_udp.h"
#include "../include/sis3820.h"
#include "../include/caen.h"
#include "../include/vmexx.h"
#include "../include/myriad.h"
#include "../include/devices.h"

#include <rtems/rtems_bsdnet.h>
#include <rtems/error.h>
#include <libcpu/io.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

/*    Function Prototypes        */

static char caen775_ctl(struct caen_ctl *);
static char caen785_ctl(struct caen_ctl *);
static char caen792_ctl(struct caen_ctl *);

static char sis3820_ctl(struct sis_ctl  *);

static char myriad_ctl(struct myriad_ctrl *);
/*
*   External Functions
*/
extern void byte_swap(unsigned char *,int);
extern unsigned int  word_swap(unsigned short *,int);

/*      Global data              */

extern struct devices DEVTBL;

/*
*   Pointers to CAEN ADCs and TDCs
*/
static struct CAEN *CAEN785_LIST[12] = {       /* V785 ADC             */
                                  (struct CAEN *)CAEN785_1,
                                  (struct CAEN *)CAEN785_2,
                                  (struct CAEN *)CAEN785_3,
                                  (struct CAEN *)CAEN785_4,
                                  (struct CAEN *)CAEN785_5,
                                  (struct CAEN *)CAEN785_6,
                                  (struct CAEN *)CAEN785_7,
                                  (struct CAEN *)CAEN785_8,
                                  (struct CAEN *)CAEN785_9,
                                  (struct CAEN *)CAEN785_10,
                                  (struct CAEN *)CAEN785_11,
                                  (struct CAEN *)CAEN785_12};

static struct CAEN *CAEN775_LIST[12] = {       /* V775 TDC             */
                                  (struct CAEN *)CAEN775_1,
                                  (struct CAEN *)CAEN775_2,
                                  (struct CAEN *)CAEN775_3,
                                  (struct CAEN *)CAEN775_4,
                                  (struct CAEN *)CAEN775_5,
                                  (struct CAEN *)CAEN775_6,
                                  (struct CAEN *)CAEN775_7,
                                  (struct CAEN *)CAEN775_8,
                                  (struct CAEN *)CAEN775_9,
                                  (struct CAEN *)CAEN775_10,
                                  (struct CAEN *)CAEN775_11,
                                  (struct CAEN *)CAEN775_12};

struct CAEN *CAEN792_LIST[12] = {       /* V792 QDC             */
  (struct CAEN *)CAEN792_1,
  (struct CAEN *)CAEN792_2,
  (struct CAEN *)CAEN792_3,
  (struct CAEN *)CAEN792_4,
  (struct CAEN *)CAEN792_5,
  (struct CAEN *)CAEN792_6,
  (struct CAEN *)CAEN792_7,
  (struct CAEN *)CAEN792_8,
  (struct CAEN *)CAEN792_9,
  (struct CAEN *)CAEN792_10,
  (struct CAEN *)CAEN792_11,
  (struct CAEN *)CAEN792_12};

struct SIS3820 * SIS3820_LIST[2] = {
  (struct SIS3820 *)SIS3820_1,
  (struct SIS3820 *)SIS3820_2};

struct MyRIAD_Registers *MYRIAD_LIST = (struct MyRIAD_Registers *) MYRIAD_1;

char *dev785;
char *dev775;
char *dev792;
char *dev3820;
char *devmyriad;


// #define DEBUG


/*****************************************************************************
*****************************************************************************/
void vmexx(void)
{
  static struct sockaddr_in cli_addr,serv_addr;
  int   sockfd,status;
  socklen_t   clilen;
  static struct UDP_Packet in_buf,out_buf;
  int    size;
  union Cmd *cmd,*rply;

  dev785 = &DEVTBL.caen785_1;
  dev775 = &DEVTBL.caen775_1;
  dev792 = &DEVTBL.caen792_1;
  dev3820 = &DEVTBL.sis3820_1;
  devmyriad = &DEVTBL.myriad;

  sockfd = socket(AF_INET,SOCK_DGRAM,0);
  if (sockfd == -1) {perror("vmexx - socket error"); exit(1);}
  memset((char *) &serv_addr,0,sizeof(serv_addr));
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_port = htons(45000+PROTO_VMEIO);
  serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  status = bind(sockfd,(struct sockaddr *)&serv_addr,sizeof(serv_addr));
  if (status == -1) {perror("vmexx - bind"); exit(1);}

  clilen = sizeof(cli_addr);
  while (1)
   {
     size = recvfrom(sockfd,&in_buf,sizeof(in_buf),0,
                                   (struct sockaddr *)&cli_addr,&clilen);
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
     cmd = (union Cmd *)in_buf.Data;
     rply = (union Cmd *)out_buf.Data;
     *rply = *cmd;
     if (cmd->conv.func == CAEN775)
       {
         rply->conv.rpystat = caen775_ctl((struct caen_ctl *)rply);
         size = sizeof(struct caen_ctl);
       }
     else if (cmd->conv.func == CAEN785)
       {
         rply->conv.rpystat = caen785_ctl((struct caen_ctl *)rply);
         size = sizeof(struct caen_ctl);
       }
     else if (cmd->conv.func == CAEN792)
       {
         rply->conv.rpystat = caen792_ctl((struct caen_ctl *)rply);
         size = sizeof(struct caen_ctl);
       }
     else if (cmd->conv.func == SIS3820MOD)
       {
         rply->sca.rpystat = sis3820_ctl((struct sis_ctl *)rply);
         size = sizeof(struct sis_ctl);
       }
     else if (cmd->conv.func == MYRIADMOD)
       {
         rply->sca.rpystat = myriad_ctl((struct myriad_ctrl *)rply);
         size = sizeof(struct myriad_ctrl);
       }
      else
       {
         size = sizeof(struct vmeio_stat);
         rply->conv.rpystat = VME_ERR_FUNC;
       }
     out_buf.DataSize = size;
     word_swap((unsigned short *)&out_buf.DataSize,2);
     byte_swap((unsigned char *)&out_buf.DataSize,4);
     out_buf.Sequence = in_buf.Sequence;
     size = size + PKTHDRLEN;
     status=sendto(sockfd,&out_buf,size,0,(struct sockaddr *)&cli_addr,clilen);
     if (status < 0) {
        perror("vmexx - error at sendto");
        fprintf(stderr,"sockfd=%i, size=%i, clilen=%i\n",sockfd,size,(int)clilen);
     }
#ifdef DEBUG
fprintf(stderr,"sockfd = %i, clilen = %i\n",sockfd,(int)clilen);
fprintf(stderr," Sent %d bytes\n",size);
#endif
   }
}
/****************************************************************************
****************************************************************************/
static char caen775_ctl(struct caen_ctl *cmd)
{
   int i,cvtnum;
   short srange,smode;
   struct CAEN *caen;

   cvtnum = cmd->cvt_num;
   if (cvtnum < 0 || cvtnum >11) return(CAEN_ERR_NONEXIST);
   if (!dev775[cvtnum]) return(CAEN_ERR_NONEXIST);
   caen =  CAEN775_LIST[cvtnum];
   byte_swap((unsigned char *)cmd->data,sizeof(cmd->data));

   if (cmd->rw)
     {
       for(i=0; i < 32; i++) caen->thresholds[i] = cmd->data[i];
       eieio();
       srange = cmd->range;
       caen->range = srange;
       if (cmd->mode) caen->bit_set2 = 0x400;
       else  caen->bit_clear2 = 0x400;
     }
   else
     {
       for(i=0; i < 32; i++) cmd->data[i] = caen->thresholds[i] & 0x1ff;
       eieio();
       srange = caen->range;
       cmd->range = srange;
       smode = caen->bit_set2;
       if (smode & 0x400) cmd->mode = 1;
       else  cmd->mode = 0;
     }
   byte_swap((unsigned char *)cmd->data,sizeof(cmd->data));
#ifdef DEBUG
   for (i=0; i <32; i++)
   {
   cvtnum = caen->buf[0];
   printf("caen = %x  %x %x %x\n",
           cvtnum,
           cvtnum & HDR_MASK,
           cvtnum & CHAN, 
           cvtnum & DAT );
   }
#endif
   return(0);
}
/****************************************************************************
****************************************************************************/
static char caen785_ctl(struct caen_ctl *cmd)
{
   int i,cvtnum;
   struct CAEN *caen;

   cvtnum = cmd->cvt_num;
   if (cvtnum < 0 || cvtnum > 11) return(CAEN_ERR_NONEXIST);
   if (!dev785[cvtnum]) return(CAEN_ERR_NONEXIST);
   caen =  CAEN785_LIST[cvtnum];
   byte_swap((unsigned char *)cmd->data,sizeof(cmd->data));

   if (cmd->rw)
     {
       for(i=0; i < 32; i++) caen->thresholds[i] = cmd->data[i];
     }
   else
     {
       for(i=0; i < 32; i++) cmd->data[i] = caen->thresholds[i] & 0x1ff;
     }
   eieio();

   byte_swap((unsigned char *)cmd->data,sizeof(cmd->data));
   return(0);
}
/****************************************************************************
****************************************************************************/
char caen792_ctl(struct caen_ctl *cmd)
{
  int i, cvtnum;
  short siped;
  struct CAEN *caen;
   /* Note that in caen_ctl, range is the same as iped. */

  cvtnum = cmd->cvt_num;
  if (cvtnum < 0 || cvtnum > 11) return(CAEN_ERR_NONEXIST);
  if (!dev792[cvtnum]) return(CAEN_ERR_NONEXIST);
  caen =  CAEN792_LIST[cvtnum];
  byte_swap((unsigned char *)cmd->data,sizeof(cmd->data));
  
  if (cmd->rw) {
    for(i=0; i < 32; i++) caen->thresholds[i] = cmd->data[i];
    siped = cmd->range;
    caen->range = siped;
  }
  else {
    for(i=0; i < 32; i++) cmd->data[i] = caen->thresholds[i] & 0x1ff;
    siped = caen->range;
    cmd->range = siped;
  }
  
  byte_swap((unsigned char *)cmd->data,sizeof(cmd->data));
  return(0);
}
/****************************************************************************
****************************************************************************/

char sis3820_ctl(struct sis_ctl *cmd)
{
   int i,scanum;
   struct SIS3820 *sis;


   scanum = cmd->sca_num;
   if (scanum < 0 || scanum > 0) return(SCA_ERR_NONEXIST);
   if (!dev3820[scanum]) return(SCA_ERR_NONEXIST);
   sis =  SIS3820_LIST[scanum];

   if (cmd->code == 0)        /* Read counters                               */
     {
       for (i=0; i < 32; i++) {
          cmd->data[i] = sis->counter[i];
       }
       byte_swap((unsigned char *)cmd->data,sizeof(cmd->data));
       word_swap((unsigned short *)cmd->data,sizeof(cmd->data)/2);
     }
   else if (cmd->code == 1)   /* Clear counters                              */
     {
       sis->Key_cnt_clear = 0;
     }
   else if (cmd->code == 2)    /* enable counting                            */
     {
       sis->op_mode = 1;
       sis->Key_enable = 0;
     }
   else if (cmd->code == 3)    /* disable counting                           */
     {
       sis->Key_disable = 0;
       sis->op_mode = 1;
     }
   return (0);
}
/****************************************************************************
****************************************************************************/

char myriad_ctl(struct myriad_ctrl *cmd)
{
   struct MyRIAD_Registers *myr;

   if (!devmyriad) return(MYR_ERR_NONEXIST);
   myr =  MYRIAD_LIST;
   if (0) {     
   } else if (1) {
   }
   return (0);
}
