/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992-2003
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
*    File:         /usr/users/mcsq/Dlinux/Dacq/load_acq_params.c
*
*    Description:  A FORTRAN callable routine which loads the data acquisition
*                  parameters into the VME processor.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    8/16/92    MCSQ         
*
*   12/30/92    MCSQ      Changed to new Ethernet library -
*                          /usr/users/mcsq/lan2
*
*    1/ 9/93    MCSQ        Change for multiple VME systems.  The VME
*                           ethernet address is by default "vme".  If
*                           the enviroment variable VME is set, that
*                           string is used instead of "vme".
*
*    9/25/95    MCSQ       New ethernet library in /usr/users/mcsq/Dlan.
*
*    9/17/96    MCSQ       Increased size of char variable server to 8.
*
*    9/04/02    MCSQ       Linux version
*
*    3/19/03    MCSQ       Changes to pkt_io.c functions.
*
*    3/29/03    MCSQ       Fixed error reporting.
*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include "../Dacq/pkt_io.h"
#include "../Dlan/orph_pf.h"
#include "../Dvme/mem_mgr.h"

/*    Function Prototypes       */
  void load_acq_params__(int *,int *);
  void load_acq_params_(int *,int *);
static void sr_close(void);
static void sr_write(char *,int );
static void put_byte(unsigned char **,unsigned char );
static void send_array(char * ,int ,int );
static void mgr_error(int);

/*   Global variables           */
static struct Vmed xbuf,rbuf;
static char                 server[8] = "vme";
static union Cmd *          inbuf = (union Cmd *)rbuf.buf;
static union Cmd *          outbuf = (union Cmd *)xbuf.buf;
static char                 prog[] = "load_acq_params";
static int                  buflen;
static unsigned char       *buf_ptr;
static int                  mem_id;

/****************************************************************************
*   FORTRAN  call format:
*
*       CALL LOAD_ACQ_PARAMS(ARRAY,COUNT)
*
*          ARRAY  -  An array of Data Acquisition parameters.  Herein, this
*                    array is treated as a BYTE array.  Therefore, any 
*                    byte swapping required due to INTEGER*2 and INTEGER*4
*                    FORTRAN arrays must be done the the user prior to calling
*                    this routine.
*          COUNT  -  Number of bytes to load into the VME processor.
*********************
*   Example:
*             INTEGER*4  BUF(4096)
*
*             CALL BYTE_SWAP(BUF,4096)    !swap words and swap bytes
*             CALL LOAD_ACQ_PARAMS(BUF,4096*4)
*
****************************************************************************/
void load_acq_params__(int *array,int *count)
{
   load_acq_params_(array,count);
}
void load_acq_params_(int *array,int *count)
{
   int i,size,status;
   char *cptr;

   if (*count > 126972)
     {
       fprintf(stderr,"Byte count too large for VME processor!\n");
       exit(3);
     }
   if ((cptr = getenv("VME")) != NULL) strcpy(server,cptr);
   xbuf.len = sizeof(struct Display);
   rbuf.len = sizeof(struct Display);
   pkt_open(&xbuf,&rbuf, CODE, 5);
/*
*    Get the memory map of the VME processor.
*/
   outbuf->display.func = DISPLAY_MEM;
   pkt_send(&xbuf);
   status = pkt_recv(&rbuf);
   if (status != 0)
     {
       mgr_error(status);
       exit(1);
     }
/*
*   Data Acquisition parameters are to be loaded in a segment named
*   Acq_Params.   Search the memory map for this segment.
*/
   for(i=0; i < MEM_SEGS; i++)
     {
       if(!strncmp(inbuf->display.tsk[i].name,"Acq_Params",10)) break;
     }
/*
*   If the segment is not found, just exit with error message.
*/
   if (i >= MEM_SEGS)
     {
       fprintf(stderr,"Can't find Acq_Params segment in VME processor\n");
       exit(2);
     }
   pkt_close();

   mem_id = i;
   buflen = sizeof(outbuf->load.data);
   buf_ptr = (unsigned char *)outbuf->load.data;
   outbuf->start.func = LOAD_MEM;
   outbuf->start.mid = mem_id;

   /*rlv This fix sets the output buffer to the maximum size, to help close. */
   rbuf.len=0;
   xbuf.len=sizeof(struct Load)-8;
   pkt_open(&xbuf,&rbuf, CODE, 5);
/*
*   Convert data to S record format and download the VME processor.
*/
   send_array((char *)array,*count,
                             (int)(inbuf->display.tsk[mem_id].start + 0x1002));
}
/****************************************************************************
*   Pack the S record into the output packet and send it when full.
****************************************************************************/
static void sr_write(char *buf,int size)
{
   int status;

   if (size >= buflen)
     {
       *buf_ptr = '\0';
       rbuf.len = 0;
       xbuf.len = sizeof(struct Load)-buflen+1;
       status = pkt_io(&xbuf,&rbuf,CODE,5);
       if (status != 0) {mgr_error(status); exit(30);}
       if (inbuf->reply.status != OK)
         {
           mgr_error(inbuf->reply.status);
           exit (99);
         }
       buf_ptr = (unsigned char *)outbuf->load.data;
       buflen = sizeof(outbuf->load.data);
     }
   memcpy(buf_ptr,buf,(size_t)size);
   buf_ptr += size;
   buflen -= size;
}
/****************************************************************************
*   Send the last packet.
****************************************************************************/
static void sr_close()
{
   int  status;

   *buf_ptr = '\0';
   rbuf.len = 0;
   xbuf.len = sizeof(struct Load)-buflen+1;
   status = pkt_io(&xbuf,&rbuf,CODE,5);
   if (status != 0) {mgr_error(status); exit(30);}
}
/****************************************************************************
*   Send the data array to the VME processor.  Routine converts the array
*   data to S record format and sends it to the VME processor.
****************************************************************************/
static void send_array(char * array,int count,int vme)
{

#define  SR_BYTES  239     /* Maximum data bytes/S record  */

   static unsigned char srbuf[SR_BYTES*2+14];
   unsigned char *cptr;
   int           chksum,byte_cnt,sr_cnt;
   unsigned char tmp;

   while(count)
    {
      byte_cnt = SR_BYTES;
      if (byte_cnt > count) byte_cnt = count;
/*
*   Initialize the record checksum and start record header.
*/
      chksum = byte_cnt + 5;
      sr_cnt = (byte_cnt + 7) * 2;
      count -= byte_cnt;
      cptr = srbuf;
      *cptr++ = 'S';
      *cptr++ = '3';
/*
*   Put in record data byte count.  Count includes the address and
*   the checksum.   Following the count is a 32-bit address of the
*   first data byte.
*/
      put_byte(&cptr,byte_cnt + 5);
      tmp = vme >> 24;
      chksum += tmp;
      put_byte(&cptr,tmp);
      tmp = vme >> 16;
      chksum += tmp;
      put_byte(&cptr,tmp);
      tmp = vme >> 8;
      chksum += tmp;
      put_byte(&cptr,tmp);
      tmp = vme;
      chksum += tmp;
      put_byte(&cptr,tmp);
/*
*   The loop here outputs the "real" data part of the S record.
*/
      while (byte_cnt)
       {
         tmp = *array++;
         chksum += tmp;
         put_byte(&cptr,tmp);
         --byte_cnt;
         ++vme;
       }
/*
*   The checksum is the ones compliment of the Modulo 256 sum of byte
*   count, the address and the data bytes.
*/
      chksum = ~chksum;
      put_byte(&cptr,(unsigned char)chksum);
      *cptr++ = '\n';
/*
*   Pass the S record to the Ethernet packet builder.
*/
      sr_write((char *)srbuf,sr_cnt+1);
   }
/*
*   Force transmission of the last packet.
*/
  sr_close();
}
/****************************************************************************
*   Convert a byte to ASCII hex.
****************************************************************************/
static void put_byte(unsigned char **buf,unsigned char byte)
{
   unsigned char tmp;
   unsigned char *cptr = *buf;

   tmp = (byte >> 4) + 0x30;
   if (tmp > 0x39) tmp += 7;
   *cptr++ = tmp;
   tmp = (byte & 0xf) + 0x30;
   if (tmp > 0x39) tmp += 7;
   *cptr++ = tmp;
   *buf = cptr;
}
/****************************************************************************
*   Report error from remote memory manager
****************************************************************************/
static void  mgr_error(int error)
{
   fprintf(stderr," %s error \7 - ",prog);
   switch (-error)
    {
      case  SRADRERR:
        fprintf(stderr,"S record address outside allocated memory\n");
        break;
      case  SRCHKSUM:
        fprintf(stderr,"S record checksum error\n");
        break;
      case  SRILLHEX:
        fprintf(stderr,"S record has nonhex character\n");
        break;
      case  SRCNTERR:
        fprintf(stderr,"S record byte count error\n");
        break;
      case  ILLINDEX:
        fprintf(stderr,"Illegal memory ID - must be in range 3 <= ID <= %d\n",
                                                                      MEM_SEGS);
        break;
      case  KILLERR:
        fprintf(stderr,"Request to kill nonexistent task\n");
        break;
      case  TSKMEM:
        fprintf(stderr,"Insufficient task memory allocated\n");
        break;
      case  ALLOCERR:
        fprintf(stderr,"Memory allocation table is full\n");
        break;
      case  ILLFUNC:
        fprintf(stderr,"Unrecognized command function code\n");
        break;
      case  PERMTSK:
        fprintf(stderr,"Request to kill a permamently resident task\n");
        break;
      case  NOMEMAVAIL:
        fprintf(stderr,"Not enough memory available\n");
        break;
      case  MEMDUPC:
        fprintf(stderr,"Duplicate memory/task name\n");
        break;
      case  ILLFREE:
        fprintf(stderr,"Free memory request error - size <= 0\n");
        break;
      case  -ETHER_OPEN:
        fprintf (stderr,"Ethernet open failure - %s\n",server);
        break;
      case  -ETHER_TRANSMIT:
        fprintf (stderr,"Ethernet transmit error - %s\n",server);
        break;
      case  -ETHER_RECEIVE:
        fprintf (stderr,"Ethernet receive timeout- %s\n",server);
        break;
      default:
        if (error < 0) error = -error;
        fprintf(stderr,"Unknown error code = %i\n",-error);
        break;
    }
}
