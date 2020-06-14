/*****************************************************************************
*
*                            HHIRF COMPUTER GROUP
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992-1998
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
*    File:         /usr/users/mcsq/Dvme3/mem_mgr.c
*
*    Description:  Memory management task for the VME processor.
*
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    4/10/91    MCSQ         Original
*
*    5/8/92     MCSQ         Set task priority just below the Ethernet
*                            driver so the host has a better chance of
*                            talking to the VME.  Also make all terminal
*                            output conditional so it can be disabled when
*                            debug info is not needed.
*
*    5/ 5/93    MCSQ         Add a function - DEVICES - to return the
*                            the Ethernet addresses used by the VME processor
*                            and status of hardware interfaces as tested
*                            at boot time.
*
*    2/12/95    MCSQ         In the function alloc, add code to zero memory
*                            allocated when start of memory block is above
*                            the Acq_Params table.
*
*    4/ 8/96    MCSQ         Set the real-time clock chip to the time
*                            of the workstation in function set_time().
*
*    3/ 4/97    MCSQ         For the CPU-60, the task Lan_Driver is changed
*                            to Lan_Driver60.
*
*    2/24/98    MCSQ         For the CPU-60, the memory used by the ethernet
*                            controller should be allocated so it cannot be
*                            used by other processes.  This is done in routine
*                            mgr_init().
*
*    4/17/98    MCSQ         Y2K patch for RTC chip.  If years since 1900 is
*                            greater than 99, load RTC chip with (years since
*                            1900) -100.  For example, in year 2001 we load
*                            the RTC chip with 01 for the two digit year.
*****************************************************************************/
#include <rtems/rtems_bsdnet.h>
#include <rtems/error.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <bsp.h>

#include  "../include/vme_sys.h"
#include  "../include/orph_udp.h"
#include  "../include/mem_mgr.h"
#include  "../include/devices.h"

/**********
***   If VMEBUG is defined certain diagnositic data is output to
***   terminal attached to port 1 of the VME processor.
#define VMEBUG
**********/

extern char *ACQ_PARAMS;
extern struct devices DEVTBL;


/*
*   Function Prototypes
*/
static int srloader(char *, int);
static int  get_record(int ,char **,char *);
static int  get_byte(char *,int *);
static int  devices_task(union Cmd *);
static void byte_swap(unsigned char *,int);
static unsigned int  word_swap(unsigned short *,int);


/****************************************************************************
****************************************************************************/
void memmgrxx(void)
{
   static struct sockaddr_in cli_addr,serv_addr;
   int   clilen,sockfd,status;
   static struct UDP_Packet in_buf, out_buf;

   union Cmd *cmd;
   unsigned char  *inbuf,*outbuf;


   sockfd = socket(AF_INET,SOCK_DGRAM,0);
   if (sockfd == -1) {perror("memmgrxx - socket error"); exit(1);}
   memset((char *) &serv_addr,0,sizeof(serv_addr));
   serv_addr.sin_family = AF_INET;
   serv_addr.sin_port = htons(45000+PROTO_CODE);
   serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
   status = bind(sockfd,(struct sockaddr *)&serv_addr,sizeof(serv_addr));
   if (status == -1) {perror("memmgrxx - bind"); exit(1);}

   while(1)
    {
      int  cnt,size = 0;

      clilen = sizeof(cli_addr);
      cnt = recvfrom(sockfd,&in_buf,sizeof(in_buf),0,
                                (struct sockaddr *)&cli_addr,&clilen);
      if (size < 0) {
         fprintf(stderr,"\nEthernet read error\n");
         exit(0);
      }

      inbuf = (unsigned char *)&in_buf + PKTHDRLEN;
      outbuf = (unsigned char *)&out_buf + PKTHDRLEN;

      byte_swap(inbuf,4);
      size = sizeof(struct Reply);      /* size of most common reply message */
      cmd = (union Cmd *)inbuf;
      switch (cmd->start.func)          /* dispatch on function code       */
       {
         case  LOAD_MEM:
          if (cmd->load.mid < 0 || cmd->load.mid > 0) 
             {
               cmd->reply.status = -ILLINDEX;
               break;
             }
          cmd->reply.status = srloader(cmd->load.data,cnt - PKTHDRLEN - 4);
          if(cmd->reply.status != OK)printf("s record error = %x\n",
                                                            cmd->reply.status);
          break;
         case  DEVICES:
          cmd->reply.status = devices_task(cmd);
          size = sizeof(struct Devices);
          break;
         default:
          cmd->reply.status = -ILLFUNC;
          break;
       }
      word_swap((unsigned short *)&cmd->reply.status,2); /* prepare reply packet */
      byte_swap((unsigned char *)&cmd->reply.status,4);
      out_buf = in_buf;
      out_buf.DataSize = size;
      word_swap((unsigned short *)&out_buf.DataSize,2);
      byte_swap((unsigned char *)&out_buf.DataSize,4);
      size = size + PKTHDRLEN;
      status=sendto(sockfd,&out_buf,size,0,(struct sockaddr *)&cli_addr,clilen);
      if (status < 0) {
        perror("memmgrxx - error at sendto");
        fprintf(stderr,"sockfd=%i, i=%i, clilen=%i\n",sockfd,size,clilen);
      }
   }
}
/****************************************************************************
*    S Record loader
*  Call:   buf  - Pointer to buffer containing S records
*          size - Buffer size in bytes
*
*  Return:  0   -  buffer OK
*       nonzero - error code from get_byte or get_record
*
*        load->load = highest loaded address + 1
*        load->start = start address from S7, S8 or S9 record.
****************************************************************************/
int srloader(char *buf, int size)
{
  char     *buf_end = buf+size;
  char     ch;
  int      err,adrlen;
  int      addr;

  while(buf < buf_end)
   {
     ch = *buf++;
     if (ch == 'S' || ch == 's')
       {
         addr = 0;
         adrlen = 2;
         ch = *buf++;
         switch (ch - '0')
           {
             case     3:        /* S3 records - 32 bit address             */
               adrlen++;        
             case     2:        /* S2 records - 24 bit address             */
               adrlen++;
             case     1:        /* S1 records - 16 bit address             */
               if ((err = get_record(adrlen,&buf,buf_end))) return (err);
               break;
             case     7:        /* S7 record - 32 bit start address        */
               adrlen++;
             case     8:        /* S8 record - 24 bit start address        */
               adrlen++;
             case     9:        /* S9 record - 16 bit start address        */
               err = get_record(adrlen,&buf,buf_end);
               if (err != OK) return(err);

               return (OK);
             case     0:
             case     4:
             case     5:        /* S0, S4 or S5 records - ignore then      */
             default:           
               while (*buf++ != '\n' && buf < buf_end);
               break;
           }       /* end switch */
       }
    }              /* end while  */
   return (OK);
}
/****************************************************************************
*    Process one S Record
*  Call:   adrlen  - number of address bytes
*          buf     - Pointer to buffer containing S records
*          buf_end - Buffer size in bytes
*
*  Return:  OK        - no error
*           SRILLHEX  - Non Hex char - returned from get_byte routine
*           SRCHKSUM  - Checksum error
*           SRCNTERR  - Illegal byte count
*           SRADRERR  - Address limits error
****************************************************************************/
static int get_record(int adrlen,char **cptr,char *buf_end)
{
  char *buf = *cptr;
  char *buf1;
  char *store;
  int  addr = 0,cnt,value;
  int  checksum,err,i;

/*
*   Get record byte count.  This is a count of data bytes following the
*   byte count including the address and checksum.
*/
  err = get_byte(buf,&cnt);
  if (err != OK) return (err);                  /* Hex conversion error    */
  if (buf + cnt * 2 >= buf_end) return (-SRCNTERR);  /* Invalid byte count */
  buf += 2;
  checksum = cnt;
  buf1 = buf;
/*
*   Check record for validity.
*/
  for (i = 0; i < cnt; i++, buf1 += 2)
    {
      if ((err = get_byte(buf1,&value)) != OK) return(err);
      checksum += value;
    }
  checksum++;
  if (checksum & 0xff) return(-SRCHKSUM);      /* Checksum error          */
/*
*   Get record start address.  adrlen is the number of address bytes.
*/
  cnt -= adrlen + 1;
  while (adrlen > 0)
    {
      addr <<= 8;
      get_byte(buf,&value);
      buf += 2;
      addr += value;
      adrlen--;
    }
/*
*   Verify that data is within the address limits specified in struct Loader.
*/
  if ((addr < 0) || ((addr + cnt) > 131072)) return (-SRADRERR);
/*
*   Load the data record to memory
*/
  store = ACQ_PARAMS + addr;
  while (cnt > 0)
    {
      get_byte(buf,&value);
      buf += 2;
      *store++ = value;
      cnt--;
    }
  buf += 2;
  *cptr = buf;
  return (OK);
}
/****************************************************************************
*    ASCII Hex to binary conversion - one byte
*  Call:   buf     - Pointer to buffer containing S records
*          value  - returned value(byte)
*
*  Return: OK       - no error
*          SRILLHEX - Non Hex character found
****************************************************************************/
static int get_byte(char *buf,int *value)
{
  int   val,val1;

  val = *buf++ - '0';
  if (val > 9) val -= 7;
  if (val > 15) val -= 32;
  if (val < 0 || val > 15) return (-SRILLHEX);
  val1 = *buf++ - '0';
  if (val1 > 9) val1 -= 7;
  if (val1 > 15) val1 -= 32;
  if (val1 < 0 || val1 > 15) return (-SRILLHEX);
  *value = (val << 4) + val1;
  return (OK);
}
/****************************************************************************
*
*  DEVICES - Send Ethernet addresses and interface device status to host.
****************************************************************************/
int  devices_task(union Cmd *cmd)
{
   struct devices *devtbl = &DEVTBL;
   unsigned char *ucptr;
   static char our_ether_address[6];
   static char host_ether_address[6];
   static char broad_ether_address[6];

   ucptr = our_ether_address;
   memcpy(cmd->devices.enet_vme,ucptr,6);
   ucptr  = host_ether_address;
   memcpy(cmd->devices.enet_host,ucptr,6);
   ucptr = broad_ether_address;
   memcpy(cmd->devices.enet_broad,ucptr,6);
   cmd->devices.devtbl = *devtbl;
   return OK;
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
