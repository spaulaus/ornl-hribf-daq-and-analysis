/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 1992
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
*    File:         /usr/users/mcsq/Dvme3/fastxx.c
*
*    Description:  A VME processor task which provides host access
*                  to registers and data in the FASTBUS 1821
*                  sequencer.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    6/22/92    MCSQ         
*
*   12/29/92    MCSQ       The code devchk.c now builds a table of flags
*                          which indicate what hardware devices are available
*                          in the VME system.  Changed this code to use
*                          the flags for LRS1131 and LRS1821.  If either
*                          of these devices are not present,  Host I/O
*                          requests return and error.  Tis better than
*                          a bus error in the VME processor.
*****************************************************************************/
#include "devices.h"
#include "vme_sys.h"
#include "system.h"
#include "vmeprom.h"
#include "orph.h"
#include "lan.h"
#include "lan_lib.h"
#include "lrs.h"
#include "fastbus.h"

#include <stdio.h>
#include <stdlib.h>

#define  MOD  0xe    /* 1821 device code  */

/*    Function Prototypes        */
void lrs_init(void);

/*      Global data              */
static struct LRS_VME *lrs_cmd_ptr = (struct LRS_VME *)LRS1;
static unsigned short *lrs = (unsigned short *)LRS1;
static struct devices *devtbl = DEVTBL;

main()
{
  int    i,size,level;
  char   *inbuf,*outbuf;
  struct Ether_Packet *out_hdr;
  struct fast_buffer *cmds;
  struct fastd *fast;
  struct fast_return *rpy;

  lrs_init();
  lan_open(PROTO_FASTBUS,&outbuf,&out_hdr);
  while (1)
   {
     size = lan_read(0,&inbuf);
     if (!size) 
       { 
         printf("\nEthernet read error\n"); 
         exit(0);
       }
     cmds = (struct fast_buffer *)inbuf;
     rpy = (struct fast_return *)outbuf;
     byte_swap_(&cmds->count,2);
     i = cmds->count;
     if(i > 369)
       {
         printf("Packet data error: %x \n",i);
         i = 0; 
         cmds->count = 0;
       }
     fast = &cmds->fasts[0];
/*
*   If the LRS1131 or LRS1821 is not present, return an error code
*   to the host I/O request.
*/
     if (!devtbl->lrs1131 || !devtbl->lrs1821)
       {
         while(i)
          {
            rpy->Status = -1;
            rpy++;
            i--;
          }
       }
     while (i)
      {
        byte_swap_(&fast->Data,2);
        level = set_intr_level_(0x500);
        if (fast->F == 0)
          {
/*
*   Write function
*/
             *(lrs+SIB_DIRECT(MOD,fast->A)) = fast->Data;
             rpy->Data = fast->Data;
          }
        else
          {
/*
*    Read function
*/
             rpy->Data = *(lrs+SIB_DIRECT(MOD,fast->A));
           }
        set_intr_level_(level);
        rpy->Status = 0;
        rpy++;
        fast++;
        i--;
      }
     i = cmds->count * 4;
     if(i != 0)
       {
         byte_swap_(outbuf,i);
         lan_reply(i,ACK);
       }
   }
}
/****************************************************************************
*   Initialize LRS 1131 Module
*
*   Call:   No arguments
*   Return: None
*
*
****************************************************************************/
void lrs_init(void)
{
  unsigned short *lrs_mem = (unsigned short *)(LRS1 + LIST);
  int  i,level;

/*
*  If the LRS1131 is not present, just return.
*/
  if (!devtbl->lrs1131) return;
  level = set_intr_level_(0x500);
  lrs_cmd_ptr->csr = 0;
  lrs_cmd_ptr->nta = 0;
  lrs_cmd_ptr->wc = 0;
  lrs_cmd_ptr->sib = 0;
  for (i=0; i < 8192; i++) *lrs_mem++ = LIST_DONE;
  for (i=0; i < 8192; i++) *lrs_mem++ = 0xaa55;
  set_intr_level_(level);
  return;
}
