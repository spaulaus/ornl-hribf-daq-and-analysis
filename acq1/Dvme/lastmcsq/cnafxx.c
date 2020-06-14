/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992-1997
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
*    File:         /usr/users/mcsq/Dvme3/cnafxx.c
*
*    Description:  A CPU40 task to do CAMAC cycles for the host.
*                  Does a list of CNAFs supplied over ethernet from the
*                  host.  Returns X, Q, Crate On-line and nonexistent crate
*                  status flags and for read functions the data.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    4/25/92    MCSQ         
*
*    8/11/92    MCSQ     Changed for new ksc.h
*
*   12/29/92    MCSQ     The code devchk.c now builds a table of flags which
*                        indicate what hardware devices are present in the 
*                        VME system.  Change this code to use the flag for
*                        KSC2917.  If it is not present, return an error
*                        to host I/O requests.
*
*    2/19/95    MCSQ     Reply packets now request an Ack from the host.
*
*   12/13/97    MCSQ     Add second KSC 2917.  Uses crate numbers 10 thru 17.
*****************************************************************************/
#include "devices.h"
#include "ksc.h"
#include "vme_sys.h"
#include "system.h"
#include "vmeprom.h"
#include "orph.h"
#include "lan.h"
#include "lan_lib.h"
#include "cnaf.h"

#include <stdio.h>
#include <stdlib.h>

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
void ksc_init(void);
int ksc_camio(struct camac *);
int ksc_online(int c);

/*      Global data              */
static struct KSC_VME *ksc1_ptr = (struct KSC_VME *)KSC1;
static struct KSC_VME *ksc2_ptr = (struct KSC_VME *)KSC2;
static struct KSC_VME noxksc;
static struct devices *devtbl = DEVTBL;

/*****************************************************************************
*****************************************************************************/
main()
{
  int    err,i,size,status;
  char   *inbuf,*outbuf;
  struct Ether_Packet *out_hdr;
  struct cnaf_buffer *cmds;
  struct camac *cam;
  struct reply *rpy;

  ksc_init();
  lan_open(PROTO_CNAF,&outbuf,&out_hdr);
  task_priority_(-1,1,69);
  if (!devtbl->ksc2917a) ksc1_ptr = &noxksc;
  if (!devtbl->ksc2917b) ksc2_ptr = &noxksc;
  while (1)
   {
     size = lan_read(0,&inbuf);
     if (!size) 
       { 
         fprintf(stderr,"\nEthernet read error\n"); 
         exit(0);
       }
     cmds = (struct cnaf_buffer *)inbuf;
     rpy = (struct reply *)outbuf;
     byte_swap_(&cmds->count,4);
     i = word_swap_(&cmds->count,2);
     if(i > 184)
       {
         fprintf(stderr,"Packet data error: %x \n",i);
         i = 0; 
         cmds->count = 0;
       }
     cam = (struct camac *)&cmds->cnafs[0];
     while (i)
      {
        byte_swap_(&cam->d[0],4);
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
        byte_swap_(rpy->d,4);
        i--;
        cam++;
        rpy++;
      }
     i = cmds->count * 8;
     if(i != 0) lan_reply(i,ACK);
   }
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
void ksc_init(void)
{
  register struct KSC_VME *ksc;
  int  i,level;

  level = set_intr_level_(0x500);
/*
*   Initialize the first KSC 2917
*/
  if (devtbl->ksc2917a)
    {
      ksc = ksc1_ptr;
      if((ksc->cse & KSC_CSE_ACT) || (ksc->csr & KSC_CSR_GO))
                              ksc->csr = KSC_CSR_RST; /* Reset 2917 interface */
      ksc->cse = KSC_CSE_COC | KSC_CSE_ABT | KSC_CSE_ERR; /* Clear DMA error and
                                                                   done flags */
      ksc->doc = KSC_DOC_CAM_RD;
      ksc->amr = KSC_AMR;
      ksc->cma = 0;
      for (i=0; i < 8192; i++) ksc->cmr = HALT;
    }
/*
*   Initialize the second KSC 2917
*/
  if (devtbl->ksc2917b)
    {
      ksc = ksc2_ptr;
      if((ksc->cse & KSC_CSE_ACT) || (ksc->csr & KSC_CSR_GO))
                              ksc->csr = KSC_CSR_RST; /* Reset 2917 interface */
      ksc->cse = KSC_CSE_COC | KSC_CSE_ABT | KSC_CSE_ERR; /* Clear DMA error and
                                                                   done flags */
      ksc->doc = KSC_DOC_CAM_RD;
      ksc->amr = KSC_AMR;
      ksc->cma = 0;
      for (i=0; i < 8192; i++) ksc->cmr = HALT;
    }
  set_intr_level_(level);
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
int ksc_online(int crate)
{
  register struct KSC_VME *ksc = ksc1_ptr;
  int  level,status = 1,tmo = 15;

  if (crate <= 7) ksc = ksc1_ptr;
  if (crate >= 10)
    {
      ksc = ksc2_ptr;
      crate -= 10;
    }
  level = set_intr_level_(0x500);
  ksc->cma = 0;
  ksc->cmr = CAM(crate,WS16,A_DIS);
  ksc->cmr = NAF(30,0,1);
  ksc->cmr = HALT;
  ksc->cma = 0;
  ksc->csr = KSC_CSR_GO;
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
  set_intr_level_(level);
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
int ksc_camio(struct camac *cambuf)
{
   int  level;
   register struct KSC_VME *ksc;
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
         level = set_intr_level_(0x500);  /* disable acq intrrupts */
         ksc->cma = 0;
         ksc->cmr = wd0;
         ksc->cmr = wd1;
         ksc->cmr = HALT;
         ksc->cma = 0;
         ksc->csr = KSC_CSR_GO;
         break;
/*
*    CAMAC read functions - F(0) thru F(7)
*/
       case  0x0:
         level = set_intr_level_(0x500);  /* disable acq intrrupts */
         ksc->cma = 0;
         ksc->cmr = wd0;
         ksc->cmr = wd1;
         ksc->cmr = HALT;
         ksc->cma = 0;
         ksc->csr = KSC_CSR_GO;
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
         level = set_intr_level_(0x500);  /* disable acq intrrupts */
         ksc->cma = 0;
         ksc->cmr = wd0;
         ksc->cmr = wd1;
         ksc->cmr = HALT;
         ksc->cma = 0;
         ksc->csr = KSC_CSR_CAM_WR | KSC_CSR_GO;
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
   do
     {
       status = ksc->csr;
     } while (tmo-- && !(status & KSC_CSR_DONE));
   if (tmo < 0)
     {
       status = KSC_CSR_TMO | KSC_CSR_RST;
       ksc->csr = KSC_CSR_RST;
     }
   set_intr_level_(level);  /* restore interrupt level */
   return (status);
}
