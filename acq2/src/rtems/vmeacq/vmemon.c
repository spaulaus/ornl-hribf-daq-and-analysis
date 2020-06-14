/***************************************************************************** *
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                         Copyright(C) 1993-1998
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
*    File:         /usr/users/mcsq/Dvme3/vmemon.c
*
*    Description:  
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   10/15/93    MCSQ       Original  
*
*    4/24/94    MCSQ       Added check for FASTBUS readout errors.  Also,
*                          if the KSC3982 List Sequencer is in use, we
*                          check for it disabled.  Either case sends
*                          message to the host.
*                          Now these tests and rate reporting occur only
*                          when acquisition is running.
*
*    3/ 9/98    MCSQ       Send rate and parameters/event info to host
*                          only is data are being acquired.
*****************************************************************************/
#include <bsp.h>
#include <rtems/rtems_bsdnet.h>
#include <rtems/error.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "../include/vme_sys.h"
#include "../include/ksc.h"
#include "../include/cnaf.h"
#include "../include/devices.h"
#include "../include/Acq_Params.h"
#include "../include/orphmsg.h"

/*    structure for calls to ksc_camio                                      */
struct camac {
     unsigned char  c;
     unsigned char  n;
     unsigned char  a;
     unsigned char  f;
	      int   d;
};

/*
*   Host Message types.  Define with bytes and words swapped to make
*   DEC happy.
*/
#define INFORM  0x01000000       /* really type = 1 */
#define WARN    0x02000000
#define PANIC   0x03000000

/*
*       Local variables
*/
static char                 error_msg[81];

/*
*    Data in the memory segment  "Acq_Params"
*/
static struct acq_share *share;

/*************************    Function Prototypes    ************************/

extern void host_message(int ,char *,char *);
static int ksc_camio(struct camac *);

#define  INTERVAL 30000

extern char *ACQ_SHARED_RAM;
/****************************************************************************
*
****************************************************************************/
void vmemon(void)
{
   unsigned long start_evt;
   int  time = INTERVAL/100;
   int  addr,size,stat,rate;
   float fsize;
   struct camac cnaf = {0,0,0,0,0};

   share = (struct acq_share *)ACQ_SHARED_RAM;
   while(1)
    {
       start_evt = share->event_number;
       rtems_task_wake_after((rtems_interval)INTERVAL);
       if (share->acqrun == 1)
         {
           rate = (share->event_number - start_evt)/time;
           fsize = share->avg_param_size;
           fsize = fsize/1.125;
           size = fsize/4.;
/*
*   Send message to acquisition logger process on the host indicating the
*   events per second and parameters per second.
*
*   Send message ONLY if data are being acquired.
*/
           if (share->event_number - start_evt)
             {
               sprintf(error_msg,"VME Stats: %i evts/sec, %i parameters/evt.",
                                                                    rate,size);
               host_message(INFORM,error_msg,"vmemon  ");
             }
           if (share->FB_enabled == 1 && share->FB_error != 0)
             {
               sprintf(error_msg,"FASTBUS readout error!");
               host_message(PANIC,error_msg,"vmemon  ");
             }
           if (share->KSC3982_enabled == 1)
             {
               cnaf.c = LIST_SEQ_C;
               cnaf.n = LIST_SEQ_N;
               cnaf.f = 0;
               cnaf.a = 2;
               stat = ksc_camio(&cnaf);
               if ((stat & KSC_CSR_NOQ) == 0)
                 {
                   addr = cnaf.d;
                   cnaf.f = 1;
                   cnaf.a = 12;
                   stat = ksc_camio(&cnaf);
                   if ((cnaf.d & 0x20) != 0)
                     {
                       addr = addr - 1;
                       sprintf(error_msg,"KSC3982 NO_X Exception at address %x",
                                                                         addr);
                     }
                   else
                     {
                       sprintf(error_msg,"KSC3982 Status = %x at address %x",
                                                                  cnaf.d,addr);
                     }
                   host_message(PANIC,error_msg,"vmemon  ");
                   cnaf.f = 16;
                   cnaf.a = 2;
                   cnaf.d = addr;
                   stat = ksc_camio(&cnaf);
                   cnaf.f = 0;
                   cnaf.a = 1;
                   stat = ksc_camio(&cnaf);
                   cnaf.f = cnaf.d & 0x1f;
                   cnaf.d = cnaf.d >> 5;
                   cnaf.a = cnaf.d & 0xf;
                   cnaf.d = cnaf.d >> 4;
                   cnaf.n = cnaf.d & 0x1f;
                   sprintf(error_msg,"KSC3982 error at N = %i, A = %i, F = %i",
                                                          cnaf.n,cnaf.a,cnaf.f);
                   host_message(PANIC,error_msg,"vmemon  ");
                 }
             }
         }
    }
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
   rtems_interrupt_level level = 0;
   register struct KSC_VME *ksc = (struct KSC_VME *)KSC1;
   register int             wd0,wd1;
   register unsigned short  status;
   register int tmo = 15;       /* Timeout is approx. 1.5*tmo microseconds */

/*
*   Build the command words for KSC 2917 interface from the data in struct
*   camac.
*/
   wd0 = CAM(cambuf->c,WS16,A_DIS);
   wd1 = NAF(cambuf->n,cambuf->a,cambuf->f);

   switch  (cambuf->f & 0x18)
     {
/*
*    CAMAC non-data transfer functions - F(8) thru F(15) & F(24) thru F(31)
*/
       case  0x8:
       case  0x18:
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
	      cambuf->d = ksc->dlr;
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
	      ksc->dlr = cambuf->d;
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
       status = KSC_CSR_TMO | KSC_CSR_NOX | KSC_CSR_NOQ;
       ksc->csr = KSC_CSR_RST;
     }
   else if ((status & KSC_CSR_TMO) != 0)
                                    status = status | KSC_CSR_NOX |KSC_CSR_NOQ;
   rtems_interrupt_enable(level);  /* restore interrupt level */
   return (status);
}
