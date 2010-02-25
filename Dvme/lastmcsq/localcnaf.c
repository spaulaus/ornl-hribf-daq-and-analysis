/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1992,1993
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
*    File:         /usr/users/mcsq/Dvme3/localcnaf.c
*
*    Description:  Routine for the VME processor.  Does CNAFs commanded
*                  from a local terminal.  Very similar in function to
*                  the code 'cnaf' for the workstation.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   11/ 1/92    MCSQ         
*****************************************************************************/
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "vme_sys.h"
#include "ksc.h"

/*    structure for calls to ksc_camio                                      */
struct camac {
     unsigned char  c;
     unsigned char  n;
     unsigned char  a;
     unsigned char  f;
     union {
             int  dat;
             unsigned short d[2];
           }data;
}  cnaf;

/*
*       Local variables
*/
static struct KSC_VME *ksc_ptr = (struct KSC_VME *)KSC1;

char  *cmds[] = {"c","n","a","f","d","x","e","l","h",NULL};

char  line[81],command[10],data[10];
/*************************    Function Prototypes    ************************/
int  ksc_camio(struct camac *);
char *gfield(char *,char *,int);

main()
{
   char  *cptr;
   int   i,err,temp,args,stat;
   int   x,q,cok,online;

   x = q = 0;
   cok = online = 1;
   cnaf.c = cnaf.a = cnaf.f = cnaf.data.dat = 0;
   cnaf.n = 1;
   printf("\nVME local interactive CNAF\n\n");
   while(1)
    {
      printf("cmd> ");
      if (fgets(line,81,stdin) == NULL) continue;
      for (i=0; i < 81; i++)
         {
           if (isupper(line[i])) line[i] = (char)tolower(line[i]);
         }
      if ((cptr = gfield(command,line,2)) == NULL) continue;
      if ((cptr = gfield(data,cptr,8)) == NULL) args = 0;
      else  args = 1;
      for (i = 0; cmds[i] != NULL; i ++)
        {
          if (strcmp(command,cmds[i]) == 0) break;
        }
      err = 0;
      temp = 0;
      switch (i)
        {
          case 0:                    /* crate number            */
            if (args == 0) err = 1;
            else if (sscanf(data,"%i",&temp) == 0) err = 1;
            else if (temp < 0 || temp > 7) err = 1;
            if (err) printf("Invalid Crate number");
            else  cnaf.c = temp;
            break;
          case 1:                    /* module number           */
            if (args == 0) err = 1;
            else if (sscanf(data,"%i",&temp) == 0) err = 1;
            else if (temp < 1 || temp > 31) err = 1;
            if (err) printf("Invalid Module number");
            else  cnaf.n = temp;
            break;
          case 2:                    /* subaddress              */
            if (args == 0) err = 1;
            else if (sscanf(data,"%i",&temp) == 0) err = 1;
            else if (temp < 0 || temp > 15) err = 1;
            if (err) printf("Invalid Module number");
            else  cnaf.a = temp;
            break;
          case 3:                    /* function code           */
            if (args == 0) err = 1;
            else if (sscanf(data,"%i",&temp) == 0) err = 1;
            else if (temp < 0 || temp > 31) err = 1;
            if (err) printf("Invalid CAMAC function");
            else  cnaf.f = temp;
            break;
          case 4:                    /* data                    */
            if (args == 0) err = 1;
            else if (sscanf(data,"%x",&temp) == 0) err = 1;
            else if (temp < 0 || temp > 0xffffff) err = 1;
            if (err) printf("Invalid CAMAC data");
            else  cnaf.data.dat = temp;
            break;
          case 5:                    /* execute                 */
            if (args == 0) err = 1;
            else if (sscanf(data,"%i",&temp) == 0) err = 1;
            else if (temp < 0) err = 1;
            if (err) temp = 1;
            while(temp)
             {
               stat = ksc_camio(&cnaf);
               if (stat & KSC_CSR_TMO)
                {
                  q = 0;
                  x = 0;
                  online = 0;
                  if (stat & KSC_CSR_RST) cok = 0;
                  else  cok = 1;
                }
               else
                {
                  cok = 1;
                  online = 1;
                  q = 1;
                  x = 1;
                  if (stat & KSC_CSR_NOX) x = 0;
                  if (stat & KSC_CSR_NOQ) q = 0;;
                }
               if (stat & (KSC_CSR_TMO | KSC_CSR_NOX | KSC_CSR_NOQ))
                {
                  if (cok == 0) printf("Crate %i does not exist",cnaf.c);
                  else if (online == 0) printf("Crate %i is Off-line",cnaf.c);
                  else  printf("C= %i, N= %i, A= %i, F= %i, D= %6.6x, Q= %1.1i,\
 X= %1.1i",cnaf.c,cnaf.n,cnaf.a,cnaf.f,cnaf.data.dat,q,x);
                  printf("\n");
                }
               --temp;
             }
            break;
          case 6:                    /* execute and list        */
            stat = ksc_camio(&cnaf);
            if (stat & KSC_CSR_TMO)
             {
               q = 0;
               x = 0;
               online = 0;
               if (stat & KSC_CSR_RST) cok = 0;
               else  cok = 1;
             }
            else
             {
               cok = 1;
               online = 1;
               q = 1;
               x = 1;
               if (stat & KSC_CSR_NOX) x = 0;
               if (stat & KSC_CSR_NOQ) q = 0;;
             }
          case 7:                    /* list CNAF, data, X and Q */
            if (cok == 0) printf("Crate %i does not exist",cnaf.c);
            else if (online == 0) printf("Crate %i is Off-line",cnaf.c);
            else  printf("C= %i, N= %i, A= %i, F= %i, D= %6.6x, Q= %1.1i,\
 X= %1.1i",cnaf.c,cnaf.n,cnaf.a,cnaf.f,cnaf.data.dat,q,x);
            printf("\n");
            break;
          case 8:
            printf(" Commands requiring an argument. dec = decimal and\
 hex = hexadecimal.\n");
            printf(" c dec - crate.\n");
            printf(" n dec - station number.\n");
            printf(" a dec - subaddress.\n");
            printf(" f dec - function.\n");
            printf(" d hex - write data\n\n");
            printf(" x [n] - execute CNAF n (decimal) times. Default n is 1.\n");
            printf(" e     - execute CNAF and list\n");
            printf(" l     - list CNAF, data, A and Q\n\n");
            printf(" h     - this message\n\n");
            break;
          default:
            printf("Unknown Command !!\n");
        }
    }
}
/***************************************************************************
***************************************************************************/
char *gfield(char *s1,char *s2,int n)
{
   char *cptr;

   *s1 = '\0';
   for (;*s2 == ' ' || *s2 == '\t'; s2++);
   for (cptr = s2; *cptr != ' ' && *cptr != '\0' && *cptr != '\n'; cptr++);
   if (cptr - s2 != 0)
     {
       if (cptr - s2 < n)
         n = cptr - s2;
       else
         n = n -1;
         strncpy(s1,s2,n);
         *(s1 + n) = '\0';
         return(cptr);
      }
    else
      return ((char *)NULL);
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
   int                      level;
   register struct KSC_VME *ksc = ksc_ptr;
   register int             wd0,wd1;
   register unsigned short  status;
   register int tmo = 15;       /* Timeout is approx. 1.5*tmo microseconds */

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
	      cambuf->data.d[1] = ksc->dlr;
	      cambuf->data.d[0] = ksc->dhr & 0xff;
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
	      ksc->dhr = cambuf->data.d[0];
	      ksc->dlr = cambuf->data.d[1];
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
