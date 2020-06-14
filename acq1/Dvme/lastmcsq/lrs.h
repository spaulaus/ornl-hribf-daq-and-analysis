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
*     for any loss or damage, direct or consequential, arrising out of the
*     use or the inability to use the product.  Before using, the USER shall
*     determine the suitability of the product for his or her intended use,
*     and the USER assumes all risk and liability whatsoever in connection
*     therewith.
*
******************************************************************************
*
*    Environment:  VME based Data Acquisition System
*
*    File:         /usr/users/mcsq/Dvme3/lrs.h
*
*    Description:  Include file for LeCroy Research Corp. model 1131 VME
*                  interface to their SIB bus (i.e. FASTBUS).
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    6/20/92     MCSQ         
*
*    8/11/92    MCSQ     Prefixed command register and bit definitions with
*                        LRS_ so that there is a distinction between similar
*                        bit definitions in Kinetic Systems and LeCroy
*                        interface modules.
*****************************************************************************/

#ifndef  LRS_H_
#define  LRS_H_


struct LRS_VME {
  volatile unsigned short csr;    /* Command and Status register          */
  volatile unsigned short nta;    /* Memory address counter               */
  volatile unsigned short wc;     /* Word count - DMA mode only           */
  volatile unsigned short sib;    /* SIB address - DMA mode only          */
 } ;

/*  Command and Status register        */

#define LRS_CSR_ERR     1       /* Error                                     */
#define LRS_CSR_RUN     2       /* Start DMA/list operation. Bit cleared upon
                                 completion of DMA/List operation.           */
#define LRS_CSR_DMA_RD  4       /* DMA transfer direction                    */
#define LRS_CSR_DMA_WR  0
#define LRS_CSR_FP_ENA  0x8     /* Enable front panel trigger                */
#define LRS_CSR_DMA     0x10    /* Set DMA mode                              */
#define LRS_CSR_LIST    0       /* Set List mode                             */

#define LRS_CSR_TA0     0x400   /* Front panel trigger address (LSB)         */
#define LRS_CSR_TA1     0x800
#define LRS_CSR_TA2     0x1000  /* Front panel trigger address (MSB)         */

/*****************************************************************************
      Parameters for building command lists
*/
#define MOD1821 0xe

#define SIB(dev,reg)  (0x000 | ((dev<<5) | (reg<<1)))
#define LIST_RD   0x1000
#define LIST_WR   0
#define LIST_DONE 0x2000
#define SIB_DIRECT(dev,reg)  (0x000 | (dev<<4) | reg)

#define DIRECT  0x400
#define LIST    0x4000
#define DATA    0x8000

#endif      /* end  LRS_H_   */
