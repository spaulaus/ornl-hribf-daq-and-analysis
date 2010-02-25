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
*    File:         /usr/users/mcsq/Dvme3/ksc.h
*
*    Description:  Include file for Kinetic Systems Corp.(KSC) Model 2917-Z1A
*                  VME interface to KSC Model 3922-Z1B CAMAC Crate Controller.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    2/4/92     MCSQ         
*
*    8/11/92    MCSQ     Prefixed command register and bit definitions with
*                        KSC_ so that there is a distinction between similar
*                        bit definitions in Kinetic Systems and LeCroy
*                        interface modules.
*
*    8/19/92    MCSQ     Make additional registers Volatile.
*****************************************************************************/

#ifndef  KSC_H_
#define  KSC_H_

/*  Register access structure                                                 */
struct KSC_VME{
volatile unsigned short cse;       /* DMA Channel Status/Error register       */
	 unsigned short dum1;
	 unsigned short doc;        /* DMA Device/Operation Control register  */
	 unsigned short scc;        /* DMA Sequence/Channel Control register  */
	 unsigned short dum2;
	 unsigned short mtc;        /* DMA Transfer Counter register          */
	 unsigned short machi;      /* DMA Address Counter (high)             */
	 unsigned short maclo;      /* DMA Address Counter (low)              */
	 char           dum3[48];
	 unsigned short LAM_icr;    /* Interrupt control registers            */
	 unsigned short done_icr;
	 unsigned short DMA_icr;
	 unsigned short abort_icr;
	 unsigned short LAM_ivr;    /* Interrupt vector registers             */
	 unsigned short done_ivr;
	 unsigned short DMA_ivr;
	 unsigned short abort_ivr;
	 char           dum4[16];
	 unsigned short amr;       /* Upper Address/Address Modifier register */
volatile unsigned short cmr;       /* Command memory data register            */
volatile unsigned short cma;       /* Command memory address register         */
	 unsigned short cwc;       /* Command word count register             */
	 unsigned short ssr;       /* LAM service request register            */
volatile unsigned short dlr;       /* CAMAC data register (low)               */
volatile unsigned short dhr;       /* CAMAC data register (high)              */
volatile unsigned short csr;       /* Command Status register                 */
       } ;


/*  Channel Status/Error register      */
#define KSC_CSE_COC  0x8000    /* Set = DMA transfer complete                 */
#define KSC_CSE_ABT  0x2000    /* Set = Command list aborted                  */
#define KSC_CSE_ERR  0x1000    /* Set = transfer terminated due to error      */
#define KSC_CSE_ACT  0x800     /* DMA Transfer in progress                    */
#define KSC_CSE_RDY  0x100
#define KSC_CSE_ERR_MSK 0x1f   /*  Error code mask.  0 = no error, 0xb = Bus  */
                               /*  error, and 0x11 = Software abort           */

/*  Device/Operation control register  */
#define KSC_DOC_CAM_WR  0x8810 /* Value for CAMAC write cycles                */
#define KSC_DOC_CAM_RD  0x8890 /* Value for CAMAC read cycles                 */

/*  Sequence/Channel control register  */
#define KSC_SCC_STR      0x80  /* Start DMA                                   */
#define KSC_SCC_ABT      0x10  /* Software abort DMA controller               */

/*  Upper address/address modifier register.  Specified value consists of
    1) Upper 8 bits of the 32 bit address are 0x80, which corresponds to 
       DRAM in the Force CPU-40.
    2) The 0x40 bit specifies 16 bit transfers on the VMEbus.
    3) The address modifier is 0x0d - Extended supervisor.
*/
#define KSC_AMR      0x804d

/*  Control Status register            */
#define KSC_CSR_ERR  0x8000   /* Set = command list terminated with an error */
#define KSC_CSR_ABT  0x4000   /* Set = Error caused an abort of command list */
#define KSC_CSR_TMO  0x2000   /* Set = Timeout on 2917/3922 bus              */
#define KSC_CSR_RST  0x1000   /* Write to reset interface to power-up        */
                              /* condition                                   */
#define KSC_CSR_LAM    0x200  /* Set = LAM is pending in one or more crates  */
#define KSC_CSR_RDY    0x100  /* Set = data regs are ready for read or write */
#define KSC_CSR_DONE   0x80   /* Set = command list completed or aborted     */
#define KSC_CSR_DMA    0x40   /* Write to enable DMA transfer                */
#define KSC_CSR_CAM_WR 0x20   /* Write for CAMAC write cycles                */
#define KSC_CSR_CAM_RD 0x00   /* Write for CAMAC read cycles                 */
#define KSC_CSR_NOX    0x4    /* CAMAC X = 0 for last cycle                  */
#define KSC_CSR_NOQ    0x2    /* CAMAC Q = 0 for last cycle                  */
#define KSC_CSR_GO     0x1    /* Write to start command list                 */

/*  Interrupt control registers.  To enable an interrupt, KSC_ICR_INT_ENA must
    be OR ed with a nonzero IRQ level(from 1 thru 7).
*/
#define KSC_ICR_INT_ENA  0xd0   /* Must add a nonzero IRQ level              */
#define KSC_ICR_AUTO_CLR 0x10

/*****************************************************************************
      Parameters for building command lists
*/

/*   Word size select                                         */
#define WS16  2      /* CAMAC transfer size - 16 bits         */
#define WS24  0      /* CAMAC transfer size - 24 bits         */

/*   Q-Mode types                                             */
#define Q_Stop   0   /* Q-Stop                                */
#define Q_Ignore 8   /* Q-Ignore                              */
#define Q_Repeat 16  /* Q-Repeat                              */
#define Q_Scan   24  /* Q-Scan                                */

/*   Abort flag                                               */
#define A_ENA     0  /*  Abort list on error                  */
#define A_DIS     1  /*  Disable list abort                   */

/*
     Command list formats

  a)  Single transfer CAMAC
	CAM  -  Crate, transfer size and Abort flag
	NAF   -  CAMAC N, A and F

  b)  Single transfer CAMAC - Immediate write
	IMM_WR
	NAF
	Data (low)
	Data (high)  Dummy word for 16-bit writes

  c)  Block CAMAC transfer
	BLOCK
	NAF
	Word count (low) - 2's complement of number of 16 bit data word to
			   be transfered to/from VMEbus.
	Word count (high)

  d)  JUMP Instruction
	JUMP
	Command memory address

  3)  HALT Instruction
	HALT
*/
/*  c = crate number
    q = Q-type
    s = transfer size
    a = Abort flag
*/
#define CAM(c,s,a)     ((c<<8) | (s) | (a))
#define IMM_WR(c,s,a)  (0x60 + (c<<8) | (s) | (a))
#define BLOCK(c,q,s,a)   (0x20 + (c<<8) | (q) | (s) | (a))
/*  n = station number
    a = subaddress
    f = function
*/
#define NAF(n,a,f)       ((n<<9) | (a<<5) | f)
#define HALT  0x80
#define JUMP  0xc0

/*  Return status structure 
*
*   The csr is the status bits from the KSC 2917 command/status register.
*   (i.e. register masked with 0xe006).  This is the only valid entry
*   for programmed I/O transfer.  For DMA transfers, the remaining
*   variables are set on return.
*/
struct KSC_STAT
       {
	 unsigned short csr;   /* KSC 2917 status register        */
	 unsigned short dma;   /* DMA status                      */
	 unsigned short cwc;   /* number of uncompleted transfers */
       };

#endif      /* end  KSC_H_   */
/****************************************************************************/
