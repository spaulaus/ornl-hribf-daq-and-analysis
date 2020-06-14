/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                         Copyright(C) 1992-2001
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
*    File:         /usr/users/mcsq/Dvme/vme_sys.h
*
*    Description:  Hardware specific parameters.
*
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    2/15/92    MCSQ         
*
*    8/19/92    MCSQ       Added Timer register definitions.
*
*    8/25/92    MCSQ       Add message pointer and flag definitions for
*                          Acquisition data buffers.
*
*    2/ 9/93    MCSQ       Add address and interrupt control for the second
*                          MC68230 Parallel Interface/Timer on the Force CPU
*                          board.
*
*    3/25/93    MCSQ       Add CES 8170 module.
*
*    3/17/94    MCSQ       Add VME base address of the ORNL trigger module.
*
*    5/ 1/94    MCSQ       Add VME base addresses for VMIC 6015 quad-serial
*                          I/O modules.
*
*    7/21/94    MCSQ       Add VME base address for LeCroy 1190 Dual Port
*                          memory module.
*
*   11/22/94    MCSQ       Add VME base addresses for DATEL 613 and DATEL 626
*                          modules used for RMS control.
*
*    3/25/94    MCSQ       Add VME base address for Green Spring IP-Stepper
*                          and IP-Digital 24.
*
*    5/27/95    MCSQ       New modules for DRS control system.
*
*   10/16/95    MCSQ       Add DATEL 628 12-bit DAC for RMS control
*
*    6/24/96    MCSQ       Add VMIC 3113A 32 channel, 12-bit ADC for
*                          LN fill system.
*
*    7/ 7/96    MCSQ       Added table for translation of CPU addresses
*                          to VMEbus module addresses.
*
*    2/17/97    MCSQ       Add three additional LRS1190 interfaces.  Add
*                          VMIC 3128 64 channel ADC for DRS.
*
*    3/ 2/97    MCSQ       For new CPU-60s.  If CPU60 is NOT defined, we
*                          use definitions for the CPU-40.  Otherwise,
*                          use CPU-60 stuff.
*
*    4/ 6/97    MCSQ       Define interrupt vector for Datel 613 ADC.
*                          Use same as for VMIC ADCs.
*
*   12/13/97    MCSQ       Add a second KSC 2917 CAMAC interface
*
*    2/20/98    MCSQ       Add event flags for LN2 filling system
*
*   11/ 4/98    MCSQ       Add ORNL CAMAC AUX controller.
*
*    2/ 8/99    MCSQ       Add VMIC 3113A for LN fill system
*
*    4/ 4/01    MCSQ       Add 10 CAEN V785 ADCs
*
*    4/11/01    MCSQ       Add 10 CAEN V775 TDCs
*
*    2/ 8/06    MCSQ       Add symbol VME_LVL1, which is a pointer to the
*                          processor register which maps VME level 1 interrupts
*                          to CPU level interrupts.
*
*    6/14/06    MCSQ       Add SIS3820_1 VME Scaler
*****************************************************************************/

#ifndef  VME_SYS_H_
#define  VME_SYS_H_

#define SYSRAM   0x1000      /* Address of struct SYRAM in VMEPROM          */
#define USERRAM  0x7000      /* Start address of user RAM in CPU40          */
#define MAXMEM   0x400000    /* Size of RAM in CPU40 & CPU60                */
#define CODE_OFF 0x1000      /* Code offset from struct TCB                 */

#ifndef  CPU60       /*  use following for CPU-40                           */

#define PIT2    0xff800e00   /* Parallel Interface/Timer #2                 */

#define LANCE   0xfef80000   /* LANCE Ethernet controller                   */
#define LAN_RAM 0xfef00000   /* LANCE shared RAM                            */

#else                /*  use following for CPU-60                           */

#define LANCE   0xfff00010   /* Am79C965 Ethernet controller                */
#define LAN_RAM 0x003e0000   /* Ethernet data buffers                       */

#endif

#define CPU40_BOOT (char *)0xffd00e00   /* Access invokes reboot of CPU/40  */

/*
*   Module addresses are for CPU access.  The CPU translates addresses
*   for the VMEbus as follows:
*
*        CPU address                    VMEbus access
*    0x01000000 thru f9ffffff     Extended access: A32: D32,D24,D16,D8
*    0xfb000000 thru fbffffff     Extended access: A24: D32,D16,D8
*    0xfc000000 thru fcfeffff     Standard access: A24: D16,D8
*    0xfcff0000 thru fcffffff     Short I/O space: A16: D16,D8
*
*   Some modules allow selection of Supervisor Access, Nonprivileged Access
*   or both.  If you can select both, do so. If not, then select
*   Supervisor Access.
*
* Example:
*          CPU address = 0xfcff0100
*          module address = 0100  Short I/O space
*/

#define KSC1    0xfcffff00   /* Base VME address of first KSC 2917          */

#define KSC2    0xfcfffe00   /* Base VME address of second KSC 2917         */

#define LRS1    0xfca10000   /* Base VME address of first LRS 1131          */

#define ACROMAG 0xfcff0000   /* Base address of digital I/O module          */

#define CES1    0x10000000   /* Base VME address of first CES 8170          */

#define  TRIGGER  0xfcfe0000  /* Base VME address of ORNL trigger module    */

#define VMIC6015A 0xfcfff000  /* Base VME address of first VMIC 6015        */
 
#define VMIC6015B 0xfcfff100  /* Base VME address of second VMIC 6015       */

#define LRS1190A  0xfc7c0000  /* Base VME address of LeCroy 1190            */

#define DATEL626A 0xfcffc900  /* Base VME address of DATEL626 16-bit DAC    */

#define DATEL613A 0xfcfa0000  /* Base VME address of DATEL613 16-bit ADC    */

#define DATEL613B 0xfcfa0100  /* Base VME address of DATEL613 16-bit ADC    */

#define STEPPER   0xfcff6000  /* Base VME address of IP-Stepper/IP-Digital  */

#define DATEL628A 0xfcff0600  /* Base VME address of DATEL628 12-bit DAC    */

#define VMIC3113A 0xfcfff800  /* Base VME address of VMIC3113A ADC          */

#define LRS1190B  0xfc780000  /* Base VME address of LeCroy 1190 #2         */

#define LRS1190C  0xfc740000  /* Base VME address of LeCroy 1190 #3         */

#define LRS1190D  0xfc700000  /* Base VME address of LeCroy 1190 #4         */

#define VMIC3128A 0xfcfef000  /* Base VME address of VMIC3128 ADC           */

#define ORNLAUX   0xfb000000  /* Base VME address for ORNL CAMAC AUX        */

#define VMIC3124  0xfcffe000  /* Base VME address of VMIC3124 ADC           */

#define CAEN785_1  0xfb010000  /* Base VME address of first CAEN V785 ADC   */

#define CAEN785_2  0xfb020000

#define CAEN785_3  0xfb030000

#define CAEN785_4  0xfb040000

#define CAEN785_5  0xfb050000

#define CAEN785_6  0xfb060000

#define CAEN785_7  0xfb070000

#define CAEN785_8  0xfb080000

#define CAEN785_9  0xfb090000

#define CAEN785_10 0xfb0a0000  /* Base VME address of last CAEN V785 ADC    */

#define CAEN775_1  0xfb0b0000  /* Base VME address of first CAEN V775 TDC   */

#define CAEN775_2  0xfb0c0000

#define CAEN775_3  0xfb0d0000

#define CAEN775_4  0xfb0e0000

#define CAEN775_5  0xfb0f0000

#define CAEN775_6  0xfb100000

#define CAEN775_7  0xfb110000

#define CAEN775_8  0xfb120000

#define CAEN775_9  0xfb130000

#define CAEN775_10 0xfb140000  /* Base VME address of last CAEN V775 TDC    */

#define SIS3820_1  0x21000000
/****************************************************************************
*          Interrupt Vector Definitions
****************************************************************************/
#define DEF_VEC      4            /* Illegal instruction vector.  Place    */
                                  /* to address of the default interrupt   */
                                  /* handler.                              */
#define VIRQ7_VEC  120            /* VME level 7 interrupt vector          */
#define VIRQ6_VEC  121
#define VIRQ5_VEC  122
#define VIRQ4_VEC  123
#define VIRQ3_VEC  124
#define VIRQ2_VEC  125
#define VIRQ1_VEC  126            /* VME level 1 interrupt vector          */

#define VME_LVL1   0xffd00204     /* VME level 1 interrupt maping register */

#define ORNL_EVT_VEC  143         /* ORNL trigger module event vector      */
#define ORNL_TIM_VEC  141         /* ORNL trigger module timer vector      */

#define VMIC_VECS     144         /* VMIC 6015 Modules for RMS control     */
                                  /* 2 modules use vectors 144 thru 175    */
#define VMIC_ADC_VEC  176         /* VMIC 3113A ADC for LN fill system     */

#define DATEL_ADC_VEC 176         /* DATEL 613 ADC                         */

#define DMA_NORM_V 236            /* DMA normal terminate interrupt vector */
#define DMA_ERR_V  235            /* DMA error terminate interrupt vector  */

#ifndef  CPU60       /*  use following for CPU-40                           */

#define LAN_VEC    246            /* LANCE controller interrupt (LOCAL6)   */
#define LAN_CTL    0xffd002a4     /* Local 6 control register              */
#define LAN_STAT   0xffd00498     /* Local 6 status register               */

#define PIT2_VEC   243            /* PI/T #2 timer interrupt (LOCAL3)      */
#define PIT2_CTL   0xffd00298     /* Local 3 control register              */
#define PIT2_STAT  0xffd0048c     /* Local 3 status register               */

#else                /*  use following for CPU-60                           */

#define LAN_VEC    247            /* Am79C965 controller interrupt (LOCAL7)*/
#define LAN_CTL    0xffd002a8     /* Local 7 control register              */
#define LAN_STAT   0xffd0049c     /* Local 7 status register               */

#endif
/****************************************************************************
*          VMEPROM message pointer slot assignments
****************************************************************************/
#define LAN_MSG_SLOT   0  /* Ethernet driver calls                         */
#define EXIT_MSG_SLOT  1  /* Task exit message                             */
#define DATA_MSG_SLOT  8  /* Acquisition data message                      */
#define ACQ_CNTL_SLOT  3  /* Command message to Acquisition task           */

/****************************************************************************
*          Event Flag Usage
****************************************************************************/
#define LAN_INT   56                 /* Set in LAN interrupt routine       */
#define LAN_MSG   (64+LAN_MSG_SLOT)  /* Message received flag              */
#define EXIT_MSG  (64+EXIT_MSG_SLOT) /* Task exit flag                     */
#define DATA_MSG  (64+DATA_MSG_SLOT) /* Acquisition data message flag      */
#define CNTL_MSG  (64+ACQ_CNTL_SLOT) /* Command message flag               */
#define ACQ_BUF_FULL  57             /* Acquisition buffer full flag       */

#define DMA_DONE  50                 /* DMA transfer finished              */

#define LNFXX_ERR 55                 /* LN2 fill system error event        */
#define LNTIMER   54                 /* LN2 timer event                    */

/*   Timer events                                                          */
#define TIM_20TH  112
#define TIM_1SEC  113

/****************************************************************************
*          DMA Definitions
****************************************************************************/
#define DMA_BASE      0XFFD00000

/*     Register offsets           */
#define ICTL_NORM     0X0230  /* Normal terminate interrupt control reg */
#define ICTL_ERROR    0X0234  /* Error terminate interrupt control reg  */
#define DMA_SRC_ATR   0X0320  /* Source attribute register              */
#define DMA_DST_ATR   0X0324  /* Destination attribute register         */
#define DMA_GEN       0X0328  /* DMA General control register           */
#define ISTAT_NORM    0X04B0  /* Normal interrupt status register       */
#define ISTAT_ERROR   0X04B4  /* Error interrupt status register        */
#define DMA_RUN_CTL   0X04C4  /* DMA Run Control/Status register        */
#define DMA_MODE      0X04EC  /* DMA mode register                      */
#define DMA_SRC_ADDR  0X0500  /* Source address (32 bits)               */
#define DMA_DST_ADDR  0X0504  /* Destination address (32 bits)          */
#define DMA_CNT       0X0508  /* Transfer counter (32 bits)             */

/*    DMA Attribute Codes         */
#define  DMA_VME32  0x00  /* Must add the AM code                */
#define  DMA_VME16  0x80  /* add the AM code                     */
#define  DMA_VME8   0x40  /* add the AM code                     */
#define  DMA_RAM32  0xc5  /* DRAM                                */
#define  DMA_SEC32  0xe5  /* Secondary bus 32 bits               */
#define  DMA_SEC16  0xed  /* Secondary bus 16 bits               */
#define  DMA_SEC8   0xf5  /* Secondary bus 8 bits                */

/*    General register bits       */
#define  DMA_SRC_CNT_DIS  0x80  /* 1 = do not increment src addr     */
#define  DMA_DST_CNT_DIS  0x40  /* 1 = do not increment dest address */
#define  DMA_ENABLE   0x1

/*    Run Control register bits   */
#define  DMA_RUN      0x80   /* Status, 1 = running -Read only   */
#define  DMA_START    0x1    /* 1 = start DMA, 0 = stop DMA      */

/****************************************************************************
*        FGA-002  Timer registers
****************************************************************************/

#define  TIM_PRELOAD  0xffd00300	/* Value to load to timer counter */
#define  TIM_CTL      0xffd00310        /* Timer control register         */
#define  TIM_COUNT    0xffd00c00        /* Timer countdown register       */
#define  TIM_ICTL     0xffd00220        /* Timer interrupt control reg    */
#define  TIM_IST      0xffd004a0        /* Timer interrupt status reg.    */

/****************************************************************************
*          
****************************************************************************/

#endif     /* end  VME_SYS_H_   */
