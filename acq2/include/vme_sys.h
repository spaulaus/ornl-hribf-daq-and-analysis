/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                         Copyright(C) 1992-2005
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
*    2/ 8/06    MCSQ       Add symbol VME_LVL1, which is a pointer to the
*                          processor register which maps VME level 1 interrupts
*                          to CPU level interrupts. 
*
*    6/14/06    MCSQ       Add SIS3820_1 VME Scaler

*    2008       RLV        Add 2 extra CAEN V775 and V785 modules
*    27 July 2009 RLV      Add extra pieces from MCSQ
*                          Add V792 module
*    31 July 2015 RLV      Add ANL/Gammasphere MyRIAD module for ORRUBA
*    12 October 2017 RLV   Add 12 more CAEN785, second SIS3820
*****************************************************************************/

#ifndef  VME_SYS_H_
#define  VME_SYS_H_

/*
*   Module addresses are for CPU access.  The CPU translates addresses
*   for the VMEbus as follows:
*
*        CPU address        VME address            VMEbus access
* 0x90000000:9effffff  0x20000000:2e000000  Extended access: A32: D32,D16,D8
* 0x9f000000:9ffeffff  0x00000000:00feffff  Extended access: A24: D32,D16,D8
* 0x9fff0000:9fffffff  0x00000000:0000ffff  Short I/O space: A16: D16,D8
*
*   Some modules allow selection of Supervisor Access, Nonprivileged Access
*   or both.  If you can select both, do so. If not, then select
*   Supervisor Access.
*
* Example:
*          CPU address = 0x9fff0100
*          module address = 0100  Short I/O space
*/

#define KSC1    0x9fffff00   /* Base VME address of first KSC 2917          */

#define KSC2    0x9ffffe00   /* Base VME address of second KSC 2917         */

#define LRS1    0x9fa10000   /* Base VME address of first LRS 1131          */

#define ACROMAG 0x9fff0000   /* Base address of digital I/O module          */

#define CES1    0x90000000   /* Base VME address of first CES 8170          */
                             /* Set module address to 0x20000000            */

#define  TRIGGER  0x9ffe0000  /* Base VME address of ORNL trigger module    */

#define VMIC6015A 0x9ffff000  /* Base VME address of first VMIC 6015        */
 
#define VMIC6015B 0x9ffff100  /* Base VME address of second VMIC 6015       */

#define DATEL626A 0x9fffc900  /* Base VME address of DATEL626 16-bit DAC    */

#define DATEL613A 0x9ffa0000  /* Base VME address of DATEL613 16-bit ADC    */

#define DATEL613B 0x9ffa0100  /* Base VME address of DATEL613 16-bit ADC    */

#define STEPPER   0x9fff6000  /* Base VME address of IP-Stepper/IP-Digital  */

#define DATEL628A 0x9fff0600  /* Base VME address of DATEL628 12-bit DAC    */

#define VMIC3113A 0x9ffff800  /* Base VME address of VMIC3113A ADC          */

#define LRS1190A  0x9f7c0000  /* Base VME address of LeCroy 1190            */
#define LRS1190B  0x9f780000  /* Base VME address of LeCroy 1190 #2         */
#define LRS1190C  0x9f740000  /* Base VME address of LeCroy 1190 #3         */
#define LRS1190D  0x9f700000  /* Base VME address of LeCroy 1190 #4         */

#define VMIC3128A 0x9ffef000  /* Base VME address of VMIC3128 ADC           */
#define VMIC3124  0x9fffe000  /* Base VME address of VMIC3124 ADC           */

#define ORNLAUX   0x9f000000  /* Base VME address for ORNL CAMAC AUX        */

#define CAEN785_1  0x9f010000  /* Base VME address of first CAEN V785 ADC   */
#define CAEN785_2  0x9f020000
#define CAEN785_3  0x9f030000
#define CAEN785_4  0x9f040000
#define CAEN785_5  0x9f050000
#define CAEN785_6  0x9f060000
#define CAEN785_7  0x9f070000
#define CAEN785_8  0x9f080000
#define CAEN785_9  0x9f090000
#define CAEN785_10 0x9f0a0000  /* Base VME address of last CAEN V785 ADC    */

#define CAEN775_1  0x9f0b0000  /* Base VME address of first CAEN V775 TDC   */
#define CAEN775_2  0x9f0c0000
#define CAEN775_3  0x9f0d0000
#define CAEN775_4  0x9f0e0000
#define CAEN775_5  0x9f0f0000
#define CAEN775_6  0x9f100000
#define CAEN775_7  0x9f110000
#define CAEN775_8  0x9f120000
#define CAEN775_9  0x9f130000
#define CAEN775_10 0x9f140000  /* Base VME address of last CAEN V775 TDC    */

#define CAEN792_1  0x9f150000  /* Base VME address of first CAEN V792 QDC   */
#define CAEN792_2  0x9f160000
#define CAEN792_3  0x9f170000
#define CAEN792_4  0x9f180000
#define CAEN792_5  0x9f190000
#define CAEN792_6  0x9f1a0000
#define CAEN792_7  0x9f1b0000
#define CAEN792_8  0x9f1c0000
#define CAEN792_9  0x9f1d0000
#define CAEN792_10 0x9f1e0000 /* Base VME address of last CAEN V792 QDC   */

#define CAEN785_11 0x9f1f0000 /* Extended number of CAEN785 */
#define CAEN785_12 0x9f200000 /* Extended number of CAEN785 */
#define CAEN775_11 0x9f210000 /* Extended number of CAEN775 */
#define CAEN775_12 0x9f220000 /* Extended number of CAEN775 */
#define CAEN792_11 0x9f230000 /* Extend CAEN V792 */
#define CAEN792_12 0x9f240000 /* Extend CAEN V792 */

#define CAEN785_13 0x9f250000 /* Extended number of CAEN785 */
#define CAEN785_14 0x9f260000 
#define CAEN785_15 0x9f270000 
#define CAEN785_16 0x9f280000 
#define CAEN785_17 0x9f290000 
#define CAEN785_18 0x9f2a0000 
#define CAEN785_19 0x9f2b0000 
#define CAEN785_20 0x9f2c0000 
#define CAEN785_21 0x9f2d0000 
#define CAEN785_22 0x9f2e0000 
#define CAEN785_23 0x9f2f0000 
#define CAEN785_24 0x9f300000 /* Extended number of CAEN785 */

#define MYRIAD_1   0x9f550000 /* One and only MyRIAD */

#define SIS3820_1  0x91000000 /* SIS VME scaler */
#define SIS3820_2  0x92000000 /* SIS VME scaler */
/****************************************************************************
*          Interrupt Vector Definitions
****************************************************************************/

#define ORNL_VME_LVL    1         /* VME interrupt level for trigger module */

#define ORNL_EVT_VEC  143         /* ORNL trigger module event vector      */
#define ORNL_TIM_VEC  141         /* ORNL trigger module timer vector      */

#define VMIC_VECS     144         /* VMIC 6015 Modules for RMS control     */
                                  /* 2 modules use vectors 144 thru 175    */
#define VMIC_ADC_VEC  176         /* VMIC 3113A ADC for LN fill system     */

#define DATEL_ADC_VEC 176         /* DATEL 613 ADC                         */

/****************************************************************************
*          VMEPROM message pointer slot assignments
****************************************************************************/
/****************************************************************************
*          Event Flag Usage
****************************************************************************/
/****************************************************************************
*          DMA Definitions
****************************************************************************/
/****************************************************************************
*          
****************************************************************************/

#endif     /* end  VME_SYS_H_   */
