/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 2006
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
*    File:         /tera/mcsq/Dlinux/Dvme3/sis3820.h
*
*    Description:  Include file for SIS 3820 VME scaler module.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    6/07/06     MCSQ         
*
*****************************************************************************/


/*
*   SIS3820 VME scaler must be A32, D32 
*/
struct SIS3820 {
    volatile unsigned long csr;           /*R/W - control/status register     */
    volatile unsigned long modid;         /*R   - mdoule ID & firmware rev reg*/
    volatile unsigned long irq_config;    /*R/W - interrupt configuration reg */
    volatile unsigned long irq_control;   /*R/W - interrupt control/status reg*/
    volatile unsigned long acq_preset;    /*R/W - acquisition preset register */
    volatile unsigned long acq_count;     /*R   - acquisition count register  */
    volatile unsigned long lne_prescale;  /*R/W - LNE prescale factor register*/
                      long dum1;
    volatile unsigned long preset_grp1;   /*R/W - preset value reg, grp 1     */
    volatile unsigned long preset_grp2;   /*R/W - preset value reg, grp 2     */
    volatile unsigned long preset_ena_hit; /*R/W - preset enable and hit reg  */
                      long dum2;
    volatile unsigned long broadcast_setup;  /*R/W - broadcast setup register */
    volatile unsigned long sdram_page;       /*R/W - SDRAM page register      */
    volatile unsigned long fifo_wd_cnt;      /*R/W - FIFO word count register */
    volatile unsigned long fifo_cnt_thres;   /*R   - FIFO word count threshold*/
    volatile unsigned long his_str_preset;   /*R/W - HISCAL_START_PRESET      */
    volatile unsigned long his_cnt;          /*R   - HISCAL_START_COUNTER     */
    volatile unsigned long his_last_cnt;     /*R   - HISCAL_LAST_ACQ_COUNTER  */
                      long dum3[45];
    volatile unsigned long op_mode;          /*R/W - operation mode register  */
    volatile unsigned long copy_disable;     /*R/W - copy disable register    */
    volatile unsigned long lne_sel;          /*R/W - LNE channel select reg   */
    volatile unsigned long preset_ch_sel;    /*R/W - PRESET channel select reg*/
                      long dum4[60];
    volatile unsigned long cnt_inhibit;      /*R/W - inhibit/count disable reg*/
    volatile unsigned long cnt_clear;        /*  W - counter clear register   */
    volatile unsigned long cnt_overflow;     /*R/W - counter overflow read&clear*/
                      long dum5[3];
    volatile unsigned long tst_pulse_msk;    /*R/W - test pulse mask register */
                      long dum6[121];
    volatile unsigned long Key_reset;        /*  W - Key reset                */
    volatile unsigned long Key_sdram_fifo_reset; /*  W - Key SDRAM/FIFO reset */
    volatile unsigned long Key_test_pulse;   /*  W - Key test pulse           */
    volatile unsigned long Key_cnt_clear;    /*  W - Key Counter clear        */
    volatile unsigned long Key_lne_shadow;   /*  W - Key VME LNE/clock shadow */
    volatile unsigned long Key_arm;          /*  W - Key operation arm        */
    volatile unsigned long Key_enable;       /*  W - Key operation enable     */
    volatile unsigned long Key_disable;      /*  W - Key operation disable    */
                      long dum7[248];
    volatile unsigned long shadow[32];       /* R - shadow registers          */
                      long dum8[96];
    volatile unsigned long counter[32];      /* R - counter registers         */
   };

#define SIS3820_FIFO_BASE    0x800000
#define SIS3820_SDRAM_BASE   0x800000

