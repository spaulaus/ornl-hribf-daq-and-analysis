/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 2000-2001
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
*    Environment: HHIRF VME based acquisition system
*
*    File:         /usr/users/mcsq/Dvme3/caen.h
*
*    Description:  Include file for CAEN 32 channel ADC and TDC.
*          
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    7/12/00    MCSQ         
*
*    5/ 2/01    MCSQ     Fix for V775 TDC.
*****************************************************************************/
#ifndef  CAEN_H_
#define  CAEN_H_


/*
*   CAEN module register structure
*/
struct CAEN {
                   int   buf[512];
                   int   dum[512];
          unsigned short firmware;
          unsigned short geo_addr;
          unsigned short blt_addr;
          unsigned short bit_set1;
          unsigned short bit_clear1;
          unsigned short intr_level;
          unsigned short intr_vector;
 volatile unsigned short status_reg1;
 volatile unsigned short control_reg1;
          unsigned short ADER_h;
          unsigned short ADER_l;
          unsigned short single_shot_reset;
          unsigned short dum18;
          unsigned short blt_ctrl;
          unsigned short dum1c;
          unsigned short dum1e;
          unsigned short event_trigger_reg;
 volatile unsigned short status_reg2;
          unsigned short event_counter_l;
          unsigned short event_counter_h;
          unsigned short increment_event;
          unsigned short increment_offset;
          unsigned short load_test_reg;
          unsigned short FCLR_Window;
          unsigned short dum30;
          unsigned short bit_set2;
          unsigned short bit_clear2;
          unsigned short mem_test_addr;
          unsigned short mem_test_word_h;
          unsigned short mem_test_word_l;
          unsigned short crate_select;
          unsigned short test_event_write;
          unsigned short event_counter_reset;
          unsigned short dum42[15];
          unsigned short range;               /*  V775 only   */
          unsigned short dum62;
          unsigned short test_addr;
          unsigned short dum66;
          unsigned short SW_comm;
          unsigned short slide_constant;      /*  V785 only   */
          unsigned short dum6c[2];
          unsigned short AAD;
          unsigned short BAD;
          unsigned short dum74[6];
          unsigned short thresholds[32];
};

/*
*   Data word types
*/

#define  HDR_MASK  0x07000000   /* header word mask for type          */

#define  HEADER    0x02000000   /* Header word                        */
#define  VALID     0x00000000   /* valid data word                    */
#define  EOB       0x04000000   /* end of block word                  */
#define  NOT_VALID 0x06000000   /* data not valid (i.e. buffer empty) */

/*
* Masks for data words
*/

#define  CHAN      0x003f0000   /* channel number                     */
#define  UN        0x00002000   /* Under threshold flag               */
#define  OV        0x00001000   /* Overflow flag                      */
#define  DAT       0x00000fff   /* ADC/TDC data (12 bits)             */

/*
*  Masks for header words
*/

#define CRATE      0x00ff0000   /* Crate number                       */
#define CNT        0x00003f00   /* data word counter                  */

/*
*  Mask for event counter
*/

#define EV_CNT     0x00ffffff   /* 24 bit event counter               */

#endif     /* end CAEN_H_    */
