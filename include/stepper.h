/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 1995
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
*    Environment: RMS Control System
*
*    File:         /usr/users/mcsq/Dvme3/stepper.h
*
*    Description:  Include file for a Green Springs IP-Stepper/IP-Digital 24.
*                  These IPs are mounted ina VIPC610 VME module.  IP-Stepper
*                  is in position A and IP-Digital 24 is in position C.
*          
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    3/22/95    MCSQ         
*
*****************************************************************************/
#ifndef  STEPPER_H_
#define  STEPPER_H_

/*
*   Register structure for the Green Spring IP-Stepper and IP-Digital 24
*   in positions A and C in a VIPC610 module.
*/

struct stepper_ip {
                     char dum1;
   volatile unsigned char m1_ctrl_stat;   /* Motor 1 control and status     */
                     char dum2;
   volatile unsigned char m1_data7;       /* Motor 1 data 0-7               */
                     char dum3;
   volatile unsigned char m1_data15;      /* Motor 1 data 8-15              */
                     char dum4;
   volatile unsigned char m1_data23;      /* Motor 1 data 16-23             */
                     char dum5;
   volatile unsigned char m2_ctrl_stat;   /* Motor 2 control and status     */
                     char dum6;
   volatile unsigned char m2_data7;       /* Motor 2 data 0-7               */
                     char dum7;
   volatile unsigned char m2_data15;      /* Motor 2 data 8-15              */
                     char dum8;
   volatile unsigned char m2_data23;      /* Motor 2 data 16-23             */
                     char dum9;
   volatile unsigned char m1_cnt_data;    /* Counter 1 data                 */
                     char duma;
   volatile unsigned char m1_cnt_ctrl;    /* Counter 1 control              */
                     char dumb[5];
   volatile unsigned char m2_cnt_data;    /* Counter 2 data                 */
                     char dumc;
   volatile unsigned char m2_cnt_ctrl;    /* Counter 2 control              */
                     char dumd[5];
   volatile unsigned char m1_int_reg;     /* Motor 1 interrupt register     */
                     char dume[3];
   volatile unsigned char m1_pol_reg;     /* Motor 1 polarity register      */
                     char dumf[3];
   volatile unsigned char m1_src_reg;     /* Motor 1 source register        */
                     char dum10[3];
   volatile unsigned char m2_int_reg;     /* Motor 2 interrupt register     */
                     char dum11[3];
   volatile unsigned char m2_pol_reg;     /* Motor 2 polarity register      */
                     char dum12[3];
   volatile unsigned char m2_src_reg;     /* Motor 2 source register        */
                     char dum13[3];
   volatile unsigned char sync_reg;       /* Sync control register          */
                     char dum14[3];
   volatile unsigned char vec_reg;        /* Interrupt vector register      */
                     char dum15[450];
   volatile unsigned char stepio[8];      /* Digitial I/O                   */
};

/*
*   IP-Stepper control register selects
*/
#define  SSCMD   0x5    /* Start/Stop command register select               */
#define  OPMODE  0x74   /* Operation mode select command register           */
#define  RSELECT 0xa0   /* Register select command register                 */
#define  OUTMODE 0xe8   /* Output mode command register                     */

/*
*   IP-Stepper register select codes.
*/
#define  R0   0         /* R0 - Down counter                                */
#define  R1   1         /* R1 - FL register                                 */
#define  R2   2         /* R2 - RH1 register                                */
#define  R3   3         /* R3 - RH2 register                                */
#define  R4   4         /* R4 - Acceleration rate register                  */
#define  R5   5         /* R5 - Deceleration rate register                  */
#define  R6   6         /* R6 - Ramping-down point register                 */
#define  R7   7         /* R7 - Multiplication register                     */
#define  R10  0x10      /* R10 - Current position register                  */
#define  R11  0x11      /* R11 - Current speed register                     */
#define  R12  0x12      /* R12 - Extension mode register 1                  */
#define  R13  0x13      /* R13 - Extension mode register 2                  */
#define  R16  0x16      /* R16 - Command buffer monitor                     */
#define  R17  0x17      /* R17 - Extension status buffer                    */

/*
*   IP-Stepper Start/Stop commnad register bits.
*/
#define  M_STOP   8      /* Stop motor      */
#define  M_START  0x10   /* Start motor     */
/*
*   IP-Stepper Operation mode select command register bits.
*/
#define  M_CW     0      /* Move clockwise         */
#define  M_CCW    0x08   /* Move counter clockwise */

#endif     /* end STEPPER_H_    */
