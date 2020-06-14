/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                          Copyright(C) 1995-1997
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
*    Environment:  DRS Control System
*
*    File:         /usr/users/mcsq/Dvme3/drsvme.h
*
*    Description:  Data structures for the DRS control functions.
*          
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    4/22/95    MCSQ
*
*   12/ 8/95    MCSQ       Change error codes
*
*    2/18/97    MCSQ       Increase size of array data in structure
*                          datel_adc_ctl to 80.  This allows for an additional
*                          64 channels from the VMIC3128 ADC.
*
*    4/ 6/97    MCSQ       Revise ADC readout.  Change datel_adc_ctl to
*                          read_adc and DATEL_ADC to READ_ADC.
*
*   11/24/97    MCSQ       Add structures for Green Spring Computers
*                          IP-Stepper/IP-Digital 24
*****************************************************************************/
#ifndef  DRSVME_H_
#define  DRSVME_H_

/*
*   Software Commands and data structures
*/
#define  VARIAN1       0x1010  /* Varian controller #1                    */
#define  VARIAN2       0x1111  /* Varian controller #2                    */
#define  VARIAN3       0x1212  /* Varian controller #3                    */
#define  VARIAN4       0x1313  /* Varian controller #4                    */
#define  DANFYSIK      0x1515  /* Danfysik Power supplies                 */
#define  READ_ADCS     0x1919  /* Datel 613 16-bit ADCs & VMIC 3128       */
#define  GS_CTRL       0x2020  /* Green Spring stepper motor control      */
#define  GS_STATUS     0x2121  /* Green Spring stepper motor status       */
#define  GRANNY        0x2222  /* Granny Phillips 303                     */

struct  varian_ctl {
         short func;      /* function code - VARIAN                    */
         char  rpystat;   /* reply status code                         */
unsigned char  cmdbytes;  /* number of bytes in command string         */
unsigned char  rpybytes;  /* number of bytes in expected response      */
unsigned char  cmd[10];   /* command string                            */
unsigned char  reply[30]; /* reply string                              */
};

struct  danfysik_ctl {
         short func;      /* function code - DANFYSIK                  */
         char  rpystat;   /* reply status code                         */
unsigned char  controller; /*  Controller number                       */
unsigned char  cmdbytes;  /* number of bytes in command string         */
unsigned char  rpymess;   /* 0 = no reply.  Non-zero for reply message */
         char  data[1024]; /* command/reply string                     */
};

struct  read_adc {
         short func;      /* function code - READ_ADCS                 */
         char  rpystat;   /* reply status code                         */
         char  dum;
         short data[80];
};

struct  drsio_stat {
         short func;
         char  rpystat;   /* reply status code                         */
};

struct  granny_ctl {
         short func;      /* function code - GRANNY                    */
         char  rpystat;   /* reply status code                         */
         char  cmd[80];   /* command string                            */
         char  reply[80]; /* reply string                              */
};

struct  stepper_ctl {
         short func;      /* function code - GS_CTRL                  */
         char  rpystat;   /* reply status code                        */
         char  dum;       /* pad                                      */
         int   steps;     /* Steps to move                            */
         int   position;  /* Position(in steps) at start of move      */
         short lspeed;    /* Low speed                                */
         short hspeed;    /* High speed.  Ramps from low to high      */
unsigned short multi;     /* Sped multiplier                          */
         short accel;     /* Acceleration/Deceleration rate           */
         char  motor;     /* Motor number                             */
         char  dir;       /* Move direction                           */
};

struct  stepper_status {
         short func;      /* function code - GS_STATUS                */
         char  rpystat;   /* reply status code                        */
         char  dum;
         int   position;  /* Returned position in steps               */
         int   in_bits;   /* Digital 24 input bits                    */
         int   out_bits;  /* Digital 24 output bits                   */
unsigned char  stat1;     /* Primary status                           */
unsigned char  stat2;     /* Extended status - R17 bits 8-15          */
unsigned char  stat3;     /* Extended status - R17 bits 0-7           */
};

union Cmd {
     struct drsio_stat reply;
     struct varian_ctl varian;
   struct danfysik_ctl danfysik;
       struct read_adc adc;
    struct stepper_ctl gs_ctl;
 struct stepper_status gs_status;
    struct  granny_ctl granny;
};

/*
*   Error codes
*/
#define  VME_ERR_FUNC            -1

#define  DANFYSIK_ERR_NONEXIST   -2
#define  DANFYSIK_ERR_NORESPOND  -3
#define  DANFYSIK_ERR_SERIALIO   -4
#define  DANFYSIK_ERR_BUFFULL    -5

#define  IG_ERR_NONEXIST   -2
#define  IG_ERR_NORESPOND  -3
#define  IG_ERR_SERIALIO   -4
#define  IG_ERR_BUFFULL    -5
#define  IG_ERR_INVALID    -6

#define  DATEL_ERR_NONEXIST -2

#define  STEP_ERR_NONEXIST -2

#endif     /* end DRSVME_H_    */
