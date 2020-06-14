/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1994-1995
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
*    Environment:  RMS Control System
*
*    File:         /usr/users/mcsq/Dvme3/rmsvme.h
*
*    Description:  Data structures for the RMS control functions.
*          
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   10/ 1/94    MCSQ
*
*   10/13/94    MCSQ       Changed structure 'alpha_ctl'.  Command and reply
*                          data part now overlap.
*
*   11/22/94    MCSQ       Add structures for DATEL 613 and DATEL 626.
*
*    3/23/95    MCSQ       Add structures for Green Spring Computers
*                          IP-Stepper/IP-Digital 24
*
*    4/23/95    MCSQ       Add structures for Granville-Phillips 303
*                          Vacuum Process Controller.
*
*    5/17/95    MCSQ       Add dacnum to structure datel_dac_ctl for
*                          a write request.  Add bitnum to structure
*                          acro_ctl.
*
*   10/12/95    MCSQ       Add error code for no response from teslameter.
*
*   12/ 8/95    MCSQ       Change error codes.
*****************************************************************************/
#ifndef  RMSVME_H_
#define  RMSVME_H_

/*
*   Software Commands and data structures
*/
#define  VARIAN1       0x1010  /* Varian controller #1                    */
#define  VARIAN2       0x1111  /* Varian controller #2                    */
#define  VARIAN3       0x1212  /* Varian controller #3                    */
#define  VARIAN4       0x1313  /* Varian controller #4                    */
#define  TESLA         0x1414  /* Teslameters                             */
#define  ALPHA         0x1515  /* Alpha Power supplies                    */
#define  DATEL_DAC     0x1717  /* Datel 626 16-bit DAC                    */
#define  ACRO          0x1818  /* Acromag 64-bit digital I/O              */
#define  DATEL_ADC     0x1919  /* Datel 613 16-bit ADC                    */
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

struct  tesla_ctl {
         short func;      /* function code - TESLA                     */
         char  rpystat;   /* reply status code                         */
unsigned char  controller;  /* Teslameter ID                           */
         char  cmd[10];   /* command string                            */
         char  reply[80]; /* reply string                              */
};

struct  alpha_ctl {
         short func;      /* function code - ALPHA                     */
         char  rpystat;   /* reply status code                         */
unsigned char  controller; /*Power supply ID                           */
unsigned char  data[1024]; /* command/reply string                     */
};

struct  datel_adc_ctl {
         short func;      /* function code - DATEL_ADC                 */
         char  rpystat;   /* reply status code                         */
         char  dum;
unsigned short data[16];
};

struct  datel_dac_ctl {
         short func;      /* function code - DATEL_DAC                 */
         char  rpystat;   /* reply status code                         */
         char  rw;
         char  dacnum;    /* DAC number for write call                 */
         char  dum;       /* pad byte                                  */
unsigned short data[14];
};

struct  acro_ctl {
         short func;      /* function code - ACRO                      */
         char  rpystat;   /* reply status code                         */
         char  rw;
         char  bitnum;    /* bit number to write                       */
         char  data[8];
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

struct  granny_ctl {
         short func;      /* function code - GRANNY                    */
         char  rpystat;   /* reply status code                         */
         char  cmd[80];   /* command string                            */
         char  reply[80]; /* reply string                              */
};

struct  rmsio_stat {
         short func;
         char  rpystat;   /* reply status code                         */
};

union Cmd {
     struct rmsio_stat reply;
     struct varian_ctl varian;
      struct tesla_ctl tesla;
      struct alpha_ctl alpha;
  struct datel_dac_ctl dac;
  struct datel_adc_ctl adc;
       struct acro_ctl acro;
    struct stepper_ctl gs_ctl;
 struct stepper_status gs_status;
    struct  granny_ctl granny;
};

/*
*   Error codes
*/
#define  VME_ERR_FUNC            -1

#define  ALPHA_ERR_NONEXIST   -2
#define  ALPHA_ERR_NORESPOND  -3
#define  ALPHA_ERR_SERIALIO   -4
#define  ALPHA_ERR_BUFFULL    -5

#define  IG_ERR_NONEXIST   -2
#define  IG_ERR_NORESPOND  -3
#define  IG_ERR_SERIALIO   -4
#define  IG_ERR_BUFFULL    -5
#define  IG_ERR_INVALID    -6

#define  TESLA_ERR_NONEXIST   -2
#define  TESLA_ERR_NORESPOND  -3
#define  TESLA_ERR_SERIALIO   -4
#define  TESLA_ERR_BUFFULL    -5
#define  TESLA_ERR_BROKEN     -6

#define  AVME_ERR_NONEXIST  -2
#define  DATEL_ERR_NONEXIST -2

#define  STEP_ERR_NONEXIST -2

#define  GRANNY_ERR_NONEXIST   -2
#define  GRANNY_ERR_NORESPOND  -3
#define  GRANNY_ERR_SERIALIO   -4
#define  GRANNY_ERR_BUFFULL    -5
#define  GRANNY_ERR_BROKEN     -6

#endif     /* end RMSVME_H_    */
