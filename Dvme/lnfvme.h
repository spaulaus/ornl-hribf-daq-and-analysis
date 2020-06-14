/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1996-2003
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
*    Environment:  LN Fill Control System
*
*    File:         /usr/users/mcsq/Dvme3/lnfvme.h
*
*    Description:  Data structures for the LN fill control functions.
*          
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    5/13/96    MCSQ
*
*    6/27/96    MCSQ      Add structure for status read/write.
*
*    7/ 9/96    MCSQ      Add High voltage control structures.
*
*    7/12/96    MCSQ      Change hardware error codes.  Add structures
*                         for function which returns the hardware status word.
*
*    7/19/96    MCSQ      Add function which returns VME time to workstation.
*                         Remove Temp #2 from detector data structure.
*                         Remove all errors for temp # 2.
*
*   10/10/96    MLH       Add definitions for LeCroy HV1440 system.
*   10/24/96    MLH       define LRS1440 to 0x5252.
*    3/07/97    MLH       Define NUM_1440 =4, not =1.
*                         Done because NUM_14xx is checked for actual ctrl no.,
*                        not for max possible no. of mainframes as stated below.
*
*    9/12/97    MCSQ      Add mapping of serial channels to HV controller
*                         types.  Symbols MF1458A,MF1458B,MF1440A and MF1440B.
*
*   12/ 5/97    MCSQ      Changed NUM_VALVES from 32 to 64.
*
*    1/30/98    MCSQ      Add structures for high voltage status
*
*    2/ 2/98    MCSQ      Add structure for data in LN_FILL segment.
*
*    2/ 8/99    MCSQ      Add tank pressure out_of_range status bit
*
*    7/ 2/99    MCSQ      Changed NUM_HV from 256 to 512 so second
*                         mainframe can be used for GE HV.
*
*    1/29/03    MCSQ      Add room temperature check.
*****************************************************************************/
#ifndef  LNFVME_H_
#define  LNFVME_H_

#include <time.h>

/*
*   All parameters are defined as integers. Furthermore, all parameters
*   are zero or positive.  A value of -1 means disable.  For example,
*   if a detector has no temperature monitors, then temp_chan should be set
*   to -1.  Also unused detectors in the group.det array should be set to -1.
*
*   A parameter value of -2 or less means DO NOT change the parameter
*   data.
*/
#define  DET_HV   4        /* Max number of HV supplies per GE det            */

struct detector {
        int enable;        /* Filling enabled, 1 or greater = yes,            */
                           /*                  0 or less = no                 */
        int group;         /* Detector group association                      */
        int status;        /* status - bit encoded                            */
        int last_time;     /* last filling time in seconds                    */
     time_t last_date;     /* date and time of last fill                      */
        int fill_interval; /* time in seconds between fillings                */
        int min_interval;  /* min time between fillings                       */
        int max_interval;  /* max time between fillings                       */
     time_t next_date;     /* date and time of next fill                      */
        int min_fill_time; /* min fill time in seconds                        */
        int max_fill_time; /* max fill time in seconds                        */
        int temp;          /* detector temperature                            */
        int max_temp;      /* upper temperature limit                         */
        int temp_chan;     /* channel number                                  */
        int volt_chan[DET_HV];  /* HV channel numbers                         */
        int alarm_chan;    /* channel number of alarm sense                   */
        int valve;         /* Fill valve bit number                           */
        int sensor;        /* overflow sensor temperature                     */
        int sensor_on;     /* Less than this is on, greater then is off       */
        int sensor_chan;   /* Overflow sensor. Channel number                 */
        int chksum;        /* check sum                                       */
};

#define  DET_GRP   8       /* Max numver of detectors per group               */

struct manifold {
        int enable;        /* 1= enable fill, all else = disable fill        */
        int status;        /* status - bit encoded                            */
     time_t next_date;     /* date and time for next cool/fill cycle          */
        int last_time;     /* last cool time in seconds                       */
        int min_cool_time;  /* min cool time in seconds                       */
        int max_cool_time;  /* max cool time in seconds                       */
        int valve;         /* Manifold valve bit number                       */
        int sensor;        /* overflow sensor temperature                     */
        int sensor_on;     /* Less than this is on, greater than is off       */
        int sensor_chan;   /* Manifold overflow sensor channel number         */
        int det[DET_GRP];  /* detectors in this group                         */
        int chksum;        /* check sum                                       */
};
/*
*   Detector and group status bits
*/

#define FILL_IN_PROGRESS     1      /* Manifold/det fill cycle in progress    */
#define FILLING              2      /* Manifold/detector filling              */
#define FILL_TIME_SHORT      4      /* Fill time was minimum fill time        */
#define FILL_TIME_LONG       8      /* Fill time was maximum fill time        */
#define TEMP_HIGH            0x10   /* Detector temperature too high          */
#define FILL_INTERVAL_LONG   0x20   /* maximum fill interval exceeded         */
#define CHKSUM_ERR           0x40   /* Startup checksum error                 */
#define TANK_EMPTY           0x80   /* LN tank may be empty                   */
#define HV_SHUTDOWN          0x100  /* HV has been shutdown                   */
#define OVERFLOW_RTD         0x200  /* Overflow RTD measures out_of_range     */
#define TEMP_RTD             0x400  /* Temp RTD measures out_of_range         */
#define HV_ERROR             0x800  /* Error attempting to do HV shutdown     */
#define LINE_PRESS           0x1000 /* Tank Pressure out_of_range             */
#define ROOM_TEMP            0x2000 /* Room temperature oft_of_range          */

/*
*   Room temperature alarm set points
*/

struct room_temp {
        int  max;       /* Max room temp in ADC units                         */
        int  min;       /* Min room temp in ADC units                         */
        int  chksum;    /* Check sum                                          */
};

/*
*   Hardware error status.  If the bit is set,
*   it means the hardware was not found at startup.
*/
#define ACRO9480_MOD   0x1   /* ACROMAG 9840 module not found                 */
#define VMIC6015_MOD   0x2   /* VMIC 6015 RS232 module not found              */
#define VMIC3113A_MOD  0x4   /* VMIC 3113A ADC module not found               */

#define RELAY_POWER    0x8   /* Relay Power supply failure                    */

/*
*   System status.
*/
#define FILL_DISABLED  0x100 /* Filling has been disabled by operator         */

/*
*   Software Commands and data structures
*/

#define GET_GROUP      0x4040   /* Set group parameters                       */
#define SET_GROUP      0x4141   /* Read group parameters                      */
#define GET_DETECTOR   0x4242   /* Set detector parameters                    */
#define SET_DETECTOR   0x4343   /* Read detector parameters                   */
#define GET_TEMP       0x4444   /* Read temperature monitors                  */
#define GET_STATUS     0x4545   /* Read group and detector status words       */
#define SET_STATUS     0x4646   /* Write group and detector status words      */
#define LRS1458        0x4747   /* Command to HV controller                   */
#define HARDWARE_STAT  0x4848   /* Read hardware status word                  */
#define FILL_ENABLE    0x4949   /* Enable detector filling                    */
#define FILL_DISABLE   0x5050   /* Disable detector filling                   */
#define GET_VME_TIME   0x5151   /* Read the VME time (time_t value)           */
#define LRS1440        0x5252   /* Command to HV1440  controller              */
#define GET_HVSTAT     0x5353   /* Read high voltage status array             */
#define SET_HVSTAT     0x5454   /* Set high voltage status array              */
#define GET_ROOM       0x5555   /* Read room temperature set points           */
#define SET_ROOM       0x5656   /* Set room temperature set points            */


/*
*   Sizes of data structures
*/
#define NUM_GROUP    8     /* Number of detector groups                      */
#define NUM_DETECTOR 30    /* Number of detectors                            */
#define NUM_TEMP     64    /* Number of temp sensors (overflow and GE temp)  */
#define NUM_VALVE    32    /* Number of valves                               */
#define NUM_HV      512    /* Number of HV channels                          */
#define NUM_1458      3    /* Number of possible 1458 HV mainframes (serial  */
#define NUM_1440      4    /* Number of possible 1440 HV mainframes   ports) */

/*
*   Map serial channels to LeCroy controller types
*/
#define MF1458A       0    /* Serial channel 0 supports 1458                 */
#define MF1458B       1    /*                1   "      1458                 */
#define MF1440A       2    /* Serial channel 2 supports 1440                 */
#define MF1440B       3    /*                3   "      1440                 */

struct  group_stat {
          short func;
          char  rpystat;
          char  group_indx;
struct manifold grp;
};

struct  detector_stat {
          short func;
          char  rpystat;
          char  detector_indx;
struct detector det;
};

struct  temperature {
          short func;
          char  rpystat;
          char  dum;
          int   data[NUM_TEMP];
};

struct  status {
          short func;
          char  rpystat;
          char  dum;
          int   gstat[NUM_GROUP];
          int   dstat[NUM_DETECTOR];
};

struct  lnfio_stat {
         short func;
         char  rpystat;   /* reply status code                         */
};

struct  lrs1458_ctl {
         short func;
         char  rpystat;
         char  mainframe;  /* Mainframe number, 0 thru 2               */
         char  data[512];  /* ASCII command string                     */
};

struct  lrs1440_ctl {
         short func;
         char  rpystat;
         char  mainframe;  /* Mainframe number,        3               */
         char  data[512];  /* ASCII command string                     */
};

struct  hardware_stat {
          short func;
          char  rpystat;
          char  dum;
          int   status;
};

struct  run_control {
          short func;
          char  rpystat;
};

struct  vme_time {
          short func;
          char  rpystat;
          char  dum;
          int   time;
};

struct  hv_stat {
          short func;
          char  rpystat;
          char  dum;
          int   data[NUM_DETECTOR];
};

struct  room_alarm {
          short func;
          char  rpystat;
          char  dum;
struct room_temp room;
};

union Cmd {
     struct lnfio_stat reply;
     struct group_stat group_io;
  struct detector_stat detector_io;
    struct temperature temp;
         struct status status;
    struct lrs1458_ctl lrs_cmd;
    struct lrs1440_ctl lrs1440_cmd;
  struct hardware_stat hardware;
    struct run_control ln_run;
       struct vme_time vmetime;
        struct hv_stat hvstat;
     struct room_alarm alarms;
};

/*
*   Error codes
*/
#define  VME_ERR_FUNC            -1
#define  LN_ERR_NONEXIST         -2
#define  LN_ERR_GRP_BUSY         -3
#define  LN_ERR_DET_BUSY         -4
#define  LN_ERR_INVALGRP         -5
#define  LN_ERR_INVALDET         -6
#define  LN_ERR_HARDWARE         -7

#define  LRS_ERR_NONEXIST        -2
#define  LRS_ERR_NORESPOND       -3
#define  LRS_ERR_SERIALIO        -4
#define  LRS_ERR_BUFFULL         -5
#define  LRS_ERR_MAINFRAME       -6


/*
*     Data to be shared with 'beeper task'
*/
struct lndata {
     struct manifold  mani[NUM_GROUP];
     struct detector  det[NUM_DETECTOR];
                 int  hvstat[NUM_DETECTOR];
    struct room_temp  room;
                 int  hardware_ok;
                 int  hardware_status;
};

#define  LNDATA  0x20000     /* Hardware address for struct lndata */

#endif     /* end LNFVME_H_    */
