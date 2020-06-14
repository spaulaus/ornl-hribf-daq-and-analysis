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
*    File:         /usr/users/mcsq/Dvme3/lnfxx.c
*
*    Description:  
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    8/ 7/96    MCSQ         
*
*   10/ 4/96    MCSQ      Fix error in put_xmit_char routine  when the
*                         the transmit buffer is full.  Was not restoring
*                         the proper interrupt level before return when
*                         call failed because the buffer was full.
*
*   10/04/96    MLH          Add LRS HV1440 code (except local_1440, sprintf).
*
*    2/ 6/97    MCSQ      Set RTD limits (MIN_RTD and MAX_RTD) to -210 deg. C
*                         and +50 deg. C.  Also enabled all checks for
*                         hardware modules present at startup.
*
*    2/ 7/97    MCSQ      Increased timeout for a response from the LRS 1548
*                         HV controller.  Check RTDs for out-of-range
*                         every 5 minutes.
*
*    2/12/97    MCSQ      Force RTD check after each SET_GROUP and
*                         SET_DETECTOR command.
*    
*    2/21/97    MLH       Change to i < 3 for the 9600-baud channels.
*
*    3/11/97    MCSQ      Major revisions to routine lrs1458_io.
*
*    3/14/97    MCSQ      Update group and detector temperatures prior
*                         to each check for RTDs out_of_range.
*
*    3/28/97    MCSQ      There are only 4 serial IO channels numbered 0
*                         thru 3.  Check for valid serial channel number in
*                         routines lrs1458_io and lrs1440_io.  Return
*                         LRS_ERR_NONEXIST for invalid channel number.
*
*    4/14/97    MCSQ      Rework lrs1440_io routine.  Provide more time
*                         to receive all reply messages.  Also allow
*                         extra time for a response to the HV "ON" command.
*                         Change version number from 1.4 to 1.5 in the
*                         startup message.
*
*    5/19/97    MCSQ      Double ADC sample rate to 10 Hz.  Change filter
*                         gain and cutoff to 1/16 and 0.1 Hz respectively.
*
*    5/25/97    MCSQ      Increase ADC sample rate to approx 60 Hz and change
*                         filter gain factor to 1/32.  Cutoff frequency is
*                         now approx 0.3 Hz.
*
*    6/ 7/97    MCSQ      Add second stage of digital filter.
*
*    6/18/97    MCSQ      Check RTDs for out-of-range every 30 minutes instead
*                         of every 5 minutes.  Also remove message which
*                         shows RTD channel number and measured value.
*
*    8/11/97    MCSQ      When the LRS 1458 mainframe is powered down and
*                         this code attempts to turn detector High Voltages,
*                         there was a problem with workstation communication.
*                         It takes almost 4 seconds to attempt to turn off
*                         one channel of HV.  Since there are 4 HV channels
*                         per detector, it takes 12 seconds to turn off one
*                         detector.  The VME system could not respond to
*                         requests for 12 seconds which resulted in 'timeouts'
*                         on the workstation.
*
*                         The HV turn off routines now queue the request.  The
*                         actual turn off is done in the routine 'main'.  Only
*                         one channel is turned off per pass of the main poll
*                         loop.  Hence, the worst case delay in responding to 
*                         workstation requests is approx 4 seconds.
*
*    8/26/97    MCSQ      Initialize both ports 2 and 3 of the VMIC 6015
*                         for LRS 1440 HV mainframes.
*
*    8/29/97    MCSQ      For LRS1440, allow extra time for a response
*                         to HV "OF" command.
*
*   10/22/97    MCSQ      Add messages to host which show fill time required
*                         to cool the overflow sensor.  Also show manifold
*                         cool time.
*
*                         Routine 'set_status' now updates battery backed
*                         RAM and also sends one message to the host to
*                         show when status was modified.
*
*   10/25/97    MCSQ      Initialize digital filter arrays with the first ADC
*                         conversion.  This avoids a dip in the temperature
*                         data following start up.
*
*   10/31/97    MCSQ      Schedule next fill relative to start of manifold
*                         cool start time minus 10 seconds.  This reduces the
*                         'drift' in the fill schedule.
*
*   11/ 6/97    MCSQ      Added a four character message type flag at the
*                         beginning of each message sent to the host.
*                         Messages type are currently M0 thru M22.
*
*   11/11/97    MCSQ      Add an automatic cool down fill sequence for
*                         start up of a warm detector.  See the routine
*                         get_fill_intervals.
*
*   11/26/97    MCSQ      Recompute next manifold cool times when host
*                         enables the manifold.
*
*   12/ 4/97    MCSQ      Change way we keep time in the main poll loop.
*                         Previously a loop counter was used.  However,
*                         attempts to access HV controllers which are
*                         disconnected or powered off causes problems
*                         with the loop count method.  Now I use current
*                         date and time and a target time for each periodic
*                         function.
*
*   12/ 5/97    MCSQ      Beep, beep and beep.
*
*   12/17/97    MCSQ      Well, the beeper hardware is Totally different
*                         from early this month.  The only way I see to
*                         keep this code working is to make a separate
*                         task for the beeper code.
*
*                         This code now sets Event #55 when a serious
*                         error is detected.  Some other task must wait
*                         for Event #55 and do what ever is required to 
*                         beep someone.
*
*                         All LeCroy 1440 stuf has been disabled so that
*                         a channel of the 6015 serial interface may
*                         be used by another task.
*
*    1/30/98    MCSQ      Add routines for workstation to read/write the
*                         high voltage status array.
*
*    2/ 3/98    MCSQ      Add routine to copy manifold, detector and
*                         hv_status data to the Memory segment LN_FILL
*                         every time beep() is called. (Currently this
*                         is every 15 minutes).  The beeper task can
*                         access this for more informative beeping.
*
*                         Also copy fill system data to LN_FILL every 30
*                         seconds.  The beeper task may use the fill system
*                         data or Event #55 or both.
*
*    2/ 9/98    MCSQ      When a manifold is disabled by the host, set
*                         next_date to 0. When any detector is enabled by the
*                         host, run schedule().
*                         Detector last_date AND next_date zero no longer
*                         suspends detector temperature checking.  Detector
*                         temperature checking is suspended while the enable
*                         is greater then 1.
*
*    2/24/98    MCSQ      If the detector temperature measures less than
*                         MIN_RTD, the high voltage is turned off.
*
*    4/16/98    MCSQ      Y2K patch.  If the year as read from the RTC chip
*                         is less than 30, add 100 to the year.  For example,
*                         if we read 10 from the RTC chip, take this to mean
*                         year 2010.
*
*    6/ 5/98    MCSQ      Now that there is a beeper, It is important that
*                         the user be notified whenever the VME system
*                         is rebooted.  A flag is set to the number of times
*                         to beep user after startup of this process.
*                         The flag may be reset by:
*                           1)  In routine beep_check after triggering
*                               the beeper event the specifed number of times.
*                           2)  Reading the hardware status word.
*                         Hence, if an operator is present when the system
*                         reboots, the operator may supress the beeper
*                         by checking the hardware status.  Otherwise,
*                         call user at first call to routine beep_check
*                         which is currently 5 minutes after reboot.
*
*                         Save the hv_status array in battery backed RAM.
*                         At start up, restore hv_status in routine
*                         getdatabase.  Added routine backup_hv_stat()
*                         to move hv_status array to battery backed RAM
*                         and compute checksum.  If there is a checksum
*                         error on restore, enable detector temp checking
*                         for ALL detectors.
*
*    6/19/98     MCSQ     Changed delay in mainloop from 100 milliseconds
*                         to 10 milliseconds to improve workstation response.
*                         Also changed HV shutdown so than only one detector
*                         is shutdown per pass thru main loop.
*
*    2/16/99     MCSQ     Add VMIC 3124 ADC for pressure sensor.  Only 1
*                         channel is used. The pressure reading
*                         is put in channel 63 of the temperature array
*                         so the workstation can read it.
*
*                         Add valve at tank which is opened after the
*                         manifold valve is opened and closed after
*                         all detectors have filled.  Use channel 29 for
*                         this valve. 
*
*    3/16/99     MCSQ     Remove use of the RTC chip to avoid Y2K problems.
*                         Time will only be set from the workstation.  Will
*                         change workstation to set time in the VME
*                         8 times daily.
*
*    4/6/99      MCSQ     Change fill state machine to add state which
*                         keeps tank valve open for 200 seconds after
*                         fill completes.
*
*    4/13/99     MCSQ     Add state to check tank pressure 5 seconds after
*                         the tank valve is opened.  If pressure is out_of_
*                         limits, disable manifold and abort fill cycle.
*
*                         Modify acromag module so we can test for relay
*                         power supply failures.  If relay power supply
*                         fails, readback of relays will show ALL relays
*                         on.  See routine relay_check().
*
*    5/17/99     MCSQ     Line pressure out-of-range did NOT activate the
*                         beeper!! Fix that in routine beep_check().
*
*                         Show measured line pressure in message to host
*                         when line pressure is out-of-range.
*
*                         Check the line pressure while waiting to close
*                         the tank valve.  If the line pressure goes
*                         out of range, close tank valve immediately.
*
*    7/19/99     MCSQ     Fix routine 'hv_off' so that it computes the
*                         correct slot and channel in the second mainframe.
*
*                         Do not allow workstation to change the temperature
*                         checking for clover detectors.
*
*   12/ 6/99     MCSQ     Add new VMIC 6015 module for the LeCroy 1440
*                         high voltage controllers.  Put back code to
*                         handle 1440s.  The new module is NOT required
*                         for filling system operation.
*
*    2/22/00     MCSQ     Attempt to correct drift in filling time and
*                         change version to 6.2
*
*    8/ 2/00     MCSQ     Reduce time that overflow sensor must remain cold
*                         from 30 seconds to 5 seconds.
*
*    1/17/01     MCSQ     Now we allow the workstation to change temperature
*                         checking for clover detectors.  The restores it to
*                         the way it was before 7/19/99.
*
*                         Change version to 6.3
*
*   12/30/02     MCSQ     Update LNTEST conditional.  Remove call to
*                         relay_check when compiled with LNTEST defined.
*
*    1/29/03     MCSQ     Add room temperature check and alarm.
*
*    2/12/03     MCSQ     Add degrees F to room temperature out-of-range
*                         message.
*
*    8/22/03     MCSQ     Increase MAX_ROOM setpoint to 28.5 deg.C
*                         (i.e. 5 deg. F greater than it was)
*
*    9/10/03     MCSQ     Add code to allow user to read and set the
*                         room temperature set points.
*****************************************************************************/
#include "devices.h"
#include "vmic6015.h"
#include "vmic3113a.h"
#include "vmic3124.h"
#include "acromag.h"
#include "vme_sys.h"
#include "system.h"
#include "syram.h"
#include "vmeprom.h"
#include "orph.h"
#include "orphmsg.h"
#include "lan.h"
#include "lan_lib.h"
#include "lnfvme.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>


/*
*   Host Message types.  Define with bytes and words swapped to make
*   DEC happy.
*/
#define INFORM  0x01000000       /* really type = 1 */
#define WARN    0x02000000
#define PANIC   0x03000000
#define HEART   0x04000000

/*
*   Message control definitions
*
*   Define HOST_MSG for message sent to workstation which booted VME system
*   Define LOCAL_MSG for message output on port 1 of VME processor
*
#define LOCAL_MSG
#define HOST_MSG
*
*/
#define HOST_MSG

/*
*   Some ASCII characters
*/
#define  CR     0x0d
#define  LF     0x0a
#define  SPACE  0x020

/*    RTD limits                 */

#define  MIN_RTD   289      /* Approx. -210 deg. C   */
#define  MAX_RTD   2431     /* Approx. +50 deg. C    */

/*    Line Pressure limits       */

#define  MIN_PRESS  1300    /* Approx.  15.0 psi     */
#define  MAX_PRESS  2285    /* Approx.  45.0 psi     */

/*    Default Room temperature limits    */

#define  MIN_ROOM  2201     /* Approx.  20.6 deg. C  */
#define  MAX_ROOM  2263     /* Approx.  28.5 deg. C  */


#define  TANK_VALVE  29     /* Relay channel for tank valve */

#define  VENT_TIME  200     /* Time (seconds) to wait before closing the  */
                            /* tank valve                                 */

/*    Function Prototypes        */
void mainloop(void);
int  getdatabase(void);
void host_message(int ,char *);
int  set_group(int ,struct manifold *);
int  get_group(int ,struct manifold *);
void backup_group(int );
int  crcgen(char *,int,int);
int  set_detector(int ,struct detector *);
int  get_detector(int ,struct detector *);
void backup_detector(int );
int  get_temp(struct temperature *);
int  get_status(struct status *);
int  set_status(struct status *);
int  get_hardware(struct hardware_stat *);
int  get_vme_time(struct vme_time *);
int  get_hvstat(struct hv_stat *);
int  set_hvstat(struct hv_stat *);
int  select_det(int);
int  open_manifold(int);
int  press_check(int grp_indx);
int  cool_manifold(int);
int  open_detectors(int);
int  fill_detectors(int);
int  schedule(int);
int  finish_fill(int);
int  vent_line(int);
void check_fill_interval(void);
void get_fill_intervals(int,int *,int*,int*);
void acro_init(void);
int  acro_write(int,int);
int  relay_check(void);
void update_temp(void);
void check_det_temp(void);
void check_rtd(void);
void group_hv_off(int);
void det_hv_off(int);
int  hv_off(int);
void vmic_adc_init(void);
void adc_intr(void);
int  lrs1458_io(struct lrs1458_ctl *);
int  lrs1440_io(struct lrs1440_ctl *);
void vmic_init(void);
void ch0_xmit(void);
void ch1_xmit(void);
void ch2_xmit(void);
void ch3_xmit(void);
void ch4_xmit(void);
void ch5_xmit(void);
void ch0_recv(void);
void ch1_recv(void);
void ch2_recv(void);
void ch3_recv(void);
void ch4_recv(void);
void ch5_recv(void);
void ch0_special(void);
void ch1_special(void);
void ch2_special(void);
void ch3_special(void);
void ch4_special(void);
void ch5_special(void);
void ch0_ext(void);
void ch1_ext(void);
void ch2_ext(void);
void ch3_ext(void);
void ch4_ext(void);
void ch5_ext(void);
void clr_recv_buf(struct recvbuf *);
void clr_xmit_buf(struct xmitbuf *);
int  get_recv_count(struct recvbuf *);
int  get_xmit_count(struct xmitbuf *);
int  put_xmit_char(int , unsigned char );
int  get_xmit_char(struct channel_data *, unsigned char *);
int  put_recv_char(struct channel_data *, unsigned char , unsigned char *);
int  get_recv_char(int , unsigned char *,unsigned char *);
void watchdog(void);
void beep_check(void);
void lnsave(void);
void backup_hv_stat(void);
void backup_room(void);
int  get_room(struct room_alarm *);
int  set_room(struct room_alarm *);
void room_check(void);
float tconv(int);

#define LN_GROUP     0xffc12000
#define LN_ROOM_TEMP 0xffc122c0
#define LN_HV_STAT   0xffc12300
#define LN_DETECTOR  0xffc12400

/*
*   Global data
*/
static struct devices *devtbl = DEVTBL;
struct channel_data channel[6];
struct acromag *avme = (struct acromag *)ACROMAG;
struct manifold  groups[NUM_GROUP];        /* detector groups data           */
struct detector  detectors[NUM_DETECTOR];  /* detector data                  */
struct room_temp room;                     /* Room temperature set points    */
int    temp[NUM_TEMP];                     /* temperature sensor data        */
int    det_to_fill[DET_GRP];               /* array of detectors to fill     */
int    short_fill[DET_GRP];                /* array of short fill time flags */
time_t vent_time;                          /* time to start vent of fill line*/
time_t start_overflow[DET_GRP];            /* time first sense overflow      */
time_t start_time;       /* time_t value for start of an operation in fill   */
time_t cycle_time;       /* start time for manifold fill cycle               */
char   ascii_time[22];   /* array for time and date                          */
char   error_msg[105];   /* message buffer for host messages                 */
int    hardware_ok;      /* 0 means one or more hardware modules missing     */
int    hardware_status;  /* bit encoded status word.  There are bits for     */
                         /* each VME module type and one for disabled by     */
                         /* operator.                                        */
int    reboot;           /* Set to number of times to beep after startup     */

struct  HV_TURN_OFF {
       int  det;         /* Detector number                                  */
       int  chan;        /* HV channel number                                */
}  hv_shutdown[NUM_DETECTOR*DET_HV];

int    hv_turn_off;      /* Number of HV channels to be turned off           */
     
int    fill_enable = 1;  /* 1 means filling enabled and 0 means filling has  */
                         /* been disabled by operator.                       */
char   hv_status[NUM_DETECTOR];  /* HV internal status.  1 means we have     */
                                 /* turned off the HV for this detector.     */
                                 /* Set to zero when workstation enables     */
                                 /* this detector.                           */
int    tank_problem;     /* test for problems with LN2 supply tank           */

int     press[16];       /* pressure data                                    */

struct lrs1458_ctl local_1458;

/*
*   Fill cycle sequence
*/
int (*fill_seq[10])(int)  = {open_manifold,press_check,cool_manifold,
                              open_detectors,fill_detectors,schedule,
                              finish_fill,vent_line,NULL,NULL};
enum state  {OPEN_MAN = 0,PRESS_CHK,COOL_MAN,OPEN_DET,FILL_DET,SCHEDULE,
                                                      FIN_FILL,VENT,STOP_FILL};

/*****************************************************************************
*
*  Dummy main routine.  All it does is change to supervisor mode and
*  call the real main - mainloop.
*****************************************************************************/
void main()
{
    super_mode_();
    mainloop();
}
/*****************************************************************************
*****************************************************************************/
void mainloop(void)
{

#define  FILL_CHK_INTERVAL    20  /* 20 seconds        */
#define  TEMP_CHK_INTERVAL     2  /*  2 seconds        */
#define  RTD_CHK_INTERVAL   1800  /* 30 minutes        */
#define  MAX_CHK_INTERVAL    300  /* 5 minutes         */
#define  WATCHDOG_INTERVAL   150  /*  2.5 minutes      */
#define  BEEP_CHK_INTERVAL   900  /* 15 minutes        */
#define  SAVE_DATA_INTERVAL   30  /* 30 seconds        */

    int    (**fill_pgm)(int);
    int    fill_busy = 0;
    int    i,size;
    int    fill_group = NUM_GROUP - 1;
    int    hv_error;
    time_t tod;
    static struct chk_receive lan_stat;
    char   *inbuf,*outbuf;
    struct Ether_Packet *out_hdr;
    union Cmd *cmd,*rply;
    static time_t fill_chk_time,temp_chk_time,rtd_chk_time;
    static time_t max_chk_time,watchdog_time,beep_chk_time;
    static time_t save_data_time;

    fill_pgm = &fill_seq[STOP_FILL];
    lan_open((char)PROTO_LNFILL,&outbuf,&out_hdr);
    task_priority_(-1,1,69);
    lan_stat.Request_Number = 0;
    reboot = 8;

/*
*    Report startup to the host
*/
    sprintf(error_msg,"M0  ********* Startup LN Filling System 6.4 *********");
    host_message(INFORM,error_msg);
/*
*    Initialize the hardware modules used in LN fill.
*/
    hardware_ok = 1;
    hardware_status = 0;
    if (devtbl->vmic6015a) vmic_init();
    else
      {
        sprintf(error_msg,"M0  VMIVME 6015 RS232 interface module not found");
        host_message(PANIC,error_msg);
        hardware_status |= VMIC6015_MOD;
        hardware_ok = 0;
      }
    if (devtbl->vmic3113a && devtbl->vmic3124) vmic_adc_init();
    else
      {
        sprintf(error_msg,"M0  VMIVME ADC modules not found");
        host_message(PANIC,error_msg);
        hardware_ok = 0;
        hardware_status |= VMIC3113A_MOD;
      }
    if (devtbl->acromag) acro_init();
    else
      {
        sprintf(error_msg,"M0  ACROMAG 9480 module not found");
        host_message(PANIC,error_msg);
        hardware_ok = 0;
        hardware_status |= ACRO9480_MOD;
      }
/*
*   Initialize HV shutdown
*/
    for (i=0; i < NUM_DETECTOR*DET_HV; i++) hv_shutdown[i].chan = -1;
    hv_turn_off = 0;

/*======================  Testing 1, 2, 3  ==================================*/
/*
*    Define LNTEST if you want to do software testing on a VME
*    system which does not have the hardware for the real
*    filling system.  This fakes hardware OK and sets all
*    RTDs to approx 64 deg K.

#define  LNTEST

*/

#ifdef  LNTEST

hardware_ok = 1;
hardware_status = 0;
for (i=0; i < 29; i++) temp[i] = 300;   /* overflow sensors             */
for (i=29; i < 63; i++) temp[i] = 900;  /* Detector temperatures        */
temp[63] = 1700;                        /* Line pressure                */

#endif
/*======================  Testing 1, 2, 3  ==================================*/

/*
*    Restore group and detector data from battery backed memory.
*    If hardware modules are missing disable everything.
*/

    time(&tod);
    getdatabase();
    if (!hardware_ok)
      {
        for (i=0; i < NUM_GROUP; i++)
          {
            groups[i].enable = -1;
            group_hv_off(i);
          }
        fill_chk_time = (time_t)0x7fffffff;
        temp_chk_time = (time_t)0x7fffffff;
        rtd_chk_time = (time_t)0x7fffffff;
        max_chk_time = (time_t)0x7fffffff;
      }
    else
      {
        fill_chk_time = tod + 60;
        temp_chk_time = tod + 5;
        rtd_chk_time = tod + 30;
        max_chk_time = tod + 30;
      }
    watchdog_time = tod + WATCHDOG_INTERVAL;
    beep_chk_time = tod + 300;
    save_data_time = tod + SAVE_DATA_INTERVAL;
/*
*   Startup initialization is done.   We run in the following loop
*   from now on.
*/
    while (1)
      {
/*
 *  Check for a message from the host workstation
 */
        lan_ioctl(EIOCHKREC,&lan_stat);
        if (lan_stat.status)
          {
            size = lan_read(5,&inbuf);  /* read with timeout          */
            if (!size) continue;        /* size of zero means timeout */
            cmd = (union Cmd *)inbuf;
            rply = (union Cmd *)outbuf;
            *rply = *cmd;
            switch (cmd->group_io.func)
             {
               case GET_GROUP:
                 rply->group_io.rpystat =
                                      get_group((int)rply->group_io.group_indx,
                                                          &rply->group_io.grp);
                 size = sizeof(struct group_stat);
                 break;
               case SET_GROUP:
                 rply->group_io.rpystat =
                                       set_group((int)cmd->group_io.group_indx,
                                                           &cmd->group_io.grp);
                 size = sizeof(struct lnfio_stat);
                 time(&tod);
                 if (hardware_ok) rtd_chk_time = tod + 30;
                 break;
               case SET_DETECTOR:
                 rply->detector_io.rpystat = set_detector(
                                           (int)cmd->detector_io.detector_indx,
                                                        &cmd->detector_io.det);
                 size = sizeof(struct lnfio_stat);
                 time(&tod);
                 if (hardware_ok) rtd_chk_time = tod + 30;
                 break;
               case GET_DETECTOR:
                 rply->detector_io.rpystat = get_detector(
                                          (int)rply->detector_io.detector_indx,
                                                       &rply->detector_io.det);
                 size = sizeof(struct detector_stat);
                 break;
               case GET_TEMP:
                 rply->temp.rpystat = get_temp((struct temperature *)rply);
                 size = sizeof(struct temperature);
                 break;
               case GET_STATUS:
                 rply->status.rpystat = get_status((struct status *)rply);
                 size = sizeof(struct status);
                 break;
               case SET_STATUS:
                 rply->status.rpystat = set_status((struct status *)cmd);

#ifndef  LNTEST
                 if (!relay_check()) hardware_status &= ~RELAY_POWER;
#endif
                 size = sizeof(struct lnfio_stat);
                 break;
               case LRS1458:
                 rply->lrs_cmd.rpystat = lrs1458_io((struct lrs1458_ctl *)rply);
                 size = sizeof(struct lrs1458_ctl);
                 break;
               case LRS1440:
                 rply->lrs_cmd.rpystat = lrs1440_io((struct lrs1440_ctl *)rply);
                 size = sizeof(struct lrs1440_ctl);
                 break;
               case HARDWARE_STAT:
                 rply->hardware.rpystat = get_hardware(
                                                 (struct hardware_stat *)rply);
                 size = sizeof(struct hardware_stat);
                 reboot = 0;
                 break;
               case FILL_ENABLE:
                 rply->hardware.rpystat = 0;
                 fill_enable = 1;
                 sprintf(error_msg,
                               "M1  LN2 Filling has been ENABLED by operator");
                 host_message(PANIC,error_msg);
                 hardware_status &= ~FILL_DISABLED;
                 size = sizeof(struct run_control);
                 break;
               case FILL_DISABLE:
                 sprintf(error_msg,
                              "M1  LN2 Filling has been DISABLED by operator");
                 host_message(PANIC,error_msg);
                 rply->hardware.rpystat = 0;
                 fill_enable = 0;
                 for (i=0; i < NUM_VALVE; i++) acro_write(i,0);
                 if (fill_busy) fill_pgm = &fill_seq[FIN_FILL];
                 else   fill_pgm = &fill_seq[STOP_FILL];
                 hardware_status |= FILL_DISABLED;
                 size = sizeof(struct run_control);
                 break;
               case GET_VME_TIME:
                 rply->vmetime.rpystat = get_vme_time((struct vme_time *)rply);
                 size = sizeof(struct vme_time);
                 break;
               case GET_HVSTAT:
                 rply->hvstat.rpystat = get_hvstat((struct hv_stat *)rply);
                 size = sizeof(struct hv_stat);
                 break;
               case SET_HVSTAT:
                 rply->hvstat.rpystat = set_hvstat((struct hv_stat *)rply);
                 size = sizeof(struct lnfio_stat);
                 break;
               case GET_ROOM:
                 rply->alarms.rpystat = get_room((struct room_alarm *)rply);
                 size = sizeof(struct room_alarm);
                 break;
               case SET_ROOM:
                 rply->alarms.rpystat = set_room((struct room_alarm *)rply);
                 size = sizeof(struct lnfio_stat);
                 break;
               default:
                 size = sizeof(struct lnfio_stat);
                 rply->reply.rpystat = VME_ERR_FUNC;
                 break;
             }
            lan_reply(size,ACK);
          }
/*
*   If filling is enabled by operator and when no group fill is in progress,
*   we check for starting another fill cycle approx every 20 seconds.  If any
*   group needs filling, we set the busy flag - fill_busy - and set the fill
*   cycle pointer to the top of the fill program.
*/
     time(&tod);
     if (fill_enable && !fill_busy && (tod >= fill_chk_time))
       {
         fill_chk_time = tod + FILL_CHK_INTERVAL;
         for (i=0; i < NUM_GROUP; i++)
           {
             fill_group++;
             if (fill_group >= NUM_GROUP) fill_group = 0;
             if (groups[fill_group].enable == 1)
               {
                 if (groups[fill_group].next_date <= tod)
                   {
                     strftime(ascii_time,20,"%d-%b-%y %H:%M:%S  ",
                                                              localtime(&tod));
                     sprintf(error_msg,
                                    "M2  Start Fill Cycle for Group %i at %s",
                                                        fill_group,ascii_time);
                     host_message(INFORM,error_msg);
                     fill_pgm = &fill_seq[OPEN_MAN];
                     fill_busy = 1;
                     break;
                   }
               }
           }
       }
/*
*  If a fill is in progress, execute the routine pointed to by
*  fill_pgm.  When the fill is complete, fill_pgm will point to a NULL
*  and we reset the fill_busy flag.
*/

     if (*fill_pgm != NULL)
       {
         i = (*fill_pgm)(fill_group);
         if (i < OPEN_MAN || i > STOP_FILL) i = STOP_FILL;
         if (i != 0) fill_pgm = &fill_seq[i];
       }
     else
      {
        fill_busy = 0;
/*
*  When two consecutive fill cycles abort due to long cool time limit,
*  we set the TANK_EMPTY status bit in the last manifold which failed.
*/
        if (tank_problem >= 2)
          {
            groups[fill_group].status |= TANK_EMPTY;
            sprintf(error_msg,"M3  Check for LN2 tank empty.");
            host_message(PANIC,error_msg);
            tank_problem = 0;
          }
      }
/*
*   Check for HV channels to be turned off
*/
     if (hv_turn_off)
       {
         int mf,slot,ch;

         for (i=0; i < NUM_DETECTOR*DET_HV; i++)
           {
             if (hv_shutdown[i].chan == -1) continue;
             if (hv_error = hv_off(hv_shutdown[i].chan))
               {
                 detectors[hv_shutdown[i].det].status |= HV_ERROR;
                 mf = hv_shutdown[i].chan/256;
                 ch = hv_shutdown[i].chan;
                 if (ch >= 256) ch = ch - 256;
                 slot = ch/16;
                 ch = ch % 16;
                 sprintf(error_msg,
                 "M4  HV shutdown error for detector %i, HV Chan S%i.%i.%i  %i",
                                        hv_shutdown[i].det,mf,slot,ch,hv_error);
                 host_message(INFORM,error_msg);
               }
             hv_shutdown[i].chan = -1;
             hv_turn_off--;
             break;
           }
       }
/*
*  Check for detectors for which the maximum fill interval
*  has been exceeded.  Disable filling for any detector
*  which exceeded the max.
*/
     if (tod >= max_chk_time)
       {
         check_fill_interval();
         max_chk_time = tod + MAX_CHK_INTERVAL;
       }
/*
*  Update temperature data and check detector temperature limits.
*/
     if (tod >= temp_chk_time)
       {
         update_temp();
         check_det_temp();
         temp_chk_time = tod + TEMP_CHK_INTERVAL;
       }
     if (tod >= rtd_chk_time)
       {
         update_temp();
         check_rtd();
         rtd_chk_time = tod + RTD_CHK_INTERVAL;
       }
/*
*  Send message to host to show we are still alive approx every 2.5 minutes.
*/
     if (tod >= watchdog_time)
       {
         watchdog();
         watchdog_time = tod + WATCHDOG_INTERVAL;
       }
/*
*  Check to see if we should beep Serious Baklash
*/
     if (tod >= beep_chk_time)
       {

#ifndef LNTEST
         if (relay_check()) hardware_status |= RELAY_POWER;
         room_check();
#endif
         beep_check();
         beep_chk_time = tod + BEEP_CHK_INTERVAL;
       }
/*
*  Save fill system data in LN_FILL for beeper task to see.
*/
     if (tod >= save_data_time)
       {
         lnsave();
         save_data_time = tod + SAVE_DATA_INTERVAL;
       }
/*
*    Execute this loop every 0.01 seconds
*/
     delay_(1);
   }
}
/****************************************************************************
*
*   Move the LN fill database to RAM.  This is done only one time
*   at startup.
*
****************************************************************************/
int getdatabase(void)
{
    int  chksum,i,j,size;
    struct manifold *group_ptr;
    struct detector *det_ptr;
    struct room_temp *room_ptr;
               char *hv_ptr;

/********************
 Zap the database for tests
char *cptr = (char *)LN_GROUP;

while(cptr <= (char *)0xffc1fff0) *cptr++ = 0x00;
********************/

/*
*    Restore the group database.  On a checksum error, disable the group
*    and inform the host of the error.
*/
    group_ptr = (struct manifold *)LN_GROUP;
    size = sizeof(struct manifold);
    for (i=0; i < NUM_GROUP; i++)
      {
        groups[i] = *group_ptr++;
        chksum = crcgen((char *)&groups[i],size-4,0);
        if (chksum != 0)
          {
            groups[i].enable = 0;
            groups[i].status = CHKSUM_ERR;
            sprintf(error_msg,
             "M0  Group %i disabled due to checksum error at lnfxx startup",i);
            host_message(PANIC,error_msg);
          }
        else  groups[i].status &= ~(FILL_IN_PROGRESS | FILLING);
      }
/*
*    Restore the detector database.  On a checksum error disable the detector
*    and inform the host.
*/
    size = sizeof(struct detector);
    det_ptr = (struct detector *)LN_DETECTOR;
    for (i=0; i < NUM_DETECTOR; i++)
      {
        detectors[i] = *det_ptr++;
        chksum = crcgen((char *)&detectors[i],size-4,0);
        if (chksum != 0)
          {
            detectors[i].enable = -1;
            detectors[i].status = CHKSUM_ERR;
            sprintf(error_msg,
            "M0  Detector %i disabled due to checksum error at lnfxx startup",
                                                                            i);
            host_message(PANIC,error_msg);
            det_hv_off(i);
          }
        else  detectors[i].status &= ~(FILL_IN_PROGRESS | FILLING);
      }
/*
*  Shutdown HV for all detectors in any group which has been disabled
*  due to a checksum error.
*/
    for (i=0; i < NUM_GROUP; i++)
      {
        if (groups[i].status & CHKSUM_ERR) group_hv_off(i);
      }
/*
*  Restore HV check status array.  On a checksum error enable temp
*  checking for ALL detectors.
*/
    size = sizeof(char) * NUM_DETECTOR;
    hv_ptr = (char *)LN_HV_STAT;
    chksum = crcgen(hv_ptr,size,0);
    if (chksum != 0)
      {
        for (i=0; i < NUM_DETECTOR; i++) hv_status[i] = 0;
        backup_hv_stat();
      }
    else
      {
        for(i=0; i < NUM_DETECTOR; i++) hv_status[i] = *hv_ptr++;
      }
/*
*  Restore room temperature alarm set points.  On a checksum error,
*  use the DEFAULT settings.
*/
    size = sizeof(struct room_temp);
    room_ptr = (struct room_temp *)LN_ROOM_TEMP;
    chksum = crcgen((char *)room_ptr,size-4,0);
    if (chksum != 0)
      {
        room.max = MAX_ROOM;
        room.min = MIN_ROOM;
      }
    else
      {
        room.max = room_ptr->max;
        room.min = room_ptr->min;
      }
    return(0);
}
/****************************************************************************
*
*  Send a message to the Host.  If enabled, also output the message
*  to the local terminal attached to the VME processor.
*  These messages are sent to the host which booted the VME system.
****************************************************************************/
void host_message(int type,char *msg)
{
  int  status,level;
  time_t  tod;
  static struct VMEmsg *host_msg;
  static int  eno;
  static struct Ether_Packet out_pkt;

#ifdef  HOST_MSG
  if(!eno)
    {
      eno = open("ln1",3);
      if (eno <= 0)
        {
          printf("Can't open device 'ln'\n");
          exit(1001);
        }
      ioctl(eno,EIOPHYSADR,out_pkt.Source);  /* Put our physical address in
                                                the packet header           */
      memcpy((char *)out_pkt.Destination,*host_ether_address,6);
      out_pkt.Order = 0;
      out_pkt.Protocol[0] = PROTO_PREFIX;
      out_pkt.Protocol[1] = PROTO_FEMSG;
      out_pkt.Ack = NOACK;
      host_msg = (struct VMEmsg *)out_pkt.Data;
      strcpy(host_msg->sender,"LN_FILL ");
    }
   host_msg->type = type;
   strcpy(host_msg->text,msg);
   status = write(eno,(char *)&out_pkt,sizeof(struct VMEmsg) + ORPH_HDR_LEN);
   if (status < 0)
     {
       printf("Write failure on device 'ln'\n");
       exit(1003);
     }
   level = set_intr_level_(0);
   delay_(2);
   set_intr_level_(level);
#endif

#ifdef  LOCAL_MSG
   time(&tod);
   strftime(ascii_time,20,"%d-%b-%y %H:%M:%S  ",localtime(&tod));
   printf("%s%s\n",ascii_time,msg);
#endif
}
/****************************************************************************
*
*   Set group parameters from the host workstation
*
****************************************************************************/
int set_group(int group_indx,struct manifold *group_param)
{
    int  i,*inptr,*outptr,size;
  
    if (!hardware_ok) return(LN_ERR_HARDWARE);
    if (group_indx < 0 || group_indx >= NUM_GROUP) return(LN_ERR_INVALGRP);
    if (groups[group_indx].status & FILL_IN_PROGRESS) return(LN_ERR_GRP_BUSY);
    size = (sizeof(struct manifold));
    byte_swap_((char *)group_param,size);
    word_swap_((short *)group_param,size/2);
    size = size/4;
    inptr = (int *)group_param;
    outptr = (int *)&groups[group_indx];
    for (i = 0; i < size; i++)
      {
/*
*  A parameter value of -2 means don't modify the current value
*/
        if (*inptr != -2) *outptr = *inptr;
        outptr++;
        inptr++;
      }
    groups[group_indx].status &= ~(FILL_IN_PROGRESS | FILLING);
    if (groups[group_indx].enable != 1) groups[group_indx].next_date = 0;
/*
*   Recompute manifold cool times.
*/
    schedule(0);
    sprintf(error_msg,"M5  Group %i parameters set by host",group_indx);
    host_message(INFORM,error_msg);
    backup_group(group_indx);
    check_det_temp();
    return(0);
}
/****************************************************************************
*
*   Get group parameters for the host workstation
*
****************************************************************************/
int get_group(int group_indx,struct manifold *group_param)
{
    int size;

    if (!hardware_ok) return(LN_ERR_HARDWARE);
    if (group_indx < 0 || group_indx >= NUM_GROUP) return(LN_ERR_INVALGRP);
    size = (sizeof(struct manifold));
    *group_param = groups[group_indx];
    byte_swap_((char *)group_param,size);
    word_swap_((short *)group_param,size/2);
    return(0);
}
/****************************************************************************
*
*   Compute the checksum and save one group in Battery backed RAM
*
****************************************************************************/
void backup_group(int group_indx)
{
    int i,size;
    struct manifold *group_ptr;

    size = sizeof(struct manifold);
    crcgen((char *)&groups[group_indx],size-4,1);
    group_ptr = (struct manifold *)LN_GROUP;
    group_ptr += group_indx;
    *group_ptr = groups[group_indx];
}
/*****************************************************************************
*
*  Routine generates/checks a 24 bit CRC using the polynomial defined
*  by POLY.
*
*  call arguments:  cptr  - character pointer to start of data block
*                   num_bytes - number of data bytes excluding the CRC
*                   genflag  - 1 means generate CRC and store in the
*                              4 bytes following the data block. NOTE:
*                              the CRC is only 24 bits, so the 4th byte
*                              is always zero.
*
*                            - 0 means check the CRC.
*
*  Routine returns the CRC.  If generating a CRC, it is the calculated
*  CRC.  If checking a data block, the return should be zero for OK and
*  nonzero for ERROR.
*****************************************************************************/

#define  POLY  0xdf3261

int crcgen(char *cptr,int num_bytes,int genflag)
{
   char chr;
   int  crc = 0xffffff, i,j,test;

/*
*  Calculate CRC for the data block.  num_bytes is the number of
*  bytes in data block. NOTE: num_bytes DOES NOT include the CRC
*  which is stored in the 4 bytes following the data.
*/
   for (j=0; j < num_bytes; j++)
    {
      chr = *cptr++;
      for (i=0; i < 8; ++i)
        {
          test = (crc ^ chr)%2;
          crc = crc >> 1;
          if (test) crc ^= POLY;
          chr = chr >> 1;
        }
     }
   if (genflag) 
     {
/*
*  WE should generate the CRC and store it in the 4 bytes following
*  the data block.
*
*  Since the CRC is only 24 bits, the 4th byte is always set to zero
*/
       test = crc;
       *cptr++ = crc;
       crc = crc /256;
       *cptr++ = crc;
       crc = crc /256;
       *cptr++ = crc;
       *cptr = 0;
       return(test);
     }
   else
     {
/*
*  WE are asked to check the data.  So include the 24 bits of CRC
*  in the calculation.  Return the result which should be zero
*  if there is no error.
*/
       for (j=0; j < 3; j++)
        {
          chr = *cptr++;
          for (i=0; i < 8; ++i)
            {
              test = (crc ^ chr)%2;
              crc = crc >> 1;
              if (test) crc ^= POLY;
              chr = chr >> 1;
            }
         }
       return(crc);
     }
}
/****************************************************************************
*
*   Set detector params
*
****************************************************************************/
int set_detector(int det_indx,struct detector *det_param)
{
    int  i,*inptr,*outptr,size;
  
    if (!hardware_ok) return(LN_ERR_HARDWARE);
    if (det_indx < 0 || det_indx >= NUM_DETECTOR) return(LN_ERR_INVALDET);
    if (detectors[det_indx].status & FILL_IN_PROGRESS) return(LN_ERR_DET_BUSY);
    size = (sizeof(struct detector));
    byte_swap_((char *)det_param,size);
    word_swap_((short *)det_param,size/2);
    size = size/4;
    inptr = (int *)det_param;
    outptr = (int *)&detectors[det_indx];
    for (i = 0; i < size; i++)
      {
/*
*  A parameter value of -2 means don't modify the current value
*/
        if (*inptr != -2) *outptr = *inptr;
        outptr++;
        inptr++;
      }
    detectors[det_indx].status &= ~(FILL_IN_PROGRESS | FILLING);
    backup_detector(det_indx);
    if (detectors[det_indx].enable >= 1)
      {
        hv_status[det_indx] = 0;
        backup_hv_stat();
      }
    schedule(0);
    sprintf(error_msg,"M6  Detector %i parameters set by host",det_indx);
    host_message(INFORM,error_msg);
    return(0);
}
/****************************************************************************
*
*   Get detector parameters for the host workstation
*
****************************************************************************/
int get_detector(int det_indx,struct detector *det_param)
{
    int size;

    if (!hardware_ok) return(LN_ERR_HARDWARE);
    if (det_indx < 0 || det_indx >= NUM_DETECTOR) return(LN_ERR_INVALDET);
    size = (sizeof(struct detector));
    *det_param = detectors[det_indx];
    byte_swap_((char *)det_param,size);
    word_swap_((short *)det_param,size/2);
    return(0);
}
/****************************************************************************
*
*   Compute the checksum and save one detector in Battery backed RAM
*
****************************************************************************/
void backup_detector(int det_indx)
{
    int  i,size;
    struct detector *det_ptr;

    size = sizeof(struct detector);
    crcgen((char *)&detectors[det_indx],size-4,1);
    det_ptr = (struct detector *)LN_DETECTOR;
    det_ptr += det_indx;
    *det_ptr = detectors[det_indx];
}
/****************************************************************************
*  Send temperature data to the host workstation.
****************************************************************************/
int  get_temp(struct temperature *rply)
{
    int i,size;

    if (!hardware_ok) return(LN_ERR_HARDWARE);
    size = sizeof(rply->data)/sizeof(short);
    for (i=0; i < size; i++) rply->data[i] = temp[i];
    word_swap_(rply->data,size);
    byte_swap_(rply->data,size*2);
    return(0);
}
/****************************************************************************
*  Send group and detector status words to the host workstation.
****************************************************************************/
int  get_status(struct status *rply)
{
    int i,size;

    if (!hardware_ok) return(LN_ERR_HARDWARE);
    for (i=0; i < NUM_GROUP; i++) rply->gstat[i] = groups[i].status;
    for (i=0; i < NUM_DETECTOR; i++) rply->dstat[i] = detectors[i].status;
    size = (sizeof(rply->gstat) + sizeof(rply->dstat))/sizeof(short);
    word_swap_(rply->gstat,size);
    byte_swap_(rply->gstat,size*2);
    return(0);
}
/****************************************************************************
*  Set group and detector status words for those which are not
*  busy filling.  Returns error code if any group is busy filling.
****************************************************************************/
int  set_status(struct status *cmd)
{
    int i,size,err = 0;

    if (!hardware_ok) return(LN_ERR_HARDWARE);
    size = (sizeof(cmd->gstat) + sizeof(cmd->dstat))/sizeof(short);
    word_swap_(cmd->gstat,size);
    byte_swap_(cmd->gstat,size*2);
    for (i=0; i < NUM_GROUP; i++)
      {
        if (groups[i].status & FILL_IN_PROGRESS) err = LN_ERR_GRP_BUSY;
        else
          {
            groups[i].status = cmd->gstat[i] & ~(FILL_IN_PROGRESS | FILLING);
            backup_group(i);
          }
      }
    for (i=0; i < NUM_DETECTOR; i++)
      {
        if (detectors[i].status & FILL_IN_PROGRESS) err = LN_ERR_DET_BUSY;
        else
          {
            detectors[i].status = cmd->dstat[i]
                                               & ~(FILL_IN_PROGRESS | FILLING);
            backup_detector(i);
          }
      }
    sprintf(error_msg,"M7  Manifold/detector status set by host");
    host_message(INFORM,error_msg);
    return(err);
}
/****************************************************************************
*  Send the hardware status word to the host workstation.
****************************************************************************/
int  get_hardware(struct hardware_stat *rply)
{
    rply->status = hardware_status;
    word_swap_(&rply->status,2);
    byte_swap_(&rply->status,4);
    return(0);
}
/****************************************************************************
*  Send the VME time to the host workstation.
****************************************************************************/
int  get_vme_time(struct vme_time *rply)
{
    time_t  tod;

    time(&tod);
    rply->time = tod;
    word_swap_(&rply->time,2);
    byte_swap_(&rply->time,4);
    return(0);
}
/****************************************************************************
*  Send high voltage status data to the host workstation.
****************************************************************************/
int  get_hvstat(struct hv_stat *rply)
{
    int i,size;

    for (i=0; i < NUM_DETECTOR; i++) rply->data[i] = hv_status[i];
    size = sizeof(rply->data)/sizeof(short);
    word_swap_(rply->data,size);
    byte_swap_(rply->data,size*2);
    return(0);
}
/****************************************************************************
*  Set high voltage status array from workstation.
****************************************************************************/
int  set_hvstat(struct hv_stat *cmd)
{
    int i,size;

    size = sizeof(cmd->data)/sizeof(short);
    word_swap_(cmd->data,size);
    byte_swap_(cmd->data,size*2);
/************
************/
    for (i=0; i < NUM_DETECTOR; i++)
      {
        if (cmd->data[i] != -2) hv_status[i] = cmd->data[i];
      }
    backup_hv_stat();
    sprintf(error_msg,"M24 HV status set by host");
    host_message(INFORM,error_msg);
    return(0);
}
/****************************************************************************
*  Determine which detectors in the specified group should be filled.
*
*  To be filled, a detector must:
*
*    1) be enabled
*    2) if last_date = 0, we always fill
*    3) if last != 0 then
*        a) time since last fill must be greater or equal to min_interval.
*        b) and time since last fill must be less than max_interval.
*
*  If time since last fill exceeds max_interval, disable this detector and
*  send message to host.
****************************************************************************/
int select_det(int grp_indx)
{
    int  i,*det,num = 0;
    int  min,norm,max;
    time_t tod,time_since_last;

    for (i=0; i < DET_GRP; i++) det_to_fill[i] = -1;

    time(&tod);
    det = groups[grp_indx].det;
    for (i=0; i < DET_GRP; i++)
      {
        if ((det[i] < 0) || (det[i] >= NUM_DETECTOR)) continue;
        if (detectors[det[i]].enable < 1) continue;
/*
*  If last_date is zero, bypass checking the max and min fill intervals.
*  This means always fill.
*/
        time_since_last = tod - detectors[det[i]].last_date;
        if (time_since_last != tod)
          {
            get_fill_intervals(det[i],&min,&norm,&max);
            if (time_since_last < min) continue;
            if (time_since_last > max)
              {
                detectors[det[i]].status |= FILL_INTERVAL_LONG;
                detectors[det[i]].enable = 0;
                sprintf(error_msg,"M8  Max interval exceeded for detector %i.\
 Fill Disabled",det[i]);
                host_message(INFORM,error_msg);
                continue;
              }
          }
        det_to_fill[i] = det[i];
        num++;
      }
    return(num);
}
/****************************************************************************
*  First routine of a fill cycle.  Mark all enabled detectors in this group
*  as busy.  Mark the group busy.  Open the manifold valve to start cooling
*  the manifold.
****************************************************************************/
int open_manifold(int grp_indx)
{
    int  i,*det;

/*
*   Find which detectors of this group should be filled.
*   If none of the detectors in this group are to be filled, set next_date
*   to maximum - approx Jan 18, 2038.
*/
    if (select_det(grp_indx) == 0)
      {
/*
*  No detectors are to be filled.  This can result due to changes
*  made in the detector data.  Send message to host and abort the
*  fill cycle.
*/
        sprintf(error_msg,"M9  No detectors ready for fill in group %i",
                                                                    grp_indx);
        host_message(INFORM,error_msg);
        schedule(0);
        return(STOP_FILL);
      }
/*
*  Mark this group and all detectors in this group busy.
*/
    det = groups[grp_indx].det;
    for (i = 0; i < DET_GRP; i++)
      {
        if (det[i] < 0 || det[i] >= NUM_DETECTOR) continue;
        detectors[det[i]].status |= FILL_IN_PROGRESS;
      }   
    groups[grp_indx].status |= (FILL_IN_PROGRESS | FILLING);
/*
*   Open the manifold solenoid valve and tell the host about it.
*/
    acro_write(groups[grp_indx].valve,1);
    sprintf(error_msg,"M10 Cool Manifold for Group %i",grp_indx);
    host_message(INFORM,error_msg);
/*
*   Open the tank solenoid valve.
*/
    acro_write(TANK_VALVE,1);
    sprintf(error_msg,"M26 Open Tank Valve");
    host_message(INFORM,error_msg);
/*
*   Save date and time so we measure and check the cool time.
*/
    time(&start_time);
    cycle_time = start_time - 10;
    return(PRESS_CHK);
}
/****************************************************************************
*  Second routine of a fill cycle.   Wait 5 seconds and then check
*  the pressure.  If out of limits, disable this manifold.
****************************************************************************/
int press_check(int grp_indx)
{
    time_t tod;
    int  i,*det;
    float  pressure;

    time(&tod);
    if (tod < (start_time + 5)) return(0);
    pressure = press[0];
    pressure = (pressure - 812.0)/32.768;
    if (press[0] < MIN_PRESS || press[0] > MAX_PRESS)
      {
/*
*   If the tank pressure is out of linits, we abort the fill cycle for
*   this manifold and disable this manifold for future filling.
*/
        acro_write(TANK_VALVE,0);
        acro_write(groups[grp_indx].valve,0);
        groups[grp_indx].status |= LINE_PRESS;
        groups[grp_indx].enable = 0;
        det = groups[grp_indx].det;
        for (i = 0; i < DET_GRP; i++)
          {
            if (det[i] < 0 || det[i] >= NUM_DETECTOR) continue;
            detectors[det[i]].status &= ~FILL_IN_PROGRESS;
          }   
        groups[grp_indx].status &= ~(FILL_IN_PROGRESS | FILLING);
        schedule(0);
        if (press[0] < MIN_PRESS)
          {
            sprintf(error_msg,"M25 Line Pressure LOW: %4.1f psig. Group %i\
 disabled",
                                                             pressure,grp_indx);
          }
        else
          {
            sprintf(error_msg,"M25 Line Pressure HIGH: %4.1f psig. Group %i\
 disabled",
                                                             pressure,grp_indx);
          }
        host_message(PANIC,error_msg);

        return(STOP_FILL);
      }

    sprintf(error_msg,"M27 Line Pressure is %4.1f psig.",pressure);
    host_message(INFORM,error_msg);

    return(COOL_MAN);
}
/****************************************************************************
*  Third routine of a fill cycle.   Keep the manifold valve open for at least
*  the min_cool_time parameter and no longer than the max_cool_time parameter.
*  The overflow sensor can close the valve at some time between min_cool_time
*  and max_cool_time.  Set status status bits in the group status if the
*  the valve close was due to min or max cool_time.
****************************************************************************/
int cool_manifold(int grp_indx)
{
    int  *det,i;
    time_t tod,fill_time,min_time;

    time(&tod);
    if (tod < start_time + groups[grp_indx].min_cool_time) return(0);
    if (tod >= start_time + groups[grp_indx].max_cool_time)
      {
        groups[grp_indx].status |= FILL_TIME_LONG;
        tank_problem++;
      }
    else
      {
        if (groups[grp_indx].sensor > groups[grp_indx].sensor_on) return(0);
        tank_problem = 0;
      }
/*
*   Close manifold solenoid valve and record the actual cool time.
*/
    acro_write(groups[grp_indx].valve,0);
    fill_time = tod - start_time;
    groups[grp_indx].last_time = fill_time;
    min_time = groups[grp_indx].min_cool_time;
    if (fill_time <= (min_time + 1))groups[grp_indx].status |= FILL_TIME_SHORT;
    groups[grp_indx].status &= ~FILLING;
    sprintf(error_msg,"M11 Man #%i Cool time - %i secs",grp_indx,fill_time);
    host_message(INFORM,error_msg);
/*
*   If manifold cool time was too long, something is wrong! Best guess is
*   that LN supply tank is empty or pressure is low.  So we abort the
*   fill cycle for this manifold and disable this manifold for future filling.
*/
    if (groups[grp_indx].status & FILL_TIME_LONG)
      {
        groups[grp_indx].enable = 0;
        det = groups[grp_indx].det;
        for (i = 0; i < DET_GRP; i++)
          {
            if (det[i] < 0 || det[i] >= NUM_DETECTOR) continue;
            detectors[det[i]].status &= ~FILL_IN_PROGRESS;
          }   
        groups[grp_indx].status &= ~(FILL_IN_PROGRESS | FILLING);
        schedule(0);
        sprintf(error_msg,"M12 Fill Cycle for group %i aborted",grp_indx);
        host_message(PANIC,error_msg);

        vent_time = tod;
        return(VENT);
      }
    return(OPEN_DET);
}
/****************************************************************************
*  Fourth routine of a fill cycle.  Now that the manifold is cool, we open
*  the valves for all enabled detectors in this group.  Also set a bit
*  in each detector status to indicate that the detector is filling.
****************************************************************************/
int open_detectors(int grp_indx)
{
    int  i;
    time_t tod;

    time(&tod);
    for (i=0; i < DET_GRP; i++)
      {
        short_fill[i] = 0;
        start_overflow[i] = -1;
        if (det_to_fill[i] < 0) continue;
/*
*  Open valve, set status and tell host.
*/
        acro_write(detectors[det_to_fill[i]].valve,1);
        detectors[det_to_fill[i]].status |= FILLING;
        sprintf(error_msg,"M13 Start Filling Detector %i",det_to_fill[i]);
        host_message(INFORM,error_msg);
      }
/*
*   Save date and time when we started filling the detectors.
*/
    time(&start_time);
    return(FILL_DET);
}
/****************************************************************************
*  Fifth routine of a fill cycle.  Each detector in the group is filled for
*  at least the min_fill_time and no longer than the max_fill_time.  Normally
*  the filling is stopped by the overflow sensor.  However, if the filling
*  is stopped by the min or max fill_time, bits are set in the detector
*  status to show what happened.
****************************************************************************/
int fill_detectors(int grp_indx)
{
    int    *det,i,done;
    int    min,norm,max;
    static time_t tod,fill_time;

    time(&tod);
/*
*    Test each enabled detector in this group for following conditions:
*      1) overflow sensor temperature.  If temperature low enough, 
*         check to see that min_fill_time has been exceeded.  If not,
*         set status bit and continue until we reach min_fill_time.
*         Close LN valve and update fill time.
*      2) If we exceed max_fill_time close LN valve and set status bit.
*/
    done = 1;
    det = groups[grp_indx].det;
    for (i=0; i < DET_GRP; i++)
      {
        if (det[i] < 0) continue;
        if (detectors[det[i]].enable < 1) continue;
        if (detectors[det[i]].status & FILLING)
          {
            if (detectors[det[i]].sensor < detectors[det[i]].sensor_on)
              {
                if (start_overflow[i] == -1) start_overflow[i] = tod;
                if (tod < (start_overflow[i]+5))
                  {
                    done = 0;
                    continue;
                  }
/*
*  Overflow sensor says it is time to close valve.  However, we always
*  fill for at least the min_fill_time.
*/
                if (tod >= start_time + detectors[det[i]].min_fill_time)
                  {
/*
*  Close valve.  Normal or min_fill_time cycle.
*/
                    acro_write(detectors[det[i]].valve,0);
                    detectors[det[i]].status &= ~FILLING;
                    fill_time = tod - start_time;
                    detectors[det[i]].last_time = fill_time;
                    detectors[det[i]].last_date = start_time;
                    get_fill_intervals(det[i],&min,&norm,&max);
                    detectors[det[i]].next_date = cycle_time + norm;
                    if (detectors[det[i]].enable > 2)
                                                 detectors[det[i]].enable -= 1;
                    if (short_fill[i] == 0)
                      {
                        sprintf(error_msg,"M14 Det #%i Fill time - %i secs",
                                                             det[i],fill_time);
                        host_message(INFORM,error_msg);
                      }
                  }
                else
                  {
                    done = 0;
                    if (short_fill[i] == 0)
                      {
                        short_fill[i] = 1;
                        detectors[det[i]].status |= FILL_TIME_SHORT;
                        fill_time = tod - start_time;
                        sprintf(error_msg,
                                      "M15 Det #%i Short fill time - %i secs",
                                                             det[i],fill_time);
                        host_message(INFORM,error_msg);
                      }
                  }
              }
            else if (tod >= start_time + detectors[det[i]].max_fill_time)
              {
/*
*  Have reached max_fill_time.  Close valve and set status and fill_time.
*/
                acro_write(detectors[det[i]].valve,0);
                detectors[det[i]].status &= ~FILLING;
                detectors[det[i]].status |= FILL_TIME_LONG;
                fill_time = tod - start_time;
                detectors[det[i]].last_time = fill_time;
                detectors[det[i]].last_date = start_time;
                get_fill_intervals(det[i],&min,&norm,&max);
                detectors[det[i]].next_date = cycle_time + norm;
                if (detectors[det[i]].enable > 2) detectors[det[i]].enable -= 1;
                sprintf(error_msg,"M16 Det #%i Long fill time - %i secs",
                                                             det[i],fill_time);
                host_message(INFORM,error_msg);
              }
            else
              {
                done = 0;
                start_overflow[i] = -1;
              }
          }
      }
/*
*   Must run this routine until all detectors have finished.
*/
    if (!done) return(0);
/*
*   Tell the host we have finished filling this group.
*/
    sprintf(error_msg,"M17 End fill cycle for Group %i",grp_indx);
    host_message(INFORM,error_msg);
    return(SCHEDULE);
}
/****************************************************************************
*  Sixth routine for a fill cycle.  This routine computes the next fill
*  time for ALL detector groups.  Routine may also be called when the host
*  loads new parameters for a group.
****************************************************************************/
int schedule(int grp_indx)
{
    time_t tod,fill,tmp_time;
    int  det,j,k;
    int  min,norm,max;

    time(&tod);
    for (j=0; j < NUM_GROUP; j++)
      {  
        fill = 0x7fffffff;     /* max UNIX time which is approx Jan 18, 2038 */
        if (groups[j].enable == 1)
          {
            for (k=0; k < DET_GRP; k++)
              {
                det = groups[j].det[k];
                if (det < 0) continue;
                if (detectors[det].enable >= 1)
                  {
/*
*  If next_date is less than current date and time, schedule the
*  next fill for this manifold immediately.  Fill time for a detector
*  is the lastest of next_date and (last_date + min_interval + 100 seconds).
*/
                    get_fill_intervals(det,&min,&norm,&max);
                    tmp_time = detectors[det].last_date + min + 100;
                    if (detectors[det].next_date > tmp_time)
                                           tmp_time = detectors[det].next_date;
                    if (tmp_time < tod) tmp_time = tod;
                    if (fill > tmp_time) fill = tmp_time;
                    if (detectors[det].next_date < tmp_time)
                                           detectors[det].next_date = tmp_time;
                  }
              }
            groups[j].next_date = fill;
            backup_group(j);
          }
      }
    return(FIN_FILL);
}
/****************************************************************************
*  Seventh routine for a fill cycle.  Reset all busy flags and tell the host
*  when this group is to fill next.
****************************************************************************/
int finish_fill(int grp_indx)
{
    time_t  tod;
    int *det,i;

/*
*   Tell the host when we are due to fill again for this group.
*/
    tod = groups[grp_indx].next_date & 0x7fffffff;
    if (tod < 18000) tod = 18000;
    strftime(ascii_time,20,"%d-%b-%y %H:%M:%S  ",localtime(&tod));
    sprintf(error_msg,"M22 Next fill for Group %i is at %s",
                                                         grp_indx,ascii_time);
    host_message(INFORM,error_msg);
/*
*  Reset the busy flag for each detector and the group
*/
    det = groups[grp_indx].det;
    for (i = 0; i < DET_GRP; i++)
      {
        if (det[i] >= 0)
          {
            detectors[det[i]].status &= ~(FILL_IN_PROGRESS | FILLING);
            backup_detector(det[i]);
          }
      }   
    groups[grp_indx].status &= ~(FILL_IN_PROGRESS | FILLING);
    backup_group(grp_indx);

    time(&tod);
    vent_time = tod;
    return(VENT);
}
/****************************************************************************
*  Eighth routine of fill cycle.  Wait about 3 minutes before we close
*  the tank valve.
****************************************************************************/
int vent_line(int grp_indx)
{
       int  i,done=0;
    time_t  tod;

    time(&tod);
    for (i=0; i < NUM_GROUP; i++)
      {
        if (groups[i].enable <= 0) continue;
        if (groups[i].next_date <= tod) done = 1;
      }
    if (done) ;
    else if (press[0] < MIN_PRESS) ;
    else if (tod < (vent_time + VENT_TIME)) return(0);
    vent_time = 0;
/*
*  Close tank solenoid valve.
*/
    acro_write(TANK_VALVE,0);

    sprintf(error_msg,"M26 Close Tank Valve");
    host_message(INFORM,error_msg);
    return(STOP_FILL);
}
/****************************************************************************
****************************************************************************/
void check_fill_interval(void)
{
    int  i;
    int  min,norm,max;
    time_t tod,time_since_last;

    time(&tod);
    for (i=0; i < NUM_DETECTOR; i++)
      {
        if (detectors[i].enable < 1) continue;
/*
*  If last_date is zero, bypass checking the max fill interval.
*/
        time_since_last = tod - detectors[i].last_date;
        if (time_since_last != tod)
          {
            get_fill_intervals(i,&min,&norm,&max);
            if (time_since_last > max)
              {
                detectors[i].status |= FILL_INTERVAL_LONG;
                detectors[i].enable = 0;
                sprintf(error_msg,"M8  Max interval exceeded for detector %i.\
 Fill Disabled",i);
                host_message(INFORM,error_msg);
              }
          }
      }
    return;
}
/****************************************************************************
****************************************************************************/
void get_fill_intervals(int indx,int *min,int *norm,int *max)
{

#define  COOLDWN  5

   int i;
/*
*   Fill schedule for start up of a warm detector.
*/
   static struct {
                int min_interval;
                int fill_interval;
                int max_interval;
} cool_down[COOLDWN] = { 14400, 28800, 28800,   /* fifth fill  - 8 hr.       */
                         10000, 21600, 12600,   /* fourth fill - 6 hr.       */
                          4800, 10800,  7200,   /* third fill  - 3 hr.       */
                           840,  5400,  2700,   /* second fill - 1.5 hr.     */
                             0,  1800,     0};  /* first fill  - 0.5 hr.     */

    i = detectors[indx].enable;
    if (i <= 2)
      {
        *min = detectors[indx].min_interval;
        *norm = detectors[indx].fill_interval;
        *max = detectors[indx].max_interval;
      }
    else
      {
        i = i - 3;
        if (i >= COOLDWN)
          {
            i = COOLDWN-1;
            detectors[indx].enable = i + 3;
          }
        *min = cool_down[i].min_interval;
        *norm = cool_down[i].fill_interval;
        *max = cool_down[i].max_interval;
      }
}
/****************************************************************************
*   ACROMAG initialization
************************** **************************************************/
void acro_init(void)
{
    if (!devtbl->acromag) return;
    avme->csr = AVME_GRN_ON | AVME_RED_OFF;
    avme->intr_vec0 = 0;
    avme->intr_vec1 = 0;
    avme->intr_vec2 = 0;
    avme->intr_vec3 = 0;
    avme->intr_vec4 = 0;
    avme->intr_vec5 = 0;
    avme->intr_vec6 = 0;
    avme->intr_vec7 = 0;
}
/****************************************************************************
*   ACROMAG Digital I/O
****************************************************************************/
int acro_write(int bitnum,int val)
{
    unsigned char mask = 1;
    volatile unsigned char *port;

    if (!devtbl->acromag) return (-1);
    if (bitnum < 0 || bitnum >= NUM_VALVE) return(-2);
/*
*       Write a bit in the I/O register
*/
    port = &(avme->port0) + bitnum/8;
    mask = mask << (bitnum % 8);
    if (val == 0)
      {
        mask = ~mask;
        *port = *port & mask;
      }
    else
      {
        *port = *port | mask;
      }
    return (0);
}
/****************************************************************************
*   Relay Power check   
****************************************************************************/
int relay_check(void)
{
    int err;

    if (!devtbl->acromag) return (-1);
/*
*  If relay power supply is off, all 4 ports should read 0xff.
*/
    err = avme->port0;
    err += avme->port1;
    err += avme->port2;
    err += avme->port3;
    if (err == (0xff * 4))
      {
        sprintf(error_msg,"M0  Relay Power Supply Failure.");
        host_message(INFORM,error_msg);
        return(1);
      }
    return(0);
}
/****************************************************************************
*   Update temperature sensor data in group and detector structures.
****************************************************************************/
void update_temp(void)
{
    int i,j;

/*
*  Update temperature sensor data in group and detector data
*/
    for (i=0; i < NUM_GROUP; i++)
      {
        j = groups[i].sensor_chan;
        if (j < 0 || j >= NUM_TEMP) continue;
        groups[i].sensor = temp[j];
      }
    for (i=0; i < NUM_DETECTOR; i++)
      {
        j = detectors[i].temp_chan;
        if (j >= 0 && j < NUM_TEMP) detectors[i].temp = temp[j];
        j = detectors[i].sensor_chan;
        if (j >= 0 && j < NUM_TEMP) detectors[i].sensor = temp[j];
      }
}
/****************************************************************************
*   Check detector temperatures.  There are a number of cases for which
*   we DO NOT check detector temperatures:
*
*   1) The detector enable is negative.  This means this detector is
*      not present or available for filling.
*
*   2) A fill cycle for this detector is in progress.
*
*
*   3) We have already turned off the HV and disabled this detector
*      for future fills.  This condition can only be reset by the
*      operator.
****************************************************************************/
void check_det_temp(void)
{
    int i,j;

/*
*   Check for over temperature 
*/
    for (i=0; i < NUM_DETECTOR; i++)
      {
        if (detectors[i].enable < 0) continue;
        if (detectors[i].enable > 2) continue;
        if (detectors[i].status & FILL_IN_PROGRESS) continue;
        if (hv_status[i]) continue;
        j = detectors[i].temp_chan;
        if (j >= 0 && j < NUM_TEMP)
          {
            if (detectors[i].enable == 2)
              {
/*
*   When measured detector temperature is more than 4 degrees below the
*   max_temp, enable temperature checking.
*/
                if (detectors[i].temp < (detectors[i].max_temp - 33))
                  {
                    detectors[i].enable = 1;
                    backup_detector(i);
                  }
              }
            else if (detectors[i].temp > detectors[i].max_temp)
              {
                detectors[i].enable = 0;
                detectors[i].status |= TEMP_HIGH;
                hv_status[i] = 1;
                backup_hv_stat();
                sprintf(error_msg,
                            "M18 Detector %i temperature high.  HV shutdown",i);
                host_message(INFORM,error_msg);
                det_hv_off(i);
                if (detectors[i].temp > MAX_RTD)
                                                detectors[i].status |= TEMP_RTD;
                backup_detector(i);
              }
            else if (detectors[i].temp < MIN_RTD)
              {
                detectors[i].enable = 0;
                detectors[i].status |= TEMP_RTD;
                hv_status[i] = 1;
                backup_hv_stat();
                sprintf(error_msg,
                    "M21 Temp RTD for detector %i out_of_range. HV Shutdown",i);
                host_message(INFORM,error_msg);
                det_hv_off(i);
                backup_detector(i);
              }
          }
        else if (detectors[i].enable == 2)
          {
            detectors[i].enable = 1;
            backup_detector(i);
          }
      }
}
/****************************************************************************
*   Check overflow and temperature sensor for measurement within
*   proper range.  Should find shorted or open RTD connections.
****************************************************************************/
void check_rtd(void)
{
    int i,rtd;

/*
*   Check overflow sensor for all enabled manifolds.
*/
    for (i=0; i < NUM_GROUP; i++)
      {
        if (groups[i].enable != 1) continue;
        rtd = groups[i].sensor_chan;
        if (rtd < 0 || rtd >= NUM_TEMP) continue;
        if (temp[rtd] < MIN_RTD || temp[rtd] > MAX_RTD)
          {
            groups[i].status |= OVERFLOW_RTD;
            sprintf(error_msg,
                            "M19 Overflow RTD for manifold %i out_of_range",i);
            host_message(INFORM,error_msg);
          }
      }
/*
*   Check detector sensors for all enabled detectors.
*/
    for (i=0; i < NUM_DETECTOR; i++)
      {
        if (detectors[i].enable < 1) continue;
        rtd = detectors[i].sensor_chan;
        if (rtd >= 0 && rtd < NUM_TEMP)
          {
            if (temp[rtd] < MIN_RTD || temp[rtd] > MAX_RTD)
              {
                detectors[i].status |= OVERFLOW_RTD;
                sprintf(error_msg,
                            "M20 Overflow RTD for detector %i out_of_range",i);
                host_message(INFORM,error_msg);
              }
          }
        rtd = detectors[i].temp_chan;
        if (rtd >= 0 && rtd < NUM_TEMP)
          {
            if (temp[rtd] < MIN_RTD || temp[rtd] > MAX_RTD)
              {
                detectors[i].status |= TEMP_RTD;
                sprintf(error_msg,
                                "M21 Temp RTD for detector %i out_of_range",i);
                host_message(INFORM,error_msg);
              }
          }
      }
}
/****************************************************************************
*    Turn off HV for all detectors associated with a manifold
****************************************************************************/
void group_hv_off(int grp_indx)
{
    int  i,j;

    for (i=0; i < DET_GRP; i++)
      {
        j = groups[grp_indx].det[i];
        if (j < 0 || j >= NUM_DETECTOR) continue;
        det_hv_off(j);
      }
}
/****************************************************************************
*    Queue request to turn off all HV for a detector.  The actual turn off
*    of the HV supplies is done in the 'main' routine.
****************************************************************************/
void det_hv_off(int det_indx)
{
    int  i,j,k;

    if (det_indx < 0 || det_indx >= NUM_DETECTOR) return;;
    for (i=0; i < DET_HV; i++)
      {
        j = detectors[det_indx].volt_chan[i];
        if (j < 0 || j >= NUM_HV) continue;
        detectors[det_indx].status |= HV_SHUTDOWN;
        for (k=0; k < NUM_DETECTOR*DET_HV; k++)
          {
            if (hv_shutdown[k].chan == -1)
              {
                hv_shutdown[k].det = det_indx;
                hv_shutdown[k].chan = j;
                hv_turn_off++;
                break;
              }
          }
      }
}
/****************************************************************************
*   Turn off HV for one channel
****************************************************************************/
int  hv_off(int chan)
{
   int  ierr;

   if (chan < 0 || chan >= NUM_HV) return (-1);
   local_1458.mainframe = chan/256;
   if (chan >= 256) chan = chan - 256;
   sprintf(local_1458.data,"LD S%i.%i CE 0",chan/16,chan % 16);
   ierr = lrs1458_io(&local_1458);
   if (strstr(local_1458.data,"ERROR") != NULL) ierr |= 1;
   return(ierr);
}
/****************************************************************************
*
*  Initialize the VMIC 3113A 64 channel, 12-bit ADC and the VMIC 3124
*  16 channel, 12-bit ADC.
*
****************************************************************************/
void vmic_adc_init(void)
{
   struct vmic_adc *vmic;
   struct vmic3124_adc *new_vmic;
   int  i;

   vmic = (struct vmic_adc *)VMIC3113A;
   vmic->csr = VMIC_SOFT_RESET | VMIC_SCAN_INTR;
   vmic->csr = VMIC_SCAN_INTR;

   *((void (**)(void))((VMIC_ADC_VEC)*4)) = adc_intr;
   vmic->ivr = VMIC_ADC_VEC;
/*
*   Initialize timers.  Timer 0 output is set for 1 millisecond period.
*   Timer 1 output is set for 17 millisecond period.  Hence, the
*   temperatures are converted every 17 milliseconds ( approx 60 Hz).
*/
   vmic->tcr = 0x34;
   vmic->timer0 = 0x40;
   vmic->timer0 = 0x1f;   /* count = 8000 */
   vmic->tcr = 0x74;
   vmic->timer1 = 17;
   vmic->timer1 = 0x00;   /* count = 17   */

   vmic->csr = VMIC_LED_OFF | VMIC_TIMER_START | VMIC_SCAN_INTR;
   vmic->icr = VMIC_ADC_INTR_ENA;

   new_vmic = (struct vmic3124_adc *)VMIC3124;
   new_vmic->csr = VMIC3124_LED_OFF;
}
/****************************************************************************
*
*  ADC interrupt routine.  The interrupt is generated by the VMIC 3113A.
*  Process data from both ADCs.
*
****************************************************************************/
void adc_intr(void)
{
#pragma ghs interrupt

    static long xtemp[NUM_TEMP],ytemp[NUM_TEMP];
    static long xpress[16],ypress[16];
    struct vmic_adc *vmic;
    struct vmic3124_adc *new_vmic;
    long diff;
    static int first_adc = 1,second_adc = 1;
    int  adc,i;

    vmic = (struct vmic_adc *)VMIC3113A;
    new_vmic = (struct vmic3124_adc *)VMIC3124;
/*
*  On first interrupt set xtemp and ytemp arrays to the intial
*  conversion values.
*/
    if (first_adc)
      {
        for (i=0; i < NUM_TEMP; i++)
          {
            adc = vmic->data[i] & 0xfffe;
            xtemp[i] = (long)adc << 15 ;
            ytemp[i] = xtemp[i];
          }
        first_adc = 0;
      }
/*
*   Read the ADC and do digital low pass filter
*/
    for (i=0; i < NUM_TEMP; i++)
      {
        adc = vmic->data[i];
        diff = ((long)adc << 15) - xtemp[i];
        xtemp[i] += diff >> 5;
      }
    for (i=0; i < NUM_TEMP; i++)
      {
        diff = xtemp[i] - ytemp[i];
        ytemp[i] += diff >> 5;
      }
/*
*  Scale data and move to temperature array
*/
    for (i=0; i < NUM_TEMP; i++) temp[i] = ytemp[i] >> 15;
    vmic->icr = VMIC_ADC_INTR_ENA;
/*
*  Do second ADC module
*
*  On first interrupt set xpress and ypress arrays to the intial
*  conversion values.
*/
    if (second_adc)
      {
        for (i=0; i < 16; i++)
          {
            adc = new_vmic->data[i] & 0xfffe;
            xpress[i] = (long)adc << 15 ;
            ypress[i] = xpress[i];
          }
        second_adc = 0;
      }
    for (i=0; i < 16; i++)
      {
        adc = new_vmic->data[i];
        diff = ((long)adc << 15) - xpress[i];
        xpress[i] += diff >> 5;
      }
    for (i=0; i < 16; i++)
      {
        diff = xpress[i] - ypress[i];
        ypress[i] += diff >> 5;
      }
/*
*  Scale data and move to pressure array
*/
    for (i=0; i < 16; i++) press[i] = ypress[i] >> 15;
/*
*   Put tank pressure in temperature array(last element) so workstation
*   can get it.
*/
    temp[NUM_TEMP-1] = press[0];

/********************  for test only ***************************
  Put pressure where we can see it with lnhis

temp[41] = press[0];
***************************************************************/


}
/****************************************************************************
*   LeCroy 1458 High Voltage controller
****************************************************************************/
int lrs1458_io(struct lrs1458_ctl *cmd)
{

#define RETRY_1458  3

  int count,err,i,j,retry = RETRY_1458;
  char  *cptr;
  unsigned char data,status,*uptr;
  static char logmsg[] = "1450";
  static char cmd_buf[sizeof(cmd->data)];
  int  ch,cmdlen,serror;

  if (!devtbl->vmic6015a) return (LRS_ERR_NONEXIST);
  ch = cmd->mainframe;
  if (ch < 0 || ch > 1) return (LRS_ERR_NONEXIST);
  strcpy(cmd_buf,cmd->data);
  cmdlen = strlen(cmd_buf);
  while(retry--)
   {
/*
 *  Send the ASCII command string.
 */
     i = 0;
     while(cmd_buf[i] != '\0')
       {
         put_xmit_char(ch,cmd_buf[i]);
         i++;
       }
     put_xmit_char(ch,CR);

/*
 *   Clear input buffer, clear channel error byte.
 */
     channel[ch].err = 0;
     clr_recv_buf(&channel[ch].ibuf);

/*
 *   Wait for a reply from the power supply
 */
     serror = 0;
     delay_(15);
     j = 0;
     count = 0;
     while(j < 30)
       {
         while(get_recv_char(ch,&data,&status))
           {
             cmd->data[count] = data;
             serror |= status;
             count++;
             if (count == cmdlen + 1) serror = 0;
             if (count >= (sizeof(cmd->data) - 1))
               {
                 cmd->data[--count] = '\0';
                 break;
               }
             else  cmd->data[count] = '\0';
           }
         if (strstr(cmd->data,"1450>") != NULL) break;
         if (strstr(cmd->data,"to begin") != NULL) break;
         delay_(2);
         j++;
       }
     strcpy(cmd->data,cmd_buf);
     cmd->data[cmdlen] = 0x0d;
     err = 0;
     if (serror)
       {
         err = LRS_ERR_SERIALIO;
uptr = (unsigned char *) 0x30000;
if (*uptr == 0 && retry == 0)
{
*uptr++ = channel[ch].err;
*uptr = retry;
uptr = (unsigned char *) 0x30010;
for (j=0; j < sizeof(cmd_buf); j++) *uptr++ = cmd_buf[j];
uptr = (unsigned char *) 0x30100;
for (j=0; j < sizeof(cmd->data); j++) *uptr++ = cmd->data[j];
}
         delay_(10);
         continue;
       }
     else if (count == 0)
       {
         err = LRS_ERR_NORESPOND;
         delay_(10);
         continue;
       }
     else if (count >= sizeof(cmd->data))
       {
         err = LRS_ERR_BUFFULL;      /* Reply buffer full */
         break;
       }
/*
 *   Do we need to "login" ?.  If so send password and retry
 *   the last command.
 */
     if (strstr(cmd->data,"to begin") != NULL)
       {
         delay_(10);
         cptr = logmsg;
         while(*cptr != '\0') put_xmit_char(ch,*cptr++);
         put_xmit_char(ch,CR);
         delay_(20);
         continue;
       }
     break;
   }
  return (err);
}
/****************************************************************************
*   LeCroy 1440 High Voltage controller
****************************************************************************/

#define RETRY_1440  3

int lrs1440_io(struct lrs1440_ctl *cmd)
{
  int count,echo_count,err,i,j,retry = RETRY_1440;
  char  *cptr;
  unsigned char data,status;
  static char selmsg[] = "M 1 ";
  static char cmd_buf[sizeof(cmd->data)];
  int  ch;

  if (!devtbl->vmic6015b) return (LRS_ERR_NONEXIST);
  ch = cmd->mainframe;
  if (ch < 1 || ch > 2) return (LRS_ERR_NONEXIST);
  strcpy(cmd_buf,cmd->data);
  while(retry--)
   {
/*
 *  Send the ASCII command string.
 */
     i = 0;
     while(cmd_buf[i] != '\0')
       {
         put_xmit_char(ch,cmd_buf[i]);
         i++;
       }
     put_xmit_char(ch,CR);
     echo_count = strlen(cmd_buf) + 2;
/*
 *   Clear input buffer, clear channel error byte.
 */
     channel[ch].err = 0;
     clr_recv_buf(&channel[ch].ibuf);
/*
*    Give the "ON" and "OF" commands extra time
*/
     if (!strncmp(cmd_buf,"ON",2)) delay_(50);
     if (!strncmp(cmd_buf,"OF",2)) delay_(300);
/*
 *   Wait for a reply from the power supply
 */
     delay_(15);
     count = 0;
     j = 0;
     while(j < 3)
       {
         i = get_recv_count(&channel[ch].ibuf);
         if (i != count)
           {
             count = i;
             if (count >= sizeof(cmd->data)) return(LRS_ERR_BUFFULL);
             j = 0;
           }
         delay_(3);
         j++;
       }
     err = 0;
     if (channel[ch].err)
       {
         err = LRS_ERR_SERIALIO;
         delay_(1);
         continue;
       }
     else if (count <= echo_count)
       {
         err = LRS_ERR_NORESPOND;
/*
*   Do we need to we need to select the mainframe??
*   If we only get an echo of our command and this is our first try, send
*   a command to select mainframe #1.
*/
         if (retry == (RETRY_1440 - 1))
           {
             delay_(10);
             cptr = selmsg;
             while(*cptr != '\0') put_xmit_char(ch,*cptr++);
             put_xmit_char(ch,CR);
             delay_(30);
           }
         continue;
       }
/*
*   Move received characters to the packet buffer.
*/
     j = 0;
     while(1)
       {
         if (!get_recv_char(ch,&data,&status)) break;
         err |= status;
         cmd->data[j] = data;
         j++;
         if (j >= sizeof(cmd->data))
           {
             err = LRS_ERR_BUFFULL;      /* Reply buffer full */
             j--;
             break;
           }
        }
     cmd->data[j] = 0;
     break;
   }
  return (err);
}

/****************************************************************************
*   Initialize VMIC 6015 Quad-Serial I/O Modules
*
****************************************************************************/
void vmic_init(void)
{
   struct vmic_ch *vmic;
   struct vmic_bim *bim;
   int  i;

/*
*  Initialize transmit buffer empty vectors
*/
   *((void (**)(void))((VMIC_VECS+4)*4)) = ch0_xmit;
   *((void (**)(void))((VMIC_VECS+0)*4)) = ch1_xmit;
   if (devtbl->vmic6015b)
     {
       *((void (**)())((VMIC_VECS+20)*4)) = ch2_xmit;
       *((void (**)())((VMIC_VECS+16)*4)) = ch3_xmit;
       *((void (**)())((VMIC_VECS+28)*4)) = ch4_xmit;
       *((void (**)())((VMIC_VECS+24)*4)) = ch5_xmit;
     }
/*
*  Initialize the receive character interrupt vectors
*/
   *((void (**)(void))((VMIC_VECS+6)*4)) = ch0_recv;
   *((void (**)(void))((VMIC_VECS+2)*4)) = ch1_recv;
   if (devtbl->vmic6015b)
     {
       *((void (**)())((VMIC_VECS+22)*4)) = ch2_recv;
       *((void (**)())((VMIC_VECS+18)*4)) = ch3_recv;
       *((void (**)())((VMIC_VECS+30)*4)) = ch4_recv;
       *((void (**)())((VMIC_VECS+26)*4)) = ch5_recv;
     }

/*
*  Initialize the special receive interrupt vectors
*/
   *((void (**)(void))((VMIC_VECS+7)*4)) = ch0_special;
   *((void (**)(void))((VMIC_VECS+3)*4)) = ch1_special;
   if (devtbl->vmic6015b)
     {
       *((void (**)())((VMIC_VECS+23)*4)) = ch2_special;
       *((void (**)())((VMIC_VECS+19)*4)) = ch3_special;
       *((void (**)())((VMIC_VECS+31)*4)) = ch4_special;
       *((void (**)())((VMIC_VECS+27)*4)) = ch5_special;
     }

/*
*  Initialize the external/status change interrupt vectors
*/
   *((void (**)(void))((VMIC_VECS+5)*4)) = ch0_ext;
   *((void (**)(void))((VMIC_VECS+1)*4)) = ch1_ext;
   if (devtbl->vmic6015b)
     {
       *((void (**)())((VMIC_VECS+21)*4)) = ch2_ext;
       *((void (**)())((VMIC_VECS+17)*4)) = ch3_ext;
       *((void (**)())((VMIC_VECS+29)*4)) = ch4_ext;
       *((void (**)())((VMIC_VECS+25)*4)) = ch5_ext;
     }

   vmic = (struct vmic_ch *)VMIC6015A;
   bim = (struct vmic_bim *)(VMIC6015A + BIM);
   bim->cr0 = bim->cr1 = bim->cr2 = bim->cr3 = 0;
   bim->vr0 = bim->vr1 = bim->vr2 = bim->vr3 = 0;
   for (i=0; i < 2; i++)
     {
       vmic->cmdreg = VMIC_CH_RESET;  /* Reset channel  */
/*
*   Setup for Asynchronous mode, X16 clock, no parity and 1 stop bit
*/
       vmic->modectl = 0x40 | VMIC_STOP1;
/*
*   Set for 9600 baud and enable baud rate generator.
*/
       vmic->tcreg = channel[i].tcreg = VMIC_9600;
       vmic->brgctl = channel[i].brgctl = 0xd;
       vmic->xmtctl = VMIC_8BITS;
       vmic->rcvctl = VMIC_8BITS;
       if (!(i % 2)) vmic->vectrg = VMIC_VECS + i * 4;
       vmic->intctl = channel[i].intctl = 0x16;
       vmic->rcvctl |= VMIC_RXENA;
       channel[i].rcvctl = vmic->rcvctl;
       vmic->xmtctl |= VMIC_TXENA;
       channel[i].xmtctl = vmic->xmtctl;
       clr_recv_buf(&channel[i].ibuf);
       clr_xmit_buf(&channel[i].obuf);
       channel[i].vmic = vmic;
       channel[i].mask = 0xff;
       channel[i].xcnt = 
                     sizeof(channel[i].ibuf.buf)/sizeof(struct indata) - 80;
       channel[i].xflg = 0;
       channel[i].xidle = 0;
       vmic++;
     }
   bim->cr2 = bim->cr3 = 0x35;
   if (devtbl->vmic6015b)
     {
       vmic = (struct vmic_ch *)VMIC6015B;
       bim = (struct vmic_bim *)(VMIC6015B + BIM);
       bim->cr0 = bim->cr1 = bim->cr2 = bim->cr3 = 0;
       bim->vr0 = bim->vr1 = bim->vr2 = bim->vr3 = 0;
       for (i=2; i < 6; i++)
         {
           vmic->cmdreg = VMIC_CH_RESET;  /* Reset channel  */
/*
*   Setup for Asynchronous mode, X16 clock, no parity and 1 stop bit
*/
           vmic->modectl = 0x40 | VMIC_STOP1;
/*
*   Set for 4800 baud and enable baud rate generator.
*/
           vmic->tcreg = channel[i].tcreg = VMIC_2400;
           vmic->brgctl = channel[i].brgctl = 0xd;
           vmic->xmtctl = VMIC_8BITS;
           vmic->rcvctl = VMIC_8BITS;
           if (!(i % 2)) vmic->vectrg = VMIC_VECS + (i+2) * 4;
           vmic->intctl = channel[i].intctl = 0x16;
           vmic->rcvctl |= VMIC_RXENA;
           channel[i].rcvctl = vmic->rcvctl;
           vmic->xmtctl |= VMIC_TXENA;
           channel[i].xmtctl = vmic->xmtctl;
           clr_recv_buf(&channel[i].ibuf);
           clr_xmit_buf(&channel[i].obuf);
           channel[i].vmic = vmic;
           channel[i].mask = 0x7f;
           channel[i].xcnt =
                        sizeof(channel[i].ibuf.buf)/sizeof(struct indata) - 80;
           channel[i].xflg = 0;
           channel[i].xidle = 0;
           vmic++;
         }
       bim->cr2 = bim->cr3 = 0x35;
     }
}

/*****************************************************************************
*****************************************************************************/
void ch0_xmit(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[0];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char data;

   if (get_xmit_char(chan,&data)) vmic->datarg = data;
   else
     {
       vmic->cmdreg = VMIC_RESET_TXPEND;
       chan->xidle = 0;
     }
}
/*****************************************************************************
*****************************************************************************/
void ch1_xmit(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[1];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char data;

   if (get_xmit_char(chan,&data)) vmic->datarg = data;
   else
     {
       vmic->cmdreg = VMIC_RESET_TXPEND;
       chan->xidle = 0;
     }
}
/*****************************************************************************
*****************************************************************************/
void ch2_xmit(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[2];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char data;

   if (get_xmit_char(chan,&data)) vmic->datarg = data;
   else
     {
       vmic->cmdreg = VMIC_RESET_TXPEND;
       chan->xidle = 0;
     }
}
/*****************************************************************************
*****************************************************************************/
void ch3_xmit(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[3];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char data;

   if (get_xmit_char(chan,&data)) vmic->datarg = data;
   else
     {
       vmic->cmdreg = VMIC_RESET_TXPEND;
       chan->xidle = 0;
     }
}
/*****************************************************************************
*****************************************************************************/
void ch4_xmit(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[4];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char data;

   if (get_xmit_char(chan,&data)) vmic->datarg = data;
   else
     {
       vmic->cmdreg = VMIC_RESET_TXPEND;
       chan->xidle = 0;
     }
}
/*****************************************************************************
*****************************************************************************/
void ch5_xmit(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[5];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char data;

   if (get_xmit_char(chan,&data)) vmic->datarg = data;
   else
     {
       vmic->cmdreg = VMIC_RESET_TXPEND;
       chan->xidle = 0;
     }
}
/*****************************************************************************
*****************************************************************************/
void ch0_recv(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[0];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char status = 0,rdata;

   rdata = vmic->datarg & chan->mask;
   put_recv_char(chan,rdata,&status);
   if (status != 0) chan->err |= status;
}
/*****************************************************************************
*****************************************************************************/
void ch1_recv(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[1];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char status = 0,rdata;

   rdata = vmic->datarg & chan->mask;
   put_recv_char(chan,rdata,&status);
   if (status != 0) chan->err |= status;
}
/*****************************************************************************
*****************************************************************************/
void ch2_recv(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[2];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char status = 0,rdata,tdata;

   rdata = vmic->datarg & chan->mask;
   put_recv_char(chan,rdata,&status);
   if (status != 0) chan->err |= status;
}
/*****************************************************************************
*****************************************************************************/
void ch3_recv(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[3];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char status = 0,rdata;

   rdata = vmic->datarg & chan->mask;
   put_recv_char(chan,rdata,&status);
   if (status != 0) chan->err |= status;
}
/*****************************************************************************
*****************************************************************************/
void ch4_recv(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[4];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char status = 0,data;

   data = vmic->datarg & chan->mask;
   put_recv_char(chan,data,&status);
   if (status != 0) chan->err = status;
}
/*****************************************************************************
*****************************************************************************/
void ch5_recv(void)
{
#pragma ghs interrupt

   struct channel_data *chan = &channel[5];
   struct vmic_ch *vmic = chan->vmic;
   unsigned char status = 0,data;

   data = vmic->datarg & chan->mask;
   put_recv_char(chan,data,&status);
   if (status != 0) chan->err = status;
}
/*****************************************************************************
*****************************************************************************/
void ch0_special(void)
{
#pragma ghs interrupt

  struct channel_data *chan = &channel[0];
  struct vmic_ch *vmic = chan->vmic;
  unsigned char status;

  status = vmic->stat1 & 0x70;
  put_recv_char(chan,vmic->datarg,&status);
  if (status != 0) chan->err |= status;
  vmic->cmdreg = VMIC_ERR_RESET;
}
/*****************************************************************************
*****************************************************************************/
void ch1_special(void)
{
#pragma ghs interrupt

  struct channel_data *chan = &channel[1];
  struct vmic_ch *vmic = chan->vmic;
  unsigned char status;

  status = vmic->stat1 & 0x70;
  put_recv_char(chan,vmic->datarg,&status);
  if (status != 0) chan->err |= status;
  vmic->cmdreg = VMIC_ERR_RESET;
}
/*****************************************************************************
*****************************************************************************/
void ch2_special(void)
{
#pragma ghs interrupt

  struct channel_data *chan = &channel[2];
  struct vmic_ch *vmic = chan->vmic;
  unsigned char status;

  status = vmic->stat1 & 0x70;
  put_recv_char(chan,vmic->datarg,&status);
  if (status != 0) chan->err |= status;
  vmic->cmdreg = VMIC_ERR_RESET;
}
/*****************************************************************************
*****************************************************************************/
void ch3_special(void)
{
#pragma ghs interrupt

  struct channel_data *chan = &channel[3];
  struct vmic_ch *vmic = chan->vmic;
  unsigned char status;

  status = vmic->stat1 & 0x70;
  put_recv_char(chan,vmic->datarg,&status);
  if (status != 0) chan->err |= status;
  vmic->cmdreg = VMIC_ERR_RESET;
}
/*****************************************************************************
*****************************************************************************/
void ch4_special(void)
{
#pragma ghs interrupt

  struct channel_data *chan = &channel[4];
  struct vmic_ch *vmic = chan->vmic;
  unsigned char status;

  status = vmic->stat1 & 0x70;
  put_recv_char(chan,vmic->datarg,&status);
  if (status != 0) chan->err |= status;
  vmic->cmdreg = VMIC_ERR_RESET;
}
/*****************************************************************************
*****************************************************************************/
void ch5_special(void)
{
#pragma ghs interrupt

  struct channel_data *chan = &channel[5];
  struct vmic_ch *vmic = chan->vmic;
  unsigned char status;

  status = vmic->stat1 & 0x70;
  put_recv_char(chan,vmic->datarg,&status);
  if (status != 0) chan->err |= status;
  vmic->cmdreg = VMIC_ERR_RESET;
}
/*****************************************************************************
*****************************************************************************/
void ch0_ext(void)
{
#pragma ghs interrupt

  struct vmic_ch *vmic = channel[0].vmic;
  unsigned char dat;

  channel[0].err |= (vmic->stat0 & 0x80);
  dat = vmic->datarg;
  vmic->cmdreg = VMIC_RESET_EXT;
}
/*****************************************************************************
*****************************************************************************/
void ch1_ext(void)
{
#pragma ghs interrupt

  struct vmic_ch *vmic = channel[1].vmic;
  unsigned char dat;

  channel[1].err |= (vmic->stat0 & 0x80);
  dat = vmic->datarg;
  vmic->cmdreg = VMIC_RESET_EXT;
}
/*****************************************************************************
*****************************************************************************/
void ch2_ext(void)
{
#pragma ghs interrupt

  struct vmic_ch *vmic = channel[2].vmic;
  unsigned char dat;

  channel[2].err |= (vmic->stat0 & 0x80);
  dat = vmic->datarg;
  vmic->cmdreg = VMIC_RESET_EXT;
}
/*****************************************************************************
*****************************************************************************/
void ch3_ext(void)
{
#pragma ghs interrupt

  struct vmic_ch *vmic = channel[3].vmic;
  unsigned char dat;

  channel[3].err |= (vmic->stat0 & 0x80);
  dat = vmic->datarg;
  vmic->cmdreg = VMIC_RESET_EXT;
}
/*****************************************************************************
*****************************************************************************/
void ch4_ext(void)
{
#pragma ghs interrupt

  struct vmic_ch *vmic = channel[4].vmic;
  unsigned char dat;

  channel[4].err |= (vmic->stat0 & 0xa8);
  dat = vmic->datarg;
  vmic->cmdreg = VMIC_RESET_EXT;
}
/*****************************************************************************
*****************************************************************************/
void ch5_ext(void)
{
#pragma ghs interrupt

  struct vmic_ch *vmic = channel[5].vmic;
  unsigned char dat;

  channel[5].err |= (vmic->stat0 & 0xa8);
  dat = vmic->datarg;
  vmic->cmdreg = VMIC_RESET_EXT;
}
/*****************************************************************************
*  Clear the receive buffer.
*****************************************************************************/
void clr_recv_buf(struct recvbuf *bufptr)
{
   int i,level;

   level = set_intr_level_(0x700);
   bufptr->input = bufptr->output = &bufptr->buf[0];
   for (i=0; i < IN_BUF_SIZE; i++)
     {
       bufptr->buf[i].err = 0;
       bufptr->buf[i].data = 0;
     }
   set_intr_level_(level);
}
/*****************************************************************************
*   Clear the transmit buffer.
*****************************************************************************/
void clr_xmit_buf(struct xmitbuf *bufptr)
{
   int level;

   level = set_intr_level_(0x700);
   bufptr->input = bufptr->output = bufptr->buf;
   set_intr_level_(level);
}
/*****************************************************************************
*  Get number of characters in the receive buffer.
*  Call:  bufptr  -  pointer to receive buffer
*
*  Return:
*          0  means buffer is empty
*        > 0  buffer character count
*         -1  means the buffer is full
*****************************************************************************/
int get_recv_count(struct recvbuf *bufptr)
{
   int ic,oc;

   ic = bufptr->input - &bufptr->buf[0];
   oc = bufptr->output - &bufptr->buf[0];
   if (ic >= oc) ic = ic - oc;
   else ic = IN_BUF_SIZE -oc +ic;
   if (ic >= IN_BUF_SIZE - 1) ic = -1;
   return (ic);
}
/*****************************************************************************
*  Get number of characters in the transmit buffer.
*  Call:  bufptr  -  pointer to transmit buffer
*
*  Return:
*          0  means buffer is empty
*        > 0  buffer character count
*         -1  means the buffer is full
*****************************************************************************/
int get_xmit_count(struct xmitbuf *bufptr)
{
   int ic,oc;

   ic = bufptr->input - bufptr->buf;
   oc = bufptr->output - bufptr->buf;
   if (ic >= oc) ic = ic - oc;
   else ic = OUT_BUF_SIZE -oc +ic;
   if (ic >= OUT_BUF_SIZE - 1) ic = -1;
   return (ic);
}
/*****************************************************************************
*  Put one character into the transmit buffer.  If the transmitter is idle,
*  get one character from the buffer and start the transmitter.
*  Call:   chan  -  output channel number
*          data  -  character data
*
*  Return:
*          0  means buffer is full.  Nonzero means OK.
*
*****************************************************************************/
int put_xmit_char(int chan, unsigned char data)
{
   struct xmitbuf *ptr = &channel[chan].obuf;
   int count,level;

   level = set_intr_level_(0x700);
   count = get_xmit_count(ptr);
   if (count < 0)
     {
       set_intr_level_(level);
       return (0);
     }
   *ptr->input++ = data;
   if ((ptr->input - ptr->buf) >= OUT_BUF_SIZE) ptr->input = ptr->buf;
   if (count == 0 && channel[chan].xidle == 0)
     {
       get_xmit_char(&channel[chan],&data);
       channel[chan].xidle = 0xff;
       channel[chan].vmic->datarg = data;
     }
   set_intr_level_(level);
   return (1);
}
/*****************************************************************************
*  Get one character from the transmit buffer.
*  Call:   ptr  -  Pointer to transmit buffer structure
*
*  Return:  0 means buffer is empty.  Nonzero means OK.
*          data -  character data
*****************************************************************************/
int get_xmit_char(struct channel_data *chan, unsigned char *data)
{
   int count;
   struct xmitbuf *ptr = &chan->obuf;

   count = get_xmit_count(ptr);
   if (!count) return 0;
   *data = *ptr->output++;
   if ((ptr->output - ptr->buf) >= OUT_BUF_SIZE) ptr->output = ptr->buf;
   return (1);
}
/*****************************************************************************
*  Put one character in the receive buffer.  Routine is called only by receive
*  interrupt routines.
*  Call:  ptr   -  pointer to the receive buffer structure
*         data  -  the received character
*         status - status bits from UART status register 1
*
*  Return:  0  means buffer is full.  Nonzero means OK.
*****************************************************************************/
int put_recv_char(struct channel_data *chan, unsigned char data,
                                                        unsigned char *status)
{
   int count;
   struct recvbuf *ptr = &chan->ibuf;

   count = get_recv_count(ptr);   /* get buffer character count     */
   if (count < 0)
     {
/*
*   Receiver buffer is full.  Put the Buffer overrun bit in the status
*   and return buffer full status.
*/
       *status |= 0x8;
       (ptr->input)->err = *status;
       return (0);
     }
    if (chan->xflg && count >= chan->xcnt)
      {
        if (!chan->xidle)
          {
            chan->xidle = 0xff;
            (chan->vmic)->datarg = XOFF;
            chan->txoff = XOFF;
          }
        else  chan->nxchar = XOFF;
      }
/*
*   Put the character and status in the buffer.  Increment the buffer
*   pointer and return OK status.
*/
   (ptr->input)->data = data;
   (ptr->input)->err = *status;
   ptr->input++;
   if ((ptr->input - ptr->buf) >= IN_BUF_SIZE) ptr->input = ptr->buf;
   return (1);
}
/*****************************************************************************
*  Get one character and the error status byte from a receive buffer.
*  Call:   chan  -  Serial channel number(0 thru 7)
*
*  return:   0  means buffer is empty.  Nonzero means valid char
*          data   -  character
*          status - error status
*
*****************************************************************************/
int get_recv_char(int chan, unsigned char *data,unsigned char *status)
{
   struct recvbuf *ptr = &channel[chan].ibuf;
   int count;

/*
*   If the buffer is empty, just return buffer empty flag.
*/
   count = get_recv_count(ptr);
   if (!count) return 0;
/*
*   Return the character and status.  Increment buffer pointer and
*   return OK status.
*/
   *data = (ptr->output)->data;
   *status = (ptr->output)->err;
   ptr->output++;
   if ((ptr->output - ptr->buf) >= IN_BUF_SIZE) ptr->output = ptr->buf;
   if (channel[chan].txoff && count < channel[chan].xcnt) ;
   return (1);
}
/****************************************************************************
*
*  Send keep alive message to all hosts.
****************************************************************************/
void watchdog(void)
{
  int  status;
  time_t  tod;
  static struct VMEmsg *host_msg;
  static int  eno;
  static struct Ether_Packet out_pkt;
  static char dest[] = {3,'m','c','s','q',0};

  if(!eno)
    {
      eno = open("ln1",3);
      if (eno <= 0)
        {
          printf("Can't open device 'ln'\n");
          exit(1001);
        }
      ioctl(eno,EIOPHYSADR,out_pkt.Source);  /* Put our physical address in
                                                the packet header           */
      memcpy((char *)out_pkt.Destination,dest,6);
      out_pkt.Order = 0;
      out_pkt.Protocol[0] = PROTO_PREFIX;
      out_pkt.Protocol[1] = PROTO_REQUEST_BOOT;
      out_pkt.Ack = NOACK;
      host_msg = (struct VMEmsg *)out_pkt.Data;
      strcpy(host_msg->sender,"Watchdog");
      host_msg->type = HEART;
    }
   time(&tod);
   strftime(ascii_time,20,"%d-%b-%y %H:%M:%S  ",localtime(&tod));
   sprintf(host_msg->text,"All is well on %s",ascii_time);
   status = write(eno,(char *)&out_pkt,sizeof(struct VMEmsg) + ORPH_HDR_LEN);
   if (status < 0)
     {
       printf("Write failure on device 'ln'\n");
       exit(1003);
     }
}
/****************************************************************************
*
****************************************************************************/
void beep_check(void)
{

/*
*    GROUP_MASK_ENA  - Mask of bit to check if manifold exists and 
*                      fill is enabled.
*    GROUP_MASK_DIS  - Mask of bit to check if manifold fill exists and
*                      fill is disabled.
*
*    DETECTOR_MASK   - Mask of bit to check if detector is enabled for
*                      normal fill.  WARNING: There are no checks during
*                      a cool down cycle.
*
*/

#define GROUP_MASK_ENA     FILL_TIME_SHORT + FILL_TIME_LONG + OVERFLOW_RTD +\
                           ROOM_TEMP
#define GROUP_MASK_DIS     FILL_TIME_LONG + TANK_EMPTY + LINE_PRESS +\
                           ROOM_TEMP
#define DETECTOR_MASK      FILL_TIME_LONG + FILL_INTERVAL_LONG + HV_SHUTDOWN +\
                           OVERFLOW_RTD + TEMP_RTD

   int i,beep = 0;

   lnsave();          /* save fill system data for beeper */

   if (reboot)
     {
       beep = 1;           /* Always beep on reboot        */
       reboot--;
     }
   if (hardware_status != 0) beep = 1;
   for (i=0; i < NUM_GROUP; i++)
     {
       if (groups[i].status & CHKSUM_ERR) beep = 1;
     }
   for (i=0; i < NUM_GROUP; i++)
     {
       if (groups[i].enable < 0) continue;
       if (groups[i].enable == 1)
         {
           if (groups[i].status & GROUP_MASK_ENA) beep = 1;
         }
       else if (groups[i].enable == 0)
         {
           if (groups[i].status & GROUP_MASK_DIS) beep = 1;
         }
     }
   for (i=0; i < NUM_DETECTOR; i++)
     {
       if (detectors[i].status & CHKSUM_ERR) beep = 1;
     }
   for (i=0; i < NUM_DETECTOR; i++)
     {
       if (detectors[i].enable < 0) continue;
       if (detectors[i].enable > 7) continue;
       if (detectors[i].status & DETECTOR_MASK) beep = 1;
     }
   if (beep)
     {
       if (!set_evt_(LNFXX_ERR))
         {
           sprintf(error_msg,"M23 ERROR - Call user");
           host_message(WARN,error_msg);
         }
     }
}
/****************************************************************************
*
*  Copy fill system data to the memory segment LN_FILL so that the
*  'beeper task' can access it.
*
****************************************************************************/
void  lnsave(void)
{
    struct lndata *lndat = (struct lndata *)LNDATA;
    int  i;

    lndat->hardware_ok = hardware_ok;
    lndat->hardware_status = hardware_status;
    for (i=0; i < NUM_GROUP; i++) lndat->mani[i] = groups[i];
    for (i=0; i < NUM_DETECTOR; i++) lndat->det[i] = detectors[i];
    for (i=0; i < NUM_DETECTOR; i++) lndat->hvstat[i] = hv_status[i];
    lndat->room = room;
}
/****************************************************************************
*
*   Compute the checksum and save hv_status array in Battery backed RAM
*
****************************************************************************/
void backup_hv_stat(void)
{
    int i,size;
    char *hv_ptr;

    size = sizeof(char) * NUM_DETECTOR;
    hv_ptr = (char *)LN_HV_STAT;
    for (i=0; i < NUM_DETECTOR; i++) *hv_ptr++ = hv_status[i];
    hv_ptr = (char *)LN_HV_STAT;
    crcgen(hv_ptr,size,1);
}
/****************************************************************************
*
*   Compute the checksum and save room temperature setpoints in Battery backed
*   RAM
*
****************************************************************************/
void backup_room(void)
{
    int i,size;
    struct room_temp *room_ptr;

    size = sizeof(struct room_temp);
    room_ptr = (struct room_temp *)LN_ROOM_TEMP;
    *room_ptr = room;
    crcgen((char *)room_ptr,size-4,1);
}
/****************************************************************************
*  Send room temperature set points to the host workstation.
****************************************************************************/
int  get_room(struct room_alarm *rply)
{
    int i,size;

    rply->room = room;
    size = sizeof(rply->room)/sizeof(short);
    word_swap_((char *)&rply->room,size);
    byte_swap_((char *)&rply->room,size*2);
    return(0);
}
/****************************************************************************
*  Set room temperature set points from workstation.
****************************************************************************/
int  set_room(struct room_alarm *cmd)
{
    int i,size;

    size = sizeof(cmd->room)/sizeof(short);
    word_swap_((char *)&cmd->room,size);
    byte_swap_((char *)&cmd->room,size*2);

    room = cmd->room;
    backup_room();
    sprintf(error_msg,"M30 Room temp alarms set by host");
    host_message(INFORM,error_msg);
    return(0);
}
/****************************************************************************
****************************************************************************/
void room_check(void)
{
   int i;
   float ftemp;

   for (i=0; i < NUM_GROUP; i++)
     {
       if (groups[i].enable < 0) continue;
       if (temp[0] < room.min)
         {
           groups[i].status += ROOM_TEMP;
           ftemp = tconv(temp[0]);
           sprintf(error_msg,
                   "M28 Room temperature low!  RTD value = %i Temp = %4.1f F",
                    temp[0],ftemp*1.8+32.0);
           host_message(INFORM,error_msg);
         }
       if (temp[0] > room.max)
         {
           groups[i].status += ROOM_TEMP;
           ftemp = tconv(temp[0]);
           sprintf(error_msg,
                    "M29 Room temperature high! RTD value = %i  Temp = %4.1f F",
                    temp[0],ftemp*1.8+32.0);
           host_message(INFORM,error_msg);
         }
       break;
     }
}
/****************************************************************************
*   Convert ADC value to temperature in degrees C
****************************************************************************/
float tconv(int adc)
{
   float ftemp,resist,fadc;

   fadc = adc;
   resist = (20000.0 * fadc)/(409600.0 - fadc);
   if (resist < 24.94)
     {
       ftemp = (resist - 24.94)/0.4300;
       ftemp = ftemp -185.0;
     }
   else if (resist < 30.05)
     {
       ftemp = (resist - 30.05)/0.4225;
       ftemp = ftemp -173.0;
     }
   else if (resist < 100.0)
     {
       ftemp = (resist - 100.0)/0.4043;
     }
   else
     {
       ftemp = (resist - 100.0)/0.3900;
     }
   return (ftemp);
}
