/******************************************************************************
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                         Copyright(C) 1992-2000
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
*     for any loss or damage, direct or consequential, arising out of the
*     use or the inability to use the product.  Before using, the USER shall
*     determine the suitability of the product for his or her intended use,
*     and the USER assumes all risk and liability whatsoever in connection
*     therewith.
*
******************************************************************************
*
*    Environment:      based Data Acquisition System
*
*    File:         /tera/mcsq/Drtems/Dvmeacq/VMEacq.c
*
*    Description:  This is the real-time acquisition code for the VME
*                  processor.  There are two parts to the code.  The first
*                  part processes tables generated and loaded into the
*                  VME processor by Milner's PAC.  These tables specify
*                  all operations required by the readout routines.  They
*                  are checked for validity and new tables more optimal for
*                  use by the real-time routines are built.  Many parts
*                  of this is hardware dependent and therefore is done
*                  here instead of in PAC.  Messages from the host control
*                  start and stop for acquisition.  The second part of this
*                  consists of routines which do readout and data stream
*                  formating.  These execute under the event interrupt.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    9/20/92    MCSQ       Original  
*
*   10/14/92    MCSQ       Several changes.  1) It was observed that the
*                          send_message_pointer procedure sometimes return
*                          "success" when in fact the message is not sent.
*                          The handshake between the acquisition interrupt
*                          "send_event" and the data transmission process
*                          has been modified to solve this problem.  Now
*                          two handshake flags are used.  Both are set when
*                          the message is sent.  The data transmission process
*                          resets the first flag when the message is received.
*                          The second flag is reset when  the buffer has been
*                          processed as it was in previous versions.
*                          2) Problems due to high event rates(i.e rates
*                          so high the system cannot process all events) are
*                          due to flaws in VMEPROM.  The code rx_vmeprom.s
*                          replaces the offending routines and also significantly
*                          improves the context switching time.  This resulted
*                          33% increase in Ethernet throughput and tolerance
*                          of extremely high event rates.  CAUTION:  Always
*                          load rx_vmeprom before starting the data acquisition
*                          system. 3)  The setup routines herein run in user
*                          mode with all interrupts enabled.  Many of these
*                          modify hardware interfaces.  Previous versions
*                          did not always disable the clock interrupt while
*                          hardware registers where being modified.  I hope
*                          all such cases have been fixed now.  4) The CAMAC
*                          windup_list was heretofore always executed TWICE!
*
*   10/30/92    MCSQ       Added return acquisition status to the command
*                          list in main.  This allows the host to determine
*                          the status of the VME acquisition system.
*                          On 'start', send message to host with the initial
*                          event counter value.
*
*   11/22/92    MCSQ       Changed host message source name from fe_init
*                          to VMEacq.  Also changed stop acquisition message.
*                          Removed diagnostic messages to the host and also
*                          to the local terminal.
*
*   11/27/92    MCSQ       Removed new-line character from all messages sent
*                          to the host.
*
*   12/15/92    MCSQ       Added host message when VME acquisition is started
*                          and when initialization completes successfully.
*
*   12/30/92    MCSQ       Add checks for presence of proper VME and FASTBUS
*                          hardware.
*
*    2/10/93    MCSQ       When event buffers fill, the event interrupt is
*                          disabled and the Parallel Interface/Timer #2 timer
*                          interrupt is enabled to check the buffers every
*                          500 us until there is available space.  We no
*                          longer attempt to count number of events lost.
*
*    3/28/93    MCSQ       The event counter is now in the memory segment
*                          with acquisition parameters loaded by the PAC.
*                          Furthermore, it is initialized to 0 only on
*                          power up.  Previously, it was set to 0 by
*                          initialization of the acquisition software.
*
*                          Changed so that the main interrupt routine
*                          runs the execution lists and checks for buffer
*                          full.  These functions were in separate functions
*                          and this change reduces the per event processing
*                          time.  It is very important to pack ethernet
*                          packets as full as possible.  Hence, the average
*                          event size is computed every 20 buffers by the
*                          xdata_proc task and passed to this code in the
*                          acquisition parameters memory segment.
*
*                          Revised the code for FERA readout.  FERA readout
*                          and formating for LeCroy 4300 ADCs has been tested.
*
*    4/12/93    MCSQ       Added code to support the ORNL trigger module.
*
*    7/ 9/93    MCSQ       The ORNL trigger module is now alive and kicking.
*                          Tested and revised code to support this module.
*
*    9/29/93    MCSQ       Added minimal changes for conversion to PAC
*                          Version 2.
*
*    2/21/94    MCSQ       Changed for Green Hills 1.8.6 and Oasys 5.16
*                          cross software.  Used the new interrupt pragma
*                          to eliminate need for assembly language 
*                          interrupt routines.  Removed all references
*                          to Acromag trigger module.
*
*    4/23/94    MCSQ       Added command to return PAC file name in function
*                          main.
*                          Added flags in shared memory for the process
*                          'vmemon' to check.  There is a flag for FASTBUS
*                          readout errors and a flag which indicates that
*                          KSC3982 is enabled.  No data are put in the event
*                          buffer if there is a FASTBUS readout error occurs.
*                          Also, no data from the list sequencer is put in
*                          event buffer unless the correct number of data
*                          words are read from the list sequencer. 
*                          Error reporting is left to the process 'vmemon'.
*
*    7/21/94    MCSQ       Add support for LeCroy 1190 Dual Port Memory.
*                          It may be used instead of the CES 8170 for FERA
*                          readout.  If both modules are present, default to
*                          the CES 8170.
*
*    8/31/94    MCSQ       Fixed the FERA format routines.  The test for a
*                          header word in the FERA readout failed for a
*                          module in crate 0 and slot 1 if the number of
*                          sub-addresses read was 16.
*
*    2/23/95    MCSQ       Add check for sufficient buffer space in list
*                          sequencer setup.  New error code ACQ_SEQ_BUFFERS
*                          means buffers are too small.
*
*                          Add a 20 millisecond delay between packets sent
*                          by host_message function.  This gives the host
*                          a better chance of receiving all our messages
*                          when the number of messages exceeds the MAXwaiting
*                          for the packetfilter.
*
*    4/ 4/95    MCSQ       Add FASTBUS module types LRS_1875 and LRS_1881.
*
*    9/21/95    R. Varner  Modify LRS_1881 readout and add LRS_1877.
*
*   10/18/95    MCSQ       Add FERA module type for Silena 4418V ADC.
*
*    2/16/96    MCSQ       Fix list of table names in acq_init routine.
*
*    2/26/96    MCSQ       Added routines to implement special CAMAC modules,
*                          count down, raw and calculated gates.  Now all
*                          features in Milner's PAC are implemented herein.
*
*    3/24/96    MCSQ       It appears that noise can cause an Event interrupt
*                          which does not set the Busy latch in the trigger
*                          module.  This was happening at the RMS when
*                          vacuum valves were CLOSED.  When this happens
*                          the event delay time counter in the trigger
*                          module does not count and hence any acquisition
*                          PAC which has a delay spec gets hung.  This means
*                          that none of the control system works because
*                          the cpu is running with the clock interrupt
*                          disabled and can not schedule the control process
*                          to run.
*
*                          The fix was to test the delay counter at the
*                          beginning of the process_event routine.  If
*                          it is zero we just return from the interrupt.
*
*    9/ 4/96    MCSQ       Add special CAMAC module code for LeCroy 3377,
*                          LeCroy 4300, and Silena 4418.  Code uses the
*                          compressed format via CAMAC.  Modules may
*                          also use FERA readout if defined by $fer in
*                          the PAC( except for LRS 3377 for now).   
*
*    9/23/96    MCSQ       Try to optimize the conditional CAMAC for
*                          faster readout.
*
*    9/29/96    MCSQ       Add routines setup_cam_start_list and run_start_list.
*                          These build and execute a list CAMAC commands.
*                          The list is executed each time acquisition is
*                          started.  The list in PAC is $run.
*
*                          Test for illegal CAMAC module types in Raw
*                          Gate specifications.  Return error for
*                          illegal types.  Force CAMAC F code for legal
*                          ones (i.e. ignore fxx spec in PAC).
*
*                          Clean up the delay time specs.
*
*   11/ 1/96    MCSQ       Changed the routine ces_fera_format to process
*                          FERA data the same way that lrs_fera_format
*                          does.  No CES8170 available to test changes but
*                          RLV will test it at MSU.
*
*                          Also added code in acq_start to set the LRS1190
*                          address register to zero each time a startvme
*                          command is done.
*
*   11/11/96    RLV        Fixed the test for transfer complete to the CES
*                          module.  The original test failed because 
*                          hsm_ptr->mem_ptr is long and it was stored in
*                          an unsigned short variable.  Changed variable
*                          to a long.  Also fixed "feradmp" stuff to
*                          work with the CES module.
*
*   1/13/97   JWK,JJK,NSC   Notre Dame
*
*       Made modifications to vmeacq.c in order to support a LeCroy
*       2277 TDC (jwk, jjk, nsc).  The changes and rationale are as follows:
*
*        1) Modified code that loads in the execution list in the 
*           KSC 2197.  Commented out lines 3028, 3029.  Changed 
*           line 3030 to a word size (WS) of 24 bits (3 bytes).
*
*        2) Changed the KSC_CSR_RDY on line 5382 to KSC_CSR_DONE
*           since RDY never comes up because you are not transferring
*           data.  You are just testing to see if Buffering is in
*           Progress (BIP) (line 5378 (ksc->cma = cam_ro->cma) 
*           does this by executing the commands loaded into the 
*           KSC command register beginning at line 3017)  DONE
*           comes up after the KSC is done executing the command
*           (in this case, the GO)  
*
*           Once BIP is false (you stop buffering) then you get a NOQ
*           response, and program breaks from the do...while loop.
*
*        3) Added the command  on line 5390
*                ksc->csr - KSC_CSR_RD | KSC_CSR_GO
*           since the KSC encounters a HALT (put into the register
*           on line 3028) which causes its command
*           register clear so you need another GO to get things
*           moving again. 
*
*    2/20/97    MCSQ       Fixed delay time for conditional readout.
*                          This has been broke since I worked on the
*                          conditional readout on 9/23/96.
*
*    2/21/97    MCSQ       Set WTM's special dummy latch data word
*                          even when user does NOT have a $lat statement
*                          in the PAC.
*
*                          Allow multiple LRS1190 FERA interface modules
*                          to be used( up to 4).
*
*    3/ 7/97    MCSQ       For new CPU-60s.  One conditional compile
*                          in routine acq_stop.  Probably do not need call
*                          to cache_inval_ for CPU-60.  However, will leave
*                          it for now.
*
*    3/22/97    MCSQ       Fix enable front panel mode for LRS 1190 modules
*                          in routine init_fera_modules.
*
*    4/20/97    MCSQ       Initialize ONRL trigger module at startup.
*                          Also discard FERA data for any LRS 1190 interface
*                          which has more than 1023 16-bit words.
*
*    4/30/97    MCSQ       Had problems with sparse readout of Phillips
*                          modules.  Our troops resist using VME BUSY to
*                          block ADC gates and TDC starts during readout
*                          of an event.  That means that you can get more
*                          data than the Word Count for the KSC2917 and
*                          when that happens the acquisition hangs.
*
*                          Put in a test for KSC2917 DONE which breaks
*                          out of a loop which was only testing DATA READY.
*
*    3/ 2/98    MCSQ       In the routine lrs_fera_format, there is a test
*                          for too much data ( > 1023 16-bit words).  If this
*                          is the case the data for this event is discarded.
*                          However, the front panel input was left disabled.
*
*                          Now the data are discarded but the front panel
*                          input is left enabled.
*
*    3/ 7/98    MCSQ       Allow user to specify the OVF enable for a Silena
*                          4418 using compressed CAMAC readout.  Now FERA
*                          and specical CAMAC are the same.
*
*    3/25/98    MCSQ       Fix routine acq_stop.  When the event interrupt is
*                          disabled, it is possible to get a spurious VMEbus
*                          interrupt.  This goes to vector 255 and the correct
*                          thing to do is an interrupt routine which just
*                          returns.  At one time in the early days of the ORNL
*                          trigger module I had all this right.  Then for
*                          some reason I improved it and screwed it up.
*
*    4/ 2/98    MCSQ       Reading the delay timer in the trigger module
*                          can yield a false reading greater than the real
*                          time because the counter may change during the
*                          VMEbus read cycle.  This is about 2.5 % of the
*                          time for near maximum delays.  The best solution
*                          is to fix the hardware but for now I have changed
*                          all the tests for delay time to read the hardware
*                          twice.  If the test is true the first time, we
*                          make the test a second time.
*
*    9/17/98    MCSQ       Add FERA module type - BAKLASH - for new clover
*                          modules.
*
*                          Add limited use of a second KSC2917 CAMAC 
*                          interface module.  Use is limited to initialization
*                          in $ini list, $fer list and $run list.  Run time
*                          readout MUST use the first CAMAC interface.
*
*                          Double size of the FERA ID table from the
*                          workstation.  This table is indexed based on
*                          interface, crate number and slot number.  Note
*                          that the inferface is taken from the crate
*                          number.  Interface 1 has crates 0 thru 7 and
*                          interface 2 is crates 10 thru 17.
*
*                          Virtual station numbers for FERAs are now assigned
*                          starting with one and are in the same order as
*                          the FERA modules table.
*
*   11/10/98    MCSQ       Many many changes over the last week.  All of it
*                          has to do with the ORNL CAMAC interface.  ALL
*                          changes are preliminary since the hardware is
*                          not yet available.  Added are routines to initialize
*                          new AUX controllers, split execution list into
*                          lists for the AUX and lists which must be done
*                          by the KSC interface.  Lists which have been
*                          split are:
*                               latch readout
*                               unconditional readout
*                               conditional readout
*                               special CAMAC module readout
*                               windup list
*
*                          The gate readout has NOT been changed.
*
*    2/ 1/99    MCSQ       The new CAMAC system is up and running -
*                          hardware and software.
*
*    4/ 5/99    MCSQ       Changes to Clover module readout.  Always do
*                          zero suppression and Hi Res data correction.
*                          Module type BAKLASH reads all parameters.
*                          Module type BAKLASH2 reads everything except
*                          BGO energy and BGO time.  Module type BAKLASH3
*                          reads only the hit pattern and the Hi Res GEs.
*
*    7/16/99    MCSQ       Double size of FERA ID table to allow 32 channels
*                          per module.  Add FERA module type LRS_3377.
*
*    2/15/00    MCSQ       Add special camac module for XIA time stamp.
*                          For now it is MCSQ_CAM.
*
*    2/24/00    MCSQ       XIA time stamp module is now type XIA_TIME.
*                          Add new table in pac for XIA readout.  User
*                          must specify crate, slot and vsn for each
*                          XIA module to be read.
*
*                          Event trigger reads all other pac specified
*                          modules and then checks the XIA for data ready.
*                          If data is ready, readout all XIA modules.
*
*    5/31/00    MCSQ       Fix setup of XIA_TIME module when an ORNL
*                          CAMAC interface is installed.  This module
*                          always uses the Kinetic Systems interface
*                          since write functions are required.
*
*    6/26/00    JWK        Add in all code from Notre Dame version from Jan 23,
*                          1997 by JJK supporting Ortec AD413 module. A couple
*                          of changes were made taking into account changing
*                          some 'if' statements into 'case' statements. All 
*                         additions are bracketed by comments containing'AD413'.
*                          Sections of added code moved unmodified still carry
*                          the Jan 23, 1997 date and JJK. Modified sections
*                          have the June 26, 2000 date and JJK, JWK. 
*
*    7/14/00    JWK        Add code for AD413 into initialization and 
*                          runtime code for special CAMAC modules in crates
*                          with AUX controller. New code bracketed by comments
*                          having July 14, 2000 and JWK. 
*
*   10/26/00    MCSQ       Changes to XIA polling.  I was reading CSR reg
*                          and masking with 0x4000. Nonzero indicated module
*                          ready for readout.  Changed mask to 0x2000 and
*                          now zero indicates module is ready for readout.
*   
*    2/08/01    JWK,JJK    Missing parameter (0) added after return statement  
*                          in setup_cam_start_list function. Without this
*                          the status is undefined and an "unknown error"
*                          could result.
*
*    2/15/01    JJK,JWK    The cam_funcond_setup function did not set up the
*                          readout delays for unconditionally-read CAMAC 
*                          modules. It relied on cam_uncond_setup to do this.
*                          However, if all unconditionally-readout modules
*                          were in crates with fast AUX modules, the module
*                          count was reduced to zero and cam_uncond_setup was
*                          exited prior to setting up the delays.
*                            The fix was to copy three lines of code from 
*                          cam_uncond_setup into cam_funcond_setup and slightly
*                          modify the IF statement at the end of cam_funcond_
*                          setup so that the delays are only computed if there
*                          are some unconditionally-read CAMAC modules to 
*                          process. Changed lines have JJK appended.
*
*    2/15/01    JJK,JWK    In the 2277 code in run_fcam_special, the test for
*                          Q=0 was backwards. JJK in comment. 
*
*    2/23/01    JWK        This is NOT a software change, but a note about 
*                          a mod put into the fast AUX hardware so the 2277 
*                          will work correctly in QREPEAT mode. The 2277 does
*                          not react properly to successive CAMAC cycles where
*                          N is not removed between cycles. To avoid sacrificing
*                          speed, a mod was put into the AUX which drops N,A   
*                          and F during S2 time IF and ONLY IF the AUX is in
*                          both 24-bit and QREPEAT mode. At this time, the 
*                          2277 is the only module known to meet this criterion.
*                          JWK, Notre Dame. 
*
*    6/19/02    MCSQ       Add readout for vme acquisition modules.  Currently
*                          includes CAEN_775 and CAEN_785.  Additions include
*                          routines init_vme_modules() and vme_format().
*
*    1/21/04    MCSQ       Add VME 100Hz clock.
*
*    2/06       MCSQ       Rtems version.  Many changes.
*    7/14/06    MCSQ       Add SIS3820 VME scaler module. Changed acq_start()
*                          to enable counting and acq_stop() to disable
*                          counting.
*    May-June 2008 RLV     Increase the number of CAEN modules to 12 ea.
*    29 July  2008 RLV     Add Catalin Matei V792 code to system
*****************************************************************************/
#include <bsp.h>
#include <rtems.h>
#include <rtems/rtems_bsdnet.h>
#include <rtems/error.h>
#include <bsp/VME.h>
#include <libcpu/io.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "../include/vme_sys.h"
#include "../include/Acq_Params.h"
#include "../include/ksc.h"
#include "../include/lrs.h"
#include "../include/lrs1821.h"
#include "../include/lrs1190.h"
#include "../include/ces.h"
#include "../include/orph_udp.h"
#include "../include/orphmsg.h"
#include "../include/acq_ctl.h"
#include "../include/devices.h"
#include "../include/trigger.h"
#include "../include/fcam.h"
#include "../include/caen.h"
#include "../include/sis3820.h"
#include "../include/myriad.h"

/**********   External data  ************/

extern struct devices DEVTBL;
extern rtems_id Que1id;
extern void *Que1buf;
extern unsigned long debugdata[1000];
extern unsigned long debugdata_index;

/**********   Data shared with external routines **********/

char ACQ_SHARED[sizeof(struct acq_share)];
char *ACQ_SHARED_RAM = ACQ_SHARED;

char ACQ_PARAMS[131072];
char *ACQ_RAM = ACQ_PARAMS;
int  ACQ_MAX_RAM = sizeof(ACQ_PARAMS);


/****************************************************************************
*
*   Conditional compilation flags.

*   DEBUG  -  Enables diagnostic output messages to the host about tables
*             built for data acquisition.  If LOCAL_MSG is also defined,
*             messages are also output to the local CRT terminal.
* LOCAL_MSG - Has effect only if DEBUG is defined.
*
*   TIMER   - If defined, the elapsed time from the start of event is
*             stored in an array at entry to various processing routines.
*
******************************************************************************/

/***************
#define  DEBUG
#define  LOCAL_MSG
#define  TIMER
***************/

/*
*    Default definitions for XIA time stamp readout
*/
#define  AOUTBUFFER  0x412a   /* address of pointer to spectra data buffer */
#define  GSLTTIMEA    0x4107   /* address of first time stamp word          */

/*
*    Codes used in the CAMAC readout tables
*/
#define CAM_RD      0   /* CAMAC read command                       */
#define CAM_WR      1   /* CAMAC write command                      */
#define CAM_NODATA  2   /* CAMAC nondata command                    */
#define CAM_RO_END (-1) /* End of CAMAC list                        */

/*
*   Host Message types.  Define with bytes and words swapped to make
*   DEC happy.
*/
#define INFORM  0x01000000       /* really type = 1 */
#define WARN    0x02000000
#define PANIC   0x03000000

/*
*   Pointers to CAEN ADCs and TDCs
*/
static struct CAEN *CAEN785_LIST[24] = {       /* V785 ADC             */
  (struct CAEN *)CAEN785_1,
  (struct CAEN *)CAEN785_2,
  (struct CAEN *)CAEN785_3,
  (struct CAEN *)CAEN785_4,
  (struct CAEN *)CAEN785_5,
  (struct CAEN *)CAEN785_6,
  (struct CAEN *)CAEN785_7,
  (struct CAEN *)CAEN785_8,
  (struct CAEN *)CAEN785_9,
  (struct CAEN *)CAEN785_10,
  (struct CAEN *)CAEN785_11,
  (struct CAEN *)CAEN785_12,
  (struct CAEN *)CAEN785_13,
  (struct CAEN *)CAEN785_14,
  (struct CAEN *)CAEN785_15,
  (struct CAEN *)CAEN785_16,
  (struct CAEN *)CAEN785_17,
  (struct CAEN *)CAEN785_18,
  (struct CAEN *)CAEN785_19,
  (struct CAEN *)CAEN785_20,
  (struct CAEN *)CAEN785_21,
  (struct CAEN *)CAEN785_22,
  (struct CAEN *)CAEN785_23,
  (struct CAEN *)CAEN785_24};

static struct CAEN *CAEN775_LIST[12] = {       /* V775 TDC             */
  (struct CAEN *)CAEN775_1,
  (struct CAEN *)CAEN775_2,
  (struct CAEN *)CAEN775_3,
  (struct CAEN *)CAEN775_4,
  (struct CAEN *)CAEN775_5,
  (struct CAEN *)CAEN775_6,
  (struct CAEN *)CAEN775_7,
  (struct CAEN *)CAEN775_8,
  (struct CAEN *)CAEN775_9,
  (struct CAEN *)CAEN775_10,
  (struct CAEN *)CAEN775_11,
  (struct CAEN *)CAEN775_12};

static struct CAEN *CAEN792_LIST[12] = {       /* V792 QDC             */
  (struct CAEN *)CAEN792_1,
  (struct CAEN *)CAEN792_2,
  (struct CAEN *)CAEN792_3,
  (struct CAEN *)CAEN792_4,
  (struct CAEN *)CAEN792_5,
  (struct CAEN *)CAEN792_6,
  (struct CAEN *)CAEN792_7,
  (struct CAEN *)CAEN792_8,
  (struct CAEN *)CAEN792_9,
  (struct CAEN *)CAEN792_10,
  (struct CAEN *)CAEN792_11,
  (struct CAEN *)CAEN792_12};

static struct SIS3820 *SIS3820_LIST[2] = {       /* SIS3820             */
  (struct SIS3820 *)SIS3820_1,
  (struct SIS3820 *)SIS3820_2};

/*
*   Structures of the internal tables
*/

/*     Internal conditional readout program                                 */
struct cond_tbl {
unsigned short  *lat;        /* pointer to latch data word                  */
	 int    mask;        /* mask value for .AND.                        */

/*  condition TRUE  (i.e. result of .AND. is nonzero)                       */
	 int    tr_ctr;      /* number of CNAFs to execute                  */
	 int    tr_cma;      /* memory address in KSC2917 for start of list */
struct cam_io   *tr_camid;   /* pointer to camac dir and id/data table      */
struct cond_tbl *tr_next;

/*  condition FALSE  (i.e. result of .AND. is zero)                         */
	 int    fa_ctr;      /* number of CNAFs to execute                  */
	 int    fa_cma;      /* memory address in KSC2917 for start of list */
struct cam_io   *fa_camid;   /* pointer to camac dir and id/data table      */
struct cond_tbl *fa_next;
    } ;

/*     Internal conditional readout program using AUX controller            */
struct fcond_tbl {
unsigned short  *lat;        /* pointer to latch data word                  */
         int    mask;        /* mask value for .AND.                        */

/*  condition TRUE  (i.e. result of .AND. is nonzero)                       */
         int    tr_ctr;      /* number of CNAFs to execute                  */
struct fcam_io  *tr_camid;   /* pointer to camac cnaf and id/data table     */
struct fcond_tbl *tr_next;

/*  condition FALSE  (i.e. result of .AND. is zero)                         */
         int    fa_ctr;      /* number of CNAFs to execute                  */
struct fcam_io  *fa_camid;   /* pointer to camac cnaf and id/data table     */
struct fcond_tbl *fa_next;
    } ;

/*  Kill list structure                                                     */
struct kill_list {
    unsigned short *lat;     /* pointer to latch data word                  */
    unsigned short mask;     /* mask for latch word                         */
              int  sense;    /* type test: 0 = NONE, nonzero = ANY          */
};

/*   Internal CAMAC execute table                                           */
struct cam_io {
  unsigned short dat;        /* data for CAMAC writes, IDs for CAMAC reads  */
	    int  dir;        /* transfer direction - read, write or nodata  */
};

/*   FastBus readout                                                        */
struct fb_readout {
  unsigned short *lat;       /* Pointer to latch word                       */
            int  mask;       /* mask for latch word                         */
            int  enable;     /* 0 disable readout, 1 = enable readout       */
            int  start;      /* Start address in 1821                       */
};

/*   FERA readout                                                           */
struct fera_readout {
  unsigned short *lat;       /* Pointer to latch word                       */
            int  mask;       /* mask for latch word                         */
            int  enable;     /* 0 disable readout, 1 = enable readout       */
};

/*   CAEN ADC and TDC readout                                               */
struct vme_readout {
  unsigned short *lat;       /* Pointer to latch word                       */
            int  mask;       /* mask for latch word                         */
            int  enable;     /* 0 disable readout, 1 = enable readout       */
};

struct caen_readout {
    struct CAEN *hwd;        /* Pointer to hardware module                  */
  unsigned short *id;        /* Pointer to ID for first channel in module   */
};

struct sis_readout {
 struct SIS3820 *hwd;        /* Pointer to hardware module                  */
  unsigned short *id;        /* Pointer to IDs for first channel in module   */
};

struct myr_readout {
  struct MyRIAD_Registers *hwd;/* Pointer to hardware module                  */
  unsigned short *id;        /* Pointer to IDs for first channel in module   */
};

/*     Conditional readout for special CAMAC modules                        */
struct cam_readout {
  unsigned short  *lat;       /* Pointer to pattern word                    */
             int  mask;       /* Bit mask for pattern word                  */
};

/*     Runtime  program for readout of special CAMAC modules                */
struct cam_ro_lst  {
   unsigned short  cma;       /* Start address in KSC 2917 memory           */
             char  c;         /* Crate number                               */
             char  n;         /* Slot number                                */
             char  mod_type;  /* Module type code                           */
             char  dum;       /* pad                                        */
};

/*     Raw gate runtime readout                                             */
struct gate_ro_lst {
   unsigned short  *pat;      /* Pointer to pattern word for test result    */
   unsigned short  mask;      /* Bit mask for pattern word                  */
   unsigned short  low;       /* Test low limit                             */
   unsigned short  high;      /* Test high limit                            */
             char  mod_type;  /* CAMAC module type.  0 means generic type   */
             char  new;       /* 1 means new CNAF, 0 means use last         */
};

/*     Count down runtime list                                              */
struct count_dwn_lst {
   unsigned short *pat;       /* Pointer to pattern word                    */
            short mask;       /* Bit mask for pattern word                  */
            short current;    /* Current count value                        */
            short initial;    /* Count down value - from PAC statement      */
};

/*     Calculated gate runtime list                                         */
struct cal_gate_lst {
   unsigned short *pat;       /* Pointer to pattern word for result         */
   unsigned short mask;       /* Bit mask for result pattern word           */
   unsigned short *pat1;      /* Pointer to raw gate pattern word #1        */
   unsigned short msk1;       /* Bit mask for raw gate #1                   */
   unsigned short not1;       /* 1 means complement test 1 result           */
   unsigned short *pat2;      /* Pointer to raw gate #2 ( optional )        */
   unsigned short msk2;       /* Bit mask for raw gate #2                   */
   unsigned short not2;       /* 1 means complement test 2 result           */
   unsigned short op;         /* op code. 0 = AND and 1 = OR                */
};

/*   Run time XIA readout                                                   */
static struct read_xia {
           int  cnaf;
           int  wc;
           int  vsn;
} xia_rd[NUM_XIA+1];

/*    structure for calls to ksc_camio                                      */
struct camac {
     unsigned char  c;
     unsigned char  n;
     unsigned char  a;
     unsigned char  f;
	      int   d;
};

/*    structure for calls to fastio                                          */
struct fastd {
     unsigned char  f;
     unsigned char  a;
     unsigned short  d;
};

/*    structure for CAMAC readout using ORNL AUX                            */
struct fcam_io  {
                  int cnaf;
       unsigned short id;
       unsigned short dir;
};

/*    structure for CAMAC special modules using ORNL AUX                    */
struct fcam_ro_lst {
          int  cnaf;
          char c;
          char n;
          char mod_type;
          char dum;
};


/*
*       Local variables
*/
static int                  fb_error;
static int                  fb_module;
static int                  fb_nta;
static char                 error_msg[81];
static unsigned short       Initial_cma = 0x20;
static unsigned short       Current_cma;
static int                  uncond_cnt;
static struct cnafdat_list   *tmp_camac;
static struct cnaf_list      *tmp_cnaf;
static struct cond_cam_pgm   *tmp_cond;
static struct raw_gate_spec  *tmp_gate;
static int                   *tmp_ids;
static int                   Faux[18];

static struct cnaf_list      CNAFnew[2000];
static int                   IDnew[2000];
static struct cond_cam_pgm   Cpgm[500];

static int                   crates_list[18];
static int                   xia_crates_list[NUM_XIA+1];
static int                   num_crates;

/*
*    Variables for event processing routines
*/
static int                  ACQ_initialized = 0;
static struct cond_tbl      *Cond_RO_str, *cond_ro, Cond_RO[500];
static struct fcond_tbl     *Fcond_RO_str, *fcond_ro, Fcond_RO[500];
static struct cam_io        *Cond_cam_str, *cond_cam, Cond_Cam[2000];
static struct fcam_io       *Fcond_cam_str, *fcond_cam, Fcond_Cam[500];
static struct cam_io        *Uncond_RO_str, *uncond_ro, Uncond_RO[500];
static struct fcam_io       *Funcond_RO_str, *funcond_ro, Funcond_RO[500];
static unsigned short       Uncond_cma;
static unsigned short       *Seq_ID_str, *seq_id, Seq_ID[400];
static unsigned short       Seq_cma,seq_transfers;
static unsigned short       seq_buf[400];
static struct cam_io        *Windup_str, *windup, WindUp[400];
static struct fcam_io       *Fwindup_str, *fwindup, FwindUp[400];
static unsigned short       Windup_cma;
static unsigned short       gate_ro_cma;
static struct count_dwn_lst *Count_dwn_str,Count_dwn_tst[50];
static struct cam_ro_lst    *Cam_RO_str,*Cam_RO_end,Cam_RO[100];
static struct fcam_ro_lst   *Fcam_RO_str,Fcam_RO[100];
static struct gate_ro_lst   *Gate_RO_str,Gate_RO[100];
static struct cal_gate_lst  *Cal_RO_str,Cal_Gate_RO[100];
static unsigned short       start_list_cma1;
static unsigned short       start_list_cma2;
static int                  xia_flag = 0;
static int                  xia_startup_cma,xia_restart_cma,xia_poll;

static int               kill_count;
static struct kill_list  Kill_List[30];

/*
*   The first latch_data word is very special and should be
*   initialized to 0xffff.   The real latch data starts at
*   Latch_data[1].
*/
static unsigned short  Latch_flag;     /* 0 means no latches, 1 means latches */
static unsigned short  Latch_cma;
static unsigned short  Latch_aux;
static unsigned short  Latch_data[50];
static unsigned short  Latch_ro[50];
static struct fcam_io  Flatch_ro[50];
static unsigned short  Fast_id[65536];
static unsigned short  Fera_id[8192];
static unsigned short  Fera_types[256];
static unsigned short  Cam_id[8192];
static unsigned short  Caen_adc_id[816];
static unsigned short  Caen_tdc_id[408];
static unsigned short  Caen_qdc_id[408];
static unsigned short  SIS_scl_id[192];
static unsigned short  Myriad_id[3]={0,0,0};

static int vme_enable;
static int vme_delay;
static struct vme_readout caen_775;
static struct vme_readout caen_785;
static struct vme_readout caen_792;
static struct vme_readout sis_3820;
static struct vme_readout myriad;

static int fera_enable;
static int ces_early_enable;
static int fera_delay;
static struct fera_readout baklash;
static struct fera_readout baklash2;
static struct fera_readout baklash3;
static struct fera_readout lrs_4300;
/*      Added support for ORTEC AD413 module.   (JJK, Jan. 29, 1997) */
static struct fera_readout ad_413;
/*      End of added support for ORTEC AD413 module.   (JJK, Jan. 29, 1997) */
static struct fera_readout lrs_3377;
static struct fera_readout gan_812f;
static struct fera_readout silena_4418;

static int fb_delay;
static struct fb_readout fb_1885;
static struct fb_readout fb_10C6;
static struct fb_readout fb_1881M;
static struct fb_readout fb_1877; /* RLV added for 1877 */
static struct fb_readout fb_1875;

static int fb_1885_count;
static int fb_10C6_count;
static int fb_1881M_count;
static int fb_1877_count; /* RLV added for 1877 */
static int fb_1875_count;

static int cam_special_delay;
static struct cam_readout phil_7164;
static struct cam_readout phil_7186;
static struct cam_readout lrs_2277;
static struct cam_readout lrs_3377C;
static struct cam_readout lrs_4300C;
/*      Added support for ORTEC AD413 module.   (JJK, Jan. 23, 1997) */
static struct cam_readout ad_413C;
/*      End of added support for ORTEC AD413 module.   (JJK, Jan. 23, 1997) */
static struct cam_readout silena_4418C;
static struct cam_readout silena_4418C;

static int cam_Cond_delay;
static int cam_Uncond_delay;

static int (*camac_seq[10])(void);
static int (*fb_seq[10])(void);
static int (*fera_seq[10])(void);
static int (*kill_seq[10])(void);

/*
*    List of pointers to LRS1190 FERA interface modules
*/

struct LRS1190 *lrs_intf[5];

/*
*    List of pointers to CAEN ADC, TDC and QDC modules
*/

static struct caen_readout caen_adcs[24];
static struct caen_readout caen_tdcs[12];
static struct caen_readout caen_qdcs[12];

/*
 *    Pointer to SIS 3820 module
 */
static struct sis_readout sis_ro[2];

/*
 *    Pointer to MYRIAD module
 */
static struct myr_readout myriad_ro;

/*
*    VME 100Hz clock
*/
       unsigned short clk100[2] = {0,0};
static            int clk100_flg;
static unsigned short clk_id_hi;
static unsigned short clk_id_low;

/*
*     Data buffers and pointers
*/
static unsigned short *Event;
static unsigned short *Startevt;
static struct data_buf *AcqBuf,*NewBuf;
static struct data_buf Buf1,Buf2;

/*
*     Trigger module timer interrupt function code
*/
static int Trig_Timer_Flag = 0;

/*
*    Data in the memory segment  "Acq_Params"
*/
static struct acq_share *share = (struct acq_share *)ACQ_SHARED;

/*
*    Device table
*/
static struct devices *devtbl = &DEVTBL;

/*
*    KSC2917 pointers
*/

static struct KSC_VME *ksc1_ptr = NULL;
static struct KSC_VME *ksc2_ptr = NULL;

#ifdef  TIMER
static unsigned char timer[32];

/*
*   Elapsed time measurements.  It is assumed that the Event interrupt
*   starts the timer countdown from a value of 255. (1 usec steps)
*/
#define Tlatch        0		/* time when latch readout starts         */
#define Tkill         1		/* time when Kill function starts         */
#define Tuncond       2         /* time at start of unconditional CAMAC   */
#define Tcond         3		/* time at start of conditional CAMAC     */
#define Trdseq        4         /* time at start of 3982 list seq readout */
#define Twind         5		/* time at start of Windup execution      */

#define Ttdcrd        6         /* time at start of FASTBUS TDC read      */
#define Ttdctrans     7         /* time at start of TDC data trans to 1131 */
#define Ttdcform      8		/* time at start of TDC data format       */
#define Tadcrd        9		/* time at start of FASTBUS ADC read      */
#define Tadctrans     10	/* time st start of ADC data trans to 1131 */
#define Tadcform      11	/* time at start of ADC data format       */
#define Tferard       12        /* time at start of FERA readout          */
#define Tseqnop       13        /* time at start of FERA nop routine      */
#define Tferafmt      14        /* time at start of FERA format           */
#define Tferafmtend   15        /* time at end of FERA format             */
#define Tfmtseq       16        /* time at start of sequencer format      */
#define Tfmtseqend    17        /* time at end of sequencer format        */
#define Tend          18        /* time at end of event readout           */
#define Tendall       19        /* time at end of event                   */

#endif

/*************************    Function Prototypes    ************************/

static int  acq_stop(void);
static int  acq_start(void);
static int  acq_init(void);
static void buffer_setup(void);
static void init_readout(void);

static int  crates_on_line(void);
static void aux_crates(void);
static int  set_CAMAC_inhibit(void);
static int  remove_CAMAC_inhibit(void);
static int  cam_cond_setup(void);
static int  cam_cond_setdelay(void);
static int  cam_fcond_setup(void);
static int  cam_cond_getf(void);
static int  cam_cond_getksc(void);

static int  cam_cond_opt(int *);
static int  prior_ref(int);

static int  latch_setup(void);
static int  flatch_setup(void);
static int  kill_setup(void);
static int  windup_setup(void);
static int  fwindup_setup(void);
static int  cam_uncond_setup(void);
static int  cam_funcond_setup(void);
static int  init_cam_modules(void);
static int  init_fb_modules(void);
static int  init_fera_modules(void);
static int  init_vme_modules(void);

static int  count_down_setup(void);
static int  gate_list_setup(void);
static int  cam_special_setup(void);
static int  cam_fspecial_setup(void);
static int  cam_special_setdelay(void);
static int init_xia(void);
static int check_xia_mod(int,int);
static int  setup_cam_start_list(void);
static void xia_start(void);
static void run_start_list(void);

static void fb_exec(int );
static void fb_write_pdreg(int );
static unsigned char fb_read_pdreg(void);
// MCSQ had debug and comments around these
//#ifdef DEBUG
//static unsigned short fb_read_reg(int);
//static void fb_write_reg(int,unsigned short);
//static int  fb_read32(void);
//static void fblist(struct fastd *fast);
///#endif

static void fb_write32(int );
static int  fb_prim_addr(int ,int );
static void fb_sec_addr(int );
static void fb_getSS(char *,char );
static void fb_reset_seq(void);

static int  ksc_camio(struct camac *);
static void fastio(struct fastd *);

void host_message(int ,char *,char *);

#ifdef  DEBUG
static int  camac_list(unsigned short ,struct cam_io *);
static void clist(struct cnafdat_list *,int);
#endif

extern void byte_swap(unsigned char *,int);
extern unsigned int  word_swap(unsigned short *,int);

/*
*   The following routines are executed when the acquisition event
*   interrupt occurs.
*/
static void process_event(void *, unsigned long);
static void trig_timer(void *, unsigned long);
static int  send_event(void);
static int  run_uncond_list(void);
static int  run_funcond_list(void);
static int  run_windup_list(void);
static int  run_fwindup_list(void);
static int  read_list_seq(void);
static int  format_list_seq(void);
static int  run_cond_list(void);
static int  run_fcond_list(void);
static int  fb_wait_done(void);
static int  fb_format_1885(void);
static int  fb_read_1885(void);
static int  fb_transfer_1885(void);
static int  fb_format_10C6(void);
static int  fb_read_10C6(void);
static int  fb_transfer_10C6(void);
static int  fb_format_1881M(void);
static int  fb_read_1881M(void);
static int  fb_transfer_1881M(void);
/*      RLV added for 1877     */
static int  fb_format_1877(void);
static int  fb_read_1877(void);
static int  fb_transfer_1877(void);
/*      End of 1877            */
static int  fb_format_1875(void);
static int  fb_read_1875(void);
static int  fb_transfer_1875(void);
static int  fera_read(void);
static int  ces_fera_format(void);
static int  lrs_fera_format(void);
static int  vme_format(void);
static int  run_kill(void);
static int  seq_nop(void);
static int  run_count_down(void);
static int  run_gate_list(void);
static int  run_cam_special(void);
static int  run_fcam_special(void);
static int  poll_xia(void);
static int  XIA_format(void);


/****************************************************************************
*
*   Command Processor
*
*  The host controls the acquisition via messages.  Control messages include:
*
*  1) Initialize - Initialize executes the setup routines which build the
*                  run-time tables for the readout routines.  You must
*                  initialize each time the PAC changes.  Changes to the
*                  PAC  DO NOT take effect until you initialize the 
*                  acquisition process.
*
*  2) Start      - Start data acquisition.  You can start acquisition only
*                  if initialization has been done and no errors were 
*                  detected in the initialization process.
*
*  3) Stop       - Stop data acquisition.  Initialization cannot not be
*                  done unless acquisition is stopped.
*
*  4) status     - Returns the run/stop status of the acquisition.
*
*  5) pacfile    - Returns the name of the pacfile.
*
*  6) host       - Returns the Ethernet address of the workstation
*                  which receives data and messages. This is the 
*                  workstation which did the last initialize command.
*
*  Errors:  Many error codes can be returned to the host.  For example,
*           a Start command when the acquisition is running returns an
*           error.  The file  acq_ctl.h contains a list of valid error
*           codes.
****************************************************************************/
void VMEacq(void)
{
   static struct sockaddr_in cli_addr,serv_addr;
   socklen_t   clilen;
   int   sockfd;
   static struct UDP_Packet in_buf,out_buf;
   struct trig *trigger = (struct trig *)TRIGGER;
   int   len,status,size;
   unsigned char  *cptr,*cptr1;
   char  *inbuf,*outbuf;
   struct tbl_index *tblptr;
   rtems_status_code sc;

   sockfd = socket(AF_INET,SOCK_DGRAM,0);
   if (sockfd == -1) {perror("cnafxx - socket error"); exit(1);}
   memset((char *) &serv_addr,0,sizeof(serv_addr));
   serv_addr.sin_family = AF_INET;
   serv_addr.sin_port = htons(45000+PROTO_FECNTRL);
   serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
   status = bind(sockfd,(struct sockaddr *)&serv_addr,sizeof(serv_addr));
   if (status == -1) {perror("VMEacq - bind"); exit(1);}

   clilen = sizeof(cli_addr);

   share->acqrun = 0;
   share->FB_enabled = 0;
   share->FB_error = 0;
   share->KSC3982_enabled = 0;

/*
*   Initialize trigger module if present
*/
   if (devtbl->trigger)
     {
      trigger->imra = 0;
      trigger->imrb = 0;
      trigger->iera = 0;
      trigger->ierb = 0;
      trigger->gpip = 0;
      trigger->ddr = ORNL_CLEAR | ORNL_BUSY | ORNL_STOPPED;
      trigger->gpip = ORNL_STOPPED;
      trigger->event_clear = 0;
      trigger->vr = (ORNL_EVT_VEC/16)*16;
      trigger->aer = 0x80;
      eieio();
      BSP_disableVME_int_lvl(ORNL_VME_LVL);
      sc = BSP_installVME_isr(ORNL_TIM_VEC, trig_timer,0);
      if(sc == RTEMS_SUCCESSFUL) printf("TIM_ISR installed\n");
      else if(sc == RTEMS_INVALID_NUMBER) printf("Invalid vector number! %i\n",
               ORNL_TIM_VEC);
      else if(sc == RTEMS_INVALID_ADDRESS) printf("Invalid ISR entry\n");
      else   {printf ("Can't install EVT_ISR; %s\n", rtems_status_text (sc));}
      sc = BSP_installVME_isr(ORNL_EVT_VEC, process_event,0);
      if(sc == RTEMS_SUCCESSFUL) printf("EVT_ISR installed\n");
      else if(sc == RTEMS_INVALID_NUMBER) printf("Invalid vector number! %i\n",
               ORNL_EVT_VEC);
      else if(sc == RTEMS_INVALID_ADDRESS) printf("Invalid ISR entry\n");
      else   {printf ("Can't install EVT_ISR; %s\n", rtems_status_text (sc));}
      BSP_enableVME_int_lvl(ORNL_VME_LVL);
     }

    if (devtbl->ksc2917a) ksc1_ptr = (struct KSC_VME *)KSC1;
    if (devtbl->ksc2917b) ksc2_ptr = (struct KSC_VME *)KSC2;

   while(1)
    {
      size = recvfrom(sockfd,&in_buf,sizeof(in_buf),0,
                                   (struct sockaddr *)&cli_addr,&clilen);
      if (size < 0)
        {
          fprintf(stderr,"\nEthernet read error\n");
          exit(0);
        }
      inbuf = (char *)in_buf.Data;
      outbuf = (char *)out_buf.Data;

      *outbuf = *(outbuf+1) = 0;
      len = 2;
      switch (*inbuf)                   /* dispatch on function code       */
       {

         case  INIT_ACQ:          /* Build run-time tables from loaded PAC */
          share->Host_Ether_Adr = cli_addr;
          if (share->acqrun == 0) *outbuf = acq_init();
          else  *outbuf = ACQ_INIT_RUN;
          break;
         case  START_ACQ:         /* Start data acquisition                */
          *outbuf = acq_start();
          if (*outbuf == ACQ_OK) share->acqrun = 1;
          break;
         case  STOP_ACQ:          /* Stop data acquisition                 */
          if (share->acqrun != 0) *outbuf = acq_stop();
          else  *outbuf = ACQ_STP_HALT;
          if (*outbuf == ACQ_OK) share->acqrun = 0;
          break;
         case  STATUS_ACQ:        /* Return acquisition status             */
          *outbuf = ACQ_OK;
          if (ACQ_initialized == 0) *(outbuf+1) = ACQ_UNINIT;
          else if (share->acqrun == 1) *(outbuf+1) = ACQ_RUN;
          else  *(outbuf+1) = ACQ_STOP;
          break;
         case  PAC_FILE:
          *outbuf = ACQ_OK;
          if (ACQ_initialized == 0) *(outbuf+1) = ACQ_UNINIT;
          tblptr = (struct tbl_index *)ACQ_RAM + INDEX_FILENAME;
          if (tblptr->length*4 != 80)
            {
              *outbuf = ACQ_INVALID_TABLE;
              break;
            }
          cptr = (unsigned char *)( ACQ_RAM + tblptr->offset * 4);
          if (*(cptr+tblptr->length*4-1) != 0xff) *(outbuf+1) = ACQ_UNINIT;
          if (*cptr == 0)
            {
              *(outbuf+2) = 0;
              len = 3;
            }
          else
            {
              cptr1 = cptr + tblptr->length*4 -3;
              for (;cptr1 > cptr; cptr1--)
                {
                  if (*cptr1 != 0x20)
                    {
                      cptr1++;
                      break;
                    }
                }
              len = cptr1-cptr;
              strncpy(outbuf+2,(char *)cptr,len);
              *(outbuf+2+len) = 0;
              len += 3;
            }
          break;
         case  HOST:
          *outbuf = ACQ_OK;
          *(outbuf+1) = 0;
          *((struct in_addr *)(outbuf+2)) = share->Host_Ether_Adr.sin_addr;
          len = 6;
          break;
         case  ZERO_CLK:
          *outbuf = ACQ_OK;
          clk100[0] = clk100[1] = 0;
          len = 1;
          break;
         default:
          *outbuf = ACQ_UNKNOWN_COMMAND;
          break;
       }
/*
*     Send command response
*/
     out_buf.DataSize = len;
     word_swap((unsigned short *)&out_buf.DataSize,2);
     byte_swap((unsigned char *)&out_buf.DataSize,4);
     out_buf.Sequence = in_buf.Sequence;
     len = len + PKTHDRLEN;
     status=sendto(sockfd,&out_buf,len,0,(struct sockaddr *)&cli_addr,clilen);
     if (status < 0) {
       perror("VMEacq - error at sendto");
     }
    }
}
/****************************************************************************
*
*   STOP Data Acquisition.
*
*   1) If acquisition is already stopped, just return error code.
*
*   2) Disable the event interrupt.
*
*   3) Set INHIBIT in all CAMAC crate controllers.  This should stop counting
*      in all CAMAC scalers.
*   
*   4) Send remainder of the data, if any, to the data format/transmission
*      process.
*
*   5) Send a special buffer(zero length) to the data format/transmission
*      process.  This causes a special Ethernet packet to be sent to the
*      host indicating that acquisition is really stopped.
*
*   6) Send message to the host logger process showing the number of
*      next event.
****************************************************************************/
static int acq_stop(void)
{
   register char  csr,*cptr;
   int   size;
   struct trig *trigger = (struct trig *)TRIGGER;
   struct FCAM *fcam = (struct FCAM *)ORNLAUX;
   struct SIS3820 *sis = (struct SIS3820 *)SIS3820_1;
   rtems_status_code sc;

   if (!devtbl->trigger) return(ACQ_STP_HALT);

   cptr = *((char **)(255*4));
   csr = trigger->iera;
   trigger->iera = csr & ~EVT_INTR_ENA;
   eieio();
   if (csr & EVT_INTR_ENA)
     {
/*
*   Set INHIBIT in all CAMAC crates to prevent scalers from counting
*/
       set_CAMAC_inhibit();
/*
*   Disable counting for the SIS3820 module.
*/
       if (devtbl->sis3820_1) {sis->Key_disable = 0; sis->op_mode = 1;}
/*
*    Reset FIFO in ORNL CAMAC interface
*/
       if (devtbl->ornlaux) fcam->cmd = RS_FIFO;
/*
*   If acquisition was enabled,  send remainder of data plus any 
*   special stop stuff.
*/
       if (NewBuf == NULL)
         {
           size = (Event - AcqBuf->Bufhdr.str_buf) * sizeof(unsigned short);
           if (size != 0)
             {
               AcqBuf->Bufhdr.totalevents = share->event_number;
               share->event_number += AcqBuf->Bufhdr.events;
               AcqBuf->Bufhdr.end_buf = Event;
               AcqBuf->Bufhdr.busy = -1;

               Que1buf = AcqBuf;
               while(1)
                 {
                   sc = rtems_message_queue_send(Que1id, &Que1buf, 4);
                   if (sc == RTEMS_SUCCESSFUL) break;
                   rtems_task_wake_after(1);
                 }
               while(1)
                 {
                   rtems_task_wake_after(1);
                   if (AcqBuf->Bufhdr.busy != -1) break;
                 }
             }
         }
       else  {while(NewBuf != NULL) rtems_task_wake_after(1);}

/*
*   Send special stop acquisition buffer - zero length buffer.  This causes
*   special Ethernet packet to be sent to host.
*/
       AcqBuf->Bufhdr.totalevents = share->event_number;
       AcqBuf->Bufhdr.events = 0;
       AcqBuf->Bufhdr.busy = -1;
       AcqBuf->Bufhdr.end_buf = AcqBuf->Bufhdr.str_buf;

       Que1buf = AcqBuf;
       while(1)
         {
           sc = rtems_message_queue_send(Que1id, &Que1buf, 4);
           if (sc == RTEMS_SUCCESSFUL) break;
           rtems_task_wake_after(1);
         }
       while(AcqBuf->Bufhdr.busy != 0) rtems_task_wake_after(1);

       Event = AcqBuf->Bufhdr.str_buf;

/*
*   Send message to acquisition logger process on the host indicating the
*   event number at stop.
*/
       sprintf(error_msg,"Stop VME acquisition.  Event Number = %u",
                                                           share->event_number);
       host_message(INFORM,error_msg,"VMEacq  ");

       trigger->iera = 0;
       trigger->imra = 0;
       trigger->gpip = ORNL_STOPPED;
       eieio();
       return(ACQ_OK);
     }
   return(ACQ_STP_HALT);
}
/****************************************************************************
*
*   START Data Acquisition.
*
*  1) If the acquisition process has never been initialized or the last
*     attempted initialization resulted in an error, return error code
*     to host.
*
*  2) If acquisition is already running, return error code.
*
*  3) Remove INHIBIT in all CAMAC crates so scalers will count and ADC/TDCs
*     will convert.
*
*  4) Initialize the event interrupt module and it's 68040 interrupt
*     vector.
*
*  5) Send message to the host logger process showing the number of
*     next event.
****************************************************************************/
static int acq_start(void)
{
   struct trig *trigger = (struct trig *)TRIGGER;
   struct FCAM *fcam = (struct FCAM *)ORNLAUX;
   struct LRS1190 **lrs_mod;
   struct hsm *hsm_ptr = (struct hsm *)CES1;
   struct SIS3820 *sis = (struct SIS3820 *)SIS3820_1;
   int  status;
   int  err;


/*
*    We can start only if a PAC has been initialized.
*/
   printf("In acq_start\n");
   if (ACQ_initialized != -1) return(ACQ_STR_NOINIT);

   status = trigger->iera;
   if (status) return(ACQ_STR_RUN);

   NewBuf = NULL;

   if ((status = crates_on_line())) return(status);
   lrs_mod = lrs_intf;
   while(*lrs_mod != NULL)
     {
       (*lrs_mod)->dat[0] = 0;
       lrs_mod++;
     }
   if ((err = remove_CAMAC_inhibit())) return(err);

/*
*   Enable counting for the SIS3820 module.
*   1 - nonclearing mode
*   10 - select scaler 1 to load next event
*   000 - select LNE front panel signal source
*   0000 - FIFO mode
*   30000 - select ctl in 1 for LNE, 4 to inhibit counting
*   200000 - select ctl out 5 is LNE pulse; 6,7 10MHz; 8 - user LED state
*/
   if (devtbl->sis3820_1) {
      sis->Key_enable = 0; 
//     sis->op_mode = 1;
      sis->op_mode = 0x00230011; 
      eieio();
   }

   /* Enable the CES8170                   */
   if (devtbl->ces8170 & ces_early_enable)
     {
       hsm_ptr->ctrl = 0;
       hsm_ptr->mem_ptr = 0;
       hsm_ptr->wrd_cnt = 0x1000;
       hsm_ptr->ctrl = 0x1000;
     }

   if (devtbl->ornlaux) fcam->cmd = RS_FIFO;

   if (start_list_cma1 || start_list_cma2) run_start_list();

   if (xia_flag) xia_start();

   trigger->iera = EVT_INTR_ENA | TIMER_INTR_ENA;
   trigger->imra = EVT_INTR_ENA;
   trigger->gpip = 0;
   trigger->event_clear = 0;
   eieio();
   sprintf(error_msg,"Start VME acquisition. Event Number = %u",
                                                          share->event_number);
   host_message(INFORM,error_msg,"VMEacq  ");
   return(ACQ_OK);
}
/****************************************************************************
*
*   Initialization of the data acquisition system.  This function builds
*   the run-time tables required for data acquisition from the PAC currently
*   loaded.  Any errors send messages to the host and also prevent start of
*   data acquisition.
****************************************************************************/
static int  acq_init(void)
{
                int i,j,rdcnt;
     unsigned short *uptr;
		int status;
		int *iptr;
   struct tbl_index *tblptr;
    struct mod_type *mods;
        struct FCAM *fcam = (struct FCAM *)ORNLAUX;
struct cnafdat_list *naf1,*naf2;
struct raw_gate_spec *gspec1,*gspec2;
      unsigned char *cptr;
	static char *table_names[] = {
	  "FILENAME","CRATES","CNAF_INIT",
	  "CAMAC_MODULES","FASTBUS_MODULES",
	  "FERA_MODULES","LATCHES","GATE_READ",
	  "RAW_GATE_SPEC","CAL_GATE_SPEC",
	  "COUNT_DOWN","KILL_LIST",
	  "UNCOND_RO","COND_RO","CNAF_LIST",
	  "ID_LIST","CAMAC_RO","FASTBUS_RO",
	  "FERA_RO","CAMAC_ID", "FASTBUS_ID",
	  "FERA_ID","WINDUP_CNAF","RUN_CNAF_LIST",
	  "XIA_LIST",
	  "CAEN_RO", 
	  "CAEN_ADC_HARD", "CAEN_TDC_HARD", "CAEN_QDC_HARD",
	  "CAEN_ADC_ID", "CAEN_TDC_ID", "CAEN_QDC_ID",
	  "CLK100", "SIS_SCL_HARD", "SIS_SCL_ID", "MYRIAD" };
		int invalid = 0;
                int sum_len = 0;


   ACQ_initialized = 0;
/*
*    Check all tables.  Offsets, lengths and delays must be within
*    reasonable ranges.
*/
   tblptr = (struct tbl_index *)ACQ_RAM;
   for(i=0; i < 37; ++i)
    {
      status = 0;
      if ((tblptr->offset < 0) || (tblptr->length < 0)) status = 1;
      else if (tblptr->offset + tblptr->length > ACQ_MAX_RAM) status = 1;      
      else if (tblptr->delay > ACQ_MAX_DELAY) status = 1;
      else if (tblptr->delay < 0) status = 1;
      if (status)
	{
	  sprintf(error_msg,"Invalid %s parameter table",table_names[i]);
	  host_message(PANIC,error_msg,"VMEacq  ");
	  invalid = 1;
	}
      sum_len += tblptr->length;

#ifdef  DEBUG
      sprintf(error_msg,"(%d) off = %x, len = %d, dly = %x %s",
                 i,  (unsigned int)tblptr->offset,tblptr->length,
                                 tblptr->delay,table_names[i]);
      host_message(INFORM,error_msg,"VMEacq  ");
#endif
      tblptr++;
    }
   if (!sum_len)  return(ACQ_INVALID_TABLE);
   if (invalid)  return(ACQ_INVALID_TABLE);

/*
*  Fix file name storage, if needed.
*/
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_FILENAME;
   if (tblptr->length*4 != 80) return(ACQ_INVALID_TABLE);
   cptr = (unsigned char *)( ACQ_RAM + tblptr->offset * 4);
   if (*(cptr+tblptr->length*4-1) != 0xff)
     {
       byte_swap((unsigned char *)cptr,tblptr->length*4);
       word_swap((unsigned short *)cptr,tblptr->length*2);
       cptr = cptr +tblptr->length*4 -2;
       *cptr++ = 0;
       *cptr = 0xff;
     }

   Current_cma = Initial_cma;	/* Set start of lists in KSC2917 */

   if (devtbl->ornlaux) fcam->cmd = RS_FIFO | RS_CAM;

/*
*    Save the list of used CAMAC crates and check all specified CAMAC
*    crates.  All such crates must be On-Line.
*/
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CRATES;
   iptr = (int *)( ACQ_RAM + tblptr->offset * 4);
   num_crates = tblptr->length;
   if ((num_crates != 0) && (!devtbl->ksc2917a)) return(ACQ_NO_KSC2917);
   for (i=0; i < num_crates; i++) crates_list[i] = *iptr++;
   if ((status = crates_on_line())) return(status);
   aux_crates();
/*
*   Set CAMAC Inhibit in all crates.
*/
   set_CAMAC_inhibit();
/*
*    Do the CAMAC initialization functions specified in the PAC
*/
   if ((status = init_cam_modules())) return(status);
/*
*    Build the start acquisition list
*/
   if ((status = setup_cam_start_list())) return(status);
/*
*   Clear all FASTBUS modules.  Clear action depends on module type.
*   Hence and an unrecognized module type is an unrecoverable error.
*/
   if ((status = init_fb_modules())) return(status);
/*
*   Clear and initialize all FERA readout modules.
*/
   if ((status = init_fera_modules())) return(status);

/*
*   Copy the camac unconditional readout list to the heap.  The latch setup
*   routine looks for a second read of the latch in the unconditional list.
*   If found, the entry in the unconditional list is marked invalid
*   by setting c=255.  The cam_uncond_setup routine then ignores entries
*   which have c=255.
*/
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_UNCOND_RO;
   naf1 = (struct cnafdat_list *)(ACQ_RAM + tblptr->offset * 4);
   uncond_cnt = tblptr->length;
   tmp_camac = calloc(tblptr->length,sizeof(struct cnafdat_list));
   naf2 = tmp_camac;
   for (i=0; i < tblptr->length; i++) *naf2++ = *naf1++;
/*
*   Setup Latch readout list in KSC2917 and AUX controllers.
*/
   Latch_flag = 0;
   if ((status = flatch_setup())) return(status);
   if ((status = latch_setup())) return(status);
/*
*   Setup unconditional readout of CAMAC modules.   Use AUX crate controllers
*   if available.  Put what we can in the KSC3982 list sequencer.  The
*    remainder goes to the KSC2917.
*/
   if ((status = cam_funcond_setup())) return(status);
   if ((status = cam_uncond_setup())) return(status);
   free(tmp_camac);
/*
*   Setup the Windup CNAF list.
*/
   tblptr = (struct tbl_index *)ACQ_RAM +INDEX_WINDUP_CNAF ;
   naf1 = (struct cnafdat_list *)( ACQ_RAM + tblptr->offset * 4);
   tmp_camac = calloc(tblptr->length,sizeof(struct cnafdat_list));
   naf2 = tmp_camac;
   for (i=0; i < tblptr->length; i++) *naf2++ = *naf1++;
   if ((status = fwindup_setup())) return(status);
   if ((status = windup_setup())) return(status);
   free(tmp_camac);
/*
*   Setup readout for special CAMAC modules
*/
   if ((status = cam_fspecial_setup())) return(status);
   if ((status = cam_special_setup())) return(status);
   cam_special_setdelay();

/*
*   Copy the raw gate list to the heap sorted in order of the CAMAC gate 
*   read table.
*/
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_GATE_READ;
   rdcnt = tblptr->length;
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_RAW_GATE_SPEC;
   tmp_gate = calloc(tblptr->length,sizeof(struct raw_gate_spec));
   gspec2 = tmp_gate;
   for (i=1; i <= rdcnt; i++)
     {
       gspec1 = (struct raw_gate_spec *)(ACQ_RAM + tblptr->offset * 4);
       for (j=0; j < tblptr->length; j++)
         {
           if (gspec1->rdindx == i) *gspec2++ = *gspec1;
           gspec1++;
         }
     }
/*
*   Setup raw and calculated gates
*/
   status = gate_list_setup();
   free(tmp_gate);
   if (status) return(status);
/*
*   Setup countdown tests
*/
   if ((status = count_down_setup())) return(status);

/*
*   Convert FASTBUS_ID, FERA_ID and CAMAC_ID tables from int arrays to 
*   unsigned short arrays.
*/
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_FASTBUS_ID;
   iptr = (int *)( ACQ_RAM + tblptr->offset * 4);
   uptr = Fast_id;
   for(i=0; i < 65536; i++) Fast_id[i] = *(iptr + 31*256);
   uptr = Fast_id;
   for(i=0; i < 32; i++)
     {
       for(j=0; j < 256; ++j) *uptr++ = *iptr++;
       uptr += 1792;
     }
/*
*   Fill table with default FERA ID
*/
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_FERA_ID;
   iptr = (int *)( ACQ_RAM + tblptr->offset * 4);
   iptr += 31*32;
   uptr = Fera_id;
   for(i=0; i < 8192; ++i) *uptr++ = *iptr;
/*
*   Move FERA IDs to table in the order of the FERA modules
*/
   uptr = Fera_id + 32;
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_FERA_MODULES;
   mods = (struct mod_type *)( ACQ_RAM + tblptr->offset * 4);
   rdcnt = tblptr->length;
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_FERA_ID;
   iptr = (int *)( ACQ_RAM + tblptr->offset * 4);
   while (rdcnt)
    {
      i = mods->c * 32 + mods->n -1;
      for (j=0; j < 32; j++) *uptr++ = *(iptr + i * 32 + j);
      rdcnt--;
      mods++;
    }

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CAMAC_ID;
   iptr = (int *)( ACQ_RAM + tblptr->offset * 4);
   uptr = Cam_id;
   for(i=0; i < 8192; ++i) *uptr++ = *iptr++;

/*
*   Move CAEN ADC, TDC and QDC ID tables
*/

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CAEN_ADC_ID;
   iptr = (int *)( ACQ_RAM + tblptr->offset * 4);
   uptr = Caen_adc_id;
   for(i=0; i < 816; ++i) *uptr++ = *iptr++;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CAEN_TDC_ID;
   iptr = (int *)( ACQ_RAM + tblptr->offset * 4);
   uptr = Caen_tdc_id;
   for(i=0; i < 408; ++i) *uptr++ = *iptr++;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CAEN_QDC_ID;
   iptr = (int *)( ACQ_RAM + tblptr->offset * 4);
   uptr = Caen_qdc_id;
   for(i=0; i < 408; ++i) *uptr++ = *iptr++;

   /*
    *  Copy the SIS scaler id table
    */
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_SIS_ID;
   iptr = (int *)( ACQ_RAM + tblptr->offset * 4);
   uptr = SIS_scl_id;
   for(i=0; i < 192; ++i) *uptr++ = *iptr++;
   /*
    *  Copy the Myriad clock ids
    */
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_MYR_ID;
   iptr = (int *)( ACQ_RAM + tblptr->offset * 4);
   uptr = Myriad_id;
   for(i=0; i < 3; ++i) *uptr++ = *iptr++;

/*
*   Setup the VME 100Hz clock
*/

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CLK_ID;
   iptr = (int *)( ACQ_RAM + tblptr->offset * 4);
   clk100_flg = tblptr->length;
   clk_id_hi = *iptr++;
   clk_id_low = *iptr;

/*
*   Clear and initialize all VME modules.
*/
   if ((status = init_vme_modules())) return(status);

#ifdef DEBUG
   host_message(INFORM,"RunTime FASTBUS ID table **************","VMEacq  ");
   sprintf(error_msg,"Fast_id = %p",Fast_id);
   host_message(INFORM,error_msg, "VMEacq  ");

   host_message(INFORM,"RunTime FERA ID table **************","VMEacq  ");
   sprintf(error_msg,"Fera_id = %p",Fera_id);
   host_message(INFORM,error_msg,"VMEacq  ");
#endif

/*
*   Setup the conditional readout
*/
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CNAF_LIST;
   tmp_cnaf = calloc(tblptr->length,sizeof(struct cnafdat_list));

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_ID_LIST;
   tmp_ids = calloc(tblptr->length,sizeof(int));

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_COND_RO;
   tmp_cond = calloc(tblptr->length,sizeof(struct cond_cam_pgm));

   cam_cond_getf();
   if ((status = cam_fcond_setup())) return(status);
   cam_cond_getksc();
   if ((status = cam_cond_setup())) return(status);
   cam_cond_setdelay();
   free(tmp_cond);
   free(tmp_ids);
   free(tmp_cnaf);
/*
*   Setup the Kill Event test list.
*/
   kill_setup();
/*
*   Setup XIA modules
*/
   if ((status = init_xia())) return(status);
/*
*   Setup readout sequence tables.
*/
   init_readout();
/*
*   Setup event buffers and initialize the Event interrupt I/O
*   module.
*/
   if (devtbl->trigger) buffer_setup();
   else  return(ACQ_NO_TRIGGER);

#ifdef TIMER
   for (i=0; i < sizeof(timer); i++) timer[i] = 0;
#endif
#ifdef  DEBUG
   sprintf(error_msg,"camac seq = %p",camac_seq);
   host_message(INFORM,error_msg,"VMEacq  ");
   sprintf(error_msg,"fb seq = %p",fb_seq);
   host_message(INFORM,error_msg,"VMEacq  ");
   sprintf(error_msg,"Event data 1 = %p, Event data 2 = %p",
                                    Buf1.Data,Buf2.Data);
   host_message(INFORM,error_msg,"VMEacq  ");

#ifdef TIMER
   sprintf(error_msg,"Timer array = %x",timer);
   host_message(INFORM,error_msg,"VMEacq  ");
#endif
#endif

   return(0);
}
/****************************************************************************
*
*   Initialize the event data buffers, the timer, and the interrupt
*   I/O module.  When finished, enable the command to start data
*   acquisition.
****************************************************************************/
static void buffer_setup(void)
{
   struct trig *trigger = (struct trig *)TRIGGER;

/*
*   Initialize data buffer headers
*/
   Buf1.Bufhdr.str_buf = Buf1.Data;
   Buf1.Bufhdr.end_buf = Buf1.Data;
   Buf1.Bufhdr.events = 0;
   Buf1.Bufhdr.busy = 0;
   Buf1.Bufhdr.ack = 0;
   Buf2.Bufhdr.str_buf = Buf2.Data;
   Buf2.Bufhdr.end_buf = Buf2.Data;
   Buf2.Bufhdr.events = 0;
   Buf2.Bufhdr.busy = 0;
   Buf2.Bufhdr.ack = 0;
  
/*
*   Initialize headers for Ethernet packets
*/
   Buf1.Head.Sequence = 0;
   Buf1.Head.DataSize = 0;
   Buf2.Head.Sequence = 0;
   Buf2.Head.DataSize = 0;
/*
*   Initialize buffer pointers
*/
   Event = Buf1.Data;
   AcqBuf = &Buf1;

   trigger->tacr = 6;           /* Set timer A for divide by 100 prescale */
   trigger->tadr = 2;           /* 50 us timer interval                  */
   trigger->tbcr = 6;           /* Set timer B for divide by 100 prescale */
   trigger->tbdr = 200;         /* 5 ms timer interval                    */
   trigger->imra = 0;
   trigger->imrb = 0;
   trigger->iera = 0;
   trigger->ierb = 0;
   trigger->vr = (ORNL_EVT_VEC/16)*16;
   trigger->aer = 0x80;
   trigger->gpip = 0;
   trigger->ddr = ORNL_CLEAR | ORNL_BUSY | ORNL_STOPPED;
   trigger->gpip = ORNL_STOPPED;
   eieio();
/*
*   Enable the command to start data acquisition.
*/
   ACQ_initialized = -1;

   sprintf(error_msg,"VME acquisition system initialized");
   host_message(INFORM,error_msg,"VMEacq  ");
   return;
}
/****************************************************************************
*
*   Build the function execution tables used in acquisition.  These tables
*   are pointers to functions.  The resulting tables determine in what
*   order and which device specific functions are executed during readout.
*   There is a table for each physical type of hardware interface (i.e.
*   CAMAC, FASTBUS and FERA readout devices).
****************************************************************************/
static void init_readout(void)
{
  int (**camac_pgm)(void) = camac_seq;
  int (**fb_pgm)(void) = fb_seq;
  int (**fera_pgm)(void) = fera_seq;
  int (**kill_pgm)(void) = kill_seq;
  int  i,j;
  struct {
      int type;
      int delay;
   } delays[3],tmp;

  for(i=0; i < sizeof(camac_seq)/4; ++i) camac_seq[i] = NULL;
  for(i=0; i < sizeof(fb_seq)/4; ++i) fb_seq[i] = NULL;
  for(i=0; i < sizeof(fera_seq)/4; ++i) fera_seq[i] = NULL;
  for(i=0; i < sizeof(kill_seq)/4; ++i) kill_seq[i] = NULL;

/*
*   Build the CAMAC execution table.  Build sequence in delay time order.
*/
  delays[0].type = 1;
  delays[0].delay = cam_Uncond_delay;
  delays[1].type = 2;
  delays[1].delay = cam_Cond_delay;
  delays[2].type = 3;
  delays[2].delay = cam_special_delay;
  for (i=0; i < 2; i++)
   {
     for (j=i+1; j < 3; j++)
      {
        if (delays[j].delay < delays[i].delay)
         {
           tmp = delays[i];
           delays[i] = delays[j];
           delays[j] = tmp;
         }
        else if (delays[j].delay == delays[i].delay)
         {
           if (delays[j].type == 1)
            {
              tmp = delays[i];
              delays[i] = delays[j];
              delays[j] = tmp;
            }
         }
      }
   }
  camac_pgm = camac_seq;
  for (i=0; i < 3; i++)
   {
     switch (delays[i].type)
      {
        case 1:
         if (Uncond_cma != 0) *camac_pgm++ = run_uncond_list;
         if (Funcond_RO_str != NULL) *camac_pgm++ = run_funcond_list;
         if (Seq_cma != 0) *camac_pgm++ = format_list_seq;
         break;
        case 2:      
         if (Cond_RO_str != NULL) *camac_pgm++ = run_cond_list;
         if (Fcond_RO_str != NULL) *camac_pgm++ = run_fcond_list;
         break;
        case 3:      
         if (Cam_RO_str != NULL) *camac_pgm++ = run_cam_special;
         if (Fcam_RO_str != NULL) *camac_pgm++ = run_fcam_special;
         break;
        default:
         break;
      }
   }
  if (xia_flag)
    {
      *camac_pgm++ = poll_xia;
      *camac_pgm++ = XIA_format;
    }

/*
*   Build the FASTBUS execution table
*/
  fb_pgm = fb_seq;
  if (fb_10C6.enable != 0)
    {
      *fb_pgm++ = fb_read_10C6;
      *fb_pgm++ = fb_transfer_10C6;
      *fb_pgm++ = fb_format_10C6;
    }
  if (fb_1881M.enable != 0)
    {
      *fb_pgm++ = fb_read_1881M;
      *fb_pgm++ = fb_transfer_1881M;
      *fb_pgm++ = fb_format_1881M;
    }
/*      RLV added block for 1877 support    */
  if (fb_1877.enable != 0)
    {
      *fb_pgm++ = fb_read_1877;
      *fb_pgm++ = fb_transfer_1877;
      *fb_pgm++ = fb_format_1877;
    }
/*      End of added 1877 block             */
  if (fb_1885.enable != 0)
    {
      *fb_pgm++ = fb_read_1885;
      *fb_pgm++ = fb_transfer_1885;
      *fb_pgm++ = fb_format_1885;
    }
  if (fb_1875.enable != 0)
    {
      *fb_pgm++ = fb_read_1875;
      *fb_pgm++ = fb_transfer_1875;
      *fb_pgm++ = fb_format_1875;
    }
/*
*   Build the FERA execution table
*/
  fera_pgm = fera_seq;
  if (vme_enable != 0)
    {
      *fera_pgm++ = vme_format;
    }
  if (fera_enable != 0)
    {
      if (devtbl->ces8170)
        {
          *fera_pgm++ = fera_read;
          if (delays[0].type != 1 && Seq_cma != 0)
            {
              *fera_pgm++ = seq_nop;
              if (delays[1].type != 1) *fera_pgm++ = seq_nop;
            }
          *fera_pgm++ = ces_fera_format;
        }
      else
        {
          *fera_pgm++ = seq_nop;
          if (delays[0].type != 1 && Seq_cma != 0)
            {
              *fera_pgm++ = seq_nop;
              if (delays[1].type != 1) *fera_pgm++ = seq_nop;
            }
          *fera_pgm++ = lrs_fera_format;
        }
    }
/*
*  Build the Kill execution table
*/
   kill_pgm = kill_seq;
   if (fb_10C6.enable != 0) *kill_pgm++ = fb_read_10C6;
   if (Windup_str != NULL) *kill_pgm++ = run_windup_list;
   if (Fwindup_str != NULL) *kill_pgm++ = run_fwindup_list;
   if (fb_10C6.enable != 0) *kill_pgm++ = fb_wait_done;
}

/****************************************************************************
*
*  The CRATES table is a list of all CAMAC crates specified for acquisition.
*  Check for each crate being On-Line.
****************************************************************************/
static int crates_on_line(void)
{
      struct camac cnaf = {0, 0, 0, 0, 0};
	      int  *crate,count,status;
              int  err = 0;

  crate = crates_list;
  count = num_crates;

  while(count--)
   {
     cnaf.c = *crate++;
     cnaf.n = 30;
     cnaf.a = 0;
     cnaf.f = 1;                            /* Read crate controller status */
     status = ksc_camio(&cnaf);
     if (status & KSC_CSR_TMO)
       {
	 sprintf(error_msg,"Crate %d does not exist!",cnaf.c);
	 host_message(PANIC,error_msg,"VMEacq  ");
         err = ACQ_CAM_NOEXIST;
	 continue;
       }
     if (cnaf.d & 0x2000)
       {
	 sprintf(error_msg,"Crate %d is off-line!",cnaf.c);
	 host_message(PANIC,error_msg,"VMEacq  ");
         err = ACQ_CAM_OFFLINE;
       }
   }
  return(err);
}
/****************************************************************************
*
*  The CRATES table is a list of all CAMAC crates specified for acquisition.
*  Look for an AUX controller in each crate.
****************************************************************************/
static void aux_crates(void)
{
      struct FCAM *fcam = (struct FCAM *)ORNLAUX;
      struct camac cnaf = {0,0,0,0,0};
              int  i;
              int  *crate,count,status;

  crate = crates_list;
  count = num_crates;

  for (i=0; i <= 18; i++) Faux[i] = -1;

  if (!devtbl->ornlaux) return;

  while(count--)
   {
     cnaf.c = *crate++;
     cnaf.n = 23;
     cnaf.a = 14;
     cnaf.f = 6;
     do
       {
         status = ksc_camio(&cnaf);
         status = status & (KSC_CSR_NOX | KSC_CSR_NOQ);
         if (status == 0)
           {
             if ((cnaf.d & 0xfff0) == 0xA0)
               {
/*
*  Found a module which looks like an AUX crate controller.  Write the crate
*  ID byte and then use the AUX interface to read the ID byte.  We
*  must get X=1 and Q=1 and the correct ID byte to verify we have a valid
*  AUX.
*/
                 cnaf.f = 22;
                 cnaf.d = cnaf.c;
                 ksc_camio(&cnaf);
                 fcam->dat.cnaf = CNAF(cnaf.c,cnaf.n,14,6);
                 eieio();
                 i = fcam->dat.l;
                 if ((i & (QMSK+XMSK)) != 0) break;
                 i = i & 0xff;
                 if (i != (0xA0 + cnaf.c)) break;
                 Faux[cnaf.c] = cnaf.c;
                 sprintf(error_msg,"AUX Controller found in Crate %d, Slot %d",
                                                                 cnaf.c,cnaf.n);
                 host_message(INFORM,error_msg,"VMEacq  ");
                 break;
               }
           }
         cnaf.n--;
       } while (cnaf.n >= 20);
     if (Faux[cnaf.c] != -1) continue;
   }
  return;
}
/****************************************************************************
*
*   Set the CAMAC Inhibit in each crate.  Function is called when data
*   acquisition stops.  INHIBIT prevents scalers from counting while
*   data acquisition is stopped.
****************************************************************************/
static int set_CAMAC_inhibit(void)
{
      struct camac cnaf = {0,0,0,0,0};
	      int  crate,status;

  if (!devtbl->ksc2917a) return(0);

  crate = ACQ_MAX_CRATE + 10;
  while(crate >= 0)
   {
     cnaf.c = crate--;
     cnaf.n = 30;
     cnaf.a = 0;
     cnaf.f = 1;                            /* Read crate controller status */
     status = ksc_camio(&cnaf);
     if (status & KSC_CSR_TMO) continue;    /* Nonexistent crate           */
     if (cnaf.d & 0x2000) continue;         /* Crate Off_line              */
     cnaf.f = 17;
     cnaf.d = 4;                            /* set inhibit                 */
     status = ksc_camio(&cnaf);
   }
  return(0);
}
/****************************************************************************
*
*   Remove CAMAC inhibit in each crate.  Function is called when data 
*   acquisition starts.  When INHIBIT is clear, scalers count and 
*   ADCs can do conversions.
****************************************************************************/
static int remove_CAMAC_inhibit(void)
{
      struct camac cnaf = {0,0,0,0,0};
	      int  crate,status;
              int  err = 0,i;

  crate = ACQ_MAX_CRATE + 10;
  while(crate >= 0)
   {
     cnaf.c = crate--;
     cnaf.n = 30;
     cnaf.a = 0;
     cnaf.f = 1;                            /* Read crate controller status */
     status = ksc_camio(&cnaf);
     if (status & KSC_CSR_TMO) continue;    /* Nonexistent crate           */
     if (cnaf.d & 0x2000) continue;         /* Crate Off_line              */
     cnaf.f = 17;
     cnaf.d = 0;                            /* reset inhibit               */
     status = ksc_camio(&cnaf);
     cnaf.f = 1;
     status = ksc_camio(&cnaf);
     if (cnaf.d &0x44)
       {
         if (xia_flag)
           {
             for (i=0; i < NUM_XIA; i++)
               {
                 if (xia_crates_list[i] == cnaf.c) break;
               }
             if (i < NUM_XIA) continue;
           }
         err = ACQ_CAM_INHIBIT;
       }
   }
  return(err);
}
/****************************************************************************
*
*   Setup the conditional CAMAC readout for KSC controllers.
****************************************************************************/
static int cam_cond_setup(void)
{
       struct cond_cam_pgm *cam;
	  struct cnaf_list *cnaf;
		      int  *ids, count;
		      int  cma = Current_cma,naf_ctr;
		      int  *id_list;
                      int  last_dir;
   register struct KSC_VME *ksc = (struct KSC_VME *)KSC1;
	  struct cnaf_list *naf;
	   struct cond_tbl *tmp_ro = Cond_RO;
     rtems_interrupt_level level;

   cam_cond_opt(&count);
   cnaf = CNAFnew;
   ids = IDnew;
   cam = Cpgm;

/*
*    If the list is empty,  set the pointer to the start of the list to
*    NULL and return.
*/
   if (count <= 0)
     {
       Cond_RO_str = NULL;
       return(0);
     }
   Cond_RO_str = Cond_RO;	/* pointer to start of conditional list */
   cond_ro = Cond_RO_str;
   Cond_cam_str = Cond_Cam;	/* pointer to associated CAMAC I/O list */
   cond_cam = Cond_cam_str;

/*
*    Build an execution list for the conditional readout.  This list is
*    essentially the same as the struct cond_cam_pgm.  Major change
*    is to replace indexes with pointers.
*/

#ifdef  DEBUG
   host_message(INFORM,"Conditional CAMAC list ***********","VMEacq  ");
#endif

   while(count)
    {
/*
*   If the 'next' entry is NULL, this is the end of the conditional
*   readout table.
*/
      cond_ro->lat = &Latch_data[cam->lat-1];   /* pointer to latch data   */
      cond_ro->mask = cam->mask;
      naf_ctr = cam->tr_num;                    /* Number of CNAFS in true */
      cond_ro->tr_ctr = naf_ctr;
      naf = cnaf + cam->tr_index - 1;           /* pointer to CNAF_LIST    */
      id_list = ids + cam->tr_index - 1;        /* pointer to ID_LIST      */
      cond_ro->tr_cma = cma;                    /* start in KSC2917        */
      cond_ro->tr_camid = cond_cam;             /* new dir/id list pointer */
      if (cam->tr_next) cond_ro->tr_next = Cond_RO_str + cam->tr_next - 1;
      else cond_ro->tr_next = NULL;
/*
*   Move the CNAFs for TRUE test to KSC2917 memory.
*/
      if (naf_ctr)
        {
/*
*    Determine transfer direction for the first CAMAC operation.
*/
          last_dir = CAM_RD;
          if (naf->f > 15 && naf->f < 24) last_dir = CAM_WR;
          while(naf_ctr)
            { 

#ifdef  DEBUG
   sprintf(error_msg,"T c = %x, n = %x, a = %x, f = %x",
						naf->c,naf->n,naf->a,naf->f);
   host_message(INFORM,error_msg,"VMEacq  ");
#endif

/*
*    Check for a change in the direction of transfer in the CAMAC list.
*    CAMAC reads and CAMAC writes must be in different lists.  Nondata
*    CAMAC operations and be in either list.  Each time the transfer
*    direction changes, we must insert a HALT.
*/

              rtems_interrupt_disable(level);
	      ksc->cma = cma;
	      if (naf->f < 8)
                {
                  cond_cam->dir = CAM_RD;
                  if (last_dir == CAM_WR)
                    {
                      ksc->cmr = HALT;
                      ++cma;
                      last_dir = CAM_RD;
                    }
                }
	      else if (naf->f > 15 && naf->f < 24)
                {
                  cond_cam->dir = CAM_WR;
                  if (last_dir == CAM_RD)
                    {
                      ksc->cmr = HALT;
                      ++cma;
                      last_dir = CAM_WR;
                    }
                }
	      else  cond_cam->dir = CAM_NODATA;
	      cond_cam->dat = *id_list++;
              if (naf->c > ACQ_MAX_CRATE) return (ACQ_CR_COND);
	      ksc->cmr = CAM(naf->c,WS16,A_DIS);
	      ksc->cmr = NAF(naf->n,naf->a,naf->f);
	      cma += 2;
	      ++cond_cam;
	      ++naf;
	      --naf_ctr;
              rtems_interrupt_enable(level);
	    }
/*
*   Put a end-of-list command at the end of the TRUE list.
*/
          rtems_interrupt_disable(level);
          ksc->cma = cma;
          ksc->cmr = HALT;
          rtems_interrupt_enable(level);
          cma += 1;
          cond_cam->dir = CAM_RO_END;
          ++cond_cam;
        }
      else  cond_ro->tr_cma = 0;
/*
*   Process the test FALSE in the same way we did the TRUE list above.
*/
      naf_ctr = cam->fa_num;
      cond_ro->fa_ctr = naf_ctr;
      naf = cnaf + cam->fa_index - 1;
      id_list = ids + cam->fa_index - 1;
      cond_ro->fa_cma = cma;
      cond_ro->fa_camid = cond_cam;
      if (cam->fa_next) cond_ro->fa_next = Cond_RO_str + cam->fa_next - 1;
      else  cond_ro->fa_next = NULL;
      if (naf_ctr)
        {
/*
*    Determine transfer direction for the first CAMAC operation.
*/
          last_dir = CAM_RD;
          if (naf->f > 15 && naf->f < 24) last_dir = CAM_WR;
          while(naf_ctr)
            { 

#ifdef  DEBUG
   sprintf(error_msg,"F c = %x, n = %x, a = %x, f = %x",
						naf->c,naf->n,naf->a,naf->f);
   host_message(INFORM,error_msg,"VMEacq  ");
#endif

/*
*    Check for a change in the direction of transfer in the CAMAC list.
*    CAMAC reads and CAMAC writes must be in different lists.  Nondata
*    CAMAC operations and be in either list.  Each time the transfer
*    direction changes, we must insert a HALT.
*/
              rtems_interrupt_disable(level);
	      ksc->cma = cma;
	      if (naf->f < 8)
                {
                  cond_cam->dir = CAM_RD;
                  if (last_dir == CAM_WR)
                    {
                      ksc->cmr = HALT;
                      ++cma;
                      last_dir = CAM_RD;
                    }
                }
	      else if (naf->f > 15 && naf->f < 24)
                {
                  cond_cam->dir = CAM_WR;
                  if (last_dir == CAM_RD)
                    {
                      ksc->cmr = HALT;
                      ++cma;
                      last_dir = CAM_WR;
                    }
                }
	      else  cond_cam->dir = CAM_NODATA;
	      cond_cam->dat = *id_list++;
              if (naf->c > ACQ_MAX_CRATE) return (ACQ_CR_COND);
	      ksc->cmr = CAM(naf->c,WS16,A_DIS);
	      ksc->cmr = NAF(naf->n,naf->a,naf->f);
	      cma += 2;
	      ++cond_cam;
	      ++naf;
	      --naf_ctr;
              rtems_interrupt_enable(level);
	    }
/*
*   Put a end-of-list command at the end of the FALSE list.
*/
          rtems_interrupt_disable(level);
          ksc->cma = cma;
          ksc->cmr = HALT;
          rtems_interrupt_enable(level);
          cma += 1;
          cond_cam->dir = CAM_RO_END;
          ++cond_cam;
        }
      else  cond_ro->fa_cma = 0;
      ++cond_ro;
      ++cam;
      --count;
    }
/*
*   End the Conditional CAMAC readout.  Fix any jumps to a dummy statement
*   at the end of the.  These are changed to NULL to terminate the list.
*/
  while(tmp_ro < cond_ro)
    {
      if (tmp_ro->tr_next >= cond_ro) tmp_ro->tr_next = NULL;
      if (tmp_ro->fa_next >= cond_ro) tmp_ro->fa_next = NULL;
      ++tmp_ro;
    }
  Current_cma = cma;
/*
*  Check for empty list
*/
  if (cond_ro == Cond_RO) Cond_RO_str = NULL;

#ifdef  DEBUG
  sprintf(error_msg,"cond_tbl = %p",Cond_RO);
  host_message(INFORM,error_msg,"VMEacq  ");
  sprintf(error_msg,"cond_cam = %p",Cond_Cam);
  host_message(INFORM,error_msg,"VMEacq  ");
  sprintf(error_msg,"Latch_data = %p",Latch_data);
  host_message(INFORM,error_msg,"VMEacq  ");
  tmp_ro = Cond_RO;
  while(tmp_ro < cond_ro)
    {
      sprintf(error_msg,"latch = %x, mask = %x",
                                         tmp_ro->lat-Latch_data,tmp_ro->mask);
      host_message(INFORM,error_msg,"VMEacq  ");
      sprintf(error_msg,"TRUE  - ctr = %x, cma = %x, camid = %p, next = %p",
							tmp_ro->tr_ctr,
							tmp_ro->tr_cma,
							tmp_ro->tr_camid,
							tmp_ro->tr_next);
      host_message(INFORM,error_msg,"VMEacq  ");
      camac_list(tmp_ro->tr_cma,tmp_ro->tr_camid);
      sprintf(error_msg,"FALSE - ctr = %x, cma = %x, camid = %p, next = %p",
							tmp_ro->fa_ctr,
							tmp_ro->fa_cma,
							tmp_ro->fa_camid,
							tmp_ro->fa_next);
      host_message(INFORM,error_msg,"VMEacq  ");
      camac_list(tmp_ro->fa_cma,tmp_ro->fa_camid);
      ++tmp_ro;
    }
#endif

  return(0);
}
/****************************************************************************
*
*   Setup the conditional CAMAC readout delay time.
****************************************************************************/
static int cam_cond_setdelay(void)
{
          struct tbl_index *tblptr;

/*
*   Compute the timer value to wait for before execution of the conditional
*   readout list.
*/
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_COND_RO;
  cam_Cond_delay = tblptr->delay;
  if (cam_Cond_delay > ACQ_MAX_DELAY) cam_Cond_delay = ACQ_MAX_DELAY;
  else if (cam_Cond_delay < 0) cam_Cond_delay = 0;

  return(0);
}
/****************************************************************************
*
*   Setup the conditional CAMAC readout for AUX controllers.
****************************************************************************/
static int cam_fcond_setup(void)
{
       struct cond_cam_pgm *cam;
	  struct cnaf_list *cnaf, *naf;
		      int  *ids, count;
		      int  naf_ctr;
		      int  *id_list;
	  struct fcond_tbl *tmp_ro = Fcond_RO;

   cam_cond_opt(&count);
   cnaf = CNAFnew;
   ids = IDnew;
   cam = Cpgm;

/*
*    If the list is empty,  set the pointer to the start of the list to
*    NULL and return.
*/
   if (count <= 0)
     {
       Fcond_RO_str = NULL;
       return(0);
     }
   Fcond_RO_str = Fcond_RO;	/* pointer to start of conditional list */
   fcond_ro = Fcond_RO_str;
   Fcond_cam_str = Fcond_Cam;	/* pointer to associated CAMAC I/O list */
   fcond_cam = Fcond_cam_str;

/*
*    Build an execution list for the conditional readout.  This list is
*    essentially the same as the struct cond_cam_pgm.  Major change
*    is to replace indexes with pointers.
*/

#ifdef  DEBUG
   host_message(INFORM,"Conditional CAMAC list ***********","VMEacq  ");
#endif

   while(count)
    {
/*
*   If the 'next' entry is NULL, this is the end of the conditional
*   readout table.
*/

      fcond_ro->lat = &Latch_data[cam->lat-1];  /* pointer to latch data   */
      fcond_ro->mask = cam->mask;
      naf_ctr = cam->tr_num;                    /* Number of CNAFS in true */
      fcond_ro->tr_ctr = naf_ctr;
      naf = cnaf + cam->tr_index - 1;           /* pointer to CNAF_LIST    */
      id_list = ids + cam->tr_index - 1;        /* pointer to ID_LIST      */
      fcond_ro->tr_camid = fcond_cam;           /* new dir/id list pointer */
      if (cam->tr_next) fcond_ro->tr_next = Fcond_RO_str + cam->tr_next - 1;
      else  fcond_ro->tr_next = NULL;
/*
*   Move the CNAFs for TRUE test to fcam_io struct.
*/
      if (naf_ctr)
        {
          while(naf_ctr)
            {
              if (naf->f < 8)
                {
                  fcond_cam->dir = CAM_RD;
                }
	      else
                {
	          fcond_cam->dir = CAM_NODATA;
                }
	      fcond_cam->id = *id_list++;
              fcond_cam->cnaf = CNAF(naf->c,naf->n,naf->a,naf->f);
	      ++fcond_cam;
	      ++naf;
	      --naf_ctr;
	    }
/*
*   Put a end-of-list command at the end of the TRUE list.
*/
          fcond_cam->cnaf = 0;
          ++fcond_cam;
        }
/*
*   Process the test FALSE in the same way we did the TRUE list above.
*/

      naf_ctr = cam->fa_num;
      fcond_ro->fa_ctr = naf_ctr;
      naf = cnaf + cam->fa_index - 1;
      id_list = ids + cam->fa_index - 1;
      fcond_ro->fa_camid = fcond_cam;
      if (cam->fa_next) {fcond_ro->fa_next = Fcond_RO_str + cam->fa_next - 1;}
      else  {fcond_ro->fa_next = NULL;}
      if (naf_ctr)
        {
          while(naf_ctr)
            { 
	      if (naf->f < 8)
                {
                  fcond_cam->dir = CAM_RD;
                }
	      else
                {
	          fcond_cam->dir = CAM_NODATA;
                }
	      fcond_cam->id = *id_list++;
              fcond_cam->cnaf = CNAF(naf->c,naf->n,naf->a,naf->f);
	      ++fcond_cam;
	      ++naf;
	      --naf_ctr;
	    }
/*
*   Put a end-of-list command at the end of the FALSE list.
*/
          fcond_cam->cnaf = 0;
          ++fcond_cam;
        }
      ++fcond_ro;
      ++cam;
      --count;
    }
/*
*  Check for empty list
*/
  if (fcond_ro == Fcond_RO)
    {
      Fcond_RO_str = NULL;
      return(0);
    }

/*
*   End the Conditional CAMAC readout.  Fix any jumps to a dummy statement
*   at the end of the list.  These are changed to NULL to terminate the list.
*/
  while(tmp_ro < fcond_ro)
    {
      if (tmp_ro->tr_next >= fcond_ro) tmp_ro->tr_next = NULL;
      if (tmp_ro->fa_next >= fcond_ro) tmp_ro->fa_next = NULL;
      ++tmp_ro;
    }

  return(0);
}
/****************************************************************************
*
****************************************************************************/
static int cam_cond_getf(void)
{
	  struct tbl_index *tblptr;
       struct cond_cam_pgm *cam1,*cam2;
	  struct cnaf_list *cnaf,*cnaf1,*cnaf2;
		      int  *ids,*ids1,*ids2, count;
		      int  naf_ctr;

/*
*   Compute pointers to the the tables we use - COND_RO, CNAF_LIST
*   and ID_LIST.
*/
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CNAF_LIST;
   cnaf1 = (struct cnaf_list *)( ACQ_RAM + tblptr->offset * 4);

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_ID_LIST;
   ids1 = (int *)( ACQ_RAM + tblptr->offset * 4);

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_COND_RO;
   cam1 = (struct cond_cam_pgm *)( ACQ_RAM + tblptr->offset * 4);
   count = tblptr->length;

   cnaf2 = tmp_cnaf;
   ids2 = tmp_ids;
   cam2 = tmp_cond;

/*
*    If the list is empty,  set the pointer to the start of the list to
*    NULL and return.
*/
   if (count <= 0)
     {
       return(0);
     }
   while(count)
     {
       *cam2 = *cam1;
       cam2->tr_num = 0;
       if (cam1->tr_num)
         {
           cam2->tr_index = (cnaf2 - tmp_cnaf) + 1;
           naf_ctr = cam1->tr_num;
           cnaf = cnaf1 + cam1->tr_index - 1;
           ids = ids1 + cam1->tr_index - 1;
           while(naf_ctr--)
             {
               if ((Faux[cnaf->c] == cnaf->c) &&
                                            ((cnaf->f < 16) || (cnaf->f > 23)))
                 {
                   *cnaf2 = *cnaf;
                   cnaf2++;
                   cam2->tr_num++;
                   *ids2++ = *ids;
                 }
               ids++;
               cnaf++;
             }
         }
       cam2->fa_num = 0;
       if (cam1->fa_num)
         {
           cam2->fa_index = (cnaf2 - tmp_cnaf) + 1;
           naf_ctr = cam1->fa_num;
           cnaf = cnaf1 + cam1->fa_index - 1;
           ids = ids1 + cam1->fa_index - 1;
           while(naf_ctr--)
             {
               if ((Faux[cnaf->c] == cnaf->c) &&
                                            ((cnaf->f < 16) || (cnaf->f > 23)))
                 {
                   *cnaf2 = *cnaf;
                   cnaf2++;
                   cam2->fa_num++;
                   *ids2++ = *ids;
                 }
               ids++;
               cnaf++;
             }
         }
       cam1++;
       cam2++;
       count--;
     }

  return(0);
}
/****************************************************************************
*
****************************************************************************/
static int cam_cond_getksc(void)
{
	  struct tbl_index *tblptr;
       struct cond_cam_pgm *cam1,*cam2;
	  struct cnaf_list *cnaf,*cnaf1,*cnaf2;
		      int  *ids,*ids1,*ids2, count;
		      int  naf_ctr;

/*
*   Compute pointers to the the tables we use - COND_RO, CNAF_LIST
*   and ID_LIST.
*/
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CNAF_LIST;
   cnaf1 = (struct cnaf_list *)( ACQ_RAM + tblptr->offset * 4);

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_ID_LIST;
   ids1 = (int *)( ACQ_RAM + tblptr->offset * 4);

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_COND_RO;
   cam1 = (struct cond_cam_pgm *)( ACQ_RAM + tblptr->offset * 4);
   count = tblptr->length;

   cnaf2 = tmp_cnaf;
   ids2 = tmp_ids;
   cam2 = tmp_cond;

/*
*    If the list is empty,  set the pointer to the start of the list to
*    NULL and return.
*/
   if (count <= 0)
     {
       return(0);
     }
   while(count)
     {
       *cam2 = *cam1;
       cam2->tr_num = 0;
       if (cam1->tr_num)
         {
           cam2->tr_index = (cnaf2 - tmp_cnaf) + 1;
           naf_ctr = cam1->tr_num;
           cnaf = cnaf1 + cam1->tr_index - 1;
           ids = ids1 + cam1->tr_index - 1;
           while(naf_ctr--)
             {
               if ((Faux[cnaf->c] != cnaf->c) || 
                                            ((cnaf->f > 15) && (cnaf->f < 24)))
                 {
                   *cnaf2 = *cnaf;
                   cnaf2++;
                   cam2->tr_num++;
                   *ids2++ = *ids;
                 }
               ids++;
               cnaf++;
             }
         }
       cam2->fa_num = 0;
       if (cam1->fa_num)
         {
           cam2->fa_index = (cnaf2 - tmp_cnaf) + 1;
           naf_ctr = cam1->fa_num;
           cnaf = cnaf1 + cam1->fa_index - 1;
           ids = ids1 + cam1->fa_index - 1;
           while(naf_ctr--)
             {
               if ((Faux[cnaf->c] != cnaf->c) || 
                                            ((cnaf->f > 15) && (cnaf->f < 24)))
                 {
                   *cnaf2 = *cnaf;
                   cnaf2++;
                   cam2->fa_num++;
                   *ids2++ = *ids;
                 }
               ids++;
               cnaf++;
             }

         }
       cam1++;
       cam2++;
       count--;
     }

  return(0);
}


/****************************************************************************
*
*   Optimize the conditional CAMAC readout for our hardware
****************************************************************************/
static int cam_cond_opt(int *cntr)
{
	  struct tbl_index *tblptr;
       struct cond_cam_pgm *cam,tmp_pgm;
	  struct cnaf_list *cnaf;
		      int  *ids, count;
                      int  camnew,pgmnew,cur,next;
                      int  i,index,num = 0;
               static int testindex[500];
/*
*   Compute pointers to the the tables we use - COND_RO, CNAF_LIST
*   and ID_LIST.
*/
   cnaf = tmp_cnaf;
   ids = tmp_ids;
   cam = tmp_cond;
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_COND_RO;
   count = tblptr->length;

/*
*    If the list is empty,  set the pointer to the start of the list to
*    NULL and return.
*/
   *cntr = 0;
   if (count == 0) return(0);
   for (i=0; i < count; i++) Cpgm[i] = *cam++;

   camnew = pgmnew = 0;
   while(count)
    {
/*
*   Check for prior reference to this test
*/
      if (!prior_ref(pgmnew))
        {
/*
*    This test now has no prior references (NOTE that PAC does not
*    allow backward references).
*/
          Cpgm[pgmnew].tr_num = 0;
          Cpgm[pgmnew].fa_num = 0;
          Cpgm[pgmnew].tr_next = 0;
          Cpgm[pgmnew].fa_next = 0;
        }
      else
        {
/*
*   Follow the TRUE for this test until we find a "real" test or we
*   reach the end.  There are two dummy test cases:
*     1) test of latch #1 with mask = 0xffffffff (Always TRUE)
*     2) test of any latch with mask = 0  (Always FALSE)
*/
          if (Cpgm[pgmnew].lat == 1 && Cpgm[pgmnew].mask == -1)
            {
              Cpgm[pgmnew].fa_num = 0;
              Cpgm[pgmnew].fa_next = 0;
            }
          index = camnew + 1;
          for (i=0; i < Cpgm[pgmnew].tr_num; i++)
            {
              CNAFnew[camnew] = cnaf[Cpgm[pgmnew].tr_index + i -1];
              IDnew[camnew] = ids[Cpgm[pgmnew].tr_index + i -1];
              camnew++;
            } 
          Cpgm[pgmnew].tr_index = index;
          next = Cpgm[pgmnew].tr_next;
          while (next)
            {
              cur = next - 1;
              if (Cpgm[cur].mask > 0) break;
              if (Cpgm[cur].mask == -1)
                {
                  num = Cpgm[cur].tr_num;
                  index = Cpgm[cur].tr_index;
                  next = Cpgm[cur].tr_next;
                }
              else if (Cpgm[cur].mask == 0)
                {
                  num = Cpgm[cur].fa_num;
                  index = Cpgm[cur].fa_index;
                  next = Cpgm[cur].fa_next;
                }
              for (i=0; i < num; i++)
                {
                  CNAFnew[camnew] = cnaf[index + i -1];
                  IDnew[camnew] = ids[index + i -1];
                  camnew++;
                  Cpgm[pgmnew].tr_num++;
                }
              if (Cpgm[cur].tr_next == 0 && Cpgm[cur].fa_next == 0)
                {
                  next = 0;
                  break;
                }
            }
          Cpgm[pgmnew].tr_next = next;
/*
*   Follow the FALSE for this test until we find a "real" test or we
*   reach the end.
*/
          index = camnew + 1;
          for (i=0; i < Cpgm[pgmnew].fa_num; i++)
            {
              CNAFnew[camnew] = cnaf[Cpgm[pgmnew].fa_index + i -1];
              IDnew[camnew] = ids[Cpgm[pgmnew].fa_index + i -1];
              camnew++;
            } 
          Cpgm[pgmnew].fa_index = index;
          next = Cpgm[pgmnew].fa_next;
          while (next)
            {
              cur = next - 1;
              if (Cpgm[cur].mask > 0) break;
              if (Cpgm[cur].mask == -1)
                {
                  num = Cpgm[cur].tr_num;
                  index = Cpgm[cur].tr_index;
                  next = Cpgm[cur].tr_next;
                }
              else if (Cpgm[cur].mask == 0)
                {
                  num = Cpgm[cur].fa_num;
                  index = Cpgm[cur].fa_index;
                  next = Cpgm[cur].fa_next;
                }
              for (i=0; i < num; i++)
                {
                  CNAFnew[camnew] = cnaf[index + i -1];
                  IDnew[camnew] = ids[index + i -1];
                  camnew++;
                  Cpgm[pgmnew].fa_num++;
                }
              if (Cpgm[cur].tr_next == 0 && Cpgm[cur].fa_next == 0)
                {
                  next = 0;
                  break;
                }
            }
          Cpgm[pgmnew].fa_next = next;
        }
      count--;
      pgmnew++;
    }     
/*
*   Now rebuild the test array to eliminate dummy tests.
*/
   count = tblptr->length;
   index = 0;
   for (i=0; i < count; i++)
     {
       if (Cpgm[i].mask > 0 || (Cpgm[i].mask == -1 && Cpgm[i].tr_num > 0))
        {
          tmp_pgm = Cpgm[i];
          Cpgm[index] = tmp_pgm;
          testindex[i] = index + 1; 
          index++;
        }
     }
/*
*   Fix next test indices to match the new test array
*/
   count = index;
   for (i=0; i < count; i++)
     {
       Cpgm[i].tr_next = testindex[Cpgm[i].tr_next - 1];
       Cpgm[i].fa_next = testindex[Cpgm[i].fa_next - 1];
     }
/*
*  Return the number of tests in the new test array.
*/
   if (camnew) *cntr = index;
   else  *cntr = 0;
   return(1);
}
/****************************************************************************
*
****************************************************************************/
static int prior_ref(int cur)
{
   int  i;

   if (cur == 0) return(1);
   for (i=0; i < cur; i++)
     {
       if (Cpgm[i].tr_next == (cur + 1)) return(1);
       if (Cpgm[i].fa_next == (cur + 1)) return(1);
     }
   return(0);
}

/****************************************************************************
*
*   Setup the latch readout list for KSC controller
****************************************************************************/
static int latch_setup(void)
{
          struct cnaf_list *naf,*uncon;
       struct cnafdat_list *uncond;
	  struct tbl_index *tblptr;
		       int count,i;
		       int cma = Current_cma;
            unsigned short *lat_ro = Latch_ro;
   register struct KSC_VME *ksc = (struct KSC_VME *)KSC1;
     rtems_interrupt_level level;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_LATCHES;
   naf = (struct cnaf_list *)( ACQ_RAM + tblptr->offset * 4);

#ifdef  DEBUG
   host_message(INFORM,"Latch list ***********","VMEacq  ");
   clist((void *)naf,tblptr->length);
#endif

   Latch_data[0] = 0xffff;      /* Put WTM's special at beginning of list */
/*
*    If the latch list is empty, set Latch_cma to zero and return.
*/
   count = tblptr->length;
   if (count == 0)
     {
       Latch_cma = 0;
       return(0);
     }
   Latch_cma = cma;
/*
*   Build the Latch readout list in the KSC2917 memory.
*/
   rtems_interrupt_disable(level);
   while(count)
    {
      if (Faux[naf->c] != naf->c)
        {
          if (naf->c > ACQ_MAX_CRATE) return (ACQ_CR_LATCH);
          ksc->cma = cma;
          ksc->cmr = CAM(naf->c,WS16,A_DIS);
          ksc->cmr = NAF(naf->n,naf->a,naf->f);
          cma += 2;
          uncond = tmp_camac;
          *lat_ro = 0;
          for (i=0; i < uncond_cnt; i++)
             {
               uncon = (struct cnaf_list *)uncond;
               if (uncon->c == naf->c && uncon->n == naf->n &&
                                      uncon->a == naf->a && uncon->f == naf->f) 
                 {
                   uncond->c = 255;
                   *lat_ro = uncond->d;
                   break;
                 }
               uncond++;
             }
          ++lat_ro;
        }
      ++naf;
      --count;
    }
/*
*   End the latch readout list.
*/
  if (lat_ro != Latch_ro)
    {
      ksc->cma = cma;
      ksc->cmr = HALT;
      Current_cma = cma + 1;
      Latch_flag |= 1;
    }
  else  Latch_cma = 0;

  rtems_interrupt_enable(level);

  return(0);
}
/****************************************************************************
*
*   Setup the latch readout list for AUX controller
****************************************************************************/
static int flatch_setup(void)
{
          struct cnaf_list *naf,*uncon;
       struct cnafdat_list *uncond;
	  struct tbl_index *tblptr;
		       int count,i;
            struct fcam_io *lat_ro = Flatch_ro;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_LATCHES;
   naf = (struct cnaf_list *)( ACQ_RAM + tblptr->offset * 4);

   Latch_aux = 0;
   Latch_data[0] = 0xffff;      /* Put WTM's special at beginning of list */
/*
*    If the latch list is empty just return.
*/
   count = tblptr->length;
   if (count == 0) return(0);
/*
*   Build the Latch readout list for AUX controller
*/
   while(count)
    {
      if (Faux[naf->c] == naf->c)
        {
          lat_ro->cnaf = CNAF(naf->c,naf->n,naf->a,naf->f);
          uncond = tmp_camac;
          lat_ro->id = 0;
          for (i=0; i < uncond_cnt; i++)
             {
               uncon = (struct cnaf_list *)uncond;
               if (uncon->c == naf->c && uncon->n == naf->n &&
                                      uncon->a == naf->a && uncon->f == naf->f) 
                 {
                   uncond->c = 255;
                   lat_ro->id = uncond->d;
                   break;
                 }
               uncond++;
             }
          ++lat_ro;
          lat_ro->cnaf = 0;
        }
      ++naf;
      --count;
    }
  if (lat_ro != Flatch_ro)
    {
      Latch_flag |= 1;
      Latch_aux = 1;
    }

  return(0);
}
/****************************************************************************
*
*   Setup the kill test list.
****************************************************************************/
static int kill_setup(void)
{
	  struct tbl_index *tblptr;
          struct cond_kill *Cond_Kill;
          struct kill_list *kill = Kill_List;
		       int count;

#ifdef  DEBUG
          static char *type[] = {"NONE","ANY"};

   host_message(INFORM,"Kill list ***********","VMEacq  ");
#endif

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_KILL_LIST;
   Cond_Kill = (struct cond_kill *)( ACQ_RAM + tblptr->offset * 4);

/*
*    If the kill list is empty, set kill_count to zero and return.
*/
   count = tblptr->length;
   kill_count = count;
   if (count == 0) return(0);
/*
*   Build the run-time kill list. 
*/
   while(count--)
    {
      kill->lat = &Latch_data[Cond_Kill->lat - 1];
      kill->mask = Cond_Kill->mask;
      kill->sense = Cond_Kill->type;
      if (kill->sense != 0) kill->sense = 1;

#ifdef  DEBUG
   sprintf(error_msg,"latch = %d, mask = %x,type = %s",
                       kill->lat - Latch_data, kill->mask,type[kill->sense]);
   host_message(INFORM,error_msg,"VMEacq  ");
#endif

      ++kill;
      ++Cond_Kill;
    }
  return(0);
}
/****************************************************************************
*
*   Setup the Windup CNAF lists for KSC controllers.
****************************************************************************/
static int windup_setup(void)
{
       struct cnafdat_list *naf;
	  struct tbl_index *tblptr;
		       int count;
		       int cma = Current_cma;
                       int last_dir;
   register struct KSC_VME *ksc = (struct KSC_VME *)KSC1;
     rtems_interrupt_level level;

   tblptr = (struct tbl_index *)ACQ_RAM +INDEX_WINDUP_CNAF ;
   naf = tmp_camac;

#ifdef  DEBUG
   host_message(INFORM,"Windup list ***********","VMEacq  ");
   clist(naf,tblptr->length);
#endif

   count = tblptr->length;
   if (count == 0)
     {
       Windup_str = NULL;
       return(0);
     }
   else  Windup_cma = cma;
   windup = WindUp;
   Windup_str = windup;
/*
*   Process the Windup CNAF list.
*/
   rtems_interrupt_disable(level);
   last_dir = CAM_RD;
   if (naf->f > 15 && naf->f < 24) last_dir = CAM_WR;
   while(count)
     {
       if (naf->c != 255)
         {
/*
*    Check for a change in the direction of transfer in the CAMAC list.
*    CAMAC reads and CAMAC writes must be in different lists.  Nondata
*    CAMAC operations and be in either list.  Each time the transfer
*    direction changes, we must insert a HALT.
*/
           ksc->cma = cma;
           if (naf->f < 8)
             {
               windup->dir = CAM_RD;
               if (last_dir == CAM_WR)
                 {
                   ksc->cmr = HALT;
                   ++cma;
                   last_dir = CAM_RD;
                 }
             }
           else if (naf->f > 15 && naf->f < 24)
             {
               windup->dir = CAM_WR;
               if (last_dir == CAM_RD)
                 {
                   ksc->cmr = HALT;
                   ++cma;
                   last_dir = CAM_WR;
                 }
             }
           else  windup->dir = CAM_NODATA;
           windup->dat = naf->d;
           if (naf->c > ACQ_MAX_CRATE) return (ACQ_CR_WIND);
           ksc->cmr = CAM(naf->c,WS16,A_DIS);
           ksc->cmr = NAF(naf->n,naf->a,naf->f);
           cma += 2;
           windup++;
         }
       ++naf;
       --count;
     }
/*
*   End the Windup CNAF list.
*/
   if (windup != WindUp)
     {
       ksc->cma = cma;
       ksc->cmr = HALT;
       rtems_interrupt_enable(level);
       ++cma;
       Current_cma = cma;
       windup->dir = CAM_RO_END;
     }
   else
     {
       Windup_str = NULL;
     }

#ifdef  DEBUG
  windup = Windup_str + camac_list(Windup_cma,Windup_str);
  if (windup->dir != CAM_RO_END)
    {
      host_message(INFORM,"Windup list does not end properly","VMEacq  ");
    }
#endif

   return(0);
}
/****************************************************************************
*
*   Setup the Windup CNAF lists for AUX controller.
****************************************************************************/
static int fwindup_setup(void)
{
       struct cnafdat_list *naf;
          struct tbl_index *tblptr;
                       int count;

   tblptr = (struct tbl_index *)ACQ_RAM +INDEX_WINDUP_CNAF ;
   naf = tmp_camac;

   count = tblptr->length;
   if (count == 0)
     {
       Fwindup_str = NULL;
       return(0);
     }
   fwindup = FwindUp;
   Fwindup_str = fwindup;
/*
*   Process the Windup CNAF list.
*/
   while(count)
     {
       if (Faux[naf->c] == naf->c)
         {
           if (naf->f > 15 && naf->f < 24) ;
           else
            {
              fwindup->cnaf = CNAF(naf->c,naf->n,naf->a,naf->f);
              if (naf->f > 7) fwindup->id = 0;
              else  fwindup->id = naf->d;
              naf->c = 255;
              fwindup++;
              fwindup->cnaf = 0;
            }
         }
       naf++;
       count--;
     }
   if (fwindup == FwindUp) Fwindup_str = NULL;

   return(0);
}
/****************************************************************************
*
*   Setup the Unconditional CNAF lists for KSC controllers.
****************************************************************************/
static int cam_uncond_setup(void)
{
	   unsigned short  temp;
		      int  count;
		      int  i,status,cma = Current_cma;
                      int  last_dir,seq_count;
   register struct KSC_VME *ksc = (struct KSC_VME *)KSC1;
	      struct camac lcnaf = {0,0,0,0,0};
       struct cnafdat_list *naf;
	  struct tbl_index *tblptr;
     rtems_interrupt_level level;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_UNCOND_RO;
   naf = tmp_camac;
   count = tblptr->length;

#ifdef  DEBUG
   host_message(INFORM,"UnConditional CAMAC list ***********","VMEacq  ");
   clist(naf,count);
#endif

   if (count == 0)
     {
       Uncond_cma = 0;
       Seq_cma = 0;
       share->KSC3982_enabled = 0;
       return(0);
     }
   uncond_ro = Uncond_RO;
   Uncond_RO_str = uncond_ro;
   seq_id = Seq_ID;
   Seq_ID_str = seq_id;
   Uncond_cma = cma;
   Seq_cma = 0;

/*
*   Check for any CNAFs which can use the KSC3982 list sequencer, if
*   one is available.
*/
   seq_count = 0;
   seq_transfers = 0;
   while(count)
     {
      if (naf->c != 255)
          if (naf->c == LIST_SEQ_C && (naf->f < 16 || naf->f > 23)) ++seq_count;
      ++naf;
      --count;
     }
       
   if (seq_count <= 6) seq_count = 0;
   if (seq_count > (sizeof(Seq_ID)/sizeof(unsigned short)))
     {
       host_message(PANIC,"List Sequencer Buffers too small!","VMEacq  ");
       return(ACQ_SEQ_BUFFERS);
     }

   naf = tmp_camac;
   count = tblptr->length;
   rtems_interrupt_disable(level);
/*
*    Initialize the KSC3982 list sequencer if it is to be used.
*/
   if (seq_count != 0)
     {
       lcnaf.c = LIST_SEQ_C;
       lcnaf.n = LIST_SEQ_N;
       lcnaf.f = 24;               /* Disable list sequencer          */
       lcnaf.a = 0;
       status = ksc_camio(&lcnaf);
       if ((status & (KSC_CSR_TMO | KSC_CSR_NOX | KSC_CSR_NOQ)) != 0)
         {
           host_message(PANIC,"No KSC3982 List Sequencer Found!","VMEacq  ");
           rtems_interrupt_enable(level);
           return(ACQ_NO_KSC3982);
         }
       lcnaf.f = 9;                /* Clear all data FIFOs            */
       lcnaf.a = 0;
       status = ksc_camio(&lcnaf);
       lcnaf.f = 17;               /* Set Timer Control reg to max rate */
       lcnaf.a = 0;
       lcnaf.d = 7;
       status = ksc_camio(&lcnaf);
       lcnaf.f = 16;               /* set NAF storage address to 0    */
       lcnaf.a = 2;
       lcnaf.d = 0;
       status = ksc_camio(&lcnaf);
       lcnaf.f = 23;               /* Clear LAM status register       */
       lcnaf.a = 12;
       lcnaf.d = 0x3fe;
       status = ksc_camio(&lcnaf);
       lcnaf.f = 16;
       lcnaf.a = 1;
       lcnaf.d = 0x8000;	   /* Fill sequencer list with End_of_List */
       for(i=0; i < 8192; i++) ksc_camio(&lcnaf);
       lcnaf.d = NAF(LIST_SEQ_N,0,9);
       status = ksc_camio(&lcnaf);
/*
*   Put a CAMAC command in the KSC2917 list to trigger the KSC3982 List
*   Sequencer.
*/
       ksc->cma = cma;
       ksc->cmr = CAM(LIST_SEQ_C,WS16,A_DIS);
       ksc->cmr = NAF(LIST_SEQ_N,0,25);
       cma += 2;
       uncond_ro->dir = CAM_NODATA;
       ++uncond_ro;
       last_dir = CAM_RD;
       share->KSC3982_enabled = 1;
     }
   else
     {
       if (naf->f > 15 && naf->f < 24) last_dir = CAM_WR;
       else  last_dir = CAM_RD;
       share->KSC3982_enabled = 0;
     }
/*
*   Process the unconditional readout.  Put only CAMAC read and nondata
*   commands in the list sequencer.  All others will be done by the KSC2917.
*/
   while(count)
    {
      if (naf->c == 255) ;
      else if (seq_count != 0 && 
                        (naf->c == LIST_SEQ_C && (naf->f < 16 || naf->f > 23)))
	{
	  lcnaf.d = NAF(naf->n,naf->a,naf->f);
	  status = ksc_camio(&lcnaf);
	  if (naf->f < 8)
            {
              *seq_id++ = naf->d;
              ++seq_transfers;
            }
	}
      else
	{
/*
*    Check for a change in the direction of transfer in the CAMAC list.
*    CAMAC reads and CAMAC writes must be in different lists.  Nondata
*    CAMAC operations and be in either list.  Each time the transfer
*    direction changes, we must insert a HALT.
*/
	  ksc->cma = cma;
          if (naf->f < 8)
            {
	      uncond_ro->dir = CAM_RD;
              if (last_dir == CAM_WR)
                {
                  ksc->cma = cma;
                  ksc->cmr = HALT;
                  ++cma;
                  last_dir = CAM_RD;
                }
            }
	  else if (naf->f > 15 && naf->f < 24)
            {
              uncond_ro->dir = CAM_WR;
              if (last_dir == CAM_RD)
                {
                  ksc->cma = cma;
                  ksc->cmr = HALT;
                  ++cma;
                  last_dir = CAM_WR;
                }
            }
	  else  uncond_ro->dir = CAM_NODATA;
	  uncond_ro->dat = naf->d;
          if (naf->c > ACQ_MAX_CRATE) return (ACQ_CR_UNCOND);
	  ksc->cmr = CAM(naf->c,WS16,A_DIS);
	  ksc->cmr = NAF(naf->n,naf->a,naf->f);
	  cma += 2;
	  ++uncond_ro;
	}
      naf++;
      count--;
    }
/*
*   If the list is empty, just exit.
*/
  if (cma == Uncond_cma)
    {
      Uncond_cma = 0;
      Seq_cma = 0;
      share->KSC3982_enabled = 0;
      return(0);
    }
/*
*   End the unconditional readout list.  First in the KSC2917 and then,
*   if present, the KSC3982 list sequencer.
*/
  ksc->cma = cma;
  ksc->cmr = HALT;
  ++cma;
  uncond_ro->dir = CAM_RO_END;
  *seq_id = 0;
  if (seq_count)
    {
/*
*   Setup sequencer readout command in KSC2917
*/
      Seq_cma = cma;
      ksc->cmr = BLOCK(LIST_SEQ_C,Q_Stop,WS16,A_DIS);
      ksc->cmr = NAF(LIST_SEQ_N,0,0);
      ksc->cmr = -2048;
      ksc->cmr = 0;
      ksc->cmr = HALT;
      cma += 5;
      temp = lcnaf.d + 0x8000;          /* Put end of list in last command  */
      lcnaf.f = 0;                      /* Read NAF memory address          */
      lcnaf.a = 2;
      status = ksc_camio(&lcnaf);
      lcnaf.d -= 1;                     /* Decrement memory address         */
      lcnaf.f = 16;
      status = ksc_camio(&lcnaf);
      lcnaf.a = 1;                      /* Write last command again         */
      lcnaf.d = temp;
      status = ksc_camio(&lcnaf);
      lcnaf.f = 26;                     /* Enable list sequencer            */
      lcnaf.a = 0;
      status = ksc_camio(&lcnaf);
    }
  rtems_interrupt_enable(level);
  Current_cma = cma;

#ifdef  DEBUG
  uncond_ro = Uncond_RO_str + camac_list(Uncond_cma,Uncond_RO_str);
  if (uncond_ro->dir != CAM_RO_END)
    {
      host_message(INFORM,"Unconditional readout list does not end properly","VMEacq  ");
    }
#endif

/*
*   Compute the timer value to wait for before execution of the
*   unconditional  readout list.
*/
  cam_Uncond_delay = tblptr->delay;
  if (cam_Uncond_delay > ACQ_MAX_DELAY) cam_Uncond_delay = ACQ_MAX_DELAY;
  else if (cam_Uncond_delay < 0) cam_Uncond_delay = 0;

  return(0);
}
/****************************************************************************
*
*   Setup the Unconditional CNAF lists for AUX crate controllers.
****************************************************************************/
static int cam_funcond_setup(void)
{
                      int  count;
       struct cnafdat_list *naf;
          struct tbl_index *tblptr;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_UNCOND_RO;
   count = tblptr->length;
   naf = tmp_camac;

   if (count == 0)
     {
       Funcond_RO_str = NULL;
       return(0);
     }
   funcond_ro = Funcond_RO;
   Funcond_RO_str = funcond_ro;

/*
*/
   while(count)
    {
      if (Faux[naf->c] == naf->c)
        {
          funcond_ro->cnaf = CNAF(naf->c,naf->n,naf->a,naf->f);
          funcond_ro->id = naf->d;
          naf->c = 255;
          funcond_ro++;
          funcond_ro->cnaf = 0;
        }
      naf++;
      count--;
    }
/*
*   If the list is empty, just exit.
*/
  if (funcond_ro == Funcond_RO_str) /* 2/15/01 JJK */
    {
      Funcond_RO_str = NULL; /* 2/15/01 JJK */
      return(0);  /* 2/15/01 JJK */
    }
/*
*   Compute the timer value to wait for before execution of the
*   unconditional  readout list.
*/
  cam_Uncond_delay = tblptr->delay;  /* 2/15/01 JJK */
  if (cam_Uncond_delay > ACQ_MAX_DELAY) cam_Uncond_delay = ACQ_MAX_DELAY; /*JJK*/
  else if (cam_Uncond_delay < 0) cam_Uncond_delay = 0; /* 2/15/01 JJK */
  return(0); /* 2/15/01 JJK */
}
/****************************************************************************
*
*   Initialize CAMAC modules.
****************************************************************************/
static int init_cam_modules(void)
{
      struct tbl_index *tblptr;
	  struct camac *cnaf;
		  int  count;
                  int  err = 0;
		  int  q,x,status;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CNAF_INIT;
   cnaf = (struct camac *)( ACQ_RAM + tblptr->offset * 4);

#ifdef  DEBUG
   host_message(INFORM,"Initialize CAMAC modules ***********","VMEacq  ");
   clist((struct cnafdat_list *)cnaf,tblptr->length);
#endif

   count = tblptr->length;
   while(count)
    {
      status = ksc_camio(cnaf);
      if ((status & (KSC_CSR_TMO | KSC_CSR_NOX | KSC_CSR_NOQ)) != 0)
	{
          q = x = 0;
	  if((status & KSC_CSR_NOQ) == 0) q = 1;
	  if((status & KSC_CSR_NOX) == 0) x = 1;
	  sprintf(error_msg,"CAMAC CNAF = %d, %d, %d, %d, Q = %d, X= %d",
					 cnaf->c,cnaf->n,cnaf->a,cnaf->f,q,x);
	  host_message(WARN,error_msg,"VMEacq  ");
          if ((status & 0x2000) != 0) err = ACQ_CAM_INIT;
	}
      cnaf++;
      count--;
    }
   return(err);
}
///****************************************************************************
//*
//*   Initialize FASTBUS modules.
//****************************************************************************/
static int init_fb_modules(void)
{
// 		int count,status;
                int  err = 0;
//    struct tbl_index *tblptr;
//     struct mod_type *mods;
//struct cond_fast_pgm *prog;
//   struct fb_readout *fast;
//
//#ifdef  DEBUG
//   host_message(INFORM,"Clear FASTBUS modules ***********","VMEacq  ");
//#endif
//
//   fb_1885.enable = 0;
//   fb_10C6.enable = 0;
//   fb_1881M.enable = 0;
//   fb_1877.enable = 0;     /*  Added by RLV for 1877    */
//   fb_1875.enable = 0;
//   share->FB_enabled = 0;
//   share->FB_error = 0;
//   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_FASTBUS_MODULES;
//   mods = (struct mod_type *)( ACQ_RAM + tblptr->offset * 4);
//   count = tblptr->length;
//   if(count == 0) return(0);
//
//   if (!devtbl->lrs1131) return(ACQ_NO_LRS1131);
//   if (!devtbl->lrs1821) return(ACQ_NO_LRS1821);
//   share->FB_enabled = 1;
//   fb_exec(REBUS);
//   while(count--)
//     {
//       if ((status = fb_prim_addr(CSR,mods->n)) != FALSE)
//	 {
//	   sprintf(error_msg,"FASTBUS module %d does not respond",mods->n);
//	   host_message(PANIC,error_msg,"VMEacq  ");
//           err = ACQ_FB_NOEXIST;
//	   mods++;
//	   continue;
//	 }
//       fb_sec_addr(0);
//
//#ifdef  DEBUG
//sprintf(error_msg,"c = %d, n = %d, dum = %d, type = %d",mods->c,mods->n,
//                                                    mods->dum,mods->type_code);
//host_message(INFORM,error_msg,"VMEacq  ");
//#endif
//
//       if (mods->type_code == LRS_1885)
//	 {
//           fb_1885.start = READ_ADC * 8;
//	   fb_write32(0x88000000);
//	   fb_exec(WR);
//	 }
//       else if (mods->type_code == LRS_1875)
//	 {
//           fb_1875.start = READ_ADC * 8;
//	   fb_write32(0x88000000); /* digital clear, analog clear */
//	   fb_exec(WR);
//	 }
//       else if (mods->type_code == PHIL_10C6)
//	 {
//           fb_10C6.start = READ_TDC * 8;
//	   fb_write32(0x80000000);
//	   fb_exec(WR);
//	 }
//       else if (mods->type_code == LRS_1881M)
//         {
//           fb_1881M.start = 0x90 * 8;
//         }
//
///*     RLV added for 1877 support                  */
//       else if (mods->type_code == LRS_1877)
//         {
//           fb_1877.start = 0xA0 * 8;
//         }
///*     End of 1877 block                           */
//
//       else
//	 {
//	   sprintf(error_msg,"Unknown FASTBUS module type in slot %d",
//								    mods->n);
//           host_message(PANIC,error_msg,"VMEacq  ");
//           err = ACQ_FB_UNKNOWN;
//	 }
//
//#ifdef  DEBUG
//       fb_exec(RD);
//       status = fb_read32();
//       sprintf(error_msg,"CSR0 - %x",status);
//       host_message(INFORM,error_msg,"VMEacq  ");
//#endif
//
//       mods++;
//     }
//
//#ifdef  DEBUG
//   host_message(INFORM,"Initialize FASTBUS readout ***********","VMEacq  ");
//#endif
//
//   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_FASTBUS_RO;
//   prog = (struct cond_fast_pgm *)( ACQ_RAM + tblptr->offset * 4);
//   count = tblptr->length;
//   if (count == 0) return(err);
//
//   while(count)
//     {
//       if (prog->type_code == LRS_1885) fast = &fb_1885;
//       else if (prog->type_code == LRS_1875) fast = &fb_1875;
//       else if (prog->type_code == PHIL_10C6) fast = &fb_10C6;
//       else if (prog->type_code == LRS_1881M) fast = &fb_1881M;
//       else if (prog->type_code == LRS_1877) fast = &fb_1877; /* RLV 1877 */
//       else
//	 {
//           host_message(PANIC,"Unknown FASTBUS module type in readout specs","VMEacq  ");
//           ++prog;
//           --count;
//           continue;
//         }
//       fast->enable = 1;
//       if (prog->lat == 1) fast->lat = NULL;
//       else  fast->lat = &Latch_data[prog->lat -1];
//       fast->mask = prog->mask;
//       ++prog;
//       --count;
//     }
//   fb_delay = tblptr->delay;
//   if (fb_delay > ACQ_MAX_DELAY) fb_delay = ACQ_MAX_DELAY;
//   else if (fb_delay < 0) fb_delay = 0;
//
//#ifdef  DEBUG
//   sprintf(error_msg,"1885 - Enab = %d, lat = %p, mask = %x",
//                                      fb_1885.enable,fb_1885.lat,fb_1885.mask);
//   host_message(INFORM,error_msg,"VMEacq  ");
//   sprintf(error_msg,"10C6 - Enab = %d, lat = %p, mask = %x",
//                                      fb_10C6.enable,fb_10C6.lat,fb_10C6.mask);
//   host_message(INFORM,error_msg,"VMEacq  ");
//   sprintf(error_msg,"1881M - Enab = %d, lat = %p, mask = %x",
//                                   fb_1881M.enable,fb_1881M.lat,fb_1881M.mask);
//   host_message(INFORM,error_msg,"VMEacq  ");
//
///*      RLV added for 1877 readout           */
//   sprintf(error_msg,"1877 - Enab = %d, lat = %p, mask = %x",
//                                      fb_1877.enable,fb_1877.lat,fb_1877.mask);
//   host_message(INFORM,error_msg,"VMEacq  ");
///*      End 1877 block                       */
//
//   sprintf(error_msg,"1875 - Enab = %d, lat = %p, mask = %x",
//                                      fb_1875.enable,fb_1875.lat,fb_1875.mask);
//   host_message(INFORM,error_msg,"VMEacq  ");
//#endif
//
   return(err);
}
/****************************************************************************
*
*   Initialize and clear FERA readout modules.
****************************************************************************/
static int init_fera_modules(void)
{
   struct tbl_index *tblptr;
    struct mod_type *mods;
       struct camac lcnaf = {0,0,0,0,0};
                int count,i,vsn = 1;
                int q,x,status;
                int err = 0;
  struct cond_fera_pgm *prog;
   struct fera_readout *fera;

   fera_enable = 0;
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_FERA_MODULES;
   mods = (struct mod_type *)( ACQ_RAM + tblptr->offset * 4);
   count = tblptr->length;
   if (count == 0) return(0);

#ifdef  DEBUG
   host_message(INFORM,"Clear FERA modules ***********","VMEacq  ");
#endif

   i = 0;
   if (devtbl->lrs1190a) lrs_intf[i++] = (struct LRS1190 *)LRS1190A;
   if (devtbl->lrs1190b) lrs_intf[i++] = (struct LRS1190 *)LRS1190B;
   if (devtbl->lrs1190c) lrs_intf[i++] = (struct LRS1190 *)LRS1190C;
   if (devtbl->lrs1190d) lrs_intf[i++] = (struct LRS1190 *)LRS1190D;
   lrs_intf[i] = NULL;
   if (!devtbl->ces8170 && (i == 0)) return(ACQ_NO_FERA);
   i = 0;
   while(lrs_intf[i] != NULL)
     {
       (lrs_intf[i])->mode = 0;
       i++;
     }
   ces_early_enable = 0;

   for (i=0; i < 256; i++) Fera_types[i] = 0;

   while(count)
    {

#ifdef  DEBUG
sprintf(error_msg,"c = %d, n = %d, dum = %d, type = %d",mods->c,mods->n,
                                                    mods->dum,mods->type_code);
host_message(INFORM,error_msg,"VMEacq  ");
#endif

      Fera_types[vsn] = mods->type_code;
      lcnaf.c = mods->c;
      lcnaf.n = mods->n;
      lcnaf.f = 9;
      lcnaf.a = 0;
      status = ksc_camio(&lcnaf);
      switch (mods->type_code)
       {
         case MCSQ_FER:
         case LRS_3377:
         case BAKLASH:
         case BAKLASH2:
         case BAKLASH3:
           ces_early_enable = 1;
           break;
         default:
          if ((status & (KSC_CSR_TMO | KSC_CSR_NOX | KSC_CSR_NOQ)) != 0)
            {
              q = x = 0;
              if ((status & KSC_CSR_NOQ) == 0) q = 1;
              if ((status & KSC_CSR_NOQ) == 0) x = 1;
              sprintf(error_msg,"FERA CNAF = %d, %d, %d, %d, Q = %d, X = %d",
                                          lcnaf.c,lcnaf.n,lcnaf.a,lcnaf.f,q,x);
              host_message(PANIC,error_msg,"VMEacq  ");
              err = ACQ_FERA_INIT;
            }
          break;
       }
      switch (mods->type_code)
       {
         case  BAKLASH:
         case  BAKLASH2:
         case  BAKLASH3:
          lcnaf.f = 17;
          lcnaf.a = 1;
          lcnaf.d = vsn;
          status = ksc_camio(&lcnaf);
          lcnaf.f = 1;
          lcnaf.a = 0;
          lcnaf.n = 30;
          break;
         case  LRS_4300:
          lcnaf.f = 0;
          status = ksc_camio(&lcnaf);
          lcnaf.f = 16;
          lcnaf.d = lcnaf.d & 0x8100;
          lcnaf.d = lcnaf.d | (0x600 + vsn);
          break;
/*      Added support for ORTEC AD413 module.   (JJK,JWK Jun 26, 2000) */
	 case AD_413:
          lcnaf.f = 0;
          status = ksc_camio(&lcnaf);
          lcnaf.f = 16;
          lcnaf.d = lcnaf.d & 0x9000;
          lcnaf.d = lcnaf.d | (0x000 + vsn);
	  break;
/*      End of added support for ORTEC AD413 module.   (JJK,JWK Jun 26, 2000) */
         case  MCSQ_FER:
         case  LRS_3377:
          lcnaf.f = 17;
          lcnaf.d = 0x2800 + vsn;
          status = ksc_camio(&lcnaf);
          lcnaf.f = 1;
          lcnaf.a = 0;
          lcnaf.n = 30;
          break;
         case  GAN_812F:
          lcnaf.f = 0;
          status = ksc_camio(&lcnaf);
          lcnaf.f = 16;
          lcnaf.d = lcnaf.d & 0x8100;
          lcnaf.d = lcnaf.d | (0x600 + vsn);
          break;
         case  SILENA_4418:
          lcnaf.f = 4;
          lcnaf.a = 14;
          status = ksc_camio(&lcnaf);
          lcnaf.f = 20;
          lcnaf.d = lcnaf.d & 0x800;
          lcnaf.d = lcnaf.d | (0x3400 + vsn);
          break;
         default:
          sprintf(error_msg,"Unknown FERA type - c = %d, n = %d, type = %d",
                                               mods->c,mods->n,mods->type_code);
          host_message(PANIC,error_msg,"VMEacq  ");
          err = ACQ_FERA_UNKNOWN;
          break;
       }
      status = ksc_camio(&lcnaf);
      if ((status & (KSC_CSR_TMO | KSC_CSR_NOX | KSC_CSR_NOQ)) != 0)
        {
          q = x = 0;
          if ((status & KSC_CSR_NOQ) == 0) q = 1;
          if ((status & KSC_CSR_NOX) == 0) x = 1;
          sprintf(error_msg,"FERA CNAF = %d, %d, %d, %d, Q = %d, X = %d",
                                          lcnaf.c,lcnaf.n,lcnaf.a,lcnaf.f,q,x);
          host_message(PANIC,error_msg,"VMEacq  ");
          err = ACQ_FERA_INIT;
        }
      --count;
      ++mods;
      ++vsn;
    }

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_FERA_RO;
   prog = (struct cond_fera_pgm *)( ACQ_RAM + tblptr->offset * 4);
   count = tblptr->length;
   if (count == 0) return(err);
   lrs_4300.enable = 0;
/*      Added support for ORTEC AD413 module.   (JJK, Jan. 29, 1997) */
   ad_413.enable = 0;
/*      End of added support for ORTEC AD413 module.   (JJK, Jan. 29, 1997) */
   lrs_3377.enable = 0;
   gan_812f.enable = 0;
   silena_4418.enable = 0;
   baklash.enable = 0;
   baklash2.enable = 0;
   baklash3.enable = 0;

   while(count)
     {
       switch (prog->type_code)
        {
          case  BAKLASH:
            fera = &baklash;
            fera_enable = 1;
            break;
          case  BAKLASH2:
            fera = &baklash2;
            fera_enable = 1;
            break;
          case  BAKLASH3:
            fera = &baklash3;
            fera_enable = 1;
            break;
          case  LRS_4300:
            fera = &lrs_4300;
            fera_enable = 1;
            break;
/*      Added support for ORTEC AD413 module.   (JJK,JWK Jun 26, 2000) */
          case  AD_413:
	    fera = &ad_413;
	    fera_enable = 1;
	    break;
/*      End of added support for ORTEC AD413 module.   (JJK,JWK Jun 26, 2000) */
          case  MCSQ_FER:
          case  LRS_3377:
            fera = &lrs_3377;
            fera_enable = 1;
            break;
          case  GAN_812F:
            fera = &gan_812f;
            fera_enable = 1;
            break;
          case  SILENA_4418:
            fera = &silena_4418;
            fera_enable = 1;
            break;
          default:
            host_message(PANIC,"Unknown FERA module type in readout specs","VMEacq  ");
            ++prog;
            --count;
            continue;
            break;
        }
       fera->enable = 1;
       if (prog->lat == 1) fera->lat = NULL;
       else  fera->lat = &Latch_data[prog->lat -1];
       fera->mask = prog->mask;
       ++prog;
       --count;
     }
   fera_delay = tblptr->delay;
   if (fera_delay > ACQ_MAX_DELAY) fera_delay = ACQ_MAX_DELAY;
   else if (fera_delay < 0) fera_delay = 0;

#ifdef  DEBUG
   sprintf(error_msg,"4300 - Enab = %d, lat = %p, mask = %x",
                                   lrs_4300.enable,lrs_4300.lat,lrs_4300.mask);
   host_message(INFORM,error_msg,"VMEacq  ");
/*      Added support for ORTEC AD413 module.   (JJK, Jan. 29, 1997) */
   sprintf(error_msg,"413 - Enab = %d, lat = %p, mask = %x",
                                   ad_413.enable,ad_413.lat,ad_413.mask);
   host_message(INFORM,error_msg,"VMEacq  ");
/*      End of added support for ORTEC AD413 module.   (JJK, Jan. 29, 1997) */
   sprintf(error_msg,"812F - Enab = %d, lat = %p, mask = %x",
                                   gan_812f.enable,gan_812f.lat,gan_812f.mask);
   host_message(INFORM,error_msg,"VMEacq  ");
   sprintf(error_msg,"4418 - Enab = %d, lat = %p, mask = %x",
                          silena_4418.enable,silena_4418.lat,silena_4418.mask);
   host_message(INFORM,error_msg,"VMEacq  ");
#endif

/*
*   Enable front panel input for any LRS 1190 modules present.
*/
   i = 0;
   while(lrs_intf[i] != NULL)
     {
       (lrs_intf[i])->mode = 1;
       i++;
     }

   return(err);
}
/****************************************************************************
*
*   Initialize and clear VME readout modules.
****************************************************************************/
static int init_vme_modules(void)
{
   struct tbl_index *tblptr;
                int count,i,j;
                int err = 0;
                int *iptr;
               char *bptr;
   struct cond_vme_pgm *prog;
   struct vme_readout *vme;

   vme_enable = 0;
   /* count the number of modules */
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CAEN_ADC_HARD;
   count = tblptr->length;
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CAEN_TDC_HARD;
   count += tblptr->length;
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CAEN_QDC_HARD;
   count += tblptr->length;
   /* Include SIS Scaler and Myriad */
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_SIS_HARD;
   count += tblptr->length;
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_MYR_HARD;
   count += tblptr->length;

   if (count == 0) return(0);

   /* initialize all possible modules to NULL */
   for (i=0; i < 24; i++) caen_adcs[i].hwd = NULL;
   for (i=0; i < 12; i++) caen_tdcs[i].hwd = NULL;
   for (i=0; i < 12; i++) caen_qdcs[i].hwd = NULL;
   for (i=0; i < 2; i++) sis_ro[i].hwd = NULL;
   myriad_ro.hwd = NULL; /* MYRIAD module - one clock */


   /* Setup pointers for hardware and parameter IDs */

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CAEN_ADC_HARD;
   iptr = (int *)( ACQ_RAM + tblptr->offset * 4);
   bptr = &devtbl->caen785_1;
   if (tblptr->length)
     {
      j=0;
      for (i=0; i < 24; i++)
        {
          if (iptr[i] != 0)
           {
             if (*(bptr+i))
               {
                 caen_adcs[j].hwd = CAEN785_LIST[i];
                 caen_adcs[j++].id = &Caen_adc_id[i*34];
                 vme_enable = 1;
               }
             else
               {
                 err = ACQ_NO_CAEN_ADC;
                 sprintf(error_msg,"CAEN ADC%i not in VME crate",i+1);
                 host_message(PANIC,error_msg,"VMEacq  ");
               }
           }
        }
      if (err) return(err);
     }

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CAEN_TDC_HARD;
   iptr = (int *)( ACQ_RAM + tblptr->offset * 4);
   bptr = &devtbl->caen775_1;
   if (tblptr->length)
     {
      j=0;
      for (i=0; i < 12; i++)
        {
          if (iptr[i] != 0)
           {
             if (*(bptr+i))  
               {
                 caen_tdcs[j].hwd = CAEN775_LIST[i];
                 caen_tdcs[j++].id = &Caen_tdc_id[i*34];
                 vme_enable = 1;
               }
             else
               {
                 err = ACQ_NO_CAEN_TDC;
                 sprintf(error_msg,"CAEN TDC%i not in VME crate",i+1);
                 host_message(PANIC,error_msg,"VMEacq  ");
               }
           }
        }
      if (err) return(err);
     }

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CAEN_QDC_HARD;
   iptr = (int *)( ACQ_RAM + tblptr->offset * 4);
   bptr = &devtbl->caen792_1;
   if (tblptr->length)
     {
      j=0;
      for (i=0; i < 12; i++)
        {
          if (iptr[i] != 0)
           {
             if (*(bptr+i))
               {
                 caen_qdcs[j].hwd = CAEN792_LIST[i];
                 caen_qdcs[j++].id = &Caen_qdc_id[i*34];
                 vme_enable = 1;
               }
             else
               {
                 err = ACQ_NO_CAEN_QDC;
                 sprintf(error_msg,"CAEN QDC%i not in VME crate",i+1);
                 host_message(PANIC,error_msg,"VMEacq  ");
               }
           }
        }
      if (err) return(err);
     }

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_SIS_HARD;
   iptr = (int *)( ACQ_RAM + tblptr->offset * 4);
   bptr = &devtbl->sis3820_1;
   if (tblptr->length) {
     j=0;
     for (i=0; i < 2; i++)
       {
	 if (iptr[i] != 0)
           {
             if (*(bptr+i))
               { 
                 sis_ro[j].hwd = SIS3820_LIST[i];
                 sis_ro[j++].id = &SIS_scl_id[i*96];
                 vme_enable = 1;
               }
             else
               {
                 err = ACQ_NO_SIS_SCL;
                 sprintf(error_msg,"SIS Scaler %i not in VME crate",i+1);
                 host_message(PANIC,error_msg,"VMEacq  ");
               }
           }
       }
     if (err) return(err);
   }
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_MYR_HARD;
   iptr = (int *)( ACQ_RAM + tblptr->offset * 4);
   bptr = &devtbl->myriad_1;
   if (tblptr->length) {
     if (iptr != 0) {
       if (*(bptr)) {
	 myriad_ro.hwd = (struct MyRIAD_Registers *)MYRIAD_1;
	 myriad_ro.id = &Myriad_id[0];
	 vme_enable = 1;
       } else {
	 err = ACQ_NO_MYRIAD;
	 sprintf(error_msg,"MYRIAD not in VME crate");
	 host_message(PANIC,error_msg,"VMEacq  ");
       }
     }
   if (err) return(err);
   }
   
/* Actually setup the hardware for readout */
/* CAEN 785 ADC */
   i = 0;
   while(caen_adcs[i].hwd != NULL)
     {
       int tmp;
       
       caen_adcs[i].hwd->bit_set2 = 0x04;    /* clear data */
       caen_adcs[i].hwd->event_counter_reset = 0;
       eieio();
       for (j=0; j < 32; j++)
        {
          tmp = caen_adcs[i].hwd->thresholds[j];
          eieio();
          tmp = tmp & 0xff;
          if (caen_adcs[i].id[j] == 0) tmp = tmp + 0x100;
          caen_adcs[i].hwd->thresholds[j] = tmp;
        }
       caen_adcs[i].hwd->bit_clear2 = 0x04;    /* remove clear data */
       i++;
     }

/* CAEN 775 TDC */
   i = 0;
   while(caen_tdcs[i].hwd != NULL)
     {
       int tmp;

       caen_tdcs[i].hwd->bit_set2 = 0x04;    /* clear data */
       caen_tdcs[i].hwd->event_counter_reset = 0;
       eieio();
       for (j=0; j < 32; j++)
        {
          tmp = caen_tdcs[i].hwd->thresholds[j];
          eieio();
          tmp = tmp & 0xff;
          if (caen_tdcs[i].id[j] == 0) tmp = tmp + 0x100;
          caen_tdcs[i].hwd->thresholds[j] = tmp;
        }
       caen_tdcs[i].hwd->bit_clear2 = 0x04;    /* remove clear data */
       i++;
     }

/* CAEN 792 QDC (also works for V965) */
   i = 0;
   while(caen_qdcs[i].hwd != NULL)
     {
       int tmp;

       caen_qdcs[i].hwd->bit_set2 = 0x04;    /* clear data */
       caen_qdcs[i].hwd->event_counter_reset = 0;
       eieio();
       for (j=0; j < 32; j++)
        {
          tmp = caen_qdcs[i].hwd->thresholds[j];
          eieio();
          tmp = tmp & 0xff;
          if (caen_qdcs[i].id[j] == 0) tmp = tmp + 0x100;
          caen_qdcs[i].hwd->thresholds[j] = tmp;
        }
       caen_qdcs[i].hwd->bit_clear2 = 0x04;    /* remove clear data */
       i++;
     }

/* SIS 3820 scaler modules */
   i = 0;
   while(sis_ro[i].hwd!=NULL) {
     
     sis_ro[i].hwd->Key_cnt_clear = 0; //Just touch this to clear the scaler
     sis_ro[i].hwd->op_mode =  0x00230011; //Setup for clock loading by trigger
     eieio();  //Force the io to complete
     i++;
   }
   
   /* MyRIAD module */
   if (myriad_ro.hwd!=NULL) {
     
     // These steps come from a note by John Anderson to Steve Pain on 
     // May 7, 2015.
     //Test the hardware_status bit 10 to see if a master clock lock 
     //is in place.  If not then send a message and stop here.  
     //The bit should be zero if locked.
#ifdef DEBUG
     sprintf(error_msg,"MYRIAD hardware status (1) is %hx\n",
	     myriad_ro.hwd->hardware_status);
     host_message(INFORM,error_msg,"VMEacq  ");
#endif
     
     if ((myriad_ro.hwd->hardware_status & 0x0400)) {
       sprintf(error_msg,"MYRIAD is not synchronized - fix and reinitialize\n");
       host_message(INFORM,error_msg,"VMEacq  ");
       return(ACQ_MYRIAD_SETUP_ERROR);
     }
     if (!(myriad_ro.hwd->hardware_status & 0x8000)) {
       sprintf(error_msg,"MYRIAD is not locked - fix and reinitialize\n");
       host_message(INFORM,error_msg,"VMEacq  ");
       return(ACQ_MYRIAD_SETUP_ERROR);
     }
#ifdef DEBUG
     sprintf(error_msg,"MYRIAD SERDES Config is %hx\n", 
	     myriad_ro.hwd->serdes_config);
     host_message(INFORM,error_msg,"VMEacq  ");
#endif
     
     // We must be synchronized - use the master clock
     //Remove bit 15, enable the master clock rather than local
     myriad_ro.hwd->serdes_config = 0x0063;  
     myriad_ro.hwd->pulsed_control = 0x8000;  //Reset the FPGA control logic
     myriad_ro.hwd->pulsed_control = 0x0024;  //Reset the FIFO and SERDES State machine
     eieio();   //Force the MVME5500 to flush the writes, in sequence
     
     volatile unsigned long tmp = myriad_ro.hwd->hardware_status;
     
#ifdef DEBUG
     sprintf(error_msg,"MYRIAD hardware status (2) is %hx\n",
	     tmp);
     host_message(INFORM,error_msg,"VMEacq  ");
#endif
     
     //Now check for success
     if (!(tmp & 0x8000)) {   //SERDES State machine lock
       sprintf(error_msg,"MYRIAD SERDES State machine lock fail\n");
       host_message(INFORM,error_msg,"VMEacq  ");
       return(1);
     }
     
     if ((tmp & 0x0400)) {   //SERDES physical lock
       sprintf(error_msg,"MYRIAD SERDES physical lock fail\n");
       host_message(INFORM,error_msg,"VMEacq  ");
       return(ACQ_MYRIAD_SETUP_ERROR);
     }
     
     if ((tmp & 0x0004)) {  //SERDES state machine lost lock
       sprintf(error_msg,"MYRIAD SERDES state machine lost lock\n");
       host_message(INFORM,error_msg,"VMEacq  ");
       return(ACQ_MYRIAD_SETUP_ERROR); //Quit before enabling triggers
     }
     // Now that all is well, enable triggers to latch the timestamps 
     //into the FIFO
     myriad_ro.hwd->gating_register = 0x0001;  //Uses NIM input to latch
     eieio();
     
     //End of the scary MyRIAD setup
   }
   
   /* This code appears to setup a readout list.  
    * Is it just for conditional readout?
    */
   
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CAEN_RO;
   prog = (struct cond_vme_pgm *)( ACQ_RAM + tblptr->offset * 4);
   count = tblptr->length;
   if (count == 0) return(err);

   caen_775.enable = 0;
   caen_785.enable = 0;
   caen_792.enable = 0;
   sis_3820.enable = 0;
   myriad.enable = 0;

   while(count)
     {
       switch (prog->type_code)
        {
          case  CAEN_775:
            vme = &caen_775;
            break;
          case  CAEN_785:
            vme = &caen_785;
            break;
          case  CAEN_792:
            vme = &caen_792;
            break;
          case  SIS_3820:
            vme = &sis_3820;
            break;
          case  MYRIAD:
            vme = &myriad;
            break;
          default:
            host_message(PANIC,"Unknown VME module type in readout specs","VMEacq  ");
            ++prog;
            --count;
            continue;
            break;
        }
       vme->enable = 1;
       if (prog->lat == 1) vme->lat = NULL;
       else  vme->lat = &Latch_data[prog->lat -1];
       vme->mask = prog->mask;
       ++prog;
       --count;
     }
   vme_delay = tblptr->delay;
   if (vme_delay > ACQ_MAX_DELAY) vme_delay = ACQ_MAX_DELAY;
   else if (vme_delay < 0) vme_delay = 0;

   return(err);
}
/******************************************************************************
*    Initialize the runtime structures for the count down tests.
*
******************************************************************************/
static int  count_down_setup(void)
{
          struct tbl_index *tblptr;
      struct count_dwn_pgm *cdpgm;
      struct count_dwn_lst *cdlst;
                      int  count;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_COUNT_DOWN;
   cdpgm = (struct count_dwn_pgm *)( ACQ_RAM + tblptr->offset * 4);
/*
*     If the list is empty, mark the runtime list empty and return.
*/
   if (tblptr->length == 0)
     {
       Count_dwn_str = NULL;
       return(0);
     }
   Count_dwn_str = Count_dwn_tst;
   count = tblptr->length;
   cdlst = Count_dwn_tst;
   if (count >= sizeof(Count_dwn_tst)/sizeof(struct count_dwn_lst))
                                                        return(ACQ_COUNT_DWN);
   while(count--)
     {
       cdlst->pat = &Latch_data[cdpgm->pat -1];
       cdlst->mask = cdpgm->mask;
       cdlst->current = cdpgm->count;
       cdlst->initial = cdpgm->count;
       cdlst++;
       cdlst->pat = NULL;
     }
   return(0);
}
/******************************************************************************
*   Initialize the runtime lists for both Raw and Calculated gates.
*
******************************************************************************/
static int  gate_list_setup(void)
{
         struct tbl_index  *tblptr;
   struct gate_read_table  *gate_cnaf,*gate_rd;
     struct raw_gate_spec  *gate_spec;
      struct cal_gate_lst  *cal_ro;
       struct gate_ro_lst  *gate_ro;
                      int  camf;
                      int  count,*cal_spec,*last,last_cnaf;
                      int  cma = Current_cma;
   register struct KSC_VME *ksc = (struct KSC_VME *)KSC1;
     rtems_interrupt_level level;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_GATE_READ;
   gate_cnaf = (struct gate_read_table *)( ACQ_RAM + tblptr->offset * 4);
   gate_rd = gate_cnaf;
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_RAW_GATE_SPEC;
   gate_spec = tmp_gate;
   count = tblptr->length;
   Gate_RO_str = NULL;
   Cal_RO_str = NULL;
/*
*    If the raw gate spec is empty, there should be no calculated gates.
*    So just return.  Note that we already have marked the runtime list as
*    empty above.
*/
   if (count == 0)    return(0);
/*
*    First process the raw gate specifications
*/
   if (count >= sizeof(Gate_RO)/sizeof(struct gate_ro_lst))
                                                        return(ACQ_RAW_GATE);
   gate_ro = Gate_RO;
   Gate_RO_str = gate_ro;
   gate_ro_cma = cma;
   last_cnaf = 0;
   while(count--)
     {
       gate_ro->low = gate_spec->low;
       gate_ro->high = gate_spec->high;
       gate_ro->mask = gate_spec->mask;
       gate_ro->pat = &Latch_data[gate_spec->pat -1];
       gate_cnaf = gate_rd + gate_spec->rdindx - 1;
       gate_ro->mod_type = gate_cnaf->type_code;
       camf = gate_cnaf->f;
       switch (gate_ro->mod_type)
        {
          case 0:            /* generic CAMAC module type */
            break;
          case MCSQ_CAM:
            break;
          case PHIL_7164:
          case PHIL_7186:
            camf = 0;
            break;
          case LRS_2277:
          case SILENA_4418C:
          case LRS_4300C:
/*      Added support for ORTEC AD413 module.   (JJK, Jan. 23, 1997) */
          case AD_413C:
/*      End of added support for ORTEC AD413 module.   (JJK, Jan. 23, 1997) */
          default:
            sprintf(error_msg,
                      "Illegal CAMAC module type in Gate Spec. Type code = %i",
                                                            gate_ro->mod_type);
            host_message(INFORM,error_msg,"VMEacq  ");
            Gate_RO_str = NULL;
            Cal_RO_str = NULL;
            return(ACQ_GATE_MODTYPE);
        }
       gate_ro->new = 0;
       if (gate_spec->rdindx != last_cnaf)
         {
           rtems_interrupt_disable(level);
           ksc->cma = cma;
           if (gate_cnaf->c > ACQ_MAX_CRATE) return (ACQ_CR_GATE);
           ksc->cmr = CAM(gate_cnaf->c,WS16,A_DIS);
           ksc->cmr = NAF(gate_cnaf->n,gate_cnaf->a,camf);
           cma += 2;
           rtems_interrupt_enable(level);
           gate_ro->new = 1;
           last_cnaf = gate_spec->rdindx;
         }
       gate_ro++;
       gate_ro->pat = NULL;
       gate_spec++;
     }
   ksc->cmr = HALT;
   Current_cma = cma + 1;
/*
*    Next process the calculated gates if any.
*/
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_RAW_GATE_SPEC;
   gate_spec = tmp_gate;
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CAL_GATE_SPEC;
   cal_spec = (int *)( ACQ_RAM + tblptr->offset * 4);
/*
*    If calculated gate list is empty, we are done.
*/
   if (tblptr->length == 0)   return(0);
   cal_ro = Cal_Gate_RO;
   Cal_RO_str = cal_ro;
   last = cal_spec + tblptr->length;
   while(cal_spec < last)
     {
       cal_ro->pat = &Latch_data[*cal_spec++ - 1];
       cal_ro->mask = *cal_spec++;
       cal_ro->not1 = *cal_spec++;
       cal_ro->pat1 = &Latch_data[(gate_spec + *cal_spec - 1)->pat -1];
       cal_ro->msk1 = (gate_spec + *cal_spec++ - 1)->mask;
       cal_ro->pat2 = NULL;
       if (*cal_spec != -1)
         {
           cal_ro->op = *cal_spec++;
           cal_ro->not2 = *cal_spec++;
           cal_ro->pat2 = &Latch_data[(gate_spec + *cal_spec - 1)->pat -1];
           cal_ro->msk2 = (gate_spec + *cal_spec++ - 1)->mask;
         }
       cal_spec++;
       cal_ro++;
       cal_ro->pat1 = NULL;
       count = cal_ro - Cal_Gate_RO;
       if (count >= sizeof(Cal_Gate_RO)/sizeof(struct cal_gate_lst))
                                                        return(ACQ_CAL_GATE);
     }
   return(0);
}
/******************************************************************************
*  Initialize the readout of special CAMAC modules in KSC crates.
******************************************************************************/
static int cam_special_setup(void)
{
          struct tbl_index *tblptr;
           struct mod_type *mods;
        struct cam_ro_lst  *cam_ro;
                struct XIA *xiaram = (struct XIA *)XIA_RAM;
                      int  count,sdelay;
                      int  cma = Current_cma;
   register struct KSC_VME *ksc = (struct KSC_VME *)KSC1;
              struct camac lcnaf = {0,0,0,0,0};
     rtems_interrupt_level level;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CAMAC_RO;

/*
*    If the list is empty,  set the pointer to the start of the list to
*    NULL and return.
*/
   count = tblptr->length;
   if (count == 0)
     {
       Cam_RO_str = NULL;
       return(0);
     }
   sdelay = tblptr->delay;
   Cam_RO_str = Cam_RO;       /* pointer to start of conditional list */
   cam_ro = Cam_RO_str;
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CAMAC_MODULES;
   mods = (struct mod_type *)( ACQ_RAM + tblptr->offset * 4);
   count = tblptr->length;
   if (count >= sizeof(Cam_RO)/sizeof(struct cam_ro_lst))
                                                        return(ACQ_CAM_RO);
/*
*   If the CAMAC readout list is not empty, this list should not be
*   be empty.  Check for empty list anyway and just exit if it is.
*/
   if (count == 0)
     {
       Cam_RO_str = NULL;
       return(0);
     }
/*
*   Copy CAMAC readout list to runtime structure.
*/
   cam_ro->n = 0;
   while(count--)
     {
       cam_ro->mod_type = mods->type_code;
       if (cam_ro->mod_type > 0)
        {
          if ((Faux[mods->c] != mods->c) || (mods->type_code == XIA_TIME))
            {
              cam_ro->c = mods->c;
              cam_ro->n = mods->n;
              cam_ro++;
              cam_ro->n = 0;
            }
        }
       mods++;
     }
/*
*   If the list is empty, just exit.
*/
   if (cam_ro == Cam_RO)
     {
       Cam_RO_str = NULL;
       return(0);
     }
/*
*   Build execution list in KSC 2917 memory.
*/
   cam_ro = Cam_RO_str;
   while(cam_ro->n != 0)
     {
       switch (cam_ro->mod_type)
        {
          case PHIL_7164:
          case PHIL_7186:
            if (cam_ro->c > ACQ_MAX_CRATE) return (ACQ_CR_CAMAC);
            rtems_interrupt_disable(level);
            cam_ro->cma = cma;
            ksc->cma = cma;
            ksc->cmr = BLOCK(cam_ro->c,Q_Stop,WS16,A_DIS); /* read data    */
            ksc->cmr = NAF(cam_ro->n,0,4);
            ksc->cmr = -18;
            ksc->cmr = 0;
            ksc->cmr = HALT;
            cma += 5;
            rtems_interrupt_enable(level);
            break;
          case LRS_2277:
/***********************************************************************
*   NOTE: Code for LeCroy 2277 TDC provided by Notre Dame
************************************************************************/
            if (cam_ro->c > ACQ_MAX_CRATE) return (ACQ_CR_CAMAC);
            rtems_interrupt_disable(level);
            cam_ro->cma = cma;
            ksc->cma = cma;
            ksc->cmr = CAM(cam_ro->c,WS16,A_DIS);
            ksc->cmr = NAF(cam_ro->n,0,27);
            ksc->cmr = HALT;

/* Changed WS16 to W24   (1/10/97 nsc, jjk, jk) */
            ksc->cmr = BLOCK(cam_ro->c,Q_Stop,WS24,A_DIS); /* read data    */
            ksc->cmr = NAF(cam_ro->n,0,0);
            ksc->cmr = -520;
            ksc->cmr = 0;
            ksc->cmr = HALT;
/* Changed cma += 10 to cme += 8     (1/10/97 nsc, jjk, jk) */
            cma += 8;
            rtems_interrupt_enable(level);
            break;
          case MCSQ_CAM:
          case XIA_TIME:
/*
*   setup for XIA time stamp roadout
*/
            if (cam_ro->c > ACQ_MAX_CRATE) return (ACQ_CR_CAMAC);
            if (xiaram->aoutbuffer <= 0) xiaram->aoutbuffer = AOUTBUFFER;
            if (xiaram->aoutbuffer >= 0x4200) xiaram->aoutbuffer = AOUTBUFFER;
            if (xiaram->gslttimea <= 0) xiaram->gslttimea = GSLTTIMEA;
            if (xiaram->gslttimea >= 0x4200) xiaram->gslttimea = GSLTTIMEA;
            lcnaf.c = cam_ro->c;
            lcnaf.n = cam_ro->n;
            lcnaf.a = 1;
            lcnaf.f = 17;
            lcnaf.d = xiaram->aoutbuffer;
            ksc_camio(&lcnaf);
            lcnaf.a = 0;
            lcnaf.f = 0;
            ksc_camio(&lcnaf);
            rtems_interrupt_disable(level);
            cam_ro->cma = cma;
            ksc->cma = cma;
            ksc->cmr = IMM_WR(cam_ro->c,WS16,A_DIS);
            ksc->cmr = NAF(cam_ro->n,1,17);
            ksc->cmr = xiaram->gslttimea;
            ksc->cmr = 0;
            ksc->cmr = BLOCK(cam_ro->c,Q_Ignore,WS16,A_DIS);
            ksc->cmr = NAF(cam_ro->n,0,0);
            ksc->cmr = -3;
            ksc->cmr = 0;
            ksc->cmr = IMM_WR(cam_ro->c,WS16,A_DIS);
            ksc->cmr = NAF(cam_ro->n,1,17);
            ksc->cmr = lcnaf.d;
            ksc->cmr = 0;
            ksc->cmr = HALT;
            cma += 13;
            rtems_interrupt_enable(level);
            break;
          case LRS_3377C:
            if (cam_ro->c > ACQ_MAX_CRATE) return (ACQ_CR_CAMAC);
            lcnaf.c = cam_ro->c;
            lcnaf.n = cam_ro->n;
            lcnaf.a = 0;
            lcnaf.f = 17;
            lcnaf.d = 0x2000;
            ksc_camio(&lcnaf);
            rtems_interrupt_disable(level);
            cam_ro->cma = cma;
            ksc->cma = cma;
            ksc->cmr = BLOCK(cam_ro->c,Q_Stop,WS16,A_DIS); /* read data    */
            ksc->cmr = NAF(cam_ro->n,0,0);
            ksc->cmr = -1030;
            ksc->cmr = 0;
            ksc->cmr = HALT;
            cma += 5;
            rtems_interrupt_enable(level);
            lcnaf.f = 1;
            ksc_camio(&lcnaf);
            lcnaf.d = lcnaf.d & 0xc000;
            if (lcnaf.d == 0x8000)
              {
                 lcnaf.a = 3;
                 ksc_camio(&lcnaf);
                 lcnaf.d = lcnaf.d/2048;
              }
            else if (lcnaf.d == 0xc000)
              {
                 lcnaf.a = 4;
                 ksc_camio(&lcnaf);
                 lcnaf.d = lcnaf.d/20;
              }
            else
              {
                sprintf(error_msg,
                       "Illegal LRS3377 Mode. C = %i, N = %i",lcnaf.c,lcnaf.n);
                host_message(PANIC,error_msg,"VMEacq  ");
              }
            if ((lcnaf.d + 5) > sdelay)
              {
                sprintf(error_msg,
                      "Readout delay too short for LRS3377. C = %i, N = %i",
                                                              lcnaf.c,lcnaf.n);
                host_message(PANIC,error_msg,"VMEacq  ");
              }
            break;
          case LRS_4300C:
            if (cam_ro->c > ACQ_MAX_CRATE) return (ACQ_CR_CAMAC);
            lcnaf.f = 9;
            lcnaf.a = 0;
            lcnaf.n = cam_ro->n;
            lcnaf.c = cam_ro->c;
            ksc_camio(&lcnaf);
            lcnaf.f = 0;
            ksc_camio(&lcnaf);
            lcnaf.d &= 0x8800;  /* save overflow suppression and ped sub bits*/
            lcnaf.d |= 0x3000;  /* set CAMAC sequential and compression  */
            lcnaf.f = 16;
            ksc_camio(&lcnaf);
            rtems_interrupt_disable(level);
            cam_ro->cma = cma;
            ksc->cma = cma;
            ksc->cmr = BLOCK(cam_ro->c,Q_Stop,WS16,A_DIS); /* read data    */
            ksc->cmr = NAF(cam_ro->n,0,2);
            ksc->cmr = -18;
            ksc->cmr = 0;
            ksc->cmr = HALT;
            cma += 5;
            rtems_interrupt_enable(level);
            break;
/*      Added support for ORTEC AD413 module.   (JJK,JWK Jun 26, 2000) */
          case AD_413C:
	  if (cam_ro->c > ACQ_MAX_CRATE) return (ACQ_CR_CAMAC);
            lcnaf.f = 9;
            lcnaf.a = 0;
            lcnaf.n = cam_ro->n;
            lcnaf.c = cam_ro->c;
            ksc_camio(&lcnaf);
            lcnaf.f = 0;
            ksc_camio(&lcnaf);
            lcnaf.d &= 0x9000;   /* save overflow suppression and singles-mode bits */
            lcnaf.d |= 0x0200;   /* set CAMAC sequential with compression */
            lcnaf.f = 16;
            ksc_camio(&lcnaf);
            rtems_interrupt_disable(level);
            cam_ro->cma = cma;
            ksc->cma = cma;
            ksc->cmr = BLOCK(cam_ro->c,Q_Stop,WS16,A_DIS); /* read data    */
            ksc->cmr = NAF(cam_ro->n,0,2);
            ksc->cmr = -6;
            ksc->cmr = 0;
            ksc->cmr = HALT;
            cma += 5;
            rtems_interrupt_enable(level);
            break;
/*      End of added support for ORTEC AD413 module.   (JJK,JWK Jun 26, 2000) */
          case SILENA_4418C:
            if (cam_ro->c > ACQ_MAX_CRATE) return (ACQ_CR_CAMAC);
            lcnaf.f = 9;
            lcnaf.a = 0;
            lcnaf.n = cam_ro->n;
            lcnaf.c = cam_ro->c;
            ksc_camio(&lcnaf);
            lcnaf.f = 4;
            lcnaf.a = 14;
            ksc_camio(&lcnaf);
            lcnaf.d = lcnaf.d & 0x800;  /* save overflow bit               */
            lcnaf.d |= 0x3000;   /* set CAMAC sequential and compression   */
            lcnaf.f = 20;
            ksc_camio(&lcnaf);
            rtems_interrupt_disable(level);
            cam_ro->cma = cma;
            ksc->cma = cma;
            ksc->cmr = BLOCK(cam_ro->c,Q_Stop,WS16,A_DIS); /* read data    */
            ksc->cmr = NAF(cam_ro->n,0,0);
            ksc->cmr = -11;
            ksc->cmr = 0;
            ksc->cmr = HALT;
            cma += 5;
            rtems_interrupt_enable(level);
            break;
          default:
            return(ACQ_CAM_UNKNOWN);
            break;
         }
       cam_ro++;
     }
   Cam_RO_end = cam_ro;
   Current_cma = cma;
   return(0);
}

/******************************************************************************
*  Initialize the readout of special CAMAC modules in crates with AUX
*   controllers.
******************************************************************************/
static int cam_fspecial_setup(void)
{
          struct tbl_index *tblptr;
           struct mod_type *mods;
        struct fcam_ro_lst  *fcam_ro;
                      int  count,sdelay;
              struct camac lcnaf = {0,0,0,0,0};

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CAMAC_RO;

/*
*    If the list is empty,  set the pointer to the start of the list to
*    NULL and return.
*/
   count = tblptr->length;
   if (count == 0)
     {
       Fcam_RO_str = NULL;
       return(0);
     }
   sdelay = tblptr->delay;
   Fcam_RO_str = Fcam_RO;       /* pointer to start of conditional list */
   fcam_ro = Fcam_RO_str;
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CAMAC_MODULES;
   mods = (struct mod_type *)( ACQ_RAM + tblptr->offset * 4);
   count = tblptr->length;
   if (count >= sizeof(Cam_RO)/sizeof(struct fcam_ro_lst))
                                                        return(ACQ_CAM_RO);
/*
*   If the CAMAC readout list is not empty, this list should not be
*   be empty.  Check for empty list anyway and just exit if it is.
*/
   if (count == 0)
     {
       Fcam_RO_str = NULL;
       return(0);
     }
/*
*   Copy CAMAC readout list to runtime structure.
*/
   fcam_ro->n = 0;
   while(count--)
     {
       fcam_ro->mod_type = mods->type_code;
       if (fcam_ro->mod_type > 0)
        {
          if ((Faux[mods->c] == mods->c) && (mods->type_code != XIA_TIME))
            {
              fcam_ro->c = mods->c;
              fcam_ro->n = mods->n;
              fcam_ro++;
              fcam_ro->n = 0;
            }
        }
       mods++;
     }
/*
*   If the list is empty, just exit.
*/
   if (fcam_ro == Fcam_RO)
     {
       Fcam_RO_str = NULL;
       return(0);
     }
/*
*
*/
   fcam_ro = Fcam_RO_str;
   while(fcam_ro->n != 0)
     {
       switch (fcam_ro->mod_type)
        {
          case PHIL_7164:
          case PHIL_7186:
            fcam_ro->cnaf = QREPEAT(fcam_ro->c,fcam_ro->n,0,4);
            break;
          case MCSQ_CAM:
          case LRS_3377C:
            lcnaf.c = fcam_ro->c;
            lcnaf.n = fcam_ro->n;
            lcnaf.a = 0;
            lcnaf.f = 17;
            lcnaf.d = 0x2000;
            ksc_camio(&lcnaf);
            fcam_ro->cnaf = QREPEAT(fcam_ro->c,fcam_ro->n,0,0);
            lcnaf.f = 1;
            ksc_camio(&lcnaf);
            lcnaf.d = lcnaf.d & 0xc000;
            if (lcnaf.d == 0x8000)
              {
                 lcnaf.a = 3;
                 ksc_camio(&lcnaf);
                 lcnaf.d = lcnaf.d/2048;
              }
            else if (lcnaf.d == 0xc000)
              {
                 lcnaf.a = 4;
                 ksc_camio(&lcnaf);
                 lcnaf.d = lcnaf.d/20;
              }
            else
              {
                sprintf(error_msg,
                       "Illegal LRS3377 Mode. C = %i, N = %i",lcnaf.c,lcnaf.n);
                host_message(PANIC,error_msg,"VMEacq  ");
              }
            if ((lcnaf.d + 5) > sdelay)
              {
                sprintf(error_msg,
                      "Readout delay too short for LRS3377. C = %i, N = %i",
                                                              lcnaf.c,lcnaf.n);
                host_message(PANIC,error_msg,"VMEacq  ");
              }
            break;
          case LRS_2277:
            fcam_ro->cnaf = QREPEAT24(fcam_ro->c,fcam_ro->n,0,0);
            break;
          case LRS_4300C:
            lcnaf.f = 9;
            lcnaf.a = 0;
            lcnaf.n = fcam_ro->n;
            lcnaf.c = fcam_ro->c;
            ksc_camio(&lcnaf);
            lcnaf.f = 0;
            ksc_camio(&lcnaf);
            lcnaf.d &= 0x8800;  /* save overflow suppression and ped sub bits*/
            lcnaf.d |= 0x3000;  /* set CAMAC sequential and compression  */
            lcnaf.f = 16;
            ksc_camio(&lcnaf);
            fcam_ro->cnaf = QREPEAT(fcam_ro->c,fcam_ro->n,0,2);
            break;
/*      Added support for ORTEC AD413 module.   (JWK, Jul. 14, 2000) */
          case AD_413C:
            lcnaf.f = 9;
            lcnaf.a = 0;
            lcnaf.n = fcam_ro->n;
            lcnaf.c = fcam_ro->c;
            ksc_camio(&lcnaf);
            lcnaf.f = 0;
            ksc_camio(&lcnaf);
            lcnaf.d &= 0x9000;   /* save overflow suppression and singles-mode bits */
            lcnaf.d |= 0x0200;   /* set CAMAC sequential with compression */
            lcnaf.f = 16;
            ksc_camio(&lcnaf);
            fcam_ro->cnaf = QREPEAT(fcam_ro->c,fcam_ro->n,0,2);
            break;
/*      End of added support for ORTEC AD413 module.   (JWK, Jul. 14, 2000) */
          case SILENA_4418C:
            lcnaf.f = 9;
            lcnaf.a = 0;
            lcnaf.n = fcam_ro->n;
            lcnaf.c = fcam_ro->c;
            ksc_camio(&lcnaf);
            lcnaf.f = 4;
            lcnaf.a = 14;
            ksc_camio(&lcnaf);
            lcnaf.d = lcnaf.d & 0x800;  /* save overflow bit               */
            lcnaf.d |= 0x3000;   /* set CAMAC sequential and compression   */
            lcnaf.f = 20;
            ksc_camio(&lcnaf);
            fcam_ro->cnaf = QREPEAT(fcam_ro->c,fcam_ro->n,0,0);
            break;
          default:
            return(ACQ_CAM_UNKNOWN);
            break;
         }
       fcam_ro++;
     }
   return(0);
}
/****************************************************************************
*   Setup delay time for special CAMAC modules.
****************************************************************************/
static int cam_special_setdelay(void)
{
                       int count;
          struct tbl_index *tblptr;
   struct cond_cam_special *prog;
        struct cam_readout *read;

   cam_special_delay = 0;
   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_CAMAC_RO;

/*
*    If the list is empty,  set the pointer to the start of the list to
*    NULL and return.
*/
   count = tblptr->length;
   if (count == 0) return(0);

/*
*   Build the conditional readout tests
*/
   prog = (struct cond_cam_special *)( ACQ_RAM + tblptr->offset * 4);
   cam_special_delay = tblptr->delay;
   if (cam_special_delay > ACQ_MAX_DELAY) cam_special_delay = ACQ_MAX_DELAY;
   else if (cam_special_delay < 0) cam_special_delay = 0;
   while(count--)
     {
       switch(prog->type_code)
        {
          case PHIL_7164:
            read = &phil_7164;
            break;
          case PHIL_7186:
            read = &phil_7186;
            break;
          case LRS_2277:
            read = &lrs_2277;
            break;
          case MCSQ_CAM:
          case LRS_3377C:
            read = &lrs_3377C;
            break;
          case LRS_4300C:
            read = &lrs_4300C;
            break;
/*      Added support for ORTEC AD413 module.   (JJK, Jan. 23, 1997) */
          case AD_413C:
            read = &ad_413C;
            break;
/*      End of added support for ORTEC AD413 module.   (JJK, Jan. 23, 1997) */
          case SILENA_4418C:
            read = &silena_4418C;
            break;
          default:
            return(ACQ_CAM_UNKNOWN);
            break;
        }
       read->mask = prog->mask;
       if (prog->lat == 1) read->lat = NULL;
       else  read->lat = &Latch_data[prog->lat -1];
     }
   return(0);
}
/****************************************************************************
*
*   Build XIA readout lists.
****************************************************************************/
static int init_xia(void)
{
                  int count,i,status;
                  int cma;
     struct tbl_index *tblptr;
      struct xia_list *cnvg;
       struct KSC_VME *ksc = (struct KSC_VME *)KSC1;
rtems_interrupt_level level;

   xia_flag = 0;
   for (i=0; i < NUM_XIA+1; i++) xia_crates_list[i] = -1;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_XIA_LIST;

/*
*    If the list is empty,  set the XIA readout flag to zero
*    return.
*/
   count = tblptr->length;
   if (count == 0) return(0);

/*
*    Check for XIA modules
*/
   cnvg = (struct xia_list *)( ACQ_RAM + tblptr->offset * 4);

   status = 0;
   while (count--)
     {
       status += check_xia_mod(cnvg->c,cnvg->n);
       cnvg++;
     }
   if (status != 0) return(ACQ_NO_XIA);

/*
*   Build readout lists for XIA modules
*/
   cnvg = (struct xia_list *)( ACQ_RAM + tblptr->offset * 4);

   rtems_interrupt_disable(level);
   cma = Current_cma;
   ksc->cma = cma;
   eieio();

   i = 0;
   count = tblptr->length;
   while (count--)
    {
      xia_rd[i].vsn = cnvg->vsn;
      xia_crates_list[i] = cnvg->c;
      if (Faux[cnvg->c] >= 0)
        {
          xia_rd[i].wc = CNAF(cnvg->c,cnvg->n,2,1)| 0x80000000;
          xia_rd[i].cnaf = CNAF(cnvg->c,cnvg->n,0,5);
        }
      else
        {
          xia_rd[i].wc = cma;
          ksc->cmr = CAM(cnvg->c,WS16,A_DIS);
          ksc->cmr = NAF(cnvg->n,2,1);
          ksc->cmr = HALT;
          cma += 3;
          xia_rd[i].cnaf = cma;
          ksc->cmr = BLOCK(cnvg->c,Q_Ignore,WS16,A_DIS);
          ksc->cmr = NAF(cnvg->n,0,0);
          ksc->cmr = 0;
          ksc->cmr = 0;
          ksc->cmr = HALT;
          cma += 5;
        }
      eieio();
      i++;
      xia_rd[i].cnaf = 0;
      cnvg++;
    }

   count = tblptr->length;
   cnvg = (struct xia_list *)( ACQ_RAM + tblptr->offset * 4);
   xia_startup_cma = cma;
   eieio();
   while (count--)
    {
      ksc->cmr = IMM_WR(cnvg->c,WS16,A_DIS);
      ksc->cmr = NAF(cnvg->n,0,17);
      ksc->cmr = 3;
      ksc->cmr = 0;
      cma += 4;
      cnvg++;
    }
   ksc->cmr = HALT;
   cma++;

   count = tblptr->length;
   cnvg = (struct xia_list *)( ACQ_RAM + tblptr->offset * 4);
   xia_restart_cma = cma;
   while (count--)
    {
      ksc->cmr = IMM_WR(cnvg->c,WS16,A_DIS);
      ksc->cmr = NAF(cnvg->n,0,17);
      ksc->cmr = 1;
      ksc->cmr = 0;
      cma += 4;
      cnvg++;
    }
   ksc->cmr = HALT;
   cma++;
   cnvg = (struct xia_list *)( ACQ_RAM + tblptr->offset * 4);
   if (Faux[cnvg->c] >= 0)
     {
       xia_poll = CNAF(cnvg->c,cnvg->n,0,1) | 0x80000000;
     }
   else
     {
       xia_poll = cma;
       ksc->cma = cma;
       ksc->cmr = CAM(cnvg->c,WS16,A_DIS);
       ksc->cmr = NAF(cnvg->n,0,1);
       ksc->cmr = HALT;
       cma += 3;
     }
   Current_cma = cma;
   rtems_interrupt_enable(level);

   xia_flag = 1;
   return(0);
}
/****************************************************************************
*
****************************************************************************/
static int  check_xia_mod(int c,int n)
{
                int status,err = 1;
      struct camac cnaf;

   if (!devtbl->ksc2917a) return(ACQ_NO_KSC2917);

   cnaf.c = c;
   cnaf.n = n;
   cnaf.a = 2;
   cnaf.f = 1;
   status = ksc_camio(&cnaf);
   if ((status & KSC_CSR_TMO) != 0) return(err);
   status = status & (KSC_CSR_NOX | KSC_CSR_NOQ);
   if (status == 0)
     {
       cnaf.a = 1;
       cnaf.f = 1;
       status = ksc_camio(&cnaf);
       status = status & (KSC_CSR_NOX | KSC_CSR_NOQ);
       if (status == 0)
         {
           err = 0;
           sprintf(error_msg,"XIA module in Crate %d, Slot %d",cnaf.c,cnaf.n);
           host_message(INFORM,error_msg,"VMEacq  ");
         }

     }
   return(err);
}
/****************************************************************************
*
*   Setup the start acquisition CAMAC list.  This list is executed each time
*   a startvme command is given.
*
*   This list is produced by the $run directives in PAC.
****************************************************************************/
static int setup_cam_start_list(void)
{
      struct tbl_index *tblptr;
       struct KSC_VME  *ksc;
          struct camac *cnaf;
                  int  cma = Current_cma;
		  int  count;
 rtems_interrupt_level level;

   tblptr = (struct tbl_index *)ACQ_RAM + INDEX_RUN_CNAF_LIST;
   cnaf = (struct camac *)( ACQ_RAM + tblptr->offset * 4);

   start_list_cma1 = 0;
   start_list_cma2 = 0;
   count = tblptr->length;
   if (!count) return (0); /* (0) added after return 2/8/01 JWK */
   rtems_interrupt_disable(level);
   if ((ksc = ksc1_ptr) != NULL)
     {
       start_list_cma1 = cma;
       ksc->cma = cma;
       eieio();
       while(count)
        {
          if (cnaf->c > 7) continue;
          if (cnaf->f > 15 && cnaf->f < 24)
            {
              ksc->cmr = IMM_WR(cnaf->c,WS24,A_DIS);
              ksc->cmr = NAF(cnaf->n,cnaf->a,cnaf->f);
              ksc->cmr = cnaf->d;
              ksc->cmr = (cnaf->d >> 16);
              cma += 4;
            }
          else
            {
              ksc->cmr = CAM(cnaf->c,WS16,A_DIS);
              ksc->cmr = NAF(cnaf->n,cnaf->a,cnaf->f);
              cma += 2;
            }
          cnaf++;
          count--;
        }
       ksc->cmr = HALT;
       cma++;
       Current_cma = cma;
     }
   if ((ksc = ksc2_ptr) != NULL)
     {
       cnaf = (struct camac *)( ACQ_RAM + tblptr->offset * 4);
       count = tblptr->length;
       start_list_cma2 = 0x20;
       ksc->cma = 0x20;
       eieio();
       while(count)
        {
          if (cnaf->c > 7) continue;
          if (cnaf->f > 15 && cnaf->f < 24)
            {
              ksc->cmr = IMM_WR(cnaf->c,WS24,A_DIS);
              ksc->cmr = NAF(cnaf->n,cnaf->a,cnaf->f);
              ksc->cmr = cnaf->d;
              ksc->cmr = (cnaf->d >> 16);
              cma += 4;
            }
          else
            {
              ksc->cmr = CAM(cnaf->c,WS16,A_DIS);
              ksc->cmr = NAF(cnaf->n,cnaf->a,cnaf->f);
              cma += 2;
            }
          cnaf++;
          count--;
        }
       ksc->cmr = HALT;
     }
   rtems_interrupt_enable(level);
   return (0);
}
/****************************************************************************
*
*   Start XIA acquisition.
****************************************************************************/
static void xia_start(void)
{
   struct KSC_VME *ksc = (struct KSC_VME *)KSC1;
   rtems_interrupt_level level;

   rtems_interrupt_disable(level);

   ksc->cma = xia_startup_cma;
   ksc->csr = KSC_CSR_CAM_WR | KSC_CSR_GO;
   eieio();
   while(!(ksc->csr & KSC_CSR_DONE));

   rtems_interrupt_enable(level);

}
/****************************************************************************
*
*   Execute the start acquisition CAMAC list.
****************************************************************************/
static void run_start_list(void)
{
     struct KSC_VME  *ksc;
        volatile int  tmp;
rtems_interrupt_level level;

   rtems_interrupt_disable(level);
   if (start_list_cma1)
     {
       ksc = ksc1_ptr;
       ksc->cma = start_list_cma1;
       ksc->csr = KSC_CSR_CAM_RD | KSC_CSR_GO;
       eieio();
       do
         {
           if (ksc->csr & KSC_CSR_RDY) tmp = ksc->dlr;
         }
       while(!(ksc->csr & KSC_CSR_DONE));
     }
   if (start_list_cma2)
     {
       ksc = ksc2_ptr;
       ksc->cma = start_list_cma2;
       ksc->csr = KSC_CSR_CAM_RD | KSC_CSR_GO;
       eieio();
       do
         {
           if (ksc->csr & KSC_CSR_RDY) tmp = ksc->dlr;
         }
       while(!(ksc->csr & KSC_CSR_DONE));
     }
   rtems_interrupt_enable(level);
   return;
}

/****************************************************************************
*
*   Start execution of sequencer macro at location func.
****************************************************************************/
static void fb_exec(int func)
{
   struct fastd fbcmd;
   int          tmo = 7;

   fbcmd.a = 7;
   fbcmd.f = 0;
   fastio(&fbcmd);
   if ((fbcmd.d & 0x20) != 0) fb_reset_seq();
   fbcmd.a = 1;
   fbcmd.f = 1;
   fbcmd.d = func * 8;
   fastio(&fbcmd);              /* Load the starting address  */
   fbcmd.a = 7;
   fbcmd.d = 1;
   fastio(&fbcmd);              /* Start execution  */
   fbcmd.f = 0;
/*
*    Wait till execution is complete or a timeout occurs.
*/
   do 
     {
       fastio(&fbcmd);
       if ((fbcmd.d & 0x20) == 0) break;
     }
   while(--tmo);
/*
*    A timeout is indicated by tmo being zero.  Otherwise, the sequencer
*    has returned to the idle loop.
*/
   if (tmo) return;
   else
     { 
/*
*    On timeout, send message to the host.
*/
       sprintf(error_msg,"Timeout executing macro at %x, LRS status = %x",
								func,fbcmd.d);
       host_message(PANIC,error_msg,"VMEacq  ");
     }
}
/****************************************************************************
*
*   Write dat to Host I/O Reg. 2 for the sequencer read.
****************************************************************************/
static void fb_write_pdreg(int dat)
{
   struct fastd fbcmd;

   fbcmd.a = 0;
   fbcmd.f = 1;
   fbcmd.d = 0x9900;
   fastio(&fbcmd);              /* set host/sequencer direction register */
   fbcmd.a = 2;
   fbcmd.d = dat;
   fastio(&fbcmd);              /* Write host register 2 for use  by the seq */
}
/****************************************************************************
*
*   Read the PDREG in the sequencer.  Generally a nonzero value means
*   an error.
****************************************************************************/
static unsigned char fb_read_pdreg(void)
{
   struct fastd fbcmd;

   fbcmd.a = 0;
   fbcmd.f = 1;
   fbcmd.d = 0x9600;
   fastio(&fbcmd);
   fbcmd.a = 2;
   fbcmd.f = 0;
   fastio(&fbcmd);
   return(fbcmd.d);
}
//More FASTBUS commenting out by MCSQ?
//#ifdef DEBUG

///****************************************************************************
//*
//*   Read register in the 1821 sequencer
//****************************************************************************/
//static unsigned short fb_read_reg(int reg)
//{
//   struct fastd fbcmd;
//
//   fbcmd.a = reg;
//   fbcmd.f = 0;
//   fastio(&fbcmd);
//   return(fbcmd.d);
//}
///****************************************************************************
//*
//*   Write register in the 1821 sequencer
//****************************************************************************/
//static void fb_write_reg(int reg,unsigned short dat)
//{
//   struct fastd fbcmd;
//
//   fbcmd.a = reg;
//   fbcmd.f = 1;
//   fbcmd.d = dat;
//   fastio(&fbcmd);
//   return;
//}
///****************************************************************************
//*
//*  Read the FASTBUS 32 bit register.
//****************************************************************************/
//static int fb_read32(void)
//{
//   static int     temp;
//   unsigned char *cptr = (unsigned char *)&temp;
//
//   fb_exec(RD_BYTE3);
//   *cptr++ = fb_read_pdreg();
//   fb_exec(RD_BYTE2);
//   *cptr++ = fb_read_pdreg();
//   fb_exec(RD_BYTE1);
//   *cptr++ = fb_read_pdreg();
//   fb_exec(RD_BYTE0);
//   *cptr = fb_read_pdreg();
//   return(temp);
//}
//#ifdef
//End of MCSQ fastbus commenting?

/****************************************************************************
*
*   Write the FASTBUS 32 bit register.
****************************************************************************/
static void fb_write32(int dat)
{
   unsigned short *uptr = (unsigned short *)&dat;

   fb_write_pdreg(*uptr++);
   fb_exec(WR32_H);
   fb_write_pdreg(*uptr);
   fb_exec(WR32_L);
}
/****************************************************************************
*
*   Do a primary address cycle to a FASTBUS module.
*
*     type -  Address space type (i.e. CSR or DSR).
*     slot -  Module physical address
****************************************************************************/
static int fb_prim_addr(int type,int slot)
{
   int  temp;

   fb_error = FALSE;
   fb_module = slot;
   fb_nta = -1;
   fb_write_pdreg(slot);
   fb_exec(type);
   if ((temp = fb_read_pdreg())) fb_getSS("PADR",temp);
   return(fb_error);
}
/****************************************************************************
*
*   Write NTA in the previously addressed module.
*
*   sec  -  NTA, Next Transfer Address
****************************************************************************/
static void fb_sec_addr(int sec)
{
   int  temp;

   if (fb_error == TRUE) return;
   fb_nta = sec;
   fb_write32(sec);
   fb_exec(WR_NTA);
   if ((temp = fb_read_pdreg())) fb_getSS("SADR",temp);
}
/****************************************************************************
*
*   Get the Slave Status for the last FASTBUS operation.
****************************************************************************/
static void fb_getSS(char *fault,char code)
{
   struct fastd fbcmd;

   fb_error = TRUE;
   fbcmd.a = 0;
   fbcmd.f = 1;
   fbcmd.d = 0x9600;
   fastio(&fbcmd);
   fbcmd.a = 2;
   fbcmd.f = 0;
   fastio(&fbcmd);
   fbcmd.d = (fbcmd.d & 0x700) >> 8;
   sprintf(error_msg,"%s error, Module = %d, NTA = %d, SS = %d, DATA = %d",
                                          fault,fb_module,fb_nta,code,fbcmd.d);
   host_message(PANIC,error_msg,"VMEacq  ");
}
/****************************************************************************
*
*   Reset the LRS1821 sequencer and the reset the FASTBUS.
****************************************************************************/
static void fb_reset_seq(void)
{
   struct fastd fbcmd;
   int          i;
   
   fbcmd.a = 3;
   fbcmd.f = 1;
   fbcmd.d = 0;                 /* Turn on 1821 Reset bit                 */
   fastio(&fbcmd);
   for(i=0; i < 30; i++);
   fbcmd.d = 0x8000;            /* After time delay, turn off 1821 Reset  */
   fastio(&fbcmd);
   fbcmd.a = 0;
   fbcmd.d = 0x9980;            /* Set sequencer to Load state            */
   fastio(&fbcmd);
   fbcmd.a = 1;
   fbcmd.d = 0;                 /* Set sequencer address to zero          */
   fastio(&fbcmd);
   fbcmd.a = 0;
   fbcmd.d = 0x9600;            /* Start sequencer at zero                */
   fastio(&fbcmd);
}
/****************************************************************************
*
*   Routine to do one CAMAC CNAF.  The CAMAC operation is described in the
*   structure camac.  A software timeout is used to prevent hanging when
*   the addressed crate is switched off-line.  The acquisition interrupt
*   is disabled while the KSC 2917 is in use.
*
*  Call:   pointer to struct camac
*
*  Return: KSC 2917 status register.
*
****************************************************************************/
static int ksc_camio(struct camac *cambuf)
{
   int                      crate;
   register struct KSC_VME *ksc = NULL;
   register int             wd0,wd1;
   register unsigned short  status;
   register int tmo = 15;       /* Timeout is approx. 1.5*tmo microseconds */
      rtems_interrupt_level level = 0;

   crate = cambuf->c;
   if (crate <= 7) ksc = ksc1_ptr;
   else if ((crate >= 10) && (crate <= 17))
     {
       crate -= 10;
       ksc = ksc2_ptr;
     }
   if (ksc == NULL) return(KSC_CSR_TMO);
/*
*   Build the command words for KSC 2917 interface from the data in struct
*   camac.
*/
   wd0 = CAM(crate,WS16,A_DIS);
   wd1 = NAF(cambuf->n,cambuf->a,cambuf->f);

   switch  (cambuf->f & 0x18)
     {
/*
*    CAMAC non-data transfer functions - F(8) thru F(15) & F(24) thru F(31)
*/
       case  0x8:
       case  0x18:
         rtems_interrupt_disable(level);  /* disable acq interrupts */
	 ksc->cma = 0;
	 ksc->cmr = wd0;
	 ksc->cmr = wd1;
	 ksc->cmr = HALT;
	 ksc->cma = 0;
	 ksc->csr = KSC_CSR_GO;
	 break;
/*
*    CAMAC read functions - F(0) thru F(7)
*/
       case  0x0:
         rtems_interrupt_disable(level);  /* disable acq interrupts */
	 ksc->cma = 0;
	 ksc->cmr = wd0;
	 ksc->cmr = wd1;
	 ksc->cmr = HALT;
	 ksc->cma = 0;
	 ksc->csr = KSC_CSR_GO;
         eieio();
	 do
	  {
	    if(ksc->csr & KSC_CSR_RDY)
	     {
	      cambuf->d = ksc->dlr;
	      break;
	     }
	    tmo--;
	  } while(!(ksc->csr & KSC_CSR_DONE) && tmo);
	 break;
/*
*   CAMAC write functions - F(16) thru F(23)
*/
       case  0x10:
         rtems_interrupt_disable(level);  /* disable acq interrupts */
	 ksc->cma = 0;
	 ksc->cmr = wd0;
	 ksc->cmr = wd1;
	 ksc->cmr = HALT;
	 ksc->cma = 0;
	 ksc->csr = KSC_CSR_CAM_WR | KSC_CSR_GO;
         eieio();
	 do
	  {
	    if(ksc->csr & KSC_CSR_RDY)
	     {
	      ksc->dlr = cambuf->d;
	      break;
	     }
	    tmo--;
	  } while(!(ksc->csr & KSC_CSR_DONE) && tmo);
	 break;
     }
/*
*   If the CAMAC operation was executed, we return the KSC 2917 status
*   register.  On a timeout (i.e. tmo == 0), clear the run flag in the
*   KSC 2917 and return timeout status.  NOTE: The caller cannot 
*   distinguish between a crate off-line and a nonexistent crate!!
*/
   eieio();
   do
     {
       status = ksc->csr;
     } while (tmo-- && !(status & KSC_CSR_DONE));
   if (tmo < 0)
     {
       status = KSC_CSR_TMO | KSC_CSR_NOX | KSC_CSR_NOQ;
       ksc->csr = KSC_CSR_RST;
     }
   else if ((status & KSC_CSR_TMO) != 0)
                                    status = status | KSC_CSR_NOX |KSC_CSR_NOQ;
   rtems_interrupt_enable(level);  /* restore interrupt level */
   return (status);
}
/****************************************************************************
*
*  Do I/O to registers in the LRS1821 Sequencer.   
*
****************************************************************************/
/*
static void fblist(struct fastd *fast)
{
  static unsigned short *lrs = (unsigned short *)LRS1;
  rtems_interrupt_level level;

  rtems_interrupt_disable(level);
  do
    {
      if ((fast->f & 0x10) == 0)
        {

//   Write function

	  *(lrs+SIB_DIRECT(MOD1821,(fast->a >> 1))) = fast->d;
	  eieio();
        }
      else
        {
	  
	   //    Read function
	   
          fast->d = *(lrs+SIB_DIRECT(MOD1821,(fast->a) >> 1));
	  eieio();
        }
    }
  while (((fast++)->f & 0x20) == 0);
  rtems_interrupt_enable(level);
}
*/
/****************************************************************************
*
*  Do I/O to registers in the LRS1821 Sequencer.   
*
****************************************************************************/
static void fastio(struct fastd *fast)
{
  static unsigned short *lrs = (unsigned short *)LRS1;
  rtems_interrupt_level level;

  rtems_interrupt_disable(level);
  if (fast->f != 0)
    {
/*
*   Write function
*/
     *(lrs+SIB_DIRECT(MOD1821,fast->a)) = fast->d;
     eieio();
    }
  else
    {
/*
*    Read function
*/
      fast->d = *(lrs+SIB_DIRECT(MOD1821,fast->a));
      eieio();
    }
  rtems_interrupt_enable(level);
}
/****************************************************************************
*
*  Set a message to the Host.  If enabled, also output the message
*  to the local terminal attached to the VME processor.
****************************************************************************/
void host_message(int type,char *msg,char *pgm)
{
  static struct sockaddr_in cli_addr;
  static int clilen,seq = 0,sockfd = -1;
  int    i,status;
  static struct VMEmsg *host_msg;
  static struct UDP_Packet out_pkt;

  if(sockfd < 0)
    {
      sockfd = socket(AF_INET,SOCK_DGRAM,0);
      if (sockfd == -1) {perror("VMEacq - socket error"); exit(1);}
      clilen = sizeof(cli_addr);
      host_msg = (struct VMEmsg *)out_pkt.Data;
      i = sizeof(struct VMEmsg);
      out_pkt.DataSize = i;
      word_swap((unsigned short *)&out_pkt.DataSize,2);
      byte_swap((unsigned char *)&out_pkt.DataSize,4);
/********
printf("i = %x, DataSize = %x\n",i,out_pkt.DataSize);
********/
    }
   strcpy(host_msg->sender,pgm);
   host_msg->type = type;
   strcpy(host_msg->text,msg);
   cli_addr = share->Host_Ether_Adr;
   cli_addr.sin_port = htons(45000+PROTO_FEMSG);
   cli_addr.sin_family = AF_INET;
   out_pkt.Sequence = seq;
   word_swap((unsigned short *)&out_pkt.Sequence,2);
   byte_swap((unsigned char *)&out_pkt.Sequence,4);
/********
printf("seq = %x, DataSize = %x\n",out_pkt.Sequence,out_pkt.DataSize);
printf("%s\n",msg);
********/
   i = sizeof(struct VMEmsg);
   i = i + PKTHDRLEN;
   status=sendto(sockfd,&out_pkt,i,0,(struct sockaddr *)&cli_addr,clilen);
   if (status < 0) {
      perror("VMEacq - error at sendto");
   }
/*************
   level = set_intr_level_(0);
   delay_(2);
   set_intr_level_(level);
*************/

   rtems_task_wake_after(2);

#ifdef  LOCAL_MSG
   printf("%s\n",msg);
#endif
}
#ifdef  DEBUG
/****************************************************************************
****************************************************************************/
static int  camac_list(unsigned short cma,struct cam_io *cam)
{
  int  c,n,a,f,wd1,wd2,count = 0;
  int  last_dir;
  unsigned short lcma = cma;
  struct KSC_VME *ksc = (struct KSC_VME *)KSC1;
  rtems_interrupt_level level;

  if (lcma == 0) return(0);
  if (cam->dir == CAM_NODATA || cam->dir == CAM_RD) last_dir = CAM_RD;
  else  last_dir = CAM_WR;
  while(cam->dir != CAM_RO_END)
    { 
      rtems_interrupt_disable(level);
      ksc->cma = lcma;
      eieio();
      wd1 = ksc->cmr;
      wd2 = ksc->cmr;
      rtems_interrupt_enable(level);
      lcma += 2;
      if (cam->dir != CAM_NODATA && cam->dir != last_dir)
        {
          if (wd1 != HALT)
            {
              sprintf(error_msg,"Change of transfer direction without HALT");
              host_message(INFORM,error_msg,"VMEacq  ");
            }
          last_dir = cam->dir;
          --lcma;
          continue;
        }
      c = wd1 >> 8;
      f = wd2 & 0x1f;
      a = (wd2 >> 5) & 0xf;
      n = (wd2 >> 9) & 0x1f;
      sprintf(error_msg,"c = %d, n = %d, a = %d, f = %d, dir = %d, id/data = %x",
                                                   c,n,a,f,cam->dir,cam->dat);
      host_message(INFORM,error_msg,"VMEacq  ");
      ++cam;
      ++count;
    }
  return(count);
}
/****************************************************************************
*
*  Formats CNAF and data structures for host messages
****************************************************************************/
static void clist(struct cnafdat_list *naf,int wc)
{
  int  i;
  sprintf(error_msg,"count = %d",wc);
  host_message(INFORM,error_msg,"VMEacq  ");
  for(i=0; i < wc; ++i)
   {
    sprintf(error_msg,"c = %d, n = %d, a = %d, f = %d, data = %x",
				      naf->c,naf->n,naf->a,naf->f,naf->d);
    host_message(INFORM,error_msg,"VMEacq  ");
    naf++;
   }
}
#endif
/****************************************************************************
*
*   The following routines execute when an event interrupt occurs.  There are
*   several important considerations for code here:
*     1)  The task ID will be that of the INTERRUPTED TASK.  Avoid any
*         VMEPROM function which uses the task ID for any purpose.
*         You will find an exception to this rule in the routine 
*         "send_event".  A message pointer is sent to another task
*         which processes data buffers for transmission to the host.
*         This case works because the receiving task does not use
*         the source task ID.  However,  waiting for event flags (
*         logical or physical) CANNOT be used.
*
*     2)  Execution will be in supervisor mode and the stack will be the
*         supervisor stack of the INTERRUPTED TASK.  These stacks are by
*         default much smaller than user mode stacks.  So excessive stack
*         storage should be avoided (i.e. use mostly static variables).
*
*     3)  The clock interrupt should be at the same, or a lower, interrupt
*         level as the event interrupt.  This prevents task rescheduling
*         by the clock interrupt while in these routines.  However, it
*         also means we CANNOT call any routines which require execution
*         of another task for completion (i.e. any Ethernet service call).
****************************************************************************
*
*   This function is called from the Event interrupt routine.  Functions are
*
*      1)  Readout and format event data as specified the the loaded PAC.
*
*          a)  Read all gated latches.
*
*          b)  Execute the Kill_List if any.
*
*          c)  Loop executing functions specified in the CAMAC, FASTBUS and FERA
*              function lists.  The loop continues until all functions in all
*              lists have completed.
*
*      2)  When a buffer is full, trigger the task which formats Ethernet
*          packets and sends then to the host.
*
****************************************************************************/
static void  process_event(void *evtarg, unsigned long evtvec)
{

   struct trig *trigger = (struct trig *)TRIGGER;
   struct KSC_VME *ksc = (struct KSC_VME *)KSC1;
   struct acq_share *shr = (struct acq_share *)ACQ_SHARED;
   int size,status;
   register int (**camac_pgm)(void); 
   register int (**fb_pgm)(void); 
   register int (**fera_pgm)(void); 


/*   Patch   */
/****************
while(1) {
trigger->event_clear = 0;
}
share->event_number++;
return;
****************/
/*  end Patch */

#ifdef  TIMER
   timer[Tlatch] = *((unsigned char *)ORNL_COUNT);
#endif

/*
*   Patch for RMS system.  We were getting event interrupts without
*   the busy latch in the event module being set.  When this happens,
*   the trigger module delay time counter does not count and any PAC
*   which has a readout delay spec will hang.  So we test the counter
*   for zero here and if it is just return from the interrupt.
*
*   NOTE: Normally the counter should be about 12 when we get here.
*/

   if (*((unsigned char*)ORNL_COUNT) == 0)
     {
       trigger->event_clear = 0;
       eieio();
       return;
     }
   Startevt = Event;
/*
*   Readout gated latches if any were specified in PAC
*/
   if (Latch_flag != 0)
     {
       unsigned short *uptr = Latch_data + 1;

       if (Latch_cma != 0)
         {
           unsigned short *lat_ro = Latch_ro,dat;
/*
*   Read KSC gated latches
*/
           ksc->cma = Latch_cma;
           ksc->csr = KSC_CSR_CAM_RD | KSC_CSR_GO;
           eieio();
           do
             {
               if (ksc->csr & KSC_CSR_RDY)
                 {
                   dat = ksc->dlr;
                   *uptr++ = dat;
                   if (*lat_ro != 0)
                     {
                       *Event++ = *lat_ro;
                       *Event++ = dat;
                     }
                   lat_ro++;
                 }
             }
           while(!(ksc->csr & KSC_CSR_DONE));
         }
       if (Latch_aux != 0)
         {
           struct fcam_io *lat = Flatch_ro;
             struct FCAM *fcam = (struct FCAM *)ORNLAUX;

           do
             {
               fcam->dat.cnaf = lat->cnaf;
               eieio();
               if (lat->id != 0)
                 {
                   *Event++ = lat->id;
                   *uptr = fcam->dat.s;
                   *Event++ = *uptr++;
                 }
               else *uptr++ = fcam->dat.s;
               lat++;
             }
           while (lat->cnaf != 0);
         }
     }
/*
*  Do gates if any specified in PAC
*/
   if (Gate_RO_str != NULL)
     {
       run_gate_list();
       if (Count_dwn_str != NULL && run_count_down())
         {
           register int (**kill_pgm)(void) = kill_seq;

           while(*kill_pgm != NULL)  kill_pgm += (*kill_pgm)();
           trigger->event_clear = 0;
	   eieio();
           Event = Startevt;
           return;
         }
     }
/*
*   Execute Kill tests only if specified in PAC
*/
   if (kill_count != 0 && run_kill())
     {
       register int (**kill_pgm)(void) = kill_seq;

       while(*kill_pgm != NULL)  kill_pgm += (*kill_pgm)();
       trigger->event_clear = 0;
       eieio();
       Event = Startevt;
       return;
     }
/*
*   VME 100Hz clock
*/
   if (clk100_flg != 0)
     {
       *Event++ = clk_id_hi;
       *Event++ = 0x7fff & *clk100;
       *Event++ = clk_id_low;
       *Event++ = *(clk100+1);
     }

   do
     {
       int count;

       camac_pgm = camac_seq;
       fb_pgm = fb_seq;
       fera_pgm = fera_seq;
       do
         {
           count = 0;
           if (*fb_pgm != NULL)
             {
               fb_pgm += (*fb_pgm)();
               ++count;
             }
           if (*fera_pgm != NULL)
             {
               fera_pgm += (*fera_pgm)();
               ++count;
             }
           if (*camac_pgm != NULL)
             {
               camac_pgm += (*camac_pgm)();
               ++count;
             }
         }
       while(count);
       break;
     }
   while(1);
/*
*   Process the Windup list
*/
   if (Windup_str != NULL) run_windup_list();
   if (Fwindup_str != NULL) run_fwindup_list();

#ifdef  TIMER
   timer[Tend] = *((unsigned char *)ORNL_COUNT);
#endif

   trigger->event_clear = 0;
   eieio();

/************
   *Event++ = 0xdfff;
   *Event++ = (short)(shr->event_number + AcqBuf->Bufhdr.events);
************/

   if (Event != Startevt)
     {
       *(unsigned int *)Event = 0xffffffff;  /* put in the End-Of-Event flag */
       Event += 2;
/*
*   Increment event counter and then check for space for another event.
*/
       AcqBuf->Bufhdr.events += 1;
       size = (Event - AcqBuf->Bufhdr.str_buf) * sizeof(unsigned short);
       if (size >= MAX_PKT_DATA - shr->avg_param_size)
         {
           status = send_event();       /* when buffer is full, send to host */
         }
       else
         {
/*
*    There is more room in the inn
*/
           AcqBuf->Bufhdr.last_event = Event;
         }
     }

#ifdef  TIMER
   timer[Tendall] = *((unsigned char *)ORNL_COUNT);
#endif

   return;
}
/****************************************************************************
*
*  This becomes the interrupt routine when all acquisition data buffers
*  are in use.  The first time here is due to an Event interrupt.  Thereafter
*  the ORNL trigger module timer interrupt brings us here.  Test the assigned
*  buffer and return until it is free.  When it becomes free, reenable the
*  Event interrupt and disable the timer interrupt.  Set interrupt handler
*  back to normal Event interrupt service routine.
****************************************************************************/
static void  trig_timer(void *trigarg, unsigned long trigvec)
{

   struct trig *trigger = (struct trig *)TRIGGER;


   if (Trig_Timer_Flag <= 0)
     {
       trigger->imra = EVT_INTR_ENA;
       eieio();
       return;
     }
   else if(Trig_Timer_Flag == 1)
     {
       if (NewBuf->Bufhdr.busy != 0) return;
       NewBuf->Bufhdr.events = 0;
       Event = NewBuf->Bufhdr.str_buf;
       AcqBuf = NewBuf;
       NewBuf = NULL;
       trigger->imra = EVT_INTR_ENA;
       eieio();
       Trig_Timer_Flag = 0;
       return;
     }
   else if (Trig_Timer_Flag == 2)
     {
       if (Buf1.Bufhdr.ack != 0 || Buf2.Bufhdr.ack != 0) return;   
       AcqBuf->Bufhdr.ack = -1;
       AcqBuf->Bufhdr.busy = -1;

       Que1buf = AcqBuf;
       rtems_message_queue_send(Que1id,&Que1buf,4);
/*
*   Switch buffers for the acquisition process.
*/
       if (AcqBuf == &Buf1)
        {
/*
*   We were using Buf1, so switch to Buf2.
*/
         if (Buf2.Bufhdr.busy < 0)
           {
             NewBuf = &Buf2;
             trigger->imra = TIMER_INTR_ENA;
	     eieio();
             Trig_Timer_Flag = 1;
             return;
           }
         NewBuf = NULL;
         Buf2.Bufhdr.events = 0;
         AcqBuf = &Buf2;
         Event = Buf2.Data;
        }
       else
        {
/*
*   We were using Buf2, so switch to Buf1.
*/
         if (Buf1.Bufhdr.busy < 0)
           {
             NewBuf = &Buf1;
             trigger->imra = TIMER_INTR_ENA;
	     eieio();
             Trig_Timer_Flag = 1;
             return;
           }
         NewBuf = NULL;
         Buf1.Bufhdr.events = 0;
         AcqBuf = &Buf1;
         Event = Buf1.Data;
        }
       trigger->imra = EVT_INTR_ENA;
       eieio();
       Trig_Timer_Flag = 0;
     }
}
/****************************************************************************
*
*   Send a message pointer to the task which formats Ethernet packets and
*   sends them to the host.  Switch to the next buffer and return.
*
*   If we cannot send a message pointer or the next buffer is not available
*   switch to an alternate interrupt service routine.  The event interrupt
*   is disabled by the alternate handlers and on interrupt by PI/T inter #2
*   check for buffer available.  When a buffer is available switch back
*   to the normal Event interrupt service routine.
****************************************************************************/
static int send_event(void)
{
   struct acq_share *shr = (struct acq_share *)ACQ_SHARED;
   struct trig *trigger = (struct trig *)TRIGGER;

/*
*   Send data buffer to Ethernet output task.
*/
   AcqBuf->Bufhdr.totalevents = shr->event_number;
   shr->event_number += AcqBuf->Bufhdr.events;
   AcqBuf->Bufhdr.end_buf = Event;
   if (Buf1.Bufhdr.ack != 0 || Buf2.Bufhdr.ack != 0)
     {
       trigger->imra = TIMER_INTR_ENA;
       eieio();
       Trig_Timer_Flag = 2;
       return (1);
     }
   AcqBuf->Bufhdr.ack = -1;
   AcqBuf->Bufhdr.busy = -1;
   Que1buf = AcqBuf;
   rtems_message_queue_send(Que1id, &Que1buf, 4);

/*
*   Switch buffers for the acquisition process.
*/
   if (AcqBuf == &Buf1)
     {
/*
*   We were using Buf1, so switch to Buf2.
*/
       if (Buf2.Bufhdr.busy < 0)
         {
           NewBuf = &Buf2;
           trigger->imra = TIMER_INTR_ENA;
	   eieio();
           Trig_Timer_Flag = 1;
           return (1);
         }
       Buf2.Bufhdr.events = 0;
       AcqBuf = &Buf2;
       Event = Buf2.Data;
     }
   else
     {
/*
*   We were using Buf2, so switch to Buf1.
*/
       if (Buf1.Bufhdr.busy < 0)
         {
           NewBuf = &Buf1;
           trigger->imra = TIMER_INTR_ENA;
	   eieio();
           Trig_Timer_Flag = 1;
           return (1);
         }
       Buf1.Bufhdr.events = 0;
       AcqBuf = &Buf1;
       Event = Buf1.Data;
     }
   return (0);
}
/******************************************************************************
******************************************************************************/
static int run_uncond_list(void)
{
   struct KSC_VME *ksc;
   unsigned short *evt;
    struct cam_io *cam;

#ifdef  TIMER
   timer[Tuncond] = *((unsigned char *)ORNL_COUNT);
#endif

/*
*   Check for readout delay.
*/
   if(*((unsigned char *)ORNL_COUNT) < cam_Uncond_delay) return(0);

#ifndef ORNL
   if (*((volatile unsigned char *)ORNL_COUNT) < cam_Uncond_delay) return(0);
#endif

   ksc = (struct KSC_VME *)KSC1;
   evt = Event;
   cam = Uncond_RO_str;
   ksc->cma = Uncond_cma;
   do
     {
       if (cam->dir == CAM_RD)
         {
           ksc->csr = KSC_CSR_CAM_RD | KSC_CSR_GO;
           eieio();
           do
             {
               *evt++ = cam->dat;
               ++cam;
               while(cam->dir == CAM_NODATA)  ++cam;
               while (!(ksc->csr & KSC_CSR_RDY));
               *evt = ksc->dlr;
               ++evt;
             }
           while(cam->dir == CAM_RD);
         }
       else if (cam->dir == CAM_WR)
         {
           ksc->csr = KSC_CSR_CAM_WR | KSC_CSR_GO;
           eieio();
           do
             {
               while (!(ksc->csr & KSC_CSR_RDY));
               ksc->dlr = cam->dat;
               ++cam;
               while(cam->dir == CAM_NODATA)  ++cam;
             }
           while(cam->dir == CAM_WR);
         }
       else
         {
           ksc->csr = KSC_CSR_GO;
           eieio();
           while(cam->dir == CAM_NODATA)  ++cam;
         }
       while(!(ksc->csr & KSC_CSR_DONE));
     }
   while(cam->dir != CAM_RO_END);
   Event = evt;
/*
*    If we have readout via the list sequencer, start the transfer from
*    the sequencer to CPU-40 memory.
*/
   if (Seq_cma != 0) read_list_seq();
   return(1);
}
/******************************************************************************
******************************************************************************/
static int run_funcond_list(void)
{
    unsigned short *evt;
    struct fcam_io *cam;
      struct FCAM *fcam;

/*
*   Check for readout delay.
*/
   if(*((unsigned char *)ORNL_COUNT) < cam_Uncond_delay) return(0);

#ifndef ORNL
   if (*((volatile unsigned char *)ORNL_COUNT) < cam_Uncond_delay) return(0);
#endif

   evt = Event;
   cam = Funcond_RO_str;
   fcam = (struct FCAM *)ORNLAUX;
   while(cam->cnaf != 0)
     {
       fcam->dat.cnaf = cam->cnaf;
       eieio();
       *evt++ = cam->id;
       cam++;
       *evt++ = fcam->dat.s;
     }
   Event = evt;
   return(1);
}
/******************************************************************************
******************************************************************************/
static int run_windup_list(void)
{
   struct KSC_VME *ksc = (struct KSC_VME *)KSC1;
    struct cam_io *cam = Windup_str;


#ifdef  TIMER
   timer[Twind] = *((unsigned char *)ORNL_COUNT);
#endif

   ksc->cma = Windup_cma;
   do
     {
       if (cam->dir == CAM_WR)
         {
           ksc->csr = KSC_CSR_CAM_WR | KSC_CSR_GO;
           eieio();
           do
             {
               while (!(ksc->csr & KSC_CSR_RDY));
               ksc->dlr = cam->dat;
               ++cam;
               while(cam->dir == CAM_NODATA)  ++cam;
             }
           while(cam->dir == CAM_WR);
         }
       else
         {
           ksc->csr = KSC_CSR_GO;
           eieio();
           while(cam->dir == CAM_NODATA)  ++cam;
         }
       while(!(ksc->csr & KSC_CSR_DONE));
     }
   while(cam->dir != CAM_RO_END);
   return(1);
}
/******************************************************************************
******************************************************************************/
static int run_fwindup_list(void)
{
   volatile unsigned short tmp;
   struct FCAM *fcam = (struct FCAM *)ORNLAUX;
   struct fcam_io *cam = Fwindup_str;

   while(cam->cnaf != 0)
    {
      fcam->dat.cnaf = cam->cnaf;
      eieio();
      cam++;
      tmp = fcam->dat.s;
    }

   return(1);
}
/******************************************************************************
******************************************************************************/
static int read_list_seq(void)
{
   struct KSC_VME *ksc = (struct KSC_VME *)KSC1;

#ifdef  TIMER
   timer[Trdseq] = *((unsigned char *)ORNL_COUNT);
#endif

   ksc->cma = Seq_cma;
   ksc->maclo = (unsigned short)seq_buf;
   ksc->machi = (unsigned short)seq_buf >> 16;
   ksc->mtc = (unsigned short)seq_transfers+1;
   ksc->cse = KSC_CSE_COC | KSC_CSE_ABT | KSC_CSE_ERR;
   ksc->doc = KSC_DOC_CAM_RD;
   ksc->scc = KSC_SCC_STR;
   ksc->csr = KSC_CSR_DMA | KSC_CSR_CAM_RD | KSC_CSR_GO;
   eieio();
   return(1);
}
/******************************************************************************
******************************************************************************/
static int format_list_seq(void)
{
   struct KSC_VME *ksc = (struct KSC_VME *)KSC1;
   register unsigned short *evt = Event;
   register unsigned short *id = Seq_ID_str;
   register unsigned short *buf_ptr;
   register            int lcount;

   while((ksc->csr & KSC_CSR_DONE) == 0 && (ksc->cse & KSC_CSE_COC) == 0)
                                                                     return(0);
#ifdef  TIMER
   timer[Tfmtseq] = *((unsigned char *)ORNL_COUNT);
#endif

   if (ksc->mtc != 0) return(1);
/************
   cache_inval_(1,seq_buf,&seq_buf[200]);
************/
   lcount = seq_transfers;
   buf_ptr = seq_buf;
   while(lcount)
    {
      *evt++ = *id++;
      *evt++ = *buf_ptr++;
      --lcount;
    }
    ksc->csr = 0x0;
    Event = evt;

#ifdef  TIMER
   timer[Tfmtseqend] = *((unsigned char *)ORNL_COUNT);
#endif

    return(1);
}
/******************************************************************************
******************************************************************************/
static int run_cond_list(void)
{

  register struct KSC_VME  *ksc;
  register struct cond_tbl *ctbl;
  register unsigned short  *evt;
  register struct cam_io   *cam;


#ifdef  TIMER
   timer[Tcond] = *((unsigned char *)ORNL_COUNT);
#endif

/*
*   Check for readout delay.
*/
   if(*((unsigned char *)ORNL_COUNT) < cam_Cond_delay) return(0);

#ifndef ORNL
   if (*((volatile unsigned char *)ORNL_COUNT) < cam_Cond_delay) return(0);
#endif

  ksc = (struct KSC_VME *)KSC1;
  ctbl = Cond_RO_str;
  evt = Event;
  cam = Cond_cam_str;
  while(ctbl != NULL)
   {
     if (*(ctbl->lat) & ctbl->mask)
       {
         if ((ksc->cma = ctbl->tr_cma) == 0)
           {
             ctbl = ctbl->tr_next;
             continue;
           }
         cam = ctbl->tr_camid;
         ctbl = ctbl->tr_next;
       }
     else
       {
         if ((ksc->cma = ctbl->fa_cma) == 0)
           {
             ctbl = ctbl->fa_next;
             continue;
           }
         cam = ctbl->fa_camid;
         ctbl = ctbl->fa_next;
       }
     do
       {
         if (cam->dir == CAM_RD)
           {
             ksc->csr = KSC_CSR_CAM_RD | KSC_CSR_GO;
             eieio();
             do
               {
                 *evt = cam->dat;
                 ++evt;
                 ++cam;
                 while(cam->dir == CAM_NODATA)  ++cam;
                 while (!(ksc->csr & KSC_CSR_RDY));
                 *evt = ksc->dlr;
                 ++evt;
               }
             while(cam->dir == CAM_RD);
           }
         else if (cam->dir == CAM_WR)
           {
             ksc->csr = KSC_CSR_CAM_WR | KSC_CSR_GO;
             eieio();
             do
               {
                 while (!(ksc->csr & KSC_CSR_RDY));
                 ksc->dlr = cam->dat;
                 ++cam;
                 while(cam->dir == CAM_NODATA)  ++cam;
               }
             while(cam->dir == CAM_WR);
           }
         else
           {
             ksc->csr = KSC_CSR_GO;
             eieio();
             while(cam->dir == CAM_NODATA)  ++cam;
           }
         while(!(ksc->csr & KSC_CSR_DONE));
       }
     while(cam->dir != CAM_RO_END);
   }
  Event = evt;
  return(1);
}
/******************************************************************************
******************************************************************************/
static int run_fcond_list(void)
{

  register struct FCAM     *fcam;
  register struct fcond_tbl *ctbl;
  register unsigned short  *evt,tmp;
  register struct fcam_io  *cam;

/*
*   Check for readout delay.
*/
   if(*((unsigned char *)ORNL_COUNT) < cam_Cond_delay) return(0);

#ifndef ORNL
   if (*((volatile unsigned char *)ORNL_COUNT) < cam_Cond_delay) return(0);
#endif

  fcam = (struct FCAM *)ORNLAUX;
  ctbl = Fcond_RO_str;
  evt = Event;
  cam = Fcond_cam_str;
  while(ctbl != NULL)
   {
     if (*(ctbl->lat) & ctbl->mask)
       {
         if (ctbl->tr_ctr == 0)
           {
             ctbl = ctbl->tr_next;
             continue;
           }
         cam = ctbl->tr_camid;
         ctbl = ctbl->tr_next;
       }
     else
       {
         if (ctbl->fa_ctr == 0)
           {
             ctbl = ctbl->fa_next;
             continue;
           }
         cam = ctbl->fa_camid;
         ctbl = ctbl->fa_next;
       }
     do
       {
         fcam->dat.cnaf = cam->cnaf;
         eieio();
         if (cam->dir == CAM_NODATA)
           {
             cam++;
             tmp = fcam->dat.s;
           }
         else
           {
             *evt++ = cam->id;
             cam++;
             *evt++ = fcam->dat.s;
           }
       }
     while(cam->cnaf);
   }
  Event = evt;
  return(1);
}
/******************************************************************************
******************************************************************************/
static int fb_wait_done(void)
{
   unsigned short *lrs = (unsigned short *)LRS1;
   unsigned short test=0;
/*
*   Wait for the 1821 code to finish.
*/
   test=*(lrs+SIB_DIRECT(MOD1821,7));
   eieio();
   if (test & 0x20) return(0);
   return(1);
}
/*****************************************************************************
*
*   Format the data from LeCroy 1885 FASTBUS ADCs.
******************************************************************************/
static int fb_format_1885(void)
{
            struct LRS_VME *lrs_cmd_ptr = (struct LRS_VME *)LRS1;
              register int idoff;
   register unsigned short *dat,*dat_end;
   register unsigned short *evt;
   register unsigned short *id;


#ifdef  TIMER
   timer[Tadcform] = *((unsigned char *)ORNL_COUNT);
#endif

/*
*    Wait till DMA transfer complete.
*/
   if (lrs_cmd_ptr->csr & LRS_CSR_RUN) return(0);

   evt = Event;
   id = Fast_id;
   dat = (unsigned short *)(LRS1 + DATA);
   dat_end = dat + fb_1885_count;
   while(dat < dat_end)
    {
      idoff = *dat;
      ++dat;
      *evt++ = *(id+idoff);
      *evt++ = *dat;
      ++dat;
    }
   Event = evt;
   return(1);
}
/******************************************************************************
******************************************************************************/
static int fb_read_1885(void)
{
   unsigned short *lrs = (unsigned short *)LRS1;


#ifdef  TIMER
   timer[Tadcrd] = *((unsigned char *)ORNL_COUNT);
#endif

/*
*   Check for readout delay.
*/
  if(*((unsigned char *)ORNL_COUNT) < fb_delay) return(0);

#ifndef ORNL
  if (*((volatile unsigned char *)ORNL_COUNT) < fb_delay) return(0);
#endif

  if ((fb_1885.lat != NULL) && !(*fb_1885.lat & fb_1885.mask)) return(3);

/*
*   Start readout of ADCs into 1821 memory.  NOTE: Needs a special
*   sequencer read routine which waits till CIP (Front panel input 1)
*   is false before starting the readout.
*/
  *(lrs+SIB_DIRECT(MOD1821,3)) = 0x9d0c;
  *(lrs+SIB_DIRECT(MOD1821,6)) = 0x3000;
  eieio();
  *(lrs+SIB_DIRECT(MOD1821,7)) = 0x10;
  *(lrs+SIB_DIRECT(MOD1821,1)) = fb_1885.start;
  eieio();
  *(lrs+SIB_DIRECT(MOD1821,7)) = 0x1;
  eieio();
  return(1);
}
/******************************************************************************
******************************************************************************/
static int fb_transfer_1885(void)
{
   struct LRS_VME *lrs_cmd_ptr = (struct LRS_VME *)LRS1;
   unsigned short *lrs = (unsigned short *)LRS1;
   int  err,size;


#ifdef  TIMER
   timer[Tadctrans] = *((unsigned char *)ORNL_COUNT);
#endif

/*
*   Wait for the ADCs to be read.
*/
   if (*(lrs+SIB_DIRECT(MOD1821,7)) & 0x20) return(0);
/*
*   Setup and start a DMA transfer from 1821 memory to 1131 memory
*/
   err = (*(lrs+SIB_DIRECT(MOD1821,2)) & 0xff);
   if (err != 0)
     {
       share->FB_error = err;
       return(2);
     }
   size = (*(lrs+SIB_DIRECT(MOD1821,6)) & 0xfff) * 2;
   fb_1885_count = size;
   if(size == 0) return(2);
   *(lrs+SIB_DIRECT(MOD1821,3)) = 0x8012;
   eieio();
   *(lrs+SIB_DIRECT(MOD1821,7)) = 0x10;
   eieio();
   lrs_cmd_ptr->nta = 0;
   lrs_cmd_ptr->sib = SIB(MOD1821,5);
   lrs_cmd_ptr->wc = size;
   eieio();
   lrs_cmd_ptr->csr = LRS_CSR_RUN + LRS_CSR_DMA + LRS_CSR_DMA_RD;
   eieio();
   return(1);
}
/*****************************************************************************
*
*   Format the data from Phillips 10C6 FASTBUS TDCs.
******************************************************************************/
static int fb_format_10C6(void)
{
            struct LRS_VME *lrs_cmd_ptr = (struct LRS_VME *)LRS1;
              register int ch,count,idoff,sub;
   register unsigned short *dat,*dat_end;
   register unsigned short *evt;
   register unsigned short *id;

#ifdef  TIMER
   timer[Ttdcform] = *((unsigned char *)ORNL_COUNT);
#endif

/*
*    Wait till DMA transfer complete.
*/
   if (lrs_cmd_ptr->csr & (unsigned short)LRS_CSR_RUN) return(0);

   evt = Event;
   dat = (unsigned short *)(LRS1 + DATA);
   dat_end = dat + fb_10C6_count;
   while(dat < dat_end)
    {
      idoff = *dat++;
      idoff &= 0x1f;
      idoff *= 2048;
      id = Fast_id + idoff;
      count = *dat++ >> 10;
      while(count > 0)
       {
         ch = *dat;
         ++dat;
         if (ch < 0x8000)
          {
            sub = ch  >> 10;
            *evt++ = *(id+sub);
            *evt++ = ch & 0x3ff;
            --count;
          }
         ch = *dat;
         ++dat;
         if (ch < 0x8000)
          {
            sub = ch  >> 10;
            *evt = *(id+sub);
            ++evt;
            *evt = ch & 0x3ff;
            ++evt;
            --count;
          }
         else if (dat > dat_end)  return(1);
       }
    }
   Event = evt;
   return(1);
}
/******************************************************************************
******************************************************************************/
static int fb_read_10C6(void)
{
   unsigned short *lrs = (unsigned short *)LRS1;

#ifdef  TIMER
   timer[Ttdcrd] = *((unsigned char *)ORNL_COUNT);
#endif

/*
*   Check for readout delay.
*/
  if(*((unsigned char *)ORNL_COUNT) < fb_delay) return(0);

#ifndef ORNL
  if (*((volatile unsigned char *)ORNL_COUNT) < fb_delay) return(0);
#endif

/*
*   Start readout of TDCs into 1821 memory.
*/
  *(lrs+SIB_DIRECT(MOD1821,3)) = 0x840c;
  *(lrs+SIB_DIRECT(MOD1821,6)) = 0x3000;
  eieio();
  *(lrs+SIB_DIRECT(MOD1821,7)) = 0x10;
  *(lrs+SIB_DIRECT(MOD1821,1)) = fb_10C6.start;
  eieio();
  *(lrs+SIB_DIRECT(MOD1821,7)) = 0x1;
  eieio();
  return(1);
}
/******************************************************************************
******************************************************************************/
static int fb_transfer_10C6(void)
{
   struct LRS_VME *lrs_cmd_ptr = (struct LRS_VME *)LRS1;
   unsigned short *lrs = (unsigned short *)LRS1;
   int  err,size;


#ifdef  TIMER
   timer[Ttdctrans] = *((unsigned char *)ORNL_COUNT);
#endif

/*
*   Wait for the TDCs to be read.
*/
   if (*(lrs+SIB_DIRECT(MOD1821,7)) & 0x20) return(0);
   if ((fb_10C6.lat != NULL) && !(*fb_10C6.lat & fb_10C6.mask)) return(2);
/*
*   Setup and start a DMA transfer from 1821 memory to 1131 memory
*/
   err = (*(lrs+SIB_DIRECT(MOD1821,2)) & 0xff);
   if (err != 0)
     {
       share->FB_error = err;
       return(2);
     }
   size = (*(lrs+SIB_DIRECT(MOD1821,6)) & 0xfff) * 2;
   fb_10C6_count = size;
   if (size == 0) return(2);
   *(lrs+SIB_DIRECT(MOD1821,3)) = 0x8012;
   eieio();
   *(lrs+SIB_DIRECT(MOD1821,7)) = 0x10;
   eieio();
   lrs_cmd_ptr->nta = 0;
   lrs_cmd_ptr->sib = SIB(MOD1821,5);
   lrs_cmd_ptr->wc = size;
   eieio();
   lrs_cmd_ptr->csr = LRS_CSR_RUN + LRS_CSR_DMA + LRS_CSR_DMA_RD;
   eieio();
   return(1);
}
/*****************************************************************************
*    Modified by RLV for slight formatting errors in 1881
*   Format the data from  LeCroy 1881M FASTBUS ADCs.
******************************************************************************/
static int fb_format_1881M(void)
{
            struct LRS_VME *lrs_cmd_ptr = (struct LRS_VME *)LRS1;
              register int count,idoff,sub;
   register unsigned short *dat,*dat_end;
   register unsigned short *evt;
   register unsigned short *id;

#ifdef  TIMER
   timer[Tadcform] = *((unsigned char *)ORNL_COUNT);
#endif

/*
*    Wait till DMA transfer complete.
*/
   if (lrs_cmd_ptr->csr & (unsigned short)LRS_CSR_RUN) return(0);

   evt = Event;
   dat = (unsigned short *)(LRS1 + DATA);
   dat_end = dat + fb_1881M_count;
   while(dat < dat_end)
    {
      idoff = *dat++  & 0xf800;
      id = Fast_id + idoff;
      count = (*dat++  & 0x007f) - 1;
      while(count > 0)
       {
         sub = (*dat++ & 0x007e) >> 1;
         *evt++ = *(id+sub);
         *evt++ = *dat++;
         --count;
       }
    }
   Event = evt;
   return(1);
}
/******************************************************************************
******************************************************************************/
static int fb_read_1881M(void)
{
   unsigned short *lrs = (unsigned short *)LRS1;

#ifdef  TIMER
   timer[Tadcrd] = *((unsigned char *)ORNL_COUNT);
#endif

/*
*   Check for readout delay.
*/
   if(*((unsigned char *)ORNL_COUNT) < fb_delay) return(0);

#ifndef ORNL
   if (*((volatile unsigned char *)ORNL_COUNT) < fb_delay) return(0);
#endif

   if ((fb_1881M.lat != NULL)  && !(*fb_1881M.lat & fb_1881M.mask)) return(3);

/*
*   Start readout of ADCs into 1821 memory.
*    RLV changed Reg 3 to suppress the pedestal subtraction 
*/
  *(lrs+SIB_DIRECT(MOD1821,3)) = 0x840c; /* Suppress pedestal subtract */
  *(lrs+SIB_DIRECT(MOD1821,6)) = 0x3000;
  eieio();
  *(lrs+SIB_DIRECT(MOD1821,7)) = 0x10;
  *(lrs+SIB_DIRECT(MOD1821,1)) = fb_1881M.start;
  eieio();
  *(lrs+SIB_DIRECT(MOD1821,7)) = 0x1;
  eieio();
  return(1);
}
/******************************************************************************
******************************************************************************/
static int fb_transfer_1881M(void)
{
   struct LRS_VME *lrs_cmd_ptr = (struct LRS_VME *)LRS1;
   unsigned short *lrs = (unsigned short *)LRS1;
   unsigned short tmplrs=0; /* use to resequence I/O */
   int  err,size;


#ifdef  TIMER
   timer[Tadctrans] = *((unsigned char *)ORNL_COUNT);
#endif

/*
*   Wait for the ADCs to be read.
*/
   tmplrs = *(lrs+SIB_DIRECT(MOD1821,7));
   if (tmplrs  & 0x20) return(0);
/*
*   Setup and start a DMA transfer from 1821 memory to 1131 memory
*/
   tmplrs = *(lrs+SIB_DIRECT(MOD1821,2));
   err = (tmplrs & 0xff);
   if (err != 0)
     {
       share->FB_error = err;
       return(2);
     }
   tmplrs = *(lrs+SIB_DIRECT(MOD1821,6));
   size = (tmplrs & 0xfff) * 2;
   fb_1881M_count = size;
   if (size == 0) return(2);
   *(lrs+SIB_DIRECT(MOD1821,3)) = 0x8012;
   eieio();
   *(lrs+SIB_DIRECT(MOD1821,7)) = 0x10;
   eieio();
   lrs_cmd_ptr->nta = 0;
   lrs_cmd_ptr->sib = SIB(MOD1821,5);
   lrs_cmd_ptr->wc = size;
   eieio();
   lrs_cmd_ptr->csr = LRS_CSR_RUN + LRS_CSR_DMA + LRS_CSR_DMA_RD;
   eieio();
   return(1);
}
/*****************************************************************************
* RLV copied and modified 1881 routines for 1877 formatting and readout
*
*   Format the data from  LeCroy 1877 FASTBUS TDCs.
******************************************************************************/
static int fb_format_1877(void)
{
  struct LRS_VME *lrs_cmd_ptr = (struct LRS_VME *)LRS1;
  register int count,idoff,sub;
   register unsigned short *dat,*dat_end;
   register unsigned short *evt;
   register unsigned short *id;

#ifdef  TIMER
   timer[Tadcform] = *((unsigned char *)ORNL_COUNT);
#endif
   
/*
*    Wait till DMA transfer complete.
*/
   if (lrs_cmd_ptr->csr & (unsigned short)LRS_CSR_RUN) return(0);

/*    Decode the event header and data words.  Fixup bit 15 in the
      data, if set, 
*/
   evt = Event;
   dat = (unsigned short *)(LRS1 + DATA);
   dat_end = dat + fb_1877_count;
   while(dat < dat_end) {
     idoff = *dat++;
     id = Fast_id + idoff;
     count = (*dat++  & 0x7ff) - 1;
     while(count > 0)
       {
         sub = (*dat++ & 0xff); 
         *evt++ = *(id+sub);
         *evt++ = *dat++;
         --count;
       }
    }
   Event = evt;
   return(1);
}
/******************************************************************************
******************************************************************************/
static int fb_read_1877(void)
{
   unsigned short *lrs = (unsigned short *)LRS1;

#ifdef  TIMER
   timer[Tadcrd] = *((unsigned char *)ORNL_COUNT);
#endif

/*
*   Check for readout delay.
*/
   if(*((unsigned char *)ORNL_COUNT) < fb_delay) return(0);

#ifndef ORNL
   if (*((volatile unsigned char *)ORNL_COUNT) < fb_delay) return(0);
#endif

   if ((fb_1877.lat != NULL) && !(*fb_1877.lat & fb_1877.mask)) return(3);

/*
*   Start readout of ADCs into 1821 memory.
*/
  *(lrs+SIB_DIRECT(MOD1821,3)) = 0x840c;  /* suppress the pedestal system */
  *(lrs+SIB_DIRECT(MOD1821,6)) = 0x3000;
  eieio();
  *(lrs+SIB_DIRECT(MOD1821,7)) = 0x10;
  *(lrs+SIB_DIRECT(MOD1821,1)) = fb_1877.start;
  eieio();
  *(lrs+SIB_DIRECT(MOD1821,7)) = 0x1;
  eieio();
  return(1);
}
/******************************************************************************
******************************************************************************/
static int fb_transfer_1877(void)
{
   struct LRS_VME *lrs_cmd_ptr = (struct LRS_VME *)LRS1;
   unsigned short *lrs = (unsigned short *)LRS1;
   unsigned short tmplrs=0; /* use to resequence I/O */
   int  err,size;


#ifdef  TIMER
   timer[Tadctrans] = *((unsigned char *)ORNL_COUNT);
#endif

/*
*   Wait for the ADCs to be read.
*/
   tmplrs = *(lrs+SIB_DIRECT(MOD1821,7));
   eieio();
   if (tmplrs  & 0x20) return(0);
/*
*   Setup and start a DMA transfer from 1821 memory to 1131 memory
*/
   tmplrs = *(lrs+SIB_DIRECT(MOD1821,2));
   eieio();
   err = (tmplrs & 0xff);
   if (err != 0)
     {
       share->FB_error = err;
       return(2);
     }
   tmplrs = *(lrs+SIB_DIRECT(MOD1821,6));
   eieio();
   size = (tmplrs & 0xfff) * 2;
   eieio();
   fb_1877_count = size;
   if (size == 0) return(2);
   *(lrs+SIB_DIRECT(MOD1821,3)) = 0x8012;
   eieio();
   *(lrs+SIB_DIRECT(MOD1821,7)) = 0x10;
   eieio();
   lrs_cmd_ptr->nta = 0;
   lrs_cmd_ptr->sib = SIB(MOD1821,5);
   lrs_cmd_ptr->wc = size;
   eieio();
   lrs_cmd_ptr->csr = LRS_CSR_RUN + LRS_CSR_DMA + LRS_CSR_DMA_RD;
   eieio();
   return(1);
}
/*****************************************************************************
*
*   Format the data from LeCroy 1875 FASTBUS TDCs.
******************************************************************************/
static int fb_format_1875(void)
{
            struct LRS_VME *lrs_cmd_ptr = (struct LRS_VME *)LRS1;
              register int idoff;
   register unsigned short *dat,*dat_end;
   register unsigned short *evt;
   register unsigned short *id;


#ifdef  TIMER
   timer[Ttdcform] = *((unsigned char *)ORNL_COUNT);
#endif

/*
*    Wait till DMA transfer complete.
*/
   if (lrs_cmd_ptr->csr & LRS_CSR_RUN) return(0);
   
   /******
   if (debugdata_index < 900) {
     dat = (unsigned short *)(LRS1 + DATA);
     dat_end = dat + fb_1875_count;
     debugdata[debugdata_index++] = (int) dat;
     debugdata[debugdata_index++] = fb_1875_count;
     debugdata[debugdata_index++] = 0xfefe;
     while(dat < dat_end) debugdata[debugdata_index++] = *dat++;
     debugdata[debugdata_index++] = 0xfefe;
     debugdata[debugdata_index++] = 0xfefe;
   }
   ********/
   evt = Event;
   id = Fast_id;
   dat = (unsigned short *)(LRS1 + DATA);
   dat_end = dat + fb_1875_count;
   while(dat < dat_end)
    {
      idoff = *dat;
      ++dat;
      *evt++ = *(id+idoff);
      *evt++ = *dat;
      ++dat;
    }
   Event = evt;
   return(1);
}
/******************************************************************************
******************************************************************************/
static int fb_read_1875(void)
{
   unsigned short *lrs = (unsigned short *)LRS1;


#ifdef  TIMER
   timer[Ttdcrd] = *((unsigned char *)ORNL_COUNT);
#endif

/*
*   Check for readout delay.
*/
   if(*((unsigned char *)ORNL_COUNT) < fb_delay) return(0);

#ifndef ORNL
   if (*((volatile unsigned char *)ORNL_COUNT) < fb_delay) return(0);
#endif

   if ((fb_1875.lat != NULL) && !(*fb_1875.lat & fb_1875.mask)) return(3);

/*
*   Start readout of TDCs into 1821 memory.  NOTE: Needs a special
*   sequencer read routine which waits till CIP (Front panel input 1)
*   is false before starting the readout.
*/
  *(lrs+SIB_DIRECT(MOD1821,3)) = 0x840c;
  *(lrs+SIB_DIRECT(MOD1821,6)) = 0x3000;
  eieio();
  *(lrs+SIB_DIRECT(MOD1821,7)) = 0x10;
  *(lrs+SIB_DIRECT(MOD1821,1)) = fb_1875.start;
  eieio();
  *(lrs+SIB_DIRECT(MOD1821,7)) = 0x1;
  eieio();
  return(1);
}
/******************************************************************************
******************************************************************************/
static int fb_transfer_1875(void)
{
   struct LRS_VME *lrs_cmd_ptr = (struct LRS_VME *)LRS1;
   unsigned short *lrs = (unsigned short *)LRS1;
   unsigned short tmplrs=0; /* use to resequence I/O */
   int  err,size;


#ifdef  TIMER
   timer[Ttdctrans] = *((unsigned char *)ORNL_COUNT);
#endif

/*
*   Wait for the TDCs to be read.
*/
   tmplrs = *(lrs+SIB_DIRECT(MOD1821,7));
   if (tmplrs  & 0x20) return(0);
/*
*   Setup and start a DMA transfer from 1821 memory to 1131 memory
*/
   tmplrs = *(lrs+SIB_DIRECT(MOD1821,2));
   err = (tmplrs & 0xff);
   if (err != 0)
     {
       share->FB_error = err;
       return(2);
     }
   tmplrs = *(lrs+SIB_DIRECT(MOD1821,6));
   size = (tmplrs & 0xfff) * 2;
   eieio();
   fb_1875_count = size;
   if(size == 0) return(2);
   *(lrs+SIB_DIRECT(MOD1821,3)) = 0x8012;
   eieio();
   *(lrs+SIB_DIRECT(MOD1821,7)) = 0x10;
   eieio();
   lrs_cmd_ptr->nta = 0;
   lrs_cmd_ptr->sib = SIB(MOD1821,5);
   lrs_cmd_ptr->wc = size;
   eieio();
   lrs_cmd_ptr->csr = LRS_CSR_RUN + LRS_CSR_DMA + LRS_CSR_DMA_RD;
   eieio();
   return(1);
}
/*****************************************************************************
*
*   Start readout for FERA devices
******************************************************************************/
static int fera_read(void)
{
   struct hsm *hsm_ptr = (struct hsm *)CES1;

#ifdef  TIMER
   timer[Tferard] = *((unsigned char *)ORNL_COUNT);
#endif

   if (!ces_early_enable)
     {
       hsm_ptr->mem_ptr = 0;
       hsm_ptr->wrd_cnt = 0x1000;
       hsm_ptr->ctrl = 0x1000;
       eieio();
     }

   return(1);
}
/*****************************************************************************
*
*   Format the data stream from FERA readout devices if the CES 8170 is
*   used for readout.
******************************************************************************/
static int ces_fera_format(void)
{
   struct hsm *hsm_ptr = (struct hsm *)CES1;
              register int ch,sub,hdr,vsn;
                       int rdout;
                       int cnt;
                       long memaddr;
   register unsigned short *dat,*dat_end;
   register unsigned short *evt = Event;
   register unsigned short *id;
   register unsigned short mod;
/*************
     static unsigned short *feradmp = (unsigned short *)0x30000;
*************/

   if (*((unsigned char *)ORNL_COUNT) < fera_delay) return (0);

#ifndef ORNL
   if (*((volatile unsigned char *)ORNL_COUNT) < fera_delay) return(0);
#endif

   memaddr = hsm_ptr->mem_ptr;
   if (memaddr != hsm_ptr->mem_ptr)
   {
      return(0);
   }

/*************
   if (feradmp < (unsigned short *)0x3f000)
  {
   dat = (unsigned short *)hsm_ptr->mem;
   hsm_ptr->ctrl = 0;
   dat_end = dat + (hsm_ptr->mem_ptr & 0x0fff);
   while(dat < dat_end) {*feradmp++ = *dat++;}
   *feradmp++ = 0xfefe;
   *feradmp++ = 0xfefe;
  }
*************/

#ifdef  TIMER
   timer[Tferafmt] = *((unsigned char *)ORNL_COUNT);
#endif

   dat = (unsigned short *)CES1;
   hsm_ptr->ctrl = 0;
   eieio();
   dat_end = dat + (hsm_ptr->mem_ptr & 0x0fff);
   while(dat < dat_end)
    {
/*
*   The first word should be a header word (i.e. bit 15 = 1).  If bit 15
*   is not a one, keep looking until we find one or reach the end of the
*   data.
*
*   When we find the header, extract the VSN.  From the VSN compute
*   the start of this module in the Fera_id table and also lookup
*   the module type in the Fera_types table.  The module type is used
*   to branch to the processing code for this module.
*/
      hdr = *dat++;
      if (hdr < 0x8000) continue;
      vsn = hdr & 0xff;
      mod = Fera_types[vsn];
      id = Fera_id + (vsn << 5);
      rdout = 1;
      switch (mod)
       {
         case  BAKLASH:
           cnt = 15;
           if (baklash.lat != NULL) rdout = *baklash.lat & baklash.mask;
           if (rdout == 0)
             {
               dat += cnt;
               break;
             }
           sub = 0;
           dat += 1;                /* skip first word  */
           *evt++ = id[sub++];
           *evt++ = *dat++;
           cnt = 4;                 /* read 4 Hi res GEs */
           while (cnt--)
             {
               ch = *dat++;
               if (ch)
                 {
                   *evt++ = id[sub];
                   *evt++ = ch ^ 0x3fff;
                 }
               sub++;
             }
           cnt = 9;                 /* read Ge times and side channels */
           while (cnt--)
             {
               ch = *dat++ & 0xfff;
               if (ch)
                 {
                   *evt++ = id[sub];
                   *evt++ = ch;
                 }
               sub++;
             }
           break;
         case  BAKLASH2:
           cnt = 15;
           if (baklash2.lat != NULL) rdout = *baklash2.lat & baklash2.mask;
           if (rdout == 0)
             {
               dat += cnt;
               break;
             }
           sub = 0;
           dat += 1;                /* skip first word  */
           *evt++ = id[sub++];
           *evt++ = *dat++;
           cnt = 4;                  /* read Hi res GEs */
           while (cnt--)
             {
               ch = *dat++;
               if (ch)
                 {
                   *evt++ = id[sub];
                   *evt++ = ch ^ 0x3fff;
                 }
               sub++;
             }
           cnt = 7;                 /* read GE times and side channels */
           while (cnt--)
             {
               ch = *dat++ & 0xfff;
               if (ch)
                 {
                   *evt++ = id[sub];
                   *evt++ = ch;
                 }
               sub++;
             }
           dat += 2;                /* discard 2 words - BGO stuff */
           break;
         case  BAKLASH3:           /* hit pattern and Hi res GEs */
           cnt = 15;
           if (baklash3.lat != NULL) rdout = *baklash3.lat & baklash3.mask;
           if (rdout == 0)
             {
               dat += cnt;
               break;
             }
           sub = 0;
           dat += 1;                /* skip first word  */
           *evt++ = id[sub++];
           *evt++ = *dat++;
           cnt = 4;                  /* readout only high res GEs  */
           while (cnt--)
             {
               ch = *dat++;
               if (ch)
                 {
                   *evt++ = id[sub];
                   *evt++ = ch ^ 0x3fff;
                 }
               sub++;
             }
           dat += 9;               /* discard 9 words  */
           break;
         case  LRS_4300:
          cnt = (hdr >> 11) & 0xf;
          if (cnt == 0) cnt = 16;
          if (lrs_4300.lat != NULL) rdout = *lrs_4300.lat & lrs_4300.mask;
          if (rdout == 0)
           {
             dat += cnt;
             break;
           }
          while (cnt--)
           {
             ch = *dat++;
             sub = ch >> 11;      /* LeCroy ADC have 16 channels and 11 bit   */
             *evt++ = id[sub];    /* data                                     */
             *evt++ = ch & 0x7ff;
           }
          break;
/*      Added support for ORTEC AD413 module.   (JJK, Jan. 29, 1997) */
         case  AD_413:
          cnt = (hdr >> 11) & 0x3;
          if (cnt == 0) cnt = 4;
          if (ad_413.lat != NULL) rdout = *ad_413.lat & ad_413.mask;
          if (rdout == 0)
           {
             dat += cnt;
             break;
           }
          while (cnt--)
           {
             ch = *dat++;
             sub = ch >> 13;      /* ORTEC ADC has 4 channels of 13 bits   */
             *evt++ = id[sub];    /* data                                     */
             *evt++ = ch & 0x1fff;
           }
          break;
/*      End of added support for ORTEC AD413 module.   (JJK, Jan. 29, 1997) */
         case  MCSQ_FER:
         case  LRS_3377:
          if (lrs_3377.lat != NULL) rdout = *lrs_3377.lat & lrs_3377.mask;
          if (rdout == 0)
            {
              while(dat < dat_end)
                {
                  if (*dat < 0x8000)
                    {
                      dat++;
                      continue;
                    }
                  break;
                }
              break;
            }
          while (dat < dat_end)
            {
              ch = *dat;
              *evt++ = *(id+((ch & 0x7c00) >> 10));
              ch = (ch & 0xff) << 8;
              dat++;
              *evt++ = (*dat & 0xff) | ch;
              dat++;
            }
           break;
         case  GAN_812F:
          cnt = (hdr >> 11) & 0x7;
          if (cnt == 0) cnt = 8;
          if (gan_812f.lat != NULL) rdout = *gan_812f.lat & gan_812f.mask;
          if (rdout == 0)
           {
             dat += cnt;
             break;
           }
          while (cnt--)
           {
             ch = *dat++;
             sub = ch >> 12;      /* GANELEC TDCs have 8 channels and 12 bit  */
             *evt++ = id[sub];    /* data                                     */
             *evt++ = ch & 0xfff;
           }
          break;
         case  SILENA_4418:
          cnt = (hdr >> 8) & 0xf;
          if (silena_4418.lat != NULL)
                                  rdout = *silena_4418.lat & silena_4418.mask;
          if (rdout == 0)
           {
             dat += (cnt + 1);
             break;
           }
          dat++;                 /* skip over pattern word                   */
          while (cnt--)
           {
             ch = *dat++;
             sub = (ch >> 12) & 0x7;  /* SILENA ADCs have 8 channels and 12   */
             *evt++ = id[sub];        /* bit data                             */
             *evt++ = ch & 0x8fff;
           }
          break;
         default:
          break;
       }
    }
   Event = evt;

   if (ces_early_enable)
     {
       hsm_ptr->mem_ptr = 0;
       hsm_ptr->wrd_cnt = 0x1000;
       hsm_ptr->ctrl = 0x1000;
       eieio();
     }

#ifdef  TIMER
   timer[Tferafmtend] = *((unsigned char *)ORNL_COUNT);
#endif

   return(1);
}
/*****************************************************************************
*
*   Format the data stream from FERA readout devices if the LeCroy 1190 is
*   used for readout.
******************************************************************************/
static int lrs_fera_format(void)
{
   struct LRS1190 *lrs_ptr, **lrs_mod;
              register int ch,sub,hdr,vsn;
                       int rdout;
                       int cnt;
   register unsigned short *dat,*dat_end;
   register unsigned short *evt = Event;
   register unsigned short *id;
   register unsigned short mod;

   if (*((unsigned char *)ORNL_COUNT) < fera_delay) return (0);

#ifndef ORNL
   if (*((volatile unsigned char *)ORNL_COUNT) < fera_delay) return(0);
#endif

   lrs_mod = lrs_intf;
   while (*lrs_mod != NULL)
     {
       lrs_ptr = *lrs_mod++;
       mod = lrs_ptr->addr;
       if(mod != lrs_ptr->addr) return(0);
     }

#ifdef  TIMER
   timer[Tferafmt] = *((unsigned char *)ORNL_COUNT);
#endif

   lrs_mod = lrs_intf;
   while (*lrs_mod != NULL)
    {
      lrs_ptr = *lrs_mod++;
      lrs_ptr->mode = 0;                   /* Disable LRS 1190 front panel   */
      eieio();

/*
*   Check for too much data in interface module.  If there is more
*   than 2047 16-bit words, just trash this one.
*/
      cnt = lrs_ptr->addr * 2;
      if (cnt > 2047) cnt = 0;
      dat = (unsigned short *)lrs_ptr->dat;
      dat_end = dat + cnt;
      while(dat < dat_end)
       {
/*
*   The first word should be a header word (i.e. bit 15 = 1).  If bit 15
*   is not a one, keep looking until we find one or reach the end of the
*   data.
*
*   When we find the header, extract the VSN.  From the VSN compute
*   the start of this module in the Fera_id table and also lookup
*   the module type in the Fera_types table.  The module type is used
*   to branch to the processing code for this module.
*/
         hdr = *dat;
         dat += 2;
         if (hdr < 0x8000) continue;
         vsn = hdr & 0xff;
         mod = Fera_types[vsn];
         id = Fera_id + (vsn << 5);
         rdout = 1;
         switch (mod)
          {
            case  BAKLASH:
             cnt = 15;
             if (baklash.lat != NULL) rdout = *baklash.lat & baklash.mask;
             if (rdout == 0)
               {
                 dat += cnt << 1;
                 break;
               }
             sub = 0;
             dat += 2;                /* skip first word  */
             *evt++ = id[sub++];
             *evt++ = *dat;
             dat += 2;
             cnt = 4;                 /* read 4 Hi res GEs */
             while (cnt--)
               {
                 ch = *dat;
                 if (ch)
                   {
                     *evt++ = id[sub];
                     *evt++ = ch ^ 0x3fff;
                   }
                 sub++;
                 dat += 2;
               }
             cnt = 9;                 /* read Ge times and side channels */
             while (cnt--)
               {
                 ch = *dat & 0xfff;
                 if (ch)
                   {
                     *evt++ = id[sub];
                     *evt++ = ch;
                   }
                 sub++;
                 dat += 2;
               }
             break;
            case  BAKLASH2:
             cnt = 15;
             if (baklash2.lat != NULL) rdout = *baklash2.lat & baklash2.mask;
             if (rdout == 0)
               {
                 dat += cnt << 1;
                 break;
               }
             sub = 0;
             dat += 2;                /* skip first word  */
             *evt++ = id[sub++];
             *evt++ = *dat;
             dat += 2;
             cnt = 4;                  /* read Hi res GEs */
             while (cnt--)
               {
                 ch = *dat;
                 if (ch)
                   {
                     *evt++ = id[sub];
                     *evt++ = ch ^ 0x3fff;
                   }
                 sub++;
                 dat += 2;
               }
             cnt = 7;                 /* read GE times and side channels */
             while (cnt--)
               {
                 ch = *dat & 0xfff;
                 if (ch)
                   {
                     *evt++ = id[sub];
                     *evt++ = ch;
                   }
                 sub++;
                 dat += 2;
               }
             dat += 4;                /* discard 2 words - BGO stuff */
             break;
            case  BAKLASH3:           /* hit pattern and Hi res GEs */
             cnt = 15;
             if (baklash3.lat != NULL) rdout = *baklash3.lat & baklash3.mask;
             if (rdout == 0)
               {
                 dat += cnt << 1;
                 break;
               }
             sub = 0;
             dat += 2;                /* skip first word  */
             *evt++ = id[sub++];
             *evt++ = *dat;
             dat += 2;
             cnt = 4;                  /* readout only high res GEs  */
             while (cnt--)
               {
                 ch = *dat;
                 if (ch)
                   {
                     *evt++ = id[sub];
                     *evt++ = ch ^ 0x3fff;
                   }
                 sub++;
                 dat += 2;
               }
             dat += 18;               /* discard 9 words  */
             break;
            case  LRS_4300:
             cnt = (hdr >> 11) & 0xf;
             if (cnt == 0) cnt = 16;
             if (lrs_4300.lat != NULL) rdout = *lrs_4300.lat & lrs_4300.mask;
             if (rdout == 0)
              {
                dat += cnt << 1;
                break;
              }
             while (cnt--)
              {
                ch = *dat;
                sub = ch >> 11;      /* LeCroy ADC have 16 channels & 11 bit  */
                *evt++ = id[sub];    /* data                                  */
                *evt++ = ch & 0x7ff;
                dat += 2;
              }
             break;
/*      Added support for ORTEC AD413 module.   (JJK, Jan. 29, 1997) */
         case  AD_413:
          cnt = (hdr >> 11) & 0x3;
          if (cnt == 0) cnt = 4;
          if (ad_413.lat != NULL) rdout = *ad_413.lat & ad_413.mask;
          if (rdout == 0)
           {
             dat += cnt << 1;
             break;
           }
          while (cnt--)
           {
             ch = *dat;
             sub = ch >> 13;      /* ORTEC ADC has 4 channels of 13 bits   */
             *evt++ = id[sub];    /* data                                     */
             *evt++ = ch & 0x1fff;
             dat += 2;
           }
          break;
/*      End of added support for ORTEC AD413 module.   (JJK, Jan. 29, 1997) */
            case  MCSQ_FER:
            case  LRS_3377:
             if (lrs_3377.lat != NULL) rdout = *lrs_3377.lat & lrs_3377.mask;
             if (rdout == 0)
               {
                 while(dat < dat_end)
                   {
                     if (*dat < 0x8000)
                       {
                         dat += 2;
                         continue;
                       }
                     break;
                   }
                 break;
               }
             while (dat < dat_end)
              {
                ch = *dat;
                *evt++ = *(id+((ch & 0x7c00) >> 10));
                ch = (ch & 0xff) << 8;
                dat += 2;
                *evt++ = (*dat & 0xff) | ch;
                dat += 2;
              }
             break;
            case  GAN_812F:
             cnt = (hdr >> 11) & 0x7;
             if (cnt == 0) cnt = 8;
             if (gan_812f.lat != NULL) rdout = *gan_812f.lat & gan_812f.mask;
             if (rdout == 0)
              {
                dat += cnt << 1;
                break;
              }
             while (cnt--)
              {
                ch = *dat;
                sub = ch >> 12;      /* GANELEC TDCs have 8 channels & 12 bit */
                *evt++ = id[sub];    /* data                                  */
                *evt++ = ch & 0xfff;
                dat += 2;
              }
             break;
            case  SILENA_4418:
             cnt = (hdr >> 8) & 0xf;
             if (silena_4418.lat != NULL)
                                  rdout = *silena_4418.lat & silena_4418.mask;
             if (rdout == 0)
              {
                dat += (cnt + 1) << 1;
                break;
              }
             dat += 2;               /* skip over pattern word                */
             while (cnt--)
              {
                ch = *dat;
                sub = (ch >> 12) & 0x7;  /* SILENA ADCs have 8 channels & 12  */
                *evt++ = id[sub];        /* bit data plus overflow            */
                *evt++ = ch & 0x8fff;
                dat += 2;
              }
             break;
            default:
             break;
          }
       }
      *(lrs_ptr->dat) = 0;         /* Set LRS 1190 address counter to zero   */
      lrs_ptr->mode = 1;           /* LRS 1190 Enable Front Panel            */
      eieio();
     }
   Event = evt;

#ifdef  TIMER
   timer[Tferafmtend] = *((unsigned char *)ORNL_COUNT);
#endif

   return(1);
}
/*****************************************************************************
*
*   Format the data stream from VME modules
******************************************************************************/
static int vme_format(void)
{
   struct CAEN *caen;
              register int ch,val,htype;
                       int rdout,i;
   register unsigned short *evt = Event;
   register unsigned short *id;

   if (*((unsigned char *)ORNL_COUNT) < vme_delay) return (0);

#ifndef ORNL
   if (*((volatile unsigned char *)ORNL_COUNT) < vme_delay) return(0);
#endif

/*
*   Read CAEN 785 ADCs
*/
   rdout = 1;
   if (caen_785.lat != NULL) rdout = *caen_785.lat & caen_785.mask;
   if (rdout && caen_785.enable)
     {
       i = 0;
       while(caen_adcs[i].hwd != NULL)
	 {
	   id = caen_adcs[i].id;
	   caen = caen_adcs[i].hwd;
	   while(1)
	     {
	       
	       val = caen->buf[0];
	       htype = val & HDR_MASK;
	       if (htype == HEADER)
		 {
		   int count;
		   
		   count = (val & CNT) >> 8;
		   while(count--)
		     {
		       val = caen->buf[0];
		       ch = (val & CHAN) >> 16;
		       val = val & 0xfff;
		       if ((val < 0xf00) && (id[ch] != 0))
			 {
			   *evt++ = id[ch];
			   *evt++ = val;
			 }
		     }
		 }
	       else if (htype == EOB)
		 {
		   if (id[32] && id[33])
		     {
		       *evt++ = id[32];
		       *evt++ = (val >> 16) & 0xff;
		       *evt++ = id[33];
		       *evt++ = val;
		     }
		 }
	       else if (htype == NOT_VALID) break;
	     }
	   i++;
	 }
     }
   
   /*
    *   Read CAEN 775 TDCs
    */
   rdout = 1;
   if (caen_775.lat != NULL) rdout = *caen_775.lat & caen_775.mask;
   if (rdout && caen_775.enable)
     {
       i = 0;
       while(caen_tdcs[i].hwd != NULL)
	 {
	   id = caen_tdcs[i].id;
	   caen = caen_tdcs[i].hwd;
	   while(1)
	     {
	       val = caen->buf[0];
	       htype = val & HDR_MASK;
	       if (htype == HEADER)
		 {
		   int count;
		   
		   count = (val & CNT) >> 8;
		   if (count < 0 || count > 31) count = 0;
		   while(count--)
		     {
		       val = caen->buf[0];
		       
		       ch = (val & CHAN) >> 16;
		       val = val & 0xfff;
		       if ((val < 0xf00) && (id[ch] != 0))
			 {
			   *evt++ = id[ch];
			   *evt++ = val;
			 }
		     }
		 }
	       else if (htype == EOB)
		 {
		   if (id[32] && id[33])
		     {
		       *evt++ = id[32];
		       *evt++ = (val >> 16) & 0xff;
		       *evt++ = id[33];
		       *evt++ = val;
		     }
		 }
	       else if (htype == NOT_VALID) 
		 break;
	     }
	   i++;
	 }
     }
   /*
    *   Read CAEN 792 QDCs
    */
   rdout = 1;
   if (caen_792.lat != NULL) rdout = *caen_792.lat & caen_792.mask;
   if (rdout && caen_792.enable) {
     i = 0;
     while(caen_qdcs[i].hwd != NULL) {
       id = caen_qdcs[i].id;
       caen = caen_qdcs[i].hwd;
       while(1) {
	 val = caen->buf[0];
	 htype = val & HDR_MASK;
	 if (htype == HEADER) {
	   int count;
	   count = (val & CNT) >> 8;
	   
	   if (count < 0 || count > 31) count = 0;
	   while(count--) {
	     val = caen->buf[0];
	     ch = (val & CHAN) >> 16;
	     val = val & 0xfff;
	     if ((val < 0xf00) && (id[ch] != 0)) {
	       *evt++ = id[ch];
	       *evt++ = val;
	     }
	   }
	 }
	 else if (htype == EOB) {
	   if (id[32] && id[33]) {
	     *evt++ = id[32];
	     *evt++ = (val >> 16) & 0xff;
	     *evt++ = id[33];
	     *evt++ = val;
	   }
	 }
	 else if (htype == NOT_VALID) 
	   break; 
       }
       i++;
     }
   }
   
   /*
    *   Read SIS Scaler
    */
   struct SIS3820 *sis;  //Just to make code easier to read
   
   //Hack for decoding the scalers
   union scalervalue {
     unsigned long fval;
     unsigned short  hval[2];
   } sval={0};
   
   union scalervalue extregister={0};
   
   rdout = 1;
   if (sis_3820.lat != NULL) rdout = *sis_3820.lat & sis_3820.mask;
   if (rdout && sis_3820.enable) {
     i = 0;
     while(sis_ro[i].hwd != NULL) {
       id = sis_ro[i].id;
       sis = sis_ro[i].hwd;
       
       //Read the extended precision register in any case
       extregister.fval = sis->ch_1_17_highbits;
       
       // Loop over all the channels in the scaler, check first if requested
       // The scaler is 32 bits, so the values have to be split over two words.
       // Channels 0 and 16 are 48 bits, with the extra bits coming from a 
       // different register in the scaler.
       
       int cidx;
       for (cidx=0; cidx<32; cidx++) {
	 
	 // For all scaler channels, check that PAC requested them
	 if (id[cidx*3] != 0) {

	   // For the extended range channels, insert the data
	   *evt++ = id[cidx*3];
	   if (cidx == 0) {
	     *evt++ = extregister.hval[1];
	   } else if (cidx == 16) {
	     *evt++ = extregister.hval[0];
	   } else {     // insert zero for others
	     *evt++ = 0;  
	   } 
	     //Read the values from the latched shadow registers, 
             //not the actively counting ones
	   sval.fval = sis->shadow[cidx];
	     
	   // The save order is 0 - MSB, 1 - middle, 3 - LSB.  
           //PPC is big-endian, need word swap
	   *evt++ = id[cidx*3+1];
	   *evt++ = sval.hval[1];
	   *evt++ = id[cidx*3+2];
	   *evt++ = sval.hval[0];
	   
	 }
       }
       i++;
     }
   }
   /*
    *   Read MyRIAD clock - This uses runes that worked in 2015 at ANL 
    *                       with a real MyRIAD
    */
   struct MyRIAD_Registers *myr;  //Just to make code easier to read
   volatile static unsigned short tmp_myr[3];
   
   rdout = 1;
   if (myriad.lat != NULL) rdout = *myriad.lat & myriad.mask;
   if (rdout && myriad.enable) {
     
     if (myriad_ro.hwd != NULL) {
       id = myriad_ro.id;
       myr = myriad_ro.hwd;
       
       // This is a fake set of reads, to setup the MyRIAD fifo.  
       // Any less is optimized away.
       tmp_myr[0] = myriad_ro.hwd->fifo_access;
       tmp_myr[1] = myr->fifo_access;
       tmp_myr[2] = myr->fifo_access;
       
       // Now we really read and transfer the clock.  
       // Notice the adjustment for Big-endian.
       *evt++ = *id+2;   //MSB
       *evt++ = myr->fifo_access;
       *evt++ = *id+1;
       *evt++ = myr->fifo_access;
       *evt++ = *id;     //LSB
       *evt++ = myr->fifo_access;
     }
   }      
   
   // End of the VME readout
   Event = evt;
   
   return(1);
}
/******************************************************************************
******************************************************************************/
static int run_kill(void)
{
   register struct kill_list *kill;
   register int count,stat;
   

#ifdef  TIMER
   timer[Tkill] = *((unsigned char *)ORNL_COUNT);
#endif

   kill = Kill_List;
   count = kill_count;
   while(count--)
     {
       stat = *(kill->lat) & kill->mask;
       if (kill->sense != 0)
         {
/*
*   Kill if ANY test.
*/
           if(stat) return(1);
         }
       else
         {
/*
*   Kill if NONE test.
*/
           if(!stat) return(1);
         }
       kill++;
     }
   return(0);
}
/*****************************************************************************
*
*   Readout sequence no operation.
******************************************************************************/
static int seq_nop(void)
{
#ifdef  TIMER
   timer[Tseqnop] = *((unsigned char *)ORNL_COUNT);
#endif

    return(1);
}
/******************************************************************************
*   Runtime routine for count down tests.
*
*   Return:    0     Means normal event processing
*              1     Means kill the event
******************************************************************************/
static int run_count_down(void)
{
   struct count_dwn_lst *cdn = Count_dwn_str;

   while(cdn->pat != NULL)                /* NULL pointer marks end of list  */
     {
       if (*cdn->pat & cdn->mask)         /* Test specified gate             */
         {
           if (--cdn->current) return(1); /* Kill event                      */
           cdn->current = cdn->initial;   /* reset counter                   */
         }
       cdn++;
     }
   return 0;
}
/******************************************************************************
*    Runtime routine for gate (Raw and computed) tests
******************************************************************************/
static int run_gate_list(void)
{
   struct KSC_VME *ksc = (struct KSC_VME *)KSC1;
   struct gate_ro_lst  *gate_ro = Gate_RO_str;
       unsigned short  dat = 0,stat;

   ksc->cma = gate_ro_cma;
   ksc->csr = KSC_CSR_CAM_RD | KSC_CSR_GO;
   eieio();
   while(gate_ro->pat != NULL)
     {
       if (gate_ro->new != 0)
         {
           while (!(stat = ksc->csr & KSC_CSR_RDY));
           dat = ksc->dlr;
           if (gate_ro->mod_type == PHIL_7164) dat = dat &0xfff;
           else if (gate_ro->mod_type == PHIL_7186) dat = dat &0xfff;
           if (stat & KSC_CSR_NOQ) dat = 0;
         }
       if (dat < gate_ro->low || dat > gate_ro->high)
         {
/*
*   Test False.  Clear bit in pattern word
*/
           *gate_ro->pat = *gate_ro->pat & ~gate_ro->mask;
         }
       else
         {
/*
*   Test True.  Set bit in pattern word
*/
           *gate_ro->pat = *gate_ro->pat | gate_ro->mask;
         }
       gate_ro++;
     }
/*
*   If there are calculated gates, execute those test also.
*/
   if (Cal_RO_str != NULL)
     {
       struct cal_gate_lst *cal_ro = Cal_RO_str;
       int  test1,test2;

       while(cal_ro->pat1 != NULL)
         {
           test1 = *cal_ro->pat1 & cal_ro->msk1;
           if (cal_ro->not1) test1 = !test1;
           if (cal_ro->pat2 != NULL)
             {
               test2 = *cal_ro->pat2 & cal_ro->msk2;
               if (cal_ro->not2) test2 = !test2;
               if (cal_ro->op) test1 = test1 || test2;
               else  test1 = test1 && test2;
             }
           if (test1) *cal_ro->pat = *cal_ro->pat | cal_ro->mask;
           else  *cal_ro->pat = *cal_ro->pat & ~cal_ro->mask;
           cal_ro++;
         }
     }
   return 0;
}
/******************************************************************************
*    Runtime routine to readout and format our special CAMAC modules.
******************************************************************************/
static int run_cam_special(void)
{
   struct KSC_VME *ksc;
   struct cam_ro_lst *cam_ro;
   register unsigned short *evt;
   register unsigned short *id;
   register unsigned short tmp,stat;
   volatile unsigned short tmpxx;

   if (*((unsigned char *)ORNL_COUNT) < cam_special_delay) return(0);

#ifndef ORNL
   if (*((volatile unsigned char *)ORNL_COUNT) < cam_special_delay) return(0);
#endif

   ksc = (struct KSC_VME *)KSC1;
   cam_ro = Cam_RO_str;
   evt = Event;
   while(cam_ro->n != 0)
     {
       switch (cam_ro->mod_type)
        {

          case PHIL_7164:
            if (phil_7164.lat != NULL &&
                                    !(*phil_7164.lat & phil_7164.mask)) break;
            ksc->cma = cam_ro->cma;
            ksc->csr = KSC_CSR_CAM_RD | KSC_CSR_GO;
            id = Cam_id + cam_ro->c * 1024 + cam_ro->n * 32;
            eieio();
            while(!(ksc->csr & KSC_CSR_DONE))
             {
               while (!((stat = ksc->csr) & KSC_CSR_RDY));
               tmp = ksc->dlr;
               if (stat & KSC_CSR_NOQ) break;
               *evt++ = *(id+(tmp >> 12));
               *evt++ = tmp & 0xfff;
             }
            break;
          case PHIL_7186:
            if (phil_7186.lat != NULL &&
                                    !(*phil_7186.lat & phil_7186.mask)) break;
            ksc->cma = cam_ro->cma;
            ksc->csr = KSC_CSR_CAM_RD | KSC_CSR_GO;
            id = Cam_id + cam_ro->c * 1024 + cam_ro->n * 32;
            eieio();
            while(!(ksc->csr & KSC_CSR_DONE))
             {
               while (!((stat = ksc->csr) & KSC_CSR_RDY));
               tmp = ksc->dlr;
               if (stat & KSC_CSR_NOQ) break;
               *evt++ = *(id+(tmp >> 12));
               *evt++ = tmp & 0xfff;
             }
            break;
          case LRS_2277:
/***********************************************************************
*   NOTE: Code for LeCroy 2277 TDC provided by Notre Dame
************************************************************************/


            if (lrs_2277.lat != NULL &&
                                      !(*lrs_2277.lat & lrs_2277.mask)) break;
            id = Cam_id + cam_ro->c * 1024 + cam_ro->n * 32;
            do
              {  
                ksc->cma = cam_ro->cma;
                ksc->csr = KSC_CSR_CAM_RD | KSC_CSR_GO;
                eieio();
/* Changed the KSC_CSR_RDY to KSC_CSR_DONE (1/10/97 nsc, jjk, jwk) */ 
/*                while (!((stat = ksc->csr) & KSC_CSR_RDY));    */
                while (!((stat = ksc->csr) & KSC_CSR_DONE));
              }
            while (!(stat & KSC_CSR_NOQ)); 

/* put another KSC..GO and KSC...RD in (1/10/97 nsc, jjk, jwk) */
/* The reason: There was a HALT executed which caused ksc->csr
   GO bit to be cleared, so we need to reset it so that it will read and go
   again. */
            ksc->csr = KSC_CSR_CAM_RD | KSC_CSR_GO;
            eieio();

            while(1)
             {
               register unsigned short tmph,param_id;

                while (!((stat = ksc->csr) & KSC_CSR_RDY));
 /*              while ( !( ((stat = ksc->csr) & KSC_CSR_RDY) || 
		          ((stat = ksc->csr) & KSC_CSR_ERR)   ));  */
               tmp = ksc->dlr;
               tmph = ksc->dhr;
               if (stat & KSC_CSR_NOQ) break;
               param_id = *(id+((tmph & 0x3f) >> 1));
               *evt++ = param_id;
               *evt++ = tmph & 0xff;
               *evt++ = param_id;
               *evt++ = tmp;
             }  
            break;
          case LRS_3377C:
            if (lrs_3377C.lat != NULL &&
                                    !(*lrs_3377C.lat & lrs_3377C.mask)) break;
            id = Cam_id + cam_ro->c * 1024 + cam_ro->n * 32;
            ksc->cma = cam_ro->cma;
            ksc->csr = KSC_CSR_CAM_RD | KSC_CSR_GO;
            eieio();
            while(!(ksc->csr & KSC_CSR_DONE))
             {
               register unsigned short datsav,param_id;

               while (!((stat = ksc->csr) & KSC_CSR_RDY));
               tmp = ksc->dlr;
               if (stat & KSC_CSR_NOQ) break;
               if (tmp & 0x8000) continue;
               param_id = *(id+((tmp & 0x7c00) >> 10));
               *evt++ = param_id;
               datsav = (tmp & 0xff) << 8;
               while (!((stat = ksc->csr) & KSC_CSR_RDY));
               tmp = ksc->dlr;
               *evt++ = (tmp & 0xff) | datsav;
               if (stat & KSC_CSR_NOQ) break;
             }
            break;
/**************
          case MCSQ_CAM:
            id = Cam_id + cam_ro->c * 1024 + cam_ro->n * 32;
            *evt++ = *id;
            *evt++ = *((volatile unsigned char *)ORNL_COUNT);
            break;
**************/
          case MCSQ_CAM:
          case XIA_TIME:
/*
*   Readout XIA time stamp
*/
            ksc->cma = cam_ro->cma;
            ksc->csr = KSC_CSR_CAM_RD | KSC_CSR_GO;
            id = Cam_id + cam_ro->c * 1024 + cam_ro->n * 32;
            eieio();
            while (!((stat = ksc->csr) & (KSC_CSR_RDY | KSC_CSR_DONE)));
            *evt++ = *id++;
            *evt++ = ksc->dlr;
            while (!((stat = ksc->csr) & (KSC_CSR_RDY | KSC_CSR_DONE)));
            *evt++ = *id++;
            *evt++ = ksc->dlr;
            while (!((stat = ksc->csr) & (KSC_CSR_RDY | KSC_CSR_DONE)));
            *evt++ = *id++;
            *evt++ = ksc->dlr;
            while(!(ksc->csr & KSC_CSR_DONE));
            break;
          case LRS_4300C:
            if (lrs_4300C.lat != NULL &&
                                    !(*lrs_4300C.lat & lrs_4300C.mask)) break;
            ksc->cma = cam_ro->cma;
            ksc->csr = KSC_CSR_CAM_RD | KSC_CSR_GO;
            id = Cam_id + cam_ro->c * 1024 + cam_ro->n * 32;
            eieio();
            while(!(ksc->csr & KSC_CSR_DONE))
             {
               register unsigned short param_id;

               while (!((stat = ksc->csr) & KSC_CSR_RDY));
               tmp = ksc->dlr;
               if (stat & KSC_CSR_NOQ) break;
               if (tmp & 0x8000) continue;
               param_id = *(id+((tmp & 0x7800) >> 11));
               *evt++ = param_id;
               *evt++ = tmp & 0x7ff;
             }
            break;
/*      Added support for ORTEC AD413 module.   (JJK, Jan. 23, 1997) */
          case AD_413C:
            if (ad_413C.lat != NULL &&
                                    !(*ad_413C.lat & ad_413C.mask)) break;
            ksc->cma = cam_ro->cma;
            ksc->csr = KSC_CSR_CAM_RD | KSC_CSR_GO;
            id = Cam_id + cam_ro->c * 1024 + cam_ro->n * 32;
            eieio();
            while(!(ksc->csr & KSC_CSR_DONE))
             {
               register unsigned short param_id;

               while (!((stat = ksc->csr) & KSC_CSR_RDY));
               tmp = ksc->dlr;
               if (stat & KSC_CSR_NOQ) break;
               if (tmp & 0x8000) continue;
               param_id = *(id+((tmp & 0x6000) >> 13));
               *evt++ = param_id;
               *evt++ = tmp & 0x1fff;
             }
            break;
/*      End of added support for ORTEC AD413 module.   (JJK, Jan. 23, 1997) */
          case SILENA_4418C:
            if (silena_4418C.lat != NULL &&
                              !(*silena_4418C.lat & silena_4418C.mask)) break;
            ksc->cma = cam_ro->cma;
            ksc->csr = KSC_CSR_CAM_RD | KSC_CSR_GO;
            id = Cam_id + cam_ro->c * 1024 + cam_ro->n * 32;
            eieio();
            while (!((stat = ksc->csr) & KSC_CSR_RDY));
            tmpxx = ksc->dlr;
            if (stat & KSC_CSR_NOQ) break;
            while (!((stat = ksc->csr) & KSC_CSR_RDY));
            tmpxx = ksc->dlr;
            while(!(ksc->csr & KSC_CSR_DONE))
             {
               register unsigned short param_id;

               while (!((stat = ksc->csr) & KSC_CSR_RDY));
               tmp = ksc->dlr;
               if (stat & KSC_CSR_NOQ) break;
               param_id = *(id+((tmp & 0x7000) >> 12));
               *evt++ = param_id;
               *evt++ = tmp & 0x8fff;
             }
            break;
          default:
            break;
         }
        cam_ro++;
     }
   Event = evt;
   return(1);
}
/******************************************************************************
*    Runtime routine to readout and format our special CAMAC modules which
*    are in crates with AUX controllers.
******************************************************************************/
static int run_fcam_special(void)
{
   struct fcam_ro_lst *fcam_ro;
   struct FCAM *fcam;
   register unsigned short *evt;
   register unsigned short *id;
   register            int tmp,cnaf;

   if (*((unsigned char *)ORNL_COUNT) < cam_special_delay) return(0);

#ifndef ORNL
   if (*((volatile unsigned char *)ORNL_COUNT) < cam_special_delay) return(0);
#endif

   fcam_ro = Fcam_RO_str;
   fcam = (struct FCAM *)ORNLAUX;
   evt = Event;
   while(fcam_ro->n != 0)
     {
       switch (fcam_ro->mod_type)
        {

          case PHIL_7164:
            if (phil_7164.lat != NULL &&
                                    !(*phil_7164.lat & phil_7164.mask)) break;
            fcam->dat.cnaf = fcam_ro->cnaf;
            eieio();
            id = Cam_id + fcam_ro->c * 1024 + fcam_ro->n * 32;
            while(1)
              {
                tmp = fcam->dat.l;
                if (tmp < 0) break;      /* test for Q = 0  */
                *evt++ = *(id+((tmp & 0xf000) >> 12));
                *evt++ = tmp & 0xfff;
              }
            break;
          case PHIL_7186:
            if (phil_7186.lat != NULL &&
                                    !(*phil_7186.lat & phil_7186.mask)) break;
            fcam->dat.cnaf = fcam_ro->cnaf;
            eieio();
            id = Cam_id + fcam_ro->c * 1024 + fcam_ro->n * 32;
            while(1)
              {
                tmp = fcam->dat.l;
                if (tmp < 0) break;      /* test for Q = 0  */
                *evt++ = *(id+((tmp & 0xf000) >> 12));
                *evt++ = tmp & 0xfff;
              }
            break;
          case LRS_2277:
/***********************************************************************
*   NOTE: Code for LeCroy 2277 TDC provided by Notre Dame
************************************************************************/


            if (lrs_2277.lat != NULL &&
                                      !(*lrs_2277.lat & lrs_2277.mask)) break;
            id = Cam_id + fcam_ro->c * 1024 + fcam_ro->n * 32;
            cnaf = CNAF(fcam_ro->c,fcam_ro->n,0,27);
            do
              {
                fcam->dat.cnaf = cnaf;
                eieio();
                tmp = fcam->dat.l;
              }
            while (!(tmp < 0));  /*Wait for Q=0 before proceeding.JJK 2/15/01 */

            fcam->dat.cnaf = fcam_ro->cnaf;
            eieio();
            while(1)
             {
               register unsigned short tmph,param_id;

               tmph = fcam->dat.s;
               tmp = fcam->dat.l;
               if (tmp < 0) break;
               param_id = *(id+((tmph & 0x3f) >> 1));
               *evt++ = param_id;
               *evt++ = tmph & 0xff;
               *evt++ = param_id;
               *evt++ = tmp;
             }
            break;
          case MCSQ_CAM:
            break;
          case LRS_3377C:
            if (lrs_3377C.lat != NULL &&
                                    !(*lrs_3377C.lat & lrs_3377C.mask)) break;
            id = Cam_id + fcam_ro->c * 1024 + fcam_ro->n * 32;
            fcam->dat.cnaf = fcam_ro->cnaf;
            eieio();
            while(1)
             {
               register unsigned short datsav,param_id;

               tmp = fcam->dat.l;
               if (tmp < 0) break;
               if (tmp & 0x8000) continue;
               param_id = *(id+((tmp & 0x7c00) >> 10));
               *evt++ = param_id;
               datsav = (tmp & 0xff) << 8;
               tmp = fcam->dat.l;
               *evt++ = (tmp & 0xff) | datsav;
               if (tmp < 0) break;
             }
            break;
/*****************
          case MCSQ_CAM:
            id = Cam_id + fcam_ro->c * 1024 + fcam_ro->n * 32;
            *evt++ = *id;
            *evt++ = *((volatile unsigned char *)ORNL_COUNT);
            break;
*****************/
          case LRS_4300C:
            if (lrs_4300C.lat != NULL &&
                                    !(*lrs_4300C.lat & lrs_4300C.mask)) break;
            fcam->dat.cnaf = fcam_ro->cnaf;
            eieio();
            id = Cam_id + fcam_ro->c * 1024 + fcam_ro->n * 32;
            while(1)
             {
               register unsigned short param_id;

               tmp = fcam->dat.l;
               if (tmp < 0) break;
               if (tmp & 0x8000) continue;
               param_id = *(id+((tmp & 0x7800) >> 11));
               *evt++ = param_id;
               *evt++ = tmp & 0x7ff;
             }
            break;
/*      Added support for ORTEC AD413 module.   (JWK, Jul. 13, 2000) */
          case AD_413C:
            if (ad_413C.lat != NULL &&
                     !(*ad_413C.lat & ad_413C.mask)) break;
            fcam->dat.cnaf = fcam_ro->cnaf;
            eieio();
            id = Cam_id + fcam_ro->c * 1024 + fcam_ro->n * 32;
            while(1)
             {
               register unsigned short param_id;

               tmp = fcam->dat.l;
               if (tmp < 0) break;
               if (tmp & 0x8000) continue;
               param_id = *(id+((tmp & 0x6000) >> 13));
               *evt++ = param_id;
               *evt++ = tmp & 0x1fff;
             }
            break;
/*      End of added support for ORTEC AD413 module.   (JWK, Jul. 13, 2000) */
          case SILENA_4418C:
            if (silena_4418C.lat != NULL &&
                              !(*silena_4418C.lat & silena_4418C.mask)) break;
            fcam->dat.cnaf = fcam_ro->cnaf;
            eieio();
            id = Cam_id + fcam_ro->c * 1024 + fcam_ro->n * 32;
            tmp = fcam->dat.l;
            if (tmp < 0) break;
            tmp = fcam->dat.l;
            if (tmp < 0) break;
            while(1)
             {
               register unsigned short param_id;

               tmp = fcam->dat.l;
               if (tmp < 0) break;
               param_id = *(id+((tmp & 0x7000) >> 12));
               *evt++ = param_id;
               *evt++ = tmp & 0x8fff;
             }
            break;
          default:
            break;
         }
        fcam_ro++;
     }
   Event = evt;
   return(1);
}
/*****************************************************************************
*
******************************************************************************/
static int poll_xia(void)
{
      struct FCAM *fcam = (struct FCAM *)ORNLAUX;
    struct KSC_VME *ksc = (struct KSC_VME *)KSC1;
                 int tmp;

  if (xia_poll < 0)
     {
       fcam->dat.cnaf = xia_poll;
       eieio();
       tmp = fcam->dat.l;
       if (tmp <= 0) tmp = 0;
     }
   else
     {
       ksc->cma = xia_poll;
       ksc->csr = KSC_CSR_CAM_RD | KSC_CSR_GO;
       eieio();
       while (!((tmp = ksc->csr) & KSC_CSR_RDY));
       tmp = ksc->dlr;
       while(!(ksc->csr & KSC_CSR_DONE));
     }
   if (!(tmp & 0x2000)) return(1);
   return(2);
}
/*****************************************************************************
*
******************************************************************************/
static int XIA_format(void)
{
              register int cnt;
   register unsigned short tmp,*evt = Event;
   register struct read_xia *xia = &xia_rd[0];
      register struct FCAM *fcam = (struct FCAM *)ORNLAUX;
            struct KSC_VME *ksc = (struct KSC_VME *)KSC1;


   if (evt != Startevt)
     {
       *(unsigned int *)evt = 0xffffffff;
       evt += 2;
       AcqBuf->Bufhdr.events += 1;
     }
   while(xia->cnaf)
     {
       if (xia->wc < 0)
         {
           fcam->dat.cnaf = xia->wc;
           eieio();
           cnt = fcam->dat.l;
           if (cnt < 0) cnt = 0;
           cnt = cnt & 0xffff;
           if (cnt == 0)
             {
               *evt++ = 2;
               *evt++ = xia->vsn;
             }
           else
             {
               fcam->dat.cnaf = xia->cnaf;   /* Do one dummy F(5) to start  */
               eieio();
               *evt = fcam->dat.s;           /* the data readout            */
/***********
cnt = 6;
***********/
               fcam->dat.cnaf = xia->cnaf;
               eieio();
               *evt++ = fcam->dat.s;
               fcam->dat.cnaf = xia->cnaf;
               eieio();
               *evt = fcam->dat.s;
               *evt++ = xia->vsn;
               cnt = cnt - 2;
               while(cnt--)
                 {
                   fcam->dat.cnaf = xia->cnaf;
                   eieio();
                   tmp = fcam->dat.s;
                   if (tmp == 0xffff) tmp = 0xfffe;
                   *evt++ = tmp;
                 }
               cnt = evt - Event;
               if (cnt % 2) *evt++ = 0xffff;
             }
         }
       else
         {
           ksc->cma = xia->wc;
           ksc->csr = KSC_CSR_CAM_RD | KSC_CSR_GO;
           eieio();
           while (!(ksc->csr & KSC_CSR_RDY));
           cnt = ksc->dlr;
           if (cnt == 0)
             {
               *evt++ = 2;
               *evt++ = xia->vsn;
             }
           else
             {
               ksc->cma = xia->cnaf + 2;
/***********
cnt = 6;
***********/
               ksc->cmr = -cnt;
               ksc->cma = xia->cnaf;
               ksc->csr = KSC_CSR_CAM_RD | KSC_CSR_GO;
               eieio();
               while (!(ksc->csr & KSC_CSR_RDY));
               *evt++ = ksc->dlr;
               while (!(ksc->csr & KSC_CSR_RDY));
               *evt = ksc->dlr;
               *evt++ = xia->vsn;
               cnt = cnt - 2;
               while(cnt--)
                 {
                   while (!(ksc->csr & KSC_CSR_RDY));
                   tmp = ksc->dlr;
                   if (tmp == 0xffff) tmp = 0xfffe;
                   *evt++ = tmp;
                 }
               cnt = evt - Event;
               if (cnt % 2) *evt++ = 0xffff;
               while(!(ksc->csr & KSC_CSR_DONE));
             }
         }
       *(unsigned int *)evt = 0xffffffff;
       evt += 2;
       AcqBuf->Bufhdr.events += 1;
       xia++;
     }
   *evt++ = 2;                 /* Special VSN to mark end of spill */
   *evt++ = 9999;
   Event = evt;

   ksc->cma = xia_restart_cma;
   ksc->csr = KSC_CSR_CAM_WR | KSC_CSR_GO;
   eieio();
   while(!(ksc->csr & KSC_CSR_DONE));

   return(1);
}
