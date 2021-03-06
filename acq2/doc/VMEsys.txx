[1m   22-Sep-17 .......... VME Front-End Acquisition System ............ PAGE   1
[22m  
 
 
   Sec Page Contents
 
   010   1  Introduction
   020   2  On the Road to Data Acquisition - Things YOU must do
   030   2  VME Hardware Setup
   040   5  Configuration of VME/Workstation
   050   7  Running the VME Acquisition
   060   9  FORTRAN Interface to CAMAC, FASTBUS and Acquisition Control
   070  14  Test and Status Routines
   080  16  VME Hardware Diagnostics
   090  17  What she'll do
 
[1m   [4mVME .010 Introduction                                                      [24m
[22m  
   ************************************************************************
 
                           Physics Division
                     Oak Ridge National Laboratory
 
                        Copyright(C) 1993-2004
 
 
 
                       IMPORTANT NOTICE TO USER
 
   All statements, technical information and recommendations contained
   herein are based on tests we believe to be reliable, but the accuracy
   or completeness thereof is not guaranteed, and the following  is
   made in lieu of all warranties expressed or implied:
 
   Our only obligation shall be to correct or replace, at our convenience,
   any part of the product proven to be defective.  We shall not be liable
   for any loss or damage, direct or consequential, arising out of the
   use or the inability to use the product.  Before using, the USER shall
   determine the suitability of the product for his or her intended use,
   and the USER assumes all risk and liability whatsoever in connection
   therewith.
 
   ************************************************************************
 
   Welcome  to  the  first  edition of VME Front-End Acquisition System user's
   manual.  This  document  is  limited  to  installation,  configuration, and
   operation of the VME Front-End Acquisition system.
 
   UNIX  scripts  and executables for the commands described herein are in the
   directory [1m/usr/hhirf[22m [24m  or [1m/usr/acq/vme[22m [24m .
 
   For   documentation  on  [1mPACOR[22m [24m ,  see  file  [1m/usr/hhirf/doc/pacor.doc[22m [24m .  This
   document is file [1m/usr/hhirf/vme/doc/VMEsys.doc[22m [24m .
 
   Throughout this document, examples of screen output will be reproduced.  In
   these examples, user input will be in Bold print.
 
 
    
[1m   22-Sep-17 .......... VME Front-End Acquisition System ............ PAGE   2
[22m  
[1m   [4mVME .020 On the Road to Data Acquisition - Things you must do              [24m
[22m  
 
       1).. Setup your VME crate ........................................
 
            see Section 030
                User's manuals for the hardware interfaces
 
       2).. Configure the VME/Workstation system .........................
 
            see Section 040
 
       3).. Load VME Acquisition software ...............................
 
            see Section 050  -  LOADACQ command
 
       4).. Prepare your PAC program ....................................
 
            see file - [1m/usr/hhirf/doc/pacor.doc[22m [24m  - PACOR user documentation
 
       5).. Compile and load your PAC ...................................
 
            see file - [1m/usr/hhirf/doc/pacor.doc[22m [24m  - PACOR user documentation
                Section 050  -  PACOR command
 
       6).. Initialize the VME Acquisition system .......................
 
            see Section 050  -  INITVME command
 
       7).. Run your custom hardware initialization procedures ..........
 
            see Section 060  -  FORTRAN callable I/O routines
 
            This is your ball of wax.
 
       8).. Start the VME acquisition system .............................
 
            see Section 050  -  STARTVME command
 
       9).. You are now ready for data acquisition.  See the PACMAN document,
            [1m/usr/hhirf/wks/doc/pacmanII.doc[22m [24m , for operating the workstation
            acquisition software.
 
 
[1m   [4mVME .030 VME Hardware Setup                                                [24m
[22m  
   This  section  covers  the installation of the VME front-end system and its
   connection the Workstation.
 
          1...  Turn off power to the VME crate before  inserting  or removing
          VME modules.
 
          2...  Forced air cooling is required for the VME crate.  Some crates
          have built-in fan assemblies.  Others require a fan assembly mounted
          below the VME crate.
 
    
[1m   22-Sep-17 .......... VME Front-End Acquisition System ............ PAGE   3
[22m  
          3...   In a vertical VME card cage, slot 1 is the leftmost slot when
          viewing the front of the crate.   The  Force  SYS68K  CPU  processor
          module MUST be installed in slot 1.
 
          4...   When installing additional modules in the crate, there should
          be no empty slots to the left of any module.
 
   [1m................ VME Processor Module .................[22m [24m 
 
   There are two hexadecimal switches labeled 1 and 2 on the  front  panel  of
   the  SYS68K CPU-40 module.  Switch 1 should be set to B and switch 2 should
   be set to F.
 
   There are two hexadecimal switches labeled 1 and 2 on the  front  panel  of
   the  SYS68K CPU-60 module.  Switch 1 should be set to 7 and switch 2 should
   be set to F.
 
   The setting of these switches control the start up of  the  VME  processor.
   It is very important these switches be set a described above!
 
   The SYS68K CPU-40/B4 has 4 serial ports.  The 4 9-pin Micro DSUB connectors
   labeled  1 thru 4 on the front panel are these serial ports. Serial ports 3
   and 4 are not currently used.
 
   The SYS68K CPU-60 has 2 serial ports.  The 2 9-pin DSUB  connectors labeled
   1 and 2 on the front panel are these serial ports.
 
   Serial  port  1  is used by the VMEPROM firmware.  Serial port 2 is used if
   you use the interactive CAMAC command program VMECNAF.
 
   It is not necessary to connect a terminal to any serial port.  If, however,
   a terminal is connected, the required terminal setup is:
 
[1m          9600 Baud, 8 bit characters, No parity, and 1 stop bit
[22m  
   The VME system must be connected to the Workstation.  You  should  seek the
   help of a network guru.
 
[1m     WARNING:  The SYS68K CPU VME processor module MUST
               be installed in slot 1 of the VME crate.
[22m  
   [1m............ KSC 2917 CAMAC Interface Module ...........[22m [24m 
 
   It  is  recommended  that the KSC 2917 module be installed in slot 2 of the
   VME crate.   The  VME  module  controls  up  to  8  KSC  3922  CAMAC  crate
   controllers.  Use  of  40 conductor twist-and-flat cable is recommended.  A
   terminator module should be connected to the bus output  connector  at  the
   last KSC 3922. See the KSC 3922 manual for cabling details.
 
   The  acquisition  system  will  use  a  KSC  3982  List  Sequencing   Crate
   Controller  if  it  is installed in slot 23 of CAMAC crate 0.  Installation
   requires a 40 conductor ribbon cable connecting the KSC  3982  to  the  KSC
   3922  on the rear of the modules and a LEMO cable from Grant Out on the KSC
   3922 to Grant In on the KSC 3982.  See KSC 3922 and  KSC  3982  manuals for
   more details.
 
    
[1m   22-Sep-17 .......... VME Front-End Acquisition System ............ PAGE   4
[22m  
   [1m........... LRS 1131 FASTBUS Interface Module ..........[22m [24m 
 
   You  need  this  module only if you have a FASTBUS system.  If present, use
   the special LeCroy cable to connect the VME module to the LRS 1821  FASTBUS
   Segment Manager/Interface.
 
   [1m........... CES 8170 High Speed Memory Module ..........[22m [24m 
 
   If you have FERA type readout devices, install a CES 8170 in the VME crate.
   The CES 8170 connects to a LRS 4301 FERA Driver CAMAC module.  Three cables
   are  required.  The data cable is a 34 conductor ribbon.  Use of twist-and-
   flat cable is recommended.  Two  LEMO  cables  are  required  for  the data
   transfer  handshake signals.  Connect WSO on the LRS 4301 to WSI on the CES
   8170.  Also, connect WAO on the CES  8170  to  WAI  on  the  LRS  4301. For
   instructions  on  connecting  FERA readout devices to the LRS 4301, see the
   LRS 4301 manual and manuals for your particular CAMAC modules.
 
   [1m........... LRS 1190 VME Dual Port Memory Module .......[22m [24m 
 
   The LRS 1190 is a replacement for the CES 8170. Setup is  very  similar  to
   that  of  the  CES 8170.  The data cable is identical to that used with the
   CES 8170.  The labeling of the front panel  LEMO  connectors  is different.
   The  LRS  1190  STROBE  input  is the same as WSI and the ACK output is the
   same as WAO.  You may use upto 5 LRS 1190s in the VME system.   See the LRS
   1190 manual for additional details.
 
   [1m................. ORNL Trigger Module ..................[22m [24m 
 
   A  VME  Event  Trigger Module is needed for all setups.  The trigger module
   accepts an Event signal, generates a Busy  output  during  readout,  and  a
   Clear  signal  a  the  end of readout.  Input and outputs are fast negative
   NIM and differential ECL.  See the VME Event  Trigger  Module  document for
   additional information.
 
   [1m..................... SMOKE TEST ......................[22m [24m 
 
   Now  that  you have installed all the VME interface modules needed, turn on
   power to the VME crate.  Make the sure the fan assembly is  also  on. After
   approx.  45 seconds, VME processor should be ready.  If you have a terminal
   attached to serial port 1, you should see  a  banner  similar  to  the  one
   below.
 
   ****************************************************************************
   *                                                                          *
   *                          ornl Physics Division                           *
   *                                                                          *
   *                    VME Processor ready for download                      *
   *                                                                          *
   ****************************************************************************
 
   ******************************************************************
   *                                                                *
   *                        V M E P R O M                           *
   *                                                                *
   *          SYS68K/CPU-40     Version  2.74   09-Apr-91           *
   *                                                                *
    
[1m   22-Sep-17 .......... VME Front-End Acquisition System ............ PAGE   5
[22m  
   *           (c) FORCE Computers  and  Eyring Research            *
   *                                                                *
   ******************************************************************
 
   ?
 
 
[1m   WARNING:  The VME processors do not always start up correctly if power
             has been off more than 15 minutes.
[22m  
   If  you  have  a  terminal  connected  to  serial port 1 and do not get the
   banner within 1 minute, press and release the Reset  switch  on  the  front
   panel  of  the  SYS68K  CPU  module.   The  banner should now appear within
   approx. 5 seconds.
 
   If no terminal is connected,   press  and  release  the  Reset  switch  one
   minute after power on.
 
[1m   [4mVME .040 Configuration of VME/Workstation                                  [24m
[22m  
   When  initially  setting up an acquisition system or after changing the VME
   processor, you must set the environment variable VME to the  VME  processor
   you  are using. The following example assumes the VME system has been setup
   as described in section 030.
 
   --------------------------------------------------------------------------
   daqdev2> [1m/usr/acq/vme/vmecpus[22m [24m 
 
   Known VME CPUs are:
       vme1  eth1  00-80-42-00-29-79
       vme2  eth1  00-80-42-04-16-14
       vme3  eth1  00-80-42-04-65-26
       vme4  eth1  00-80-42-04-44-53
       vme5  eth1  00-80-42-04-53-88
       vme6  eth1  00-80-42-04-55-66
       vme7  eth1  00-80-42-04-58-72
       vme8  eth1  00-80-42-04-18-68
       vme9  eth1  00-80-42-04-13-91
       vme10  eth1  00-80-42-04-25-52
       vme11  eth1  00-80-42-04-09-38
       vme12  eth1  00-80-42-04-77-27
       vme20  eth1  00-80-42-0d-03-62
       vme21  eth1  00-80-42-0d-0e-20
       vme22  eth1  00-80-42-0d-02-a7
 
    Working... This will take about 15 seconds
 
    Checking for VME cpus attached to daqdev2.phy.ornl.gov
 
   CPUs ready for boot are:
      vme4
   daqdev2>
   --------------------------------------------------------------------------
 
   In the above example, processor vme4 is connected to  the  workstation.  If
   this  is  the  processor  you  are  using,  you  should set the environment
    
[1m   22-Sep-17 .......... VME Front-End Acquisition System ............ PAGE   6
[22m  
   variable VME and boot the VME processor.
 
   --------------------------------------------------------------------------
   daqdev2> [1msetenv VME vme4[22m [24m 
   daqdev2> [1m/usr/acq/vme/bootvme[22m [24m 
   *********************** WARNING ************************
   * The VME processor is being rebooted.                 *
   *                                                      *
   * To run data acquisition, YOU MUST execute three      *
   * additional commands:                                 *
   *   1)  loadacq                                        *
   *   2)  pacor  userfile l                              *
   *   3)  initvme                                        *
   *********************** WARNING ************************
   Boot VME processor vme4
   Reboot the VME processor
   Loading the VME operating system
   VME operating system is loaded.
   daqdev2>
   --------------------------------------------------------------------------
 
   Next, you should run [1mvmehardware[22m [24m  to see what VME modules are  available  in
   your VME system.
 
   --------------------------------------------------------------------------
   daqdev2> [1m/usr/acq/vme/vmehardware[22m [24m 
   VME System Hardware Configuration
 
       VME Processor Logical Name: vme4
   VME Processor Ethernet Address: 00-80-42-04-44-53
    Default Host Ethernet Address: 00-02-b3-96-b0-f1
           Boot Multicast Address: 03-6d-63-73-71-00
   Available Interface Modules are:
 
   KSC 2917A -  CAMAC Interface Module
   CES 8170  -  FERA Readout Module
   TRIGGER   -  ORNL Trigger Module
   daqdev2>
 
   --------------------------------------------------------------------------
 
   You  should verify that the list of Available Interface Modules agrees with
   the hardware installed in the VME crate.
 
   Next, you should examine your  [1m.login[22m [24m   and  [1m.cshrc[22m [24m   files.   Look  for  any
   statements of the form:
 
       setenv VME vme?
 
   If there are any such statements, you should
 
          1)...  Delete all such statements or change to the VME processor you
          are using.
 
          2)... Logout and login again.
 
    
[1m   22-Sep-17 .......... VME Front-End Acquisition System ............ PAGE   7
[22m  
          You have now completed the configuration procedure.
 
   If the VME processor you are using is not detected by  [1mvmecpus[22m [24m ,  there  are
   many  possible  causes.  These include incorrect setup of the VME processor
   module, defective VME processor module, defective transceivers, etc.,  etc.
   The  following  is  a  check  list  for  the  most  likely problems. A more
   detailed diagnostic procedure is in section 080.
 
          1... Check the ethernet connection between the VME  system  and  the
          Workstation.
 
          2... Make sure the fan assembly for the VME crate is operating.  The
          SYS68K CPU MUST have forced air cooling.
 
          3...  Check the setting of the two hexadecimal switches on the front
          panel of the SYS68K CPU. For a SYS68K CPU40, switch 1 should be  set
          to  B  and switch 2 should be set to F. For a SYS68K CPU60, switch 1
          should be set to 7 and switch 2 should be set to F.
 
          4... Press and release the Reset switch on the front  panel  of  the
          SYS68K  CPU  module.   The  VME  processors  do  not always start up
          correctly if power has been off more than a few minutes.
 
   Now repeat the configuration procedure from the beginning. If  the  failure
   persists, see section 080 for additional diagnostics.
 
 
[1m   [4mVME .050 Running the VME Acquisition System                                [24m
[22m  
   If  you  are  looking for just a few good commands, here they are. [1mPACOR[22m [24m  is
   in the directory [1m/usr/hhirf[22m [24m .  All  other  commands  are  in  the  directory
   [1m/usr/acq/vme[22m [24m .
 
   [1mBOOTVME ...............................................[22m [24m 
 
   This  command  loads  the  code for a minimal operating system into the VME
   processor. After the basic operating system is loaded, the workstation  can
   communicate directly with hardware such as FASTBUS and CAMAC.
 
   --------------------------------------------------------------------------
   daqdev2> [1m/usr/acq/vme/bootvme[22m [24m 
   *********************** WARNING ************************
   * The VME processor is being rebooted.                 *
   *                                                      *
   * To run data acquisition, YOU MUST execute three      *
   * additional commands:                                 *
   *   1)  loadacq                                        *
   *   2)  pacor  userfile l                                *
   *   3)  initvme                                        *
   *********************** WARNING ************************
   Reboot the VME processor
   Loading the VME operating system
   VME operating system is loaded.
   daqdev2>
   --------------------------------------------------------------------------
 
    
[1m   22-Sep-17 .......... VME Front-End Acquisition System ............ PAGE   8
[22m  
   [1mTESTVME ...............................................[22m [24m 
 
   Testvme  loads  the  VME  processor  with  a  code which generates an event
   stream.  Each event has 28 parameters.  The IDs are 1  thru  28.  Within an
   event  the data for every parameter has the same value.  That data value is
   incremented after each event and has a range of 0 thru 4095.
 
   --------------------------------------------------------------------------
   daqdev2> [1m/usr/acq/vme/testvme[22m [24m 
   *********************** WARNING ************************
   *  Normal Acquisition Codes in the VME processor are   *
   *  being replaced with test programs.  These test      *
   *  programs generate an event stream for testing the   *
   *  VME/Workstation connection.                          *
   *                                                      *
   * NOTE: When finished testing, you should execute the  *
   *       the command - loadacq                          *
   *********************** WARNING ************************
   Delete the acquisition tasks in the VME processor
 
   Now load the VME/Workstation test programs.
 
   VME/Workstation test codes are now loaded.
   daqdev2>
   --------------------------------------------------------------------------
 
   [1mLOADACQ ...............................................[22m [24m 
 
   Once the basic operating system is loaded(BOOTVME above), you can load  the
   acquisition  code  into  the  VME processor. This is the code which decodes
   your readout instructions, responds to Event inputs,  reads  the  specified
   hardware  and  sends event data to the workstation. This is a necessary but
   not sufficient step in starting data acquisition.
 
   --------------------------------------------------------------------------
   daqdev2> [1m/usr/acq/vme/loadacq[22m [24m 
   Delete the acquisition tasks in the VME processor
 
   Now load the acquisition tasks.  Any error messages here are BAD news.
 
   Acquisition system now loaded
   daqdev2>
   --------------------------------------------------------------------------
 
   [1mPACOR .................................................[22m [24m 
 
   [1mPACOR[22m [24m  is a compiler which generates and loads tables  of  readout  commands
   into    the    VME    processor.     For    detailed    instructions,   see
   [1m/usr/hhirf/doc/pacor.doc[22m [24m .
 
   --------------------------------------------------------------------------
   daqdev2> [1m/usr/hhirf/pacor test l[22m [24m 
   #BYTES OBJECT CODE GENERATED =   16808
   CONSIDER YOURSELF LOADED WITH  16808  BYTES
   NO ERRORS
   daqdev2>
    
[1m   22-Sep-17 .......... VME Front-End Acquisition System ............ PAGE   9
[22m  
   --------------------------------------------------------------------------
 
 
   [1mINITVME ...............................................[22m [24m 
 
   Currently, the VME acquisition system does  not  recognize  when  you  have
   loaded  a  new  PAC.  Therefore,  you  MUST  tell it. Note that you can NOT
   initialize the VME acquisition system if VME acquisition is  running.  Many
   errors  are possible during the initialization of the VME(nonexistent CAMAC
   crates or FASTBUS modules etc. etc.). Only the first  detected  error  will
   be reported.
 
   --------------------------------------------------------------------------
   daqdev2> [1m/usr/acq/vme/initvme[22m [24m 
   daqdev2>
   --------------------------------------------------------------------------
 
   [1mSTARTVME ..............................................[22m [24m 
 
   If  the  INITVME  executed  without  error,  you  may  now  start  the  VME
   acquisition system. You  must  STARTVME  if  you  want  data  sent  to  the
   workstation.
 
   --------------------------------------------------------------------------
   daqdev2> [1m/usr/acq/vme/startvme[22m [24m 
   daqdev2>
   --------------------------------------------------------------------------
 
   [1mSTOPVME ...............................................[22m [24m 
 
   Stop  the  VME  acquisition. Readout by the VME processor stops and no more
   data are sent to the workstation.
 
   --------------------------------------------------------------------------
   daqdev2> [1m/usr/acq/vme/stopvme[22m [24m 
   daqdev2>
   --------------------------------------------------------------------------
 
   [1mSTATVME ...............................................[22m [24m 
 
   You may ask for the state of the VME acquisition system with this  command.
   Possible responses are 1) not initialized, 2) running and 3) stopped.
 
   --------------------------------------------------------------------------
   daqdev2> [1m/usr/acq/vme/statvme[22m [24m 
   VME Acquisition is running
   daqdev2>
   --------------------------------------------------------------------------
 
 
[1m   [4mVME .060 FORTRAN Interface to CAMAC and FASTBUS                            [24m
[22m  
   This  section  describes  FORTRAN  callable  routines  for  CAMAC,  FASTBUS
   hardware I/O and control of the VME  acquisition  system.   The  names  and
   calling  parameters  of  the CAMAC routines are identical to the Concurrent
   system and therefore, very  minimal  changes  are  required  to  convert  a
    
[1m   22-Sep-17 .......... VME Front-End Acquisition System ............ PAGE  10
[22m  
   Concurrent program for use with the Workstation/VME system.
 
   For  new  programs, CAMACIO is recommended as a replacement for BHIO.  This
   is   functionally  equivalent  but  the  routine  name  is  a  little  more
   indicative of the function performed.
 
   Since the hardware interface to FASTBUS is substantially different  on  the
   Workstation/VME  system,  Concurrent programs for FASTBUS will require more
   changes.
 
   Two new routines are available - CAMLIST and  FBLIST.   Whereas  the  basic
   routines  do  one  CAMAC  or  FASTBUS  operation  per  call, these routines
   process a list of operations.   This  is  much  more  efficient  when large
   amounts  of data must be transferred (i.e. loading pedestal data to the LRS
   1821).
 
   Another new routine allows control of the front-end acquisition system.
 
   All routines described in this section are in the library
 
          [1m/usr/acq/vme/vmelib.a[22m [24m 
 
   Any of your FORTRAN programs which have calls to  these  routines  must  be
   linked  with  this library.  Of course, any of these routines may be called
   by C language programs.
 
   [1m...................... CAMAC I/O ......................[22m [24m 
 
   The main CAMAC I/O routines on the Concurrent system were BHIO and  CMCBSC.
   These  are  implemented   here  with the same names and calling parameters.
   While the syntax for calling these routines remains  the  same,  the  crate
   number  parameter has a different range. On the Concurrent system the crate
   number could be 1 thru 63.   On  the  Workstation/VME  system  crate number
   should be in the range of 0 thru 7.
 
   All  CAMAC  I/O  routines return the status of the last operation. Normally
   the returned status should be zero.  The error codes are:
 
         Code         Meaning
           0       Normal return, X = 1 and Q = 1
           1       X = 0 and Q = 1
           2       X = 1 and Q = 0
           3       X = 0 and Q = 0
         '80'X     Timeout
       'c004'X     Illegal crate number
       'c005'X     Illegal module number
       'c006'X     Illegal subaddress
       'c009'X     Illegal function code
       'a000'X     Crate off-line or nonexistent
 
   The file [1m/usr/acq/vme/cam_fast.for[22m [24m  is a FORTRAN include file which  assigns
   mnemonics  to  the  possible  error codes returned by CAMAC and FASTBUS I/O
   routines.
 
   [1mBHIO ........................Concurrent Compatibility..[22m [24m 
 
    
[1m   22-Sep-17 .......... VME Front-End Acquisition System ............ PAGE  11
[22m  
          CALL BHIO(MODE,LU,C,N,A,F,DATA,NUM,STAT)
     where
            MODE  - integer*4, zero means 16-bit CAMAC transfers
                            nonzero means 24-bit CAMAC transfers
            LU    - Not referenced in Workstation/VME version
            C     - integer*4, Crate number, 0 thru 7
            N     - integer*4, CAMAC slot number
            A     - integer*4, CAMAC Subaddress
            F     - integer*4, CAMAC Function code
            DATA  - integer*2 array for 16-bit CAMAC transfers
                    integer*4 array for 24-bit CAMAC transfers
            NUM   - integer*4, number of CAMAC data transfers
            STAT  - integer*4, returned status of the last CAMAC cycle
 
   This routine executes CAMAC cycles to a single CNAF.  If the CAMAC function
   code is a read or a write, then NUM data transfers are  executed.   If  the
   value  of STAT is nonzero, the routine IPBERR is called to report the CAMAC
   error.    There   is   a   default   IPBERR   routine   in   the    library
   [1m/usr/acq/vme/vmelib.a[22m [24m .   The  default routine always outputs the message to
   the STDERR.  An example error message:
 
   CAMAC Error - C,N,A,F =  1  1  0  0  Crate Off_Line
 
   If the user has a custom IPBERR routine, it will replace  the  one  in  the
   library  provided the object file containing the routine occurs in the link
   list prior to the library.  The file  [1m/usr/acq/vme/ipberr.f[22m [24m   is  an example
   of a custom error routine.
 
   [1mCMCBSC ......................Concurrent Compatibility..[22m [24m 
 
          CALL CMCBSC(B,C,N,A,F,MODE,TMO,DATA,NUM,STAT)
     where
            B     - Not referenced in Workstation/VME version
            C     - integer*4, Crate number, 0 thru 7
            N     - integer*4, CAMAC slot number
            A     - integer*4, CAMAC Subaddress
            F     - integer*4, CAMAC Function code
            TMO   - integer*4, timeout in seconds
            MODE  - integer*4, zero means 16-bit CAMAC transfers
                            nonzero means 24-bit CAMAC transfers
            DATA  - integer*2 array for 16-bit CAMAC transfers
                    integer*4 array for 24-bit CAMAC transfers
            NUM   - integer*4, number of CAMAC data transfers
            STAT  - integer*4, returned status of the CAMAC operation
 
   This  routine  is included strictly for Concurrent compatibility and is not
   recommended for new software.
 
   [1mCAMACIO................................................[22m [24m 
 
          CALL CAMACIO(MODE,C,N,A,F,DATA,NUM,STAT)
     where
            MODE  - integer*4, zero means 16-bit CAMAC transfers
                            nonzero means 24-bit CAMAC transfers
            C     - integer*4, Crate number, 0 thru 7
            N     - integer*4, CAMAC slot number
    
[1m   22-Sep-17 .......... VME Front-End Acquisition System ............ PAGE  12
[22m  
            A     - integer*4, CAMAC Subaddress
            F     - integer*4, CAMAC Function code
            DATA  - integer*2 array for 16-bit CAMAC transfers
                    integer*4 array for 24-bit CAMAC transfers
            NUM   - integer*4, number of CAMAC data transfers
            STAT  - integer*4, returned status of the last CAMAC cycle
 
   This   routine  is  recommended  for  new  programs.   It  is  functionally
   equivalent to BHIO.  The CAMAC error handler routine  is  CAMERR.  The user
   may  provide  a  custom  error  handler  by  the  same   name.   The   file
   [1m/usr/acq/vme/camerr.f[22m [24m  is an example of a custom error routine.
 
   [1mCAMLIST ...............................................[22m [24m 
 
          CALL CAMLIST(C,N,A,F,MODE,DATA,NUM,STAT)
     where
            C     - integer*4 array, Crate number, 0 thru 7
            N     - integer*4 array, CAMAC slot number
            A     - integer*4 array, CAMAC Subaddress
            F     - integer*4 array, CAMAC Function code
            MODE  - integer*4, zero means 16-bit CAMAC transfers
                            nonzero means 24-bit CAMAC transfers
            DATA  - integer*2 array for 16-bit CAMAC transfers
                    integer*4 array for 24-bit CAMAC transfers
            NUM   - integer*4, number of CAMAC operations in list
            STAT  - integer*4 array, returned status of the CAMAC operation
 
   All  calling  parameters, except MODE, are arrays.  Also the status of each
   CAMAC cycle is return in the STAT array.
 
   [1m..................... FASTBUS I/O .....................[22m [24m 
 
   These routines assume that the FASTBUS  system  includes  a  LRS  1131  VME
   module which connects to a LRS 1821 Segment Manager/Interface.
 
   ALL  FASTBUS  routines  return  a status which is normally zero.  A nonzero
   value indicates an error and the possible error codes are:
 
         Code         Meaning
         '80'X     Timeout
       'c006'X     Illegal register
       'a000'X     Device not available
 
   [1mFASTBUSIO .............................................[22m [24m 
 
          CALL FASTBUSIO(A,RW,DATA,NUM,STAT)
     where
            A     - integer*4, LRS1821 register number
            RW    - integer*4, 0 means read, nonzero means write
            DATA  - integer*2 array
            NUM   - integer*4, number of data transfers
            STAT  - integer*4, returned status
 
   This routine accesses a single register in the  LRS  1821.   Multiple  data
   transfers to/from the specified register are allowed.
 
    
[1m   22-Sep-17 .......... VME Front-End Acquisition System ............ PAGE  13
[22m  
   [1mFBLIST ................................................[22m [24m 
 
          CALL FBLIST(A,RW,DATA,NUM,STAT)
     where
            A     - integer*4 array, LRS1821 register
            RW    - integer*4 array, 0 means read, nonzero means write
            DATA  - integer*2 array
            NUM   - integer*4, number of operations in list
            STAT  - integer*4 array, returned status
 
   Typically  operations  to  FASTBUS  require a sequence of transfers to/from
   multiple registers in the LRS 1821.  All call parameters  for  this routine
   are  arrays.   This allows many register transfer operations to sent to the
   front-end system in a single ethernet packet.  Use  of  this  function will
   significantly reduce the time required to load the pedestal memory.
 
   [1m................. ACQUISITION CONTROL ..................[22m [24m 
 
   The  VME acquisition system recognizes several commands: initialize, start,
   stop, status and zero.  This routine allows a user program  to  execute any
   command and get the status of the VME acquisition system.
 
   [1mACQ_VME_CTRL ...........................................[22m [24m 
 
          CALL ACQ_VME_CTRL(STAT,COMMAND)
     where
            STAT    - integer*4, returned status
            COMMAND - character*8, Command, MUST be lower case.
                      Recognized Commands are:
                       init
                       start
                       stop
                       status
                       zero
 
   The  value  of  STAT  depends  on  the command.  For init, start or stop, a
   negative value is an error code and zero means  success.   For  the  status
   command,  negative  is  an error code and positive is the encoded status of
   the VME acquisition system.
 
   The file [1m/usr/acq/vme/acq_vme_ctrl.for[22m [24m   assigns  mnemonics  for  the  error
   codes  and  acquisition  status.  This file may be included in your FORTRAN
   code.
 
   The routine acq_vme_error may be used to convert an  error/status  code  to
   an ASCII string.
 
 
          CALL ACQ_VME_ERROR(ERROR,STRING)
 
     where
           INT*4  ERROR - Error/status code return by the routine
                          acq_vme_ctrl.
     return
           CHARACTER*(*)  -  ASCII message for this error code.
 
    
[1m   22-Sep-17 .......... VME Front-End Acquisition System ............ PAGE  14
[22m  
 
[1m   [4mVME .070 Test and Status Routines                                          [24m
[22m  
 
   [1mCNAF ..................................................[22m [24m 
 
   CNAF  is  an  interactive  program  for  testing CAMAC hardware.  This is a
   direct descendant of the Concurrent program by the same name.
 
   --------------------------------------------------------------------------
   daqdev2> [1m/usr/acq/vme/cnaf[22m [24m 
    CNAF - Workstation/VME Data Acquisition System  CNT
    Type HELP for list of commands
 
   >[1mhelp[22m [24m 
 
     Commands requiring an argument. dec = decimal and hex = hexadecimal.
 
    c dec -  crate.   0 thru 7
    n dec -  station number.   1 thru 31
    a dec -  subaddress.   0 thru 15
    f dec -  function.   0 thru 31
    d hex -  data
 
    x [n] - execute CNAF n times.  Default n is 1.
    e     - execute CNAF and list.
    l     - list CNAF, data, X and Q
 
    end   - exit program
    exit  - exit program
    help  - this message
 
   >[1mc 0[22m [24m 
   >[1mn 1[22m [24m 
   >[1mf 0[22m [24m 
   >e[22m [24m 
    C= 0 N= 1 A= 0 F= 0 D=  F01F Q= 1 X= 1
   >
   --------------------------------------------------------------------------
 
 
   [1mVMECNAF ...............................................[22m [24m 
 
   VMECNAF is very similar to the CNAF for the Workstation.  It, however, runs
   in the VME processor.  To use  VMECNAF,  you  must  connect  a  terminal to
   serial port 2 of the VME processor.  The required terminal setup is:
 
[1m          9600 Baud, 8 bit characters, No parity, and 1 stop bit
[22m  
   The  code  for  the VME processor must be downloaded by the Workstation. To
   do this run [1m/usr/acq/vme/vmecnaf[22m [24m .
 
   --------------------------------------------------------------------------
   daqdev2> [1m/usr/acq/vme/vmecnaf[22m [24m 
   Load the VME processor CNAF program.  Uses Port 3
   daqdev2>
    
[1m   22-Sep-17 .......... VME Front-End Acquisition System ............ PAGE  15
[22m  
   --------------------------------------------------------------------------
 
   After the code is loaded, you should get a prompt on the terminal connected
   to port 2 of the VME processor.
 
   --------------------------------------------------------------------------
   VME local interactive CNAF
 
 
   cmd> [1mhelp[22m [24m 
    Commands requiring an argument. dec = decimal and hex = hexadecimal.
    c dec - crate.
    n dec - station number.
    a dec - subaddress.
    f dec - function.
    d hex - write data
 
    x [n] - execute CNAF n (decimal) times. Default n is 1.
    e     - execute CNAF and list
    l     - list CNAF, data, A and Q
 
    h     - this message
 
   cmd>
   --------------------------------------------------------------------------
 
   [1mFIFO ..................................................[22m [24m 
 
   FIFO is a test routine which requires a Kinetic Systems  3841  FIFO  Memory
   CAMAC  Module.  This is a very good test for the ethernet connection to the
   VME system, the VME processor, the KSC 2917 CAMAC interface,  and  the  KSC
   3922 Crate Controller.  To exit the test, you must type CTRL C.
 
   --------------------------------------------------------------------------
   daqdev2> [1m/usr/acq/vme/fifo[22m [24m 
    Crate Number?[1m 0[22m [24m 
    FIFO Slot?[1m 2[22m [24m 
   ^Cforrtl: error: process interrupted (SIGINT)
   daqdev2>
 
   --------------------------------------------------------------------------
 
   [1mLT ....................................................[22m [24m 
 
   LT  lists  the  tasks  and memory segment allocations in the VME processor.
   The example below shows the status when all data  acquisition  software  is
   loaded.
 
   --------------------------------------------------------------------------
   daqdev2> [1m/usr/acq/vme/lt[22m [24m 
 
   VME Processor: vme4
    ID          Memory/Task    Size       Address      pc    evt1/evt2  pri/time
     0  T00             Task0    8    7000    9000  FF002CC4   97    0   64    1 W
     1  T01        Lan_Driver   16    9000    D000      ABF8   56   64   70    1 W
     2  T02           Mem_mgr   76    D000   20000      FDA8    0    0   69    1 R
    
[1m   22-Sep-17 .......... VME Front-End Acquisition System ............ PAGE  16
[22m  
     3  M          Acq_Params  128   20000   40000
     4  T03            cnafxx   42   40000   4A800     424B0   -1    1   69    1 W
     5  T04            fastxx   40   4A800   54800     4C9A8   -1    1   63    1 W
     6  T05         data_proc   42   54800   5F000     56C38   72    0   69    1 W
     7  T06            VMEacq 1174   5F000  184800     6873C   -1    1   69    1 W
     8  T07            vmemon   42  184800  18F000    186C02 -128    0   69    1 W
     9  T08             vmexx   40  18F000  199000    19123C   -1    1   69    1 W
   daqdev2>
   --------------------------------------------------------------------------
 
   Use  of  LT  is a good way to check the VME system.  If LT responds with an
   Ethernet transmit error,  the  VME  processor  is  belly  up  and  must  be
   rebooted. See BOOTVME in section 050.
 
   --------------------------------------------------------------------------
   daqdev2> [1m/usr/acq/vme/lt[22m [24m 
    lt error  - Ethernet transmit error.
   daqdev2>
 
   --------------------------------------------------------------------------
 
   [1mVMEHARDWARE ...........................................[22m [24m 
 
   VMEHARDWARE displays a list of hardware modules in the VME system.
 
   --------------------------------------------------------------------------
   daqdev2> [1m/usr/acq/vme/vmehardware[22m [24m 
   VME System Hardware Configuration
 
       VME Processor Logical Name: vme1
   VME Processor Ethernet Address: 00-80-42-00-29-79
    Default Host Ethernet Address: 08-00-2b-24-e0-5b
           Boot Multicast Address: 03-6d-63-73-71-00
   Available Interface Modules are:
 
   KSC 2917 -  CAMAC Interface Module
   LRS 1131 -  FASTBUS Interface Module
   CES 8170 -  FERA Readout Module
   TRIGGER  -  ORNL Trigger Module
   daqdev2>
   --------------------------------------------------------------------------
 
[1m   [4mVME .080 VME Hardware Diagnostics                                          [24m
[22m  
   The  VME  processor requires +5 Volts, +12 Volts and -12 Volts.  Check your
   VME crate power supply for presence of all three output voltages.
 
   When the VME processor is executing the boot loader, a  special  packet  is
   sent  every  10  seconds.   This  test  determines  if  the  Workstation is
   receiving these  packets.   The  routine  [1m/usr/acq/vme/listen[22m [24m   is  used  to
   receive and display the packets.
 
   --------------------------------------------------------------------------
   daqdev2> [1m/usr/acq/vme/listen eth1 4f-51[22m [24m 
   Protocol:  4f-51
   Device H/W address: 00:02:b3:96:b0:f1
    
[1m   22-Sep-17 .......... VME Front-End Acquisition System ............ PAGE  17
[22m  
   Interface flags = 0x1043
   Interface eth1 is UP
   Interface MTU = 1500
   *********************  Packet  *************************
   Time - Wed Nov 10 14:08:10 2004
    Packet Length: 64
   Multicast
      Destination: 03-6d-63-73-71-00
           Source: 00-80-42-04-44-53
         Protocol: 4f-51
    Buffer Length: 50
     Ack: 0
   Order: 0
     PID: 0
   56 32 2e 30 20 49 50 4c 20 46 6f 72 63 65 20 43    V2.0 IPL Force C
   50 55 2d 34 30 20 3a 20 52 65 61 64 79 20 66 6f    PU-40 : Ready fo
   72 20 64 6f 77 6e 6c 6f 61 64 0d 00                r download..
   --------------------------------------------------------------------------
 
   If  the  Workstation is not receiving packets like the one above, check the
   ethernet connection between the VME system and the Workstation.
 
   If no packets are being received by the Workstation, further tests  of  the
   VME  processor are required.  Tests of the VME processor require a terminal
   connected to serial port 1.  The required setup for the terminal is:
 
[1m          9600 Baud, 8 bit characters, No parity, and 1 stop bit
[22m  
   The boot loader code for the VME processor  is  stored  in  battery  backed
   RAM.   There  is a backup copy of the boot loader in EEPROM.  The procedure
   below restores the battery backed RAM from the EEPROM.
 
          1)... There are two hexadecimal switches on the front panel  of  the
          VME processor module.  Set both switches to F.
 
          2)...  Press  and  release  the  Reset  switch  on the VME processor
          module. You should now get a banner similar to the example below  on
          the terminal connected to serial port 1.
 
   ******************************************************************
   *                                                                *
   *                        V M E P R O M                           *
   *                                                                *
   *          SYS68K/CPU-40     Version  2.74   09-Apr-91           *
   *                                                                *
   *           (c) FORCE Computers  and  Eyring Research            *
   *                                                                *
   ******************************************************************
 
   ?
 
          3)...  At the ? prompt, type go $ffc80000.  The VME processor should
          respond with a line "Trap #0"  followed  by  a  dump  of  the  68040
          registers.
 
   ? [1mgo $ffc80000[22m [24m 
    
[1m   22-Sep-17 .......... VME Front-End Acquisition System ............ PAGE  18
[22m  
   Trap #0
   ....
   ....
 
   ?
 
          4)...  Set  switch  1  to B for a CPU40 or 7 for a CPU60. Then press
          and release the Reset switch. Now you should get  a  banner  on  the
          terminal similar to that shown in section 030.
 
          5)... At the Workstation, retry [1mvmecpus[22m [24m .
 
   If  the  system  still  doesn't  work, it is time to call for HELP and find
   something else to amuse yourself with until help arrives.
 
 
[1m   [4mVME .090 What she'll do                                                    [24m
[22m  
   Measurements of the event rate and readout time as a function of number  of
   parameters  and parameter type were made. The event generator was a pulser.
   To determine the event rate , the  pulser  rate  was  increased  until  the
   front-end  system  begins  losing  events.  The  pulser   trigged   a   NIM
   latch(Joerger  Model  GG  Gate  generator) which was the Event input to the
   VME system.  When readout completes, the VME system resets  the  NIM latch.
   The  reported readout time is the time the latch is asserted. The following
   table has the rates and readout times for CAMAC and FERA devices.
 
   Kbytes/   Packets/   Events/    Readout time   Parameter
     Sec       Sec        Sec         usec          type
 
   193        132       24162          33          1 CAMAC
   267        184        9551          83          6 CAMAC
   285        198        8892          87          7 CAMAC
   302        210        8400          91          8 CAMAC
   335        230        6436         110         12 CAMAC
   249        306        4289         158         24 CAMAC
   462        347        3120         210         36 CAMAC
 
   192        131       24028          34          1 FERA
   465        323       11380          48          8 FERA
   573        402        8440          64         16 FERA
   647        446        4902          99         32 FERA
 
 
   CAMAC readout is  presently  considerably  slower  than  FERA.   The  CAMAC
   readout  uses different methods depending on the number of parameters read.
   If the number of parameters to be read is 6 or less,  readout  is  strictly
   programmed  I/O.   For  7  or more parameters, a KSC 3892 List Sequencer is
   used followed by a DMA transfer from the KSC 3982 to the VME  system.   The
   CAMAC readout times can be approximated as follows:
 
   For 6 or fewer parameters:
 
       readout time (microseconds) = 23 + 10*n
 
   For 7 or more parameters:
    
[1m   22-Sep-17 .......... VME Front-End Acquisition System ............ PAGE  19
[22m  
 
       readout time (microseconds) = 54 + 4.4*n
 
 
   FERA read times can be approximated:
 
       readout time (microseconds) = 32 + 2*n
 
   The  above  approximations  are  valid  only  if  reading  a single type of
   hardware device.  When readout consists of a  both  type  of  devices, some
   operations  can  be  done  concurrently  resulting in a faster readout than
   predicted from the above.  The  following  table  shows  readout  times for
   varying numbers of CAMAC and FERA parameters.
 
   Readout time    Parameter types
      usec
 
     51           1 CAMAC +  1 FERA
     57           1 CAMAC +  4 FERA
    114           6 CAMAC +  8 FERA
    130           6 CAMAC + 16 FERA
    119           7 CAMAC + 16 FERA
    102           8 CAMAC +  8 FERA
    118           8 CAMAC + 16 FERA
    155           8 CAMAC + 32 FERA
    112          12 CAMAC +  8 FERA
    126          12 CAMAC + 16 FERA
    215          36 CAMAC + 32 FERA
 
