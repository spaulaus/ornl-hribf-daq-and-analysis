*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1993-1996
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
*    Environment:  VME based Data Acquisition System
*
*    File:         /usr/users/mcsq/Dacq/for_acq_ctrl.for
*
*    Description:  Status return codes from for_acq_ctrl.c
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    1/30/93    MCSQ         Converted acq_ctl.h to FORTRAN include
*
*    3/29/93    MCSQ         Added code for no KSC3982 list sequencer
*
*    7/21/94    MCSQ         Changed ACQ_NO_CES8170 to ACQ_NO_FERA.  This
*                            code now means that no CES 8170 nor LRS 1190
*                            module is available.
*
*   10/ 1/96    MCSQ         Add new error codes.
*****************************************************************************/
      integer*4 ACQ_RUN
      integer*4 ACQ_STOP
      integer*4 ACQ_UNINIT
      integer*4 ACQ_OK
      integer*4 ACQ_UNKNOWN_COMMAND
      integer*4 ACQ_STR_HALT
      integer*4 ACQ_STR_RUN
      integer*4 ACQ_STR_NOINIT
      integer*4 ACQ_INIT_RUN
      integer*4 ACQ_INVALID_TABLE
      integer*4 ACQ_CAM_NOEXIST
      integer*4 ACQ_CAM_OFFLINE
      integer*4 ACQ_CAM_INHIBIT
      integer*4 ACQ_CAM_INIT
      integer*4 ACQ_FB_NOEXIST
      integer*4 ACQ_FB_UNKNOWN
      integer*4 ACQ_FERA_UNKNOWN
      integer*4 ACQ_FERA_INIT
      integer*4 ACQ_NO_KSC2917
      integer*4 ACQ_NO_LRS1131
      integer*4 ACQ_NO_LRS1821
      integer*4 ACQ_NO_ACROMAG
      integer*4 ACQ_NO_FERA
      integer*4 ACQ_NO_TRIGGER
      integer*4 ACQ_NO_KSC3982
      integer*4 ACQ_SEQ_BUFFERS
      integer*4 ACQ_COUNT_DWN
      integer*4 ACQ_RAW_GATE
      integer*4 ACQ_CAL_GATE
      integer*4 ACQ_CAM_RO
      integer*4 ACQ_CAM_UNKNOWN
      integer*4 ACQ_GATE_MODTYPE
      integer*4 ETHERNET_ERROR
*
*   Acquisition status codes
*
      parameter (ACQ_RUN = 1)      ! Acquisition running
      parameter (ACQ_STOP = 2)     ! Acquisition stopped
      parameter (ACQ_UNINIT = 3)   ! Acquisition not initialized
*
*    Returned status codes.
*
      parameter (ACQ_OK = 0)       ! All went well
*
*    Error status codes
*
      parameter (ACQ_UNKNOWN_COMMAND = -1)

      parameter (ACQ_STR_HALT = -5)   ! Acquisition already stopped
      parameter (ACQ_STR_RUN = -4)    ! Acquisition already running
      parameter (ACQ_STR_NOINIT = -3) ! Start failed. System has not been
*                                      initialized.
      parameter (ACQ_INIT_RUN = -6)   ! Init error - acquisition is running
      parameter (ACQ_INVALID_TABLE = -2) ! Invalid Acq table index
      parameter (ACQ_CAM_NOEXIST = -10)  ! nonexistent crate
      parameter (ACQ_CAM_OFFLINE = -11)  ! Crate off-line
      parameter (ACQ_CAM_INHIBIT = -12)  ! Crate Inhibit stuck
      parameter (ACQ_CAM_INIT = -13)     ! Initialization list CAMAC timeout
      parameter (ACQ_FB_NOEXIST = -20)   ! FASTBUS module does not respond
      parameter (ACQ_FB_UNKNOWN = -21)   ! Unknown FASTBUS module type
      parameter (ACQ_FERA_UNKNOWN = -30) ! Unknown FERA module type
      parameter (ACQ_FERA_INIT = -31)    ! CAMAC timeout initializing FERA module
      parameter (ACQ_NO_KSC2917 = -40)   ! No CAMAC interface present
      parameter (ACQ_NO_LRS1131 = -41)   ! No VME FASTBUS interface present
      parameter (ACQ_NO_LRS1821 = -42)   ! No FASTBUS 1821 sequencer present
      parameter (ACQ_NO_ACROMAG = -43)   ! No Acromag digital I/O present
      parameter (ACQ_NO_FERA    = -44)   ! No FERA  interface present
      parameter (ACQ_NO_TRIGGER = -45)   ! No ORNL trigger module present
      parameter (ACQ_NO_KSC3982 = -46)   ! No KSC3982 List Sequencer present
      parameter (ACQ_SEQ_BUFFERS = -47)  ! Sequencer buffers are too small
      parameter (ACQ_COUNT_DWN = -48)    ! Count down list too large
      parameter (ACQ_RAW_GATE = -49)     ! Raw gate list too large
      parameter (ACQ_CAL_GATE = -50)     ! Calculated gate list too large
      parameter (ACQ_CAM_RO = -51)       ! Special CAMAC ro list too large
      parameter (ACQ_CAM_UNKNOWN  = -52) ! Unknown CAMAC module type
      parameter (ACQ_GATE_MODTYPE = -53) ! Illegal module type in gate spec.

      parameter (ETHERNET_ERROR = -127)  !
