*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 1993
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
*    File:         /usr/users/mcsq/Dacq/cam_fast.for
*
*    Description:  Status return codes from CAMAC and FASTBUS routines
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    4/23/93    MCSQ         
******************************************************************************
      integer*4 NOX
      integer*4 NOQ
      integer*4 NOX_NOQ
      integer*4 NOQ_NOX
      integer*4 TIMEOUT
      integer*4 ILL_CRATE
      integer*4 ILL_MODULE
      integer*4 ILL_ADDRESS
      integer*4 ILL_FUNCTION
      integer*4 CRATE_OFF_LINE
*
*  All codes are possible for CAMAC I/O.  For FASTBUS I/O, only
*  TIMEOUT and ILL_ADDRESS are possible.
*
      parameter (NOQ = 1)                  ! X = 1, Q = 0
      parameter (NOX = 2)                  ! X = 0, Q = 1
      parameter (NOX_NOQ = 3)              ! X = 0, Q = 0
      parameter (NOQ_NOX = 3)              ! X = 0, Q = 0
      parameter (TIMEOUT = 128)            ! Timeout
      parameter (ILL_CRATE = 'c004'X)      !Illegal CAMAC crate number 
      parameter (ILL_MODULE = 'c005'X)     !Illegal CAMAC module number 
      parameter (ILL_ADDRESS = 'c006'X)    !Illegal CAMAC subaddress 
      parameter (ILL_FUNCTION = 'c009'X)   !Illegal CAMAC function code
      parameter (CRATE_OFF_LINE = 'a000'X) !CAMAC crate off line
