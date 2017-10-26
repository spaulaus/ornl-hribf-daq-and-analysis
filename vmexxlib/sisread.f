******************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 2002
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
*    Environment: 
*
*    File:         vmexxlib/sisread.f
*
*    Description: 
*
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    8/16/01    MCSQ
*
*****************************************************************************/

      implicit none

      integer*4 i,ierr,strlen
      integer*4 dat(64),func,scanum
      character*80 errmsg

*     Read the scaler number only once at the launch
      write(6,1000)
1000  format('SCA number ? ',$)
      read(5,1010) scanum
1010  format(i2)

*    Help for the helpless
      write(6,1004) "0 - read", "1 - clear", "2 - enable", 
     .              "3 - disable counting", "4 - enable 25MHz test",
     .              "5 - disable 25MHz test", "6 - read CSR",
     .              "7 - read module ID and firmware",
     .              "8 - read Op mode register"
1004  format (1x, A, /)
*     Loop until ^C
10    continue

*     Clear the data receiving array
      do i=1,64
       dat(i) = -1
      enddo

*     Request the function code
      write(6,1005)
1005  format('Func code ? ',$)
      read(5,1010) func

*     Execute the function
      call sis_sca_ctl(scanum,func,dat,ierr)
      if (ierr .ne. 0) then
         call sis_error(ierr,errmsg)
         write(6,9010)errmsg(1:strlen(errmsg))
9010     format('*** ',a,' ***')
         call exit(1)
      endif

*     Display results

*     Display the channel readout
      if (func .eq. 0) then
*       Hex
        write(6,9020) dat
9020    format(4z12)
*       Decimal
        write(6,90201) dat
90201   format(4i12)

*     Display the CSR
      else if (func .eq. 6) then
        write(6,9021) dat(1)
9021    format(1X, "CSR = ", z12)
*     Display the module id and firmware revision
      else if (func .eq. 7) then
        write(6,9022) dat(1)
9022    format(1X, "ModuleID = ", z12)
*     Display the op mode register
      else if (func .eq. 8) then
        write(6,90221) dat(1)
90221   format(1X, "OpMode = ", z12)
      endif
      go to 10
*
      end
