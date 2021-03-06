******************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                             Copyright(C) 2001
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
*    File:         /usr/users/mcsq/Drms/caenw.f
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
      integer*4 dat(32),range,mode,adcnum
      character*80 errmsg

      do i=1,32
       dat(i) = 3
      enddo
      write(6,1000)
***1000  format('TDC number ? ',$)
1000  format('ADC number ? ',$)
      read(5,1010) adcnum
1010  format(i2)
      range = 30
      mode = 0
****      call caen_tdc_write(adcnum,dat,range,mode,ierr)
      call caen_adc_write(adcnum,dat,ierr)
      if (ierr .ne. 0) then
         call caen_error(ierr,errmsg)
         write(6,9010) errmsg(1:strlen(errmsg))
9010     format('*** ',a,' ***')
      endif
*
      end
