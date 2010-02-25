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
*    File:         /usr/users/mcsq/Dlinux/Dvme/caenr.f
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
      integer*4 dat(32),func,scanum
      character*80 errmsg

10    continue
      do i=1,32
       dat(i) = -1
      enddo
      write(6,1000)
1000  format('SCA number ? ',$)
      read(5,1010) scanum
1010  format(i2)
      write(6,1005)
1005  format('Func code ? ',$)
      read(5,1010) func
      call sis_sca_ctl(scanum,func,dat,ierr)
      if (ierr .ne. 0) then
         call sis_error(ierr,errmsg)
         write(6,9010)errmsg(1:strlen(errmsg))
9010     format('*** ',a,' ***')
         call exit(1)
      endif
      if (func .eq. 0) then
        do i=1,32
          dat(i) = iand(dat(i),'7fffffff'x)
        enddo
        write(6,9020) dat
9020    format(4i12)
      endif
      go to 10
*
      end
