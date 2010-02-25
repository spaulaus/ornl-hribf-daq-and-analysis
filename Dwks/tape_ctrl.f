******************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                            Copyright(C) 1996
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
*    Environment:  Data Acquisition System
*
*    File:         /usr/users/mcsq/Drms/alpha.f
*
*    Description:  
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*   12/ 4/96    MCSQ
*
*****************************************************************************/

      implicit none

      character*80 cmd,tmp
      character*60 errmsg
      integer*4 i,ierr,strlen
      integer*4 indx(2,10),itype(10),nf,inext

10    type 9000
9000  format('tape_ctrl> ',$)
      accept 1000,cmd
1000  format(a)
      call strparse(cmd,indx,itype,nf,inext)
      if (cmd(1:1) .eq. ' ') go to 10
      if (cmd .eq. 'END' .or. cmd .eq. 'end') then
        call exit(0)
      elseif (cmd .eq. 'EXIT' .or. cmd .eq. 'exit') then
        call exit(0)
      endif
      call acq_log("MCSQ",cmd,ierr)
      if (ierr .ne. 0) then
        call acq_log_error(ierr,errmsg)
        type 9010,errmsg(1:strlen(errmsg))
      endif
      call acq_tape_ctrl(cmd,ierr)
      if (ierr .ne. 0) then
        call acq_tape_error(ierr,errmsg)
        type 9010,errmsg(1:strlen(errmsg))
9010    format('*** ',a,' ***')
      endif
      go to 10
*
      end
