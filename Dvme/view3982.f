******************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1994-2002
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
*    Environment:  VME Acquisition for Linux
*
*    File:         /usr/users/mcsq/Dlinux/Dvme/view3982.f
*
*    Description:  Displays status and instructions loaded in the KSC3982
*                  List Sequencer Crate controller.
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    4/30/94    MCSQ         
******************************************************************************
      implicit none

      include '../Dacq/cam_fast.for'

      integer*2  ldata(8192)
      integer*4  i,stat,savadr,savstat,restore
      integer*4  LIST_SEQ_C,LIST_SEQ_N
      integer*4  lcount
      character*1 ichar
      equivalence (ldata,savadr)

      parameter (LIST_SEQ_C = 0)
      parameter (LIST_SEQ_N = 23)
*
*   First we try to read the address register.  If Q = 1, the sequencer is
*   not enabled.  If Q = 0, it is enabled for use by the acquisition system.
*   Any other status is bad news.  Just report and exit.
*
      call camacio(1,LIST_SEQ_C,LIST_SEQ_N,2,0,savadr,1,stat)
      if (stat .eq. 0) then
         restore = 0
         write(*,'KSC3982 List sequencer is not enabled')
      else if (stat .eq. NOQ) then
         restore = 1
         write(*,'KSC3982 List sequencer is enabled')
      else
         go to 10
      endif
*
*   Get sequencer status and report to user.
*
      call camacio(1,LIST_SEQ_C,LIST_SEQ_N,12,1,savstat,1,stat)
      if (stat .ne. 0) go to 10
      write(6,9000)savstat,savadr
9000  format(' KSC3982 status = ',z4,'h at address = ',z4,'h')
      lcount = 22
      if (iand(savstat,'20'x) .ne. 0) then
        write(6,9010)savadr-1
9010    format(' NO-X Exception at address = ',z4)
        lcount = lcount - 1
      endif
      if (iand(savstat,'40'x) .ne. 0) then
        write(*,'Timer Exception')
        lcount = lcount - 1
      endif
      if (iand(savstat,'80'x) .ne. 0) then
        write(*,'Write Data FIFO Exception')
        lcount = lcount - 1
      endif
      if (iand(savstat,'100'x) .ne. 0) then
        write(*,'Read Data FIFO Exception')
        lcount = lcount - 1
      endif
      if (iand(savstat,'200'x) .ne. 0) then
        write(*,'External LAM Trigger')
        lcount = lcount - 1
      endif
*
*  Now stop the sequencer( if enabled ).  Read the instruction data,
*  restore the address register, and restore the enabled state to that
*  on entry.
*
      call camacio(0,LIST_SEQ_C,LIST_SEQ_N,0,24,0,0,stat)
      if (stat .ne. 0) go to 10
      call camacio(0,LIST_SEQ_C,LIST_SEQ_N,2,16,0,1,stat)
      if (stat .ne. 0) go to 10
      call camacio(0,LIST_SEQ_C,LIST_SEQ_N,1,0,savadr,8192,stat)
      if (stat .ne. 0) go to 10
      call camacio(1,LIST_SEQ_C,LIST_SEQ_N,2,16,savadr,1,stat)
      if (stat .ne. 0) go to 10
      if (restore .eq. 1) then
         call camacio(0,LIST_SEQ_C,LIST_SEQ_N,0,26,0,0,stat)
         if (stat .ne. 0) go to 10
      else
      endif
*
*  Display the instruction data.  Shown in hex and naf format.
*
      write(6,9020)
9020  format(/,'Addr  Instr      Decoded Instr')
      lcount = lcount - 2
      do i = 1,8192
        call display(i-1,ldata(i))
        if (ldata(i) .lt. 0) go to 10
        lcount = lcount - 1
        if (lcount .le. 0) then
          write(6,9030)
9030      format('-- MORE --  type CR to cotinue or q to quit. ? ',$)
          read(5,1000)ichar
1000      format(a1)
          if (ichar .eq. 'q') go to 10
          lcount = 24
        endif
      enddo
10    continue

      end
*******************************************************************************
*  Routine to disassemble the sequencer instruction data word and display
*  on the standard output.
*******************************************************************************
      subroutine  display(addr,data)
*
      implicit none
      integer*2 data
      integer*4 addr,cmd,n,a,f,q,halt
      integer*4 mindex,ishift
      character*32 mess(3) /'Halt','Q Ignore','Q Wait'/

      cmd = data
      halt = 0
      if (cmd .lt. 0) halt = 1
      q = 0
      if (iand(cmd,'4000'x) .ne. 0) q = 1
      f = iand(cmd,'1f'x)
      cmd = ishft(cmd,-5)
      a = iand(cmd,'f'x)
      cmd = ishft(cmd,-4)
      n = iand(cmd,'1f'x)
      if (halt .ne. 0) then
        mindex = 1
      else if (q .eq. 0) then
        mindex = 2
      else
        mindex = 3
      endif
      write(6,9000)addr,data,n,a,f,mess(mindex)
9000  format(z4.4,':  ',z4,'   naf =',3i3,4x,a10)
      end
