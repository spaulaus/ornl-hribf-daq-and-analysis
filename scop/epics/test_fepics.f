      program test_fepics
      implicit none
c
c   Test the fepics routines to interface to John Sinclair
c
      real*4  pvs(10)
      data pvs/0.,1.,2.,3.,4.,5.,6.,7.,8.,9./
      integer errnum
      character*128 errmsg
      integer i,j

      errnum=0
      errmsg=''
c     initialize the connection.  
      write(*,*) 'Begin by opening the socket'
      call fopen_epics(10,'rms ',errnum, errmsg)
      if (errnum .ne. 0) then
         write(*,*) 'There was an error opening EPICS'
         write(*,*) 'Errnum=', errnum, ' msg=',errmsg
         write(*,*) 'Stopping the program'
         stop
      endif

c     write data for a while
      errnum=0
      errmsg=''
      write(*,*) 'Continue by writing  to the PV''s'
      do i=1,10
         call sleep(1)
         do j=1,10
            pvs(j)=pvs(j)+10
         enddo
         write(*,*) 'New Values:',pvs
         call fwrite_epics(10, pvs, errnum, errmsg)
         if (errnum .ne. 0) then
            write(*,*) 'There was an error writing EPICS'
            write(*,*) 'Errnum=', errnum, ' msg=',errmsg
            write(*,*) 'Continuing the program'
            stop
         endif
      enddo

c     End the connection
      errnum=0
      errmsg=''
      write(*,*) 'Finish by closing the socket'
      call fclose_epics(errnum, errmsg)
      if (errnum .ne. 0) then
         write(*,*) 'There was an error closing EPICS'
         write(*,*) 'Errnum=', errnum, ' msg=',errmsg
         write(*,*) 'Stopping the program'
         stop
      endif

      stop
      end
