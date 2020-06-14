*
      SUBROUTINE ACQLOG(prog,message)
C
      implicit none
C
      character*(*) prog,message
c
      integer*4  mstype /1/      !Message type 1, Inform
      integer*4  i,ierr,ntry/0/
      character*12 acqname
      character*8 namprog
C
      namprog = prog
c
      i = len(message)
      if (i .gt. 104) i = 104
      if (ntry .eq. 0) then
         call log_msg(mstype, namprog, message, i, ierr)
         if (ntry .eq. 0 .and. ierr .ne. 0) then
           call getenv("VME",acqname)
           if (acqname(1:1) .eq. " ") acqname = "vme"
           call open_acq_log(acqname,ierr)
           call log_msg(mstype, namprog, message, i, ierr)
           if (ierr .ne. 0) ntry = 1
         endif
      endif
C
      message = ' '
C
      RETURN
      END
