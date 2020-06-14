C$PROG MESSLOG
*
*   Special version of MESSLOG.F for sending info to acqusition log
*   
*  Revisions:
*               W.T. Milner     Orignial
*
*     8/12/92   R.L. Varner     Add message queue output for acquisition
*                               system.
*
*    12/ 1/92   MCSQ            In the beginning output was to the acquisition
*                               logger and also to the lemo log file.  Now
*                               output is only to the acquisition logger as
*                               long as function fsndmsg returns ierr zero.
*                               Only if ierr is nonzero we do output to the
*                               log file.
*
*    11/14/93   MCSQ            Eliminated log file.  Message is sent to
*                               acquisition log queue only.
*
*     2/ 8/94   MCSQ            Change so that codes don't have to explicitly
*                               open_acq_ipc_ to use the acquisition message
*                               log.  
*
      SUBROUTINE MESSLOG(LUA,LUB)
C
      implicit none
C
      integer*4 lua,lub
      INTEGER*4 MSS20(20),MSS26(26)
      integer*4 mssg,namprog,logut,logup
      character*4 lisflg,msgf
      character*112 cmssg
      equivalence (mssg,cmssg)
C
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      EQUIVALENCE (MSS20(1),MSSG(1)),(MSS26(1),MSSG(1))
C
      integer*4  mstype /1/      !Message type 1, Inform
      integer*4  i,ierr,ntry/0/
      character*12 acqname
C
      IF (LUA.GT.0) THEN 
         WRITE(LUA,10)MSS20
   10    FORMAT(1H ,19A4,A3)
      ENDIF
C
      IF ((LUB.GT.0) .AND. (LISFLG.EQ.'LON ')) THEN
C
          if (ntry .eq. 0) then
            call log_msg(mstype, namprog, mss26, 104, ierr)
            if (ntry .eq. 0 .and. ierr .ne. 0) then
              call getenv("VME",acqname)
              if (acqname(1:1) .eq. " ") acqname = "vme"
              call open_acq_log(acqname,ierr)
              call log_msg(mstype, namprog, mss26, 104, ierr)
              if (ierr .ne. 0) ntry = 1
            endif
          endif
      ENDIF
C
  100 DO 110 I=1,28
         MSSG(I)='20202020'X
  110 CONTINUE
C
      RETURN
      END
