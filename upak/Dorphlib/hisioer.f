C$PROG HISIOER   - Displays HISIO error messages
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/15/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE HISIOER(IERR,MSER)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      INTEGER*4 MSER(10)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(IERR.EQ.0) RETURN
C
      WRITE(CMSSG,10)MSER
   10 FORMAT(10A4)
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
C$PROG SHMDATA   - SHM BLOCK DATA
C
      BLOCKDATA SHMDATA
C
C     ------------------------------------------------------------------
C     Initialize the shared memory common blocks
C     ------------------------------------------------------------------
C
      LOGICAL SHMFLG      !Flags to indicate that shared memory is
C                         !active on this channel
C
      LOGICAL SHMUSE      !Flag to show shared memory in use
C
      INTEGER SHMID       !ID returned by system to creator of shared 
C                         !memory
C
C     ------------------------------------------------------------------
      COMMON /SharedMem/ SHMID(20), SHMFLG(20), SHMUSE
C     ------------------------------------------------------------------
C
      DATA SHMUSE /.FALSE./
      DATA SHMFLG /20*.FALSE./
      DATA SHMID  /20*0/
C
      END
C
