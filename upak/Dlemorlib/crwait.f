C$PROG CRWAIT    - Waits for [RETURN] to be typed
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE CRWAIT
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      INTEGER*4    IT
C
      SAVE
C
C     ------------------------------------------------------------------
C
      READ(5,10)IT
   10 FORMAT(A4)
C
      WRITE(CMSSG,20)
   20 FORMAT('[RETURN] REQUEST RECEIVED - PROCESS CONTINUING')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
