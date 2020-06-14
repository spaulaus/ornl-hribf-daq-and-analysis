C$PROG CCAST     - Sets MSGF & renables CTRL/C 
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/13/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE CCAST(IDUM)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      SAVE
C
C     ------------------------------------------------------------------
C
      MSGF='XXXX'                           !SET MSG-FLAG
C
      CALL CTCNIT                           !ENABLE CTRL/C AGAIN
C
      CALL STOP_RTIMER
C
      RETURN
      END

