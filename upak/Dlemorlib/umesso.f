C$PROG UMESSO    - Displays/logs user-generated messages
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE UMESSO(ILOGF,MESBUF)
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
      INTEGER*4    ILOGF,MESBUF(13),I
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C     ILOGF=1 SAYS SEND TO CONSOLE ONLY
C     ILOGF=2 SAYS PRINT ONLY
C     ILOGF=3 SAYS SEND TO CONSOLE AND PRINT
C     ------------------------------------------------------------------
C
      CMSSG=' '
C
      WRITE(CMSSG,20)MESBUF
   20 FORMAT(13A4)
C
      IF(ILOGF.EQ.1) GO TO 30
      IF(ILOGF.EQ.2) GO TO 40
      IF(ILOGF.EQ.3) GO TO 50
      RETURN
C
   30 CALL MESSLOG(LOGUT,0)
      RETURN
   40 CALL MESSLOG(0,LOGUP)
      RETURN
   50 CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
