C$PROG LISLOG    - Writes message to terminal & file (maybe lemor.log)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE LISLOG(LU,OFLG)
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
      CHARACTER*4  OFLG
C
      INTEGER*4    IMES(20),LU
C
      EQUIVALENCE (IMES,MSSG)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(OFLG.EQ.'FILE') GO TO 20
C
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
   20 WRITE(LU,25)IMES
   25 FORMAT(20A4)
      CALL MESSLOG(LOGUT,0)
      RETURN
      END
