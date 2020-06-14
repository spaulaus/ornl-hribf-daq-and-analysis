C$PROG TABLOG    - Table logger for FITU 
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
C
      SUBROUTINE TABLOG(LUA,LUB)
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
      INTEGER*4    LUA,LUB,N,I,NTRY
C
      INTEGER*4    DATIM(5),MSS20(20)
C
      CHARACTER*4  CMSS(28)
C
      EQUIVALENCE  (MSS20,MSSG),(CMSS,MSSG)
C     ------------------------------------------------------------------
C
      IF(LUA.LE.0) GO TO 20
C
      WRITE(LUA,10)MSS20
   10 FORMAT(1H ,19A4,A3)
C
   20 IF(LUB.LE.0.OR.LISFLG.NE.'LON ') GO TO 100
C
      N=1
      DO 40 I=1,26
      IF(CMSS(I).NE.'    ') N=I
   40 CONTINUE
C
crlvg95     CALL FSEEK(LUB,0,2)
C
   50 WRITE(LUB,55)(MSSG(I),I=1,N)
   55 FORMAT(26A4)
C
  100 CMSSG=' '
C
      RETURN
      END
