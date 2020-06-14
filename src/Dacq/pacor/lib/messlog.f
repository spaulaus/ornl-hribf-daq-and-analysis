C$PROG MESSLOG
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 04/30/90
C     ************************************************************
C
      SUBROUTINE MESSLOG(LUA,LUB)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      INTEGER*4 MSS20(20),MSS26(26)
C
      EQUIVALENCE (MSS20(1),MSSG(1)),(MSS26(1),MSSG(1))
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(LUA.LE.0) GO TO 20
C
      WRITE(LUA,10)MSS20
   10 FORMAT(1H ,19A4,A3)
C
   20 IF(LUB.LE.0.OR.LISFLG.NE.'LON ') GO TO 100
C
      WRITE(LUB,55)MSS26
   55 FORMAT(1H ,26A4)
C
  100 CMSSG=' '
C
      RETURN
      END
