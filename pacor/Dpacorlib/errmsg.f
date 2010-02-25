C$PROG ERRMSG
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 04/30/90
C     ************************************************************
C
      SUBROUTINE ERRMSG(LUA,LUB)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C
      INTEGER*4 MSS15(15),MSS20(20)
C
      EQUIVALENCE (MSS15(1),MSSG(1)),(MSS20(1),MSSG(1))
C
      SAVE
C     ------------------------------------------------------------------
C
      IF(LUA.LE.0) GO TO 20
C
      WRITE(LUA,10)MSS15
   10 FORMAT(1H ,15A4)
C
   20 IF(LUB.LE.0.OR.LISFLG.NE.'LON ') GO TO 100
C
      WRITE(LUB,30)MSS20
   30 FORMAT(1H ,20A4)
C
  100 CMSSG=' '
C
      NERR=NERR+1
C
      RETURN
      END
