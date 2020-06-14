C$PROG MESSLOG1
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE MESSLOG1(LUA,LUB)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      INTEGER*4 MSS20(20),MSS(20)
C
      CHARACTER*80 CMSS
C
      EQUIVALENCE (MSS20,MSSG),(CMSS,MSS)
C     ------------------------------------------------------------------
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(LUA.LE.0) GO TO 35
C
      WRITE(CMSS,10)MSS20
   10 FORMAT(19A4,A3)
C
      MSS(20)=0
C
      CALL WRITOVER(MSS)
C
   35 CMSSG=' '
      RETURN
      END
