C$PROG PATMSK
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE PATMSK(NAME,IDX,LWN,MSK)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      COMMON/PAC9/ NAMP(3,9),LATW(200,9),BITN(200,9),NPB(9),NPAT
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4 NAME(3)
C
      SAVE
C
C     ************************************************************
C     GIVEN PAT-NAME "NAME" AND PAT-INDEX "IDX"
C     CONSTRUCT LATCH-WORD-NUMBER "LWN" AND MASK "MSK"
C     ************************************************************
C
      LWN=1
      MSK=0
C
      DO 20 K=1,NPAT
      DO 10 I=1,3
      IF(NAME(I).NE.NAMP(I,K)) GO TO 20
   10 CONTINUE
      GO TO 50
   20 CONTINUE
      GO TO 100
C
   50 IF(IDX.GT.NPB(K)) GO TO 110
C
      LWN=LATW(IDX,K)
      MSK=0
      KBIT=BITN(IDX,K)-1
      MSK=IBSET(MSK,KBIT)
      RETURN
C
  100 WRITE(CMSSG,105)NAME
  105 FORMAT('BIT PATTERN NAME NOT DEFINED = ',3A4)
      GO TO 200
  110 WRITE(CMSSG,115)NAME,IDX
  115 FORMAT('FOR BIT PATTERN - ',3A4,' UNDEFINED INDEX =',I5)
C
  200 CALL ERRLOG(LOGUT,LOGUP)
      RETURN 
      END
