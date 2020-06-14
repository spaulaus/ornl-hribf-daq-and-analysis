C$PROG VMETCHEKA - Checks "event list" for VME-time within limits
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 11/15/2004
C     ******************************************************************
C
      SUBROUTINE VMETCHEKA(IDLST,DALST,NPAR,STAT)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SC28/ CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM,VMETIMC,CLONOF
      INTEGER*4    CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM
      CHARACTER*16                                    VMETIMC
      CHARACTER*4                                             CLONOF
C     ------------------------------------------------------------------
C
      INTEGER*4    IDLST(*),DALST(*),NPAR,STAT,THI,TLO,TIM,I
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      STAT=3
C
      IF(CLIDLO.LE.0) RETURN
C
      IF(VMETHI.LE.0) RETURN
C
      THI=-1
      TLO=-1
C
      DO 10 I=1,NPAR
      IF(IDLST(I).EQ.CLIDHI) THI=DALST(I)
      IF(IDLST(I).EQ.CLIDLO) TLO=DALST(I)
   10 CONTINUE
C
      IF(THI.GE.0.AND.TLO.GE.0) GO TO 50
C
      STAT=2
C
      RETURN
C
   50 TIM=65536*THI+TLO
C
      STAT=0
C
      IF(TIM.LT.VMETLO) STAT=-1
      IF(TIM.GT.VMETHI) STAT= 1
C
      RETURN
C
      END

