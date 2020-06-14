C$PROG ERRCOMP   - Calculates an uncertainy array used for display
C

C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 08/18/2000
C     ******************************************************************
C
      SUBROUTINE ERRCOMP(UNCERT)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4 MXDAT
C
      PARAMETER (MXDAT=500)
C     ------------------------------------------------------------------
      COMMON/FT03/ XIN(MXDAT),YIN(MXDAT),UIN(MXDAT),NDAT
      REAL*8       XIN,       YIN,       UIN
      INTEGER*4                                     NDAT
C     ------------------------------------------------------------------
      COMMON/FT10/ KINDUU,KINDU,USUN,ALUN,UMUL
      INTEGER*4    KINDUU,KINDU
      REAL*8                    USUN,ALUN,UMUL
C     ------------------------------------------------------------------
      INTEGER*4    I
C
      REAL*4       UNCERT(*)
C
      REAL*8       DSQRT,YY
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C     KINDUU= 1 SAYS ALL UNCERT = ALUN (%)
C     KINDUU= 2 SAYS UNSPECIFIED UNCERT = USUN (%)
C     KINDU = 1 SAYS COMPUTE UNCERT AS COUNTING STATISTICS
C     KINDU = 2 SAYS UNCERT GIVEN BY "UIN" IS ABSOLUTE
C     KINDU = 3 SAYS UNCERT GIVEN BY "UIN" IS IN %
C     ------------------------------------------------------------------
C
C
C     ------------------------------------------------------------------
C     COMPUTE THE UNCERT U(I) BY REQUIRED METHOD 
C     ------------------------------------------------------------------
C
      IF(KINDUU.EQ.1) GO TO 50
C
      IF(KINDU.EQ.1)  GO TO 100
C
      GO TO 300
C
   50 DO 60 I=1,NDAT
      UNCERT(I)=0.01*ALUN*YIN(I)
   60 CONTINUE
      RETURN
C
  100 DO 110 I=1,NDAT
      YY=YIN(I)
      IF(YY.LT.1.0) YY=1.0
      UNCERT(I)=DSQRT(YY)*UMUL
  110 CONTINUE
      RETURN
C
  300 DO 320 I=1,NDAT
      IF(UIN(I).EQ.0.0) GO TO 310
      IF(KINDU.EQ.2) UNCERT(I)=UIN(I)*UMUL
      IF(KINDU.EQ.3) UNCERT(I)=0.01*UIN(I)*UMUL
      GO TO 320
  310 UNCERT(I)=0.01*USUN*YIN(I)
  320 CONTINUE
      RETURN
C
      END

