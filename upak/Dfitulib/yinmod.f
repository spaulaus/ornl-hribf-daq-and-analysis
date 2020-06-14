C$PROG YINMOD    - Generates statistical spread in YIN array
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
C
      SUBROUTINE YINMOD(MODE)
C
      IMPLICIT NONE 
C
      INTEGER*4  MXDAT
C
      PARAMETER (MXDAT=500)
C
C     ------------------------------------------------------------------
      COMMON/FT03/ XIN(MXDAT),YIN(MXDAT),UIN(MXDAT),NDAT
      REAL*8       XIN,       YIN,       UIN
      INTEGER*4                                     NDAT
C     ------------------------------------------------------------------
      COMMON/FT15/ YSAV(MXDAT),ABSUN(MXDAT)
      REAL*8       YSAV
      REAL*4                   ABSUN
C     ------------------------------------------------------------------
      CHARACTER*4  MODE
C
      REAL*4       GRAN12,YADD
C
      INTEGER*4    ISEED,I
C
      DATA         ISEED/-1/
C     ------------------------------------------------------------------
C
      IF(MODE.EQ.'SAVE') GO TO 100
C
      IF(MODE.EQ.'REST') GO TO 200
C
      IF(MODE.EQ.'MODI') GO TO 300
C
      RETURN
C
  100 CALL ERRCOMP(ABSUN)
C
      DO 110 I=1,NDAT
      YSAV(I)=YIN(I)
  110 CONTINUE
      RETURN
C
  200 DO 210 I=1,NDAT
      YIN(I)=YSAV(I)
  210 CONTINUE
      RETURN
C
  300 DO 310 I=1,NDAT
      YADD=GRAN12(ISEED)*ABSUN(I)/2.12
      YIN(I)=YSAV(I)+YADD
  310 CONTINUE
      RETURN
C
      END
C
