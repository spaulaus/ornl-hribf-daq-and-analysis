C$PROG FOX       - Fit function for non-linear routine SMIN
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
C
      REAL*8 FUNCTION FOX(X)
C
      IMPLICIT NONE
C
      INTEGER*4    MXDAT
C
      PARAMETER   (MXDAT=500)
C
C     ------------------------------------------------------------------
      COMMON/FT03/ XIN(MXDAT),YIN(MXDAT),UIN(MXDAT),NDAT
      REAL*8       XIN,       YIN,       UIN
      INTEGER*4                                     NDAT
C     ------------------------------------------------------------------
      COMMON/FT06/ F(64)
      REAL*8       F
C     ------------------------------------------------------------------
      COMMON/FT07/ YCAL(MXDAT),KFOX
C
      REAL*8       YCAL
C
      INTEGER*4                KFOX
C     ------------------------------------------------------------------
      COMMON/FT09/ SKIPF(MXDAT),TCHISQ
      CHARACTER*4  SKIPF
      REAL*8                    TCHISQ
C     ------------------------------------------------------------------
      COMMON/FT11/ WGT(MXDAT)
      REAL*8       WGT
C     ------------------------------------------------------------------
      COMMON/FTUSER/ NONEG(64),NV,IFUNK,UTIT
      INTEGER*4      NONEG,    NV,IFUNK
      CHARACTER*40                      UTIT
C     ------------------------------------------------------------------
      REAL*8       X(*),SUM,ADD
C
      REAL*8       USERFOX,A(64)
C
      INTEGER*4    I
C     ------------------------------------------------------------------
C
      DO 10 I=1,NV
      A(I)=X(I)*F(I)
   10 CONTINUE
C
      DO 100 I=1,NDAT
C
      YCAL(I)=USERFOX(A,XIN(I))
C
  100 CONTINUE
C
      SUM=0.0
C
      TCHISQ=0.0
C
      DO 200 I=1,NDAT
C
      ADD=WGT(I)*(YCAL(I)-YIN(I))**2
C
      TCHISQ=TCHISQ+ADD
C
      IF(SKIPF(I).EQ.'YES ') GO TO 200
C
      SUM=SUM+ADD
C
  200 CONTINUE
C
      FOX=SUM
C
      RETURN
C
      END

