C$PROG WTCOMP    - Calculates weight values for non-linear fit
C

C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 08/16/2000
C     ******************************************************************
C
      SUBROUTINE WTCOMP
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
      COMMON/FT11/ WGT(MXDAT)
      REAL*8       WGT
C     ------------------------------------------------------------------
      INTEGER*4    I
C
      REAL*4       ABSUN(MXDAT)
C     ------------------------------------------------------------------
C
      CALL ERRCOMP(ABSUN)
C
      DO 10 I=1,NDAT
C
      WGT(I)=1.0/(ABSUN(I)*ABSUN(I))
C
   10 CONTINUE
C
      RETURN
C
      END
