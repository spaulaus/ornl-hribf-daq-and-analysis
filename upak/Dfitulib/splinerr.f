C$PROG SPLINERR  - Displays/Logs Spline fit error messages
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE SPLINERR(IERR)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      INTEGER*4   IERR
C     ------------------------------------------------------------------
C
      IF(IERR.EQ.0) RETURN
C
      IF(IERR.EQ.1) THEN
      WRITE(CMSSG,110)
      GO TO 500
      ENDIF
C
      IF(IERR.EQ.2) THEN
      WRITE(CMSSG,120)
      GO TO 500
      ENDIF
C
      IF(IERR.EQ.3) THEN
      WRITE(CMSSG,130)
      GO TO 500
      ENDIF
C
      IF(IERR.EQ.4) THEN
      WRITE(CMSSG,140)
      GO TO 500
      ENDIF
C
      IF(IERR.GE.5) THEN
      WRITE(CMSSG,150)
      GO TO 500
      ENDIF
C
      RETURN
C
C     ------------------------------------------------------------------
  110 FORMAT('Spline error - Number of data points is less than 2')
  120 FORMAT('Spline error - S is negative')
  130 FORMAT('Spline error - EPS.LT.0 or EPS.GT.1.0')
  140 FORMAT('Spline error - X-values not strictly increasing')
  150 FORMAT('Spline error - some 2nd derivative value is non-positive')
C
  500 CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
