C$PROG LIMXV     - Decodes ASCII REAL*4  - checks specified limits
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE LIMXV(IWD,XLO,XHI,XV,IERR)
C
      INTEGER*4 IWD(2)
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO DECODE A RIGHT-JUSTIFIED ASCII FIELD CONTAINED IN
C     "IWD" AND RETURN A FLOATING-POINT VALUE IN XV
C
C     IERR = 0  SAYS, EVERYTHING OK
C     IERR = 1  SAYS, ILLEGAL SYNTAX (FACKED-UP ASCII FIELD)
C     IERR = 2  SAYS, NUMBER IS NOT IN THE RANGE XLO TO XHI
C
C     IF(IERR.NE.0) XV IS UNCHANGED
C     ------------------------------------------------------------------
C
      IERR=0
      CALL MILV(IWD,IT,XT,KIND,JERR)
      IF(JERR.NE.0) GO TO 20
      IF(XT.LT.XLO) GO TO 30
      IF(XT.GT.XHI) GO TO 30
      XV=XT
      RETURN
C
   20 IERR=1
      RETURN
C
   30 IERR=2
      RETURN
      END
