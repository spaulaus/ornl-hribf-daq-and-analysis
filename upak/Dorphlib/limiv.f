C$PROG LIMIV     - Decodes ACSII integer & checks specified limits
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE LIMIV(IWD,ILO,IHI,IV,IERR)
C
      INTEGER*4 IWD(2)
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO DECODE A RIGHT-JUSTIFIED ASCII FIELD CONTAINED IN
C     "IWD" AND RETURN A FIXED-POINT VALUE IN IV
C
C     IERR = 0  SAYS, EVERYTHING OK
C     IERR = 1  SAYS, ILLEGAL SYNTAX (FACKED-UP ASCII FIELD)
C     IERR = 2  SAYS, NUMBER IS NOT IN THE RANGE ILO TO IHI
C
C     IF(IERR.NE.0) IV IS UNCHANGED
C     ------------------------------------------------------------------
C
      IERR=0
      CALL MILV(IWD,IT,XT,KIND,JERR)
      IF(KIND.NE.1) GO TO 20
      IF(JERR.NE.0) GO TO 20
      IF(IT.LT.ILO) GO TO 30
      IF(IT.GT.IHI) GO TO 30
      IV=IT
      RETURN
C
   20 IERR=1
      RETURN
C
   30 IERR=2
      RETURN
      END
