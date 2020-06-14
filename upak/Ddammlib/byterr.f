C$PROG BYTERR    - Function to check for legal-first byte in filename
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 08/20/2005 - for gnu
C     ******************************************************************
C
      INTEGER*4 FUNCTION BYTERR(IBY)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      BYTE       IBY
C
      BYTE       X30,X39,X41,X5A,X61,X7A
C
      DATA       X30/Z'30'/  ! digit 0
      DATA       X39/Z'39'/  ! digit 9
      DATA       X41/Z'41'/  ! A
      DATA       X5A/Z'5A'/  ! Z
      DATA       X61/Z'61'/  ! a
      DATA       X7A/Z'7A'/  ! z
C
      SAVE
C
C     ------------------------------------------------------------------
C     BYTERR=0  says byte is     alpha-numeric
C     BYTERR=1  says byte is NOT alpha-numeric
C     ------------------------------------------------------------------
C
      IF(IBY.GE.X30.AND.IBY.LE.X39) GO TO 10
C
      IF(IBY.GE.X41.AND.IBY.LE.X5A) GO TO 10
C
      IF(IBY.GE.X61.AND.IBY.LE.X7A) GO TO 10
C
      BYTERR=1
C
      RETURN
C
   10 BYTERR=0
C
      RETURN
      END
