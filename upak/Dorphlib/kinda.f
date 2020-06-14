C$PROG KINDA     - Function to determine byte type
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      INTEGER*4 FUNCTION KINDA(IWD)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4  IWD,KIND
C
      INTEGER*4  X30,X39,X2B,X2D,X2E,X20,X2C,X3D,X2F,X28,X29
C
      DATA       X30/Z'30'/  ! digit 0
      DATA       X39/Z'39'/  ! digit 9
      DATA       X2B/Z'2B'/  ! +
      DATA       X2D/Z'2D'/  ! -
      DATA       X2E/Z'2E'/  ! .
      DATA       X20/Z'20'/  ! blank
      DATA       X2C/Z'2C'/  ! ,
      DATA       X3D/Z'3D'/  ! =
      DATA       X2F/Z'2F'/  ! /
      DATA       X28/Z'28'/  ! (
      DATA       X29/Z'29'/  ! )
C
      SAVE
C
C     ------------------------------------------------------------------
C     KINDA=1 SAYS NUMERIC (DIGITS 0 THRU 9 + - . )
C     KINDA=2 SAYS NON-NUMERIC BUT NOT A DELIMITER
C     KINDA=3 SAYS A DELIMITER (BLANK , = / ( )  )
C     ------------------------------------------------------------------
C
      IF(IWD.GE.X30.AND.IWD.LE.X39) GO TO 10
C
      IF(IWD.EQ.X2B.OR.IWD.EQ.X2D.OR.IWD.EQ.X2E) GO TO 10
C
      IF(IWD.EQ.X20.OR.IWD.EQ.X2C.OR.IWD.EQ.X3D) GO TO 20
C
      IF(IWD.EQ.X2F.OR.IWD.EQ.X28.OR.IWD.EQ.X29) GO TO 20
C
      KIND=2
C
      GO TO 30
C
   10 KIND=1
C
      GO TO 30
C
   20 KIND=3
C
   30 KINDA=KIND
C
      RETURN
      END
