C$PROG KINDA
      INTEGER*4 FUNCTION KINDA(IWD)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4  IWD,KIND
C
      INTEGER*4  X30,X39,X2B,X2D,X2E,X20,X2C,X3D,X2F,X28,X29
C
      DATA       X30/'30'X/  ! digit 0
      DATA       X39/'39'X/  ! digit 9
      DATA       X2B/'2B'X/  ! +
      DATA       X2D/'2D'X/  ! -
      DATA       X2E/'2E'X/  ! .
      DATA       X20/'20'X/  ! blank
      DATA       X2C/'2C'X/  ! ,
      DATA       X3D/'3D'X/  ! =
      DATA       X2F/'2F'X/  ! /
      DATA       X28/'28'X/  ! (
      DATA       X29/'29'X/  ! )
C
C     ------------------------------------------------------------------
C     FUNCTION TO DETERMINE BYTE "TYPE"
C
C     KINDA=1 SAYS NUMERIC (DIGITS 0 THRU 9 + - . )
C     KINDA=2 SAYS NON-NUMERIC BUT NOT A DELIMITER
C     KINDA=3 SAYS A DELIMITER (BLANK , = / ( )  )
C     ------------------------------------------------------------------
C
      IF(IWD.GE.X30.AND.IWD.LE.X39)              GO TO 10
      IF(IWD.EQ.X2B.OR. IWD.EQ.X2D)              GO TO 10
C
      IF(IWD.EQ.X20.OR.IWD.EQ.X2C.OR.IWD.EQ.X3D) GO TO 20
      IF(IWD.EQ.X2F.OR.IWD.EQ.X28.OR.IWD.EQ.X29) GO TO 20
      IF(IWD.EQ.X2E)                             GO TO 20
C
      KIND=2
      GO TO 30
   10 KIND=1
      GO TO 30
   20 KIND=3
   30 KINDA=KIND
      RETURN
      END
