C$PROG KINOP
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/24/2002
C     ******************************************************************
C
      FUNCTION KINOP(IT)
C
      INTEGER*4  X2B,X2D,X2F,X2A
C
      DATA       X2B,X2D,X2F,X2A/'2B'X,'2D'X,'2F'X,'2A'X/
C
      SAVE
C
C     ******************************************************************
C     RETURNS OPERATOR-TYPE FOR BYTE CONTAINED IN - IT
C     ******************************************************************
C
      IV=0
C
      IF(IT.EQ.X2B) IV=1              !TST FOR +
      IF(IT.EQ.X2D) IV=2              !TST FOR -
      IF(IT.EQ.X2F) IV=3              !TST FOR /
      IF(IT.EQ.X2A) IV=4              !TST FOR *
C
      KINOP=IV
      RETURN
      END
