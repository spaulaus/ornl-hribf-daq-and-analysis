C$PROG KASUP4
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/18/92
C     ************************************************************
C
      SUBROUTINE KASUP4(IBY)
C
      BYTE IBY(4)
C
      BYTE  X20,X61,X7A
      DATA  X20,X61,X7A/'20'X,'61'X,'7A'X/
C
      SAVE
C
C     ************************************************************
C     CONVERT 4-BYTES TO UPPER-CASE
C     ************************************************************
C
      DO 10 I=1,4
      IT=IBY(I)
      IF(IT.LT.X61.OR.IT.GT.X7A) GO TO 10
      IBY(I)=IT-X20
   10 CONTINUE
      RETURN
      END
