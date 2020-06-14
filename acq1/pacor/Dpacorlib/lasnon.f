C$PROG LASNON
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/18/92
C     ************************************************************
C
      FUNCTION LASNON(IBY,IA,IB)
C
      BYTE IBY(1)
C
      BYTE X20,X3B,X21
      DATA X20,X3B,X21/'20'X,'3B'X,'21'X/
C
      SAVE
C
C     ************************************************************
C     FUNCTION TO RETURN LOCATION OF LAST NON-BLANK UP TO ; OR !
C     ************************************************************
C
      LN=0
      DO 10 I=IA,IB
      IF(IBY(I).EQ.X21) GO TO 20
      IF(IBY(I).EQ.X3B) GO TO 20
      IF(IBY(I).EQ.X20) GO TO 10
      LN=I
   10 CONTINUE
C
   20 LASNON=LN
      RETURN
      END
