C$PROG NXDG
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/18/92
C     ************************************************************
C
      FUNCTION NXDG(IBY,IA,IB)
C
      BYTE     IBY(*)
C
      BYTE     X30,X39
C
      DATA     X30,X39/'30'X,'39'X/
C
      SAVE
C
C     ************************************************************
C     RETURNS LOCATION OF FIRST "DIGIT" IN IBY BETWEEN IA & IB
C     RETURNS 0, IF NONE FOUND
C     ************************************************************
C
      DO 10 I=IA,IB
      IF(IBY(I).GE.X30.AND.IBY(I).LE.X39) GO TO 20
   10 CONTINUE
      NXDG=0
      RETURN
C
   20 NXDG=I
      RETURN
      END
