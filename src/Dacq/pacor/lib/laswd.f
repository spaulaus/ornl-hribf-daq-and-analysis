C$PROG LASWD
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/18/92
C     ************************************************************
C
      FUNCTION LASWD(IBY,IA,IB)
C
      BYTE    IBY(*)
C
      BYTE    X20
      DATA    X20/'20'X/
C
      SAVE
C
C     ************************************************************
C     LOCATES BEGINNING OF LAST NON-BLANK FIELD BETWEEN IA & IB 
C     ************************************************************
C
      LASWD=0
      JB=LASNON(IBY,IA,IB)
      IX=JB
      IF(IX.LE.0)  RETURN
   50 IF(IX.LT.IA) RETURN
      IF(IBY(IX).EQ.X20) GO TO 100
      IX=IX-1
      GO TO 50
C
  100 IX=IX+1
      DO 110 I=IX,JB
      IBY(I)=X20
  110 CONTINUE
      LASWD=IX
      RETURN
      END
