C$PROG NXBL
      FUNCTION NXBL(IBY,IA,IB)
C
      BYTE     IBY(*)
C
      BYTE     X20,X21,X3B
C
      DATA     X20,X21,X3B/'20'X,'21'X,'3B'X/
C
      SAVE
C
C     FUNCTION TO RETURN LOCATION OF NEXT BLANK
C     TERMINATES ON ; OR !
C
      DO 10 I=IA,IB
      IF(IBY(I).EQ.X20) GO TO 20
      IF(IBY(I).EQ.X3B) GO TO 15
      IF(IBY(I).EQ.X21) GO TO 15
   10 CONTINUE
   15 I=0
   20 NXBL=I
      RETURN
      END
