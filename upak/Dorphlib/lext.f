C$PROG LEXT      - Locates . first byte of filename extension
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      FUNCTION LEXT(IBY,IA,IB)
C
      BYTE IBY(*),X2E,X5D
C
      DATA X2E,X5D/Z'2E',Z'5D'/
C
      SAVE
C
C     ------------------------------------------------------------------
C     LOCATES DOT - FIRST BYTE OF FILENAME EXTENSION
C     ------------------------------------------------------------------
C
      LDOT=0
      LBRA=0
C
      DO 10 I=IA,IB
      IF(IBY(I).EQ.X2E) LDOT=I  !LOCATION OF  .
      IF(IBY(I).EQ.X5D) LBRA=I  !LOCATION OF  ]
   10 CONTINUE
C
      IF(LBRA.GT.LDOT)    LDOT=0
      LEXT=LDOT
      RETURN
      END
