C$PROG NXLABL
      FUNCTION NXLABL(IDUM)
C
      CHARACTER*4  CNUM
      INTEGER*4    NUM
      EQUIVALENCE (CNUM,NUM)
C
      DATA LABN/0/
C
      INTEGER*4  X20
      DATA       X20/z'20'/
C
      SAVE
C
C     **************************************************************
C     RETURNS A SEQUENCE OF 4-BYTE LABELS OF THE FORM: @ZZZ
C     **************************************************************
C
      LABN=LABN+1
      WRITE(CNUM,10)LABN
   10 FORMAT(Z4)
      CALL ISBYTE(z'40',NUM,0)
      DO 20 I=1,2
      CALL ILBYTE(IT,NUM,I)
      IF(IT.NE.X20) GO TO 30
      CALL ISBYTE(z'30',NUM,I)
   20 CONTINUE
C
   30 NXLABL=NUM
      RETURN
      END
