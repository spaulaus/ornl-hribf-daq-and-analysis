C$PROG LASUB
      SUBROUTINE LASUB(LABL)
C
      COMMON/OOO/ LLLST(40),NULST(40),NLLST
C
      SAVE
C
C     **************************************************************
C     TESTS LABL VS ELEMENTS OF "LLLST" (LOOP LABEL LIST) AND
C
C     REPLACES LABEL WITH CORRESPONDING VALUE FROM "NULST", IF FOUND
C     **************************************************************
C
      DO 10 I=1,NLLST
      IF(LABL.EQ.LLLST(I)) GO TO 20
   10 CONTINUE
      RETURN
C
   20 LABL=NULST(I)
      RETURN
      END
