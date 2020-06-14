C$PROG LACOM     - Compares two label arrays
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      INTEGER*4 FUNCTION LACOM(LAA,LAB,NA,NB)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4 LAA(20,10),LAB(20,10),NA,NB
C
      INTEGER*4 NO,YES,I,J
C
      CHARACTER*4 CNO,CYES
C
      EQUIVALENCE (CNO,NO),(CYES,YES)
C
      DATA CNO,CYES/'NO  ','YES '/
C
      SAVE
C     ------------------------------------------------------------------
C
      LACOM=NO
C
      IF(NA.NE.NB) RETURN
C
      DO 20 J=1,NA
      DO 10 I=1,20
      IF(LAA(I,J).NE.LAB(I,J)) RETURN
   10 CONTINUE
   20 CONTINUE
      LACOM=YES
      RETURN
      END
