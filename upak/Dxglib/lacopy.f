C$PROG LACOPY    - Copies label string from one array to another
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE LACOPY(LAA,LAB,NA,NB)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4 LAA(20,10),LAB(20,10),NA,NB
C
      INTEGER*4 I,J
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DO 20 J=1,NA
      DO 10 I=1,20
      LAB(I,J)=LAA(I,J)
   10 CONTINUE
   20 CONTINUE
      NB=NA
      RETURN
      END
