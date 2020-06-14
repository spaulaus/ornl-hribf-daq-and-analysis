C$PROG SORGET    - Tests for and returns file-code (M N O P Q R S T)
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SORGET(ITST,KSOR,IOF)
C
      INTEGER*4 KSOT(7),INPUT
      character*4 cksot(7), cinput
      equivalence (cksot,ksot), (cinput, input)
C
      DATA     cINPUT/'N   '/
C
      DATA cKSOT/'M   ','N   ','O   ','P   ','Q   ','R   ','S   '/
C
      SAVE
C
      KSOR=INPUT
      IOF=0
C
      DO 10 I=1,7
      IF(ITST.EQ.KSOT(I)) GO TO 20
   10 CONTINUE
      RETURN
C
   20 KSOR=ITST
      IOF=1
      RETURN
      END
