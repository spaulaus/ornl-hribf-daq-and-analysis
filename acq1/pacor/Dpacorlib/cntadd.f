C$PROG CNTADD
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CNTADD(CTST,NTST,TTST,C,N,T,NUM)
C
      INTEGER*4 CTST,NTST,TTST,C(*),N(*),T(*),NUM
C
      SAVE
C
C     ************************************************************
C     ADD ENTRIES TO FASTBUS & FERA MODULE TABLES
C     ************************************************************
C
      DO 100 I=1,NUM
      IF(CTST.EQ.C(I).AND.NTST.EQ.N(I)) RETURN
  100 CONTINUE
C
      NUM=NUM+1
      C(NUM)=CTST
      N(NUM)=NTST
      T(NUM)=TTST
C
      RETURN
      END
