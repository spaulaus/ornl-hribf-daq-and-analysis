C$PROG CLRADD
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CLRADD(TC,TN,TA,TF,C,N,A,F,NUM)
C
      INTEGER*4 C(*),N(*),A(*),F(*),TC,TN,TA,TF,NUM
C
C
      SAVE
C
C     ************************************************************
C     ADD ENTRIES TO THE CAMAC MODULE CLEAR-LIST 
C     ************************************************************
C
      DO 100 I=1,NUM
      IF(TC.EQ.C(I).AND.TN.EQ.N(I)) RETURN
  100 CONTINUE
C
      NUM=NUM+1
      C(NUM)=TC
      N(NUM)=TN
      A(NUM)=TA
      F(NUM)=TF
      RETURN
      END
