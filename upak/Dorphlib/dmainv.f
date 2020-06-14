C$PROG DMAINV    - Matrix inverson routine (REAL*4)
C
C     ******************************************************************
C     LAST MODIFIED 06/13/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE DMAINV(M,N)
C
      COMMON/MAINV/ A(20,20),B(20,1),DET,IFS
C
      INTEGER*4 IROW(20),JCOL(20)
C
      DATA IP,JP/0,0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DO 2 I=1,M
      IROW(I)=0
      JCOL(I)=0
    2 CONTINUE
      DET=1.0
      IFS=1
      DO 23 K=1,M
      PIVOT=0.0
      DO 8 J=1,M
      IF(JCOL(J).NE.0) GO TO 8
      DO 7 I=1,M
      IF(IROW(I).NE.0) GO TO 7
      X=ABS(A(I,J))
      IF(X.LT.PIVOT) GO TO 7
      PIVOT=X
      JP=J
      IP=I
    7 CONTINUE
    8 CONTINUE
      PIVOT=A(IP,JP)
      DET=DET*PIVOT
      IF(PIVOT.NE.0.0) GO TO 10
      IFS=0
      RETURN
   10 IROW(IP)=JP
      JCOL(JP)=IP
      A(IP,JP)=1.0/PIVOT
      DO 12 I=1,M
      IF(I.NE.IP) A(I,JP)=-A(I,JP)/PIVOT
   12 CONTINUE
      DO 17 J=1,M
      IF(J.EQ.JP) GO TO 17
      IF(A(IP,J).EQ.0.0) GO TO 17
      ALPHA=A(IP,J)
      A(IP,J)=ALPHA/PIVOT
      DO 16 I=1,M
      IF(I.NE.IP) A(I,J)=A(I,J)+ALPHA*A(I,JP)
   16 CONTINUE
   17 CONTINUE
      IF(N.EQ.0) GO TO 23
      DO 22 J=1,N
      IF(B(IP,J).EQ.0.0) GO TO 22
      ALPHA=B(IP,J)
      B(IP,J)=ALPHA/PIVOT
      DO 21 I=1,M
      IF(I.NE.IP) B(I,J)=B(I,J)+ALPHA*A(I,JP)
   21 CONTINUE
   22 CONTINUE
   23 CONTINUE
      DO 30 K=1,M
   24 I=IROW(K)
      IF(I.EQ.K) GO TO 30
      DET=-DET
      DO 26 J=1,M
      TEMP=A(I,J)
      A(I,J)=A(K,J)
      A(K,J)=TEMP
   26 CONTINUE
      IF(N.EQ.0) GO TO 29
      DO 28 J=1,N
      TEMP=B(I,J)
      B(I,J)=B(K,J)
      B(K,J)=TEMP
   28 CONTINUE
   29 IROW(K)=IROW(I)
      IROW(I)=I
      GO TO 24
   30 CONTINUE
      DO 34 K=1,M
   31 J=JCOL(K)
      IF(J.EQ.K) GO TO 34
      DO 33 I=1,M
      TEMP=A(I,J)
      A(I,J)=A(I,K)
      A(I,K)=TEMP
   33 CONTINUE
      JCOL(K)=JCOL(J)
      JCOL(J)=J
      GO TO 31
   34 CONTINUE
      RETURN
      END
