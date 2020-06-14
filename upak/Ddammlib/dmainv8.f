C$PROG DMAINV8   - Matrix inversion routine (REAL*8 version)
C
C     ******************************************************************
C     Double Pivotal Matrix Inversion Routine
C
C     From IDAHO FALLS - long, long ago
C
C     Written by D.H. Gipson - Phillips Petrolium Co., June 1962
C
C     Comments added by W.T. Milner, 9/30/2003
C     ******************************************************************
C
      SUBROUTINE DMAINV8(M,N)
C
C     ------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON/MAINV8/ A(50,50),B(50,50),DET,IFS
C
      INTEGER*4 IROW(50),JCOL(50)
C
      DATA IP,JP/0,0/
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C     At CALL time:
C
C     A contains the matrix to be inverted
C
C     B contains one or more vectors to be "operated on"
C
C     M is the size of the square matrix A
C
C     M is also the size of one of the 1-D vectors B
C
C     N is the number of vectors B to be operated on
C
C     As dimensioned B(50,50), B can contain up to 50 1-D vectors
C     Therefore N can be any number from 0 to 50
C     ------------------------------------------------------------------
C
C     On RETURN:
C
C     A is replaced by A-inverse
C
C     B is replaced by A-inverse times B
C
C     DET contains the determinant of the input matrix A
C
C     IFS = 1 says the matrix A is not singular
C
C     IFS = 0 says the matrix A is singular (i.e. error)
C   
C     ------------------------------------------------------------------
C     Initialization                                            Part (1)
C     ------------------------------------------------------------------
C
      DO 2 I=1,M
      IROW(I)=0
      JCOL(I)=0
    2 CONTINUE
      DET=1.0
      IFS=1
      DO 23 K=1,M
C
C     ------------------------------------------------------------------
C     Find the pivot element                                    Part (2)
C     ------------------------------------------------------------------
C
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
C
C     ------------------------------------------------------------------
C     Test to see if matrix is singular                         Part (2)
C     ------------------------------------------------------------------
C
      PIVOT=A(IP,JP)
      DET=DET*PIVOT
      IF(PIVOT.NE.0.0) GO TO 10
      IFS=0
      RETURN
C
C     ------------------------------------------------------------------
C     Remember location of pivot element                        Part (2)
C     ------------------------------------------------------------------
C
   10 IROW(IP)=JP
      JCOL(JP)=IP
C
C     ------------------------------------------------------------------
C     Arithmetic operations on pivotal column                   Part (3)
C     ------------------------------------------------------------------
C
      A(IP,JP)=1.0/PIVOT
      DO 12 I=1,M
      IF(I.NE.IP) A(I,JP)=-A(I,JP)/PIVOT
   12 CONTINUE
C
C     ------------------------------------------------------------------
C     Arithmetic operations on other columns of (A)             Part (4)
C     ------------------------------------------------------------------
C
      DO 17 J=1,M
      IF(J.EQ.JP) GO TO 17
      IF(A(IP,J).EQ.0.0) GO TO 17
      ALPHA=A(IP,J)
      A(IP,J)=ALPHA/PIVOT
      DO 16 I=1,M
      IF(I.NE.IP) A(I,J)=A(I,J)+ALPHA*A(I,JP)
   16 CONTINUE
   17 CONTINUE
C
C     ------------------------------------------------------------------
C     Arithmetic operations on the comumns of (B)               Part (4)
C     ------------------------------------------------------------------
C
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
C
C     ------------------------------------------------------------------
C     Row permutations                                          Part (5)
C     ------------------------------------------------------------------
C
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
C
C     ------------------------------------------------------------------
C     Column permutations
C     ------------------------------------------------------------------
C
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
