C$PROG FINORD    - Finds asending order of an array
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 12/01/98
C     ******************************************************************
C
      SUBROUTINE FINORD(X,IORD,IUSE,N)
C   
      DIMENSION X(1),IORD(1),IUSE(1)
C   
C     ------------------------------------------------------------------
C     ROUTINE TO FIND ASSENDING ORDER OF "X" AND STORE INDICES IN "IORD"
C     ------------------------------------------------------------------
C   
      IORD(1)=1
      IF(N.LE.1) RETURN
      DO 10 I=1,N
      IUSE(I)=0
   10 CONTINUE
C   
      DO 50 J=1,N
      XLO=1.0E36
      ILO=1
C   
      DO 20 I=1,N
      IF(IUSE(I).NE.0) GO TO 20
      IF(X(I).GE.XLO)  GO TO 20
      XLO=X(I)
      ILO=I
   20 CONTINUE
C   
      IORD(J)=ILO
      IUSE(ILO)=1
C   
   50 CONTINUE
C   
      RETURN
      END
