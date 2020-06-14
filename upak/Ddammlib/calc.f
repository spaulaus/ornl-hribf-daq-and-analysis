C$PROG CALC      - Does calculations for GFIT
C
C     ******************************************************************
C     From Saltmarsh & Halbert
C     ******************************************************************
C
C   CALCULATE VALUE OF A THEORETICAL FUNCTION -- CALC --
C          CALCULATES THE VALUE OF A THEORETICAL FUNCTION Y FOR A GIVEN
C          SET OF PARAMETERS P(I) AT A GIVEN VALUE OF THE INDEPENDENT
C          VARIABLE X.ALSO CALCULATES THE DERIVATIVES OF Y W.R.T. THE P
C          THE PK(6) ARE THE VARIOUS COMPONENTS OF Y,THAT IS
C          THE BACKGROUND (P(1)),AND FIVE PEAKS (PK(2-6))
C             Y=(SUM I=1,6) PK(I)
C             PK(1)=P(16)+P(17)*X+P(18)*X*X ..........IBOP=0
C          OR      =EXP(...............)    ..........IBOP=1
C             PK(I)(I NOT=1) =(P(IA)/1.772458)*(2.*SQRT(0.69315)/P(IW))
C                            *EXP(-((X-POS)/2.*P(IW)*SQRT(0.69315))**2)
C             WHERE  P(IA)=P(I+5)
C                    P(IW)=P(I+10)=P(11) IF IWOP=0
C                    POS=P(I)..........IPOP=0
C                 OR POS=P(I)+P(1).....IPOP=1,I G.T.2
C     ******************************************************************
C
      SUBROUTINE CALC(X,Y,P,D,PK,IBOP,IPOP,IWOP)
      DIMENSION PK(6),P(18),D(18)
C          FIRST SET ALL THE WIDTHS TO SOMETHING POSITIVE
      DO 1 I=11,15
      IF(P(I))2,2,1
    2 P(I)=1.
    1 CONTINUE
      Y=0.
      DO 3 J=1,5
      JW=J+10
      JA=J+5
      IF(IWOP)4,4,5
    4 P(JW)=P(11)
    5 IF(IPOP)6,6,7
    6 POS=P(J)
      GO TO 8
    7 IF(J-1)6,6,9
    9 POS=P(J)+P(1)
    8 W=P(JW)/(2.*SQRT(0.69315))
      A=P(JA)/1.772458
      S=((X-POS)/W)**2
      IF(40.-S)20,20,21
   20 SEXP=0.
      GO TO 22
   21 SEXP=EXP(-S)
   22 PK(J+1)=A*SEXP/W
      Y=Y+PK(J+1)
      D(JA)=SEXP/(W*1.772458)
      D(JW)=PK(J+1)*(2.*S-1.)/W
    3 D(J) =2.*PK(J+1)*(X-POS)/W**2
      IF(IWOP)10,10,11
   10 DO 12 J=12,15
   12 D(11)=D(11)+D(J)
   11 IF(IPOP)13,13,14
   14 DO 15 J=2,5
   15 D(1)=D(1)+D(J)
   13 PK(1)=P(16)+P(17)*X+P(18)*X*X
      D(16)=1.
      D(17)=X
      D(18)=X*X
      IF(IBOP)16,16,17
   17 PK(1)=EXP(PK(1))
      DO 18 J=16,18
   18 D(J)=D(J)*PK(1)
   16 Y=Y+PK(1)
C          NOW SET THE PEAK COMPONENTS = PEAK + BACKGROUND
      DO 19 J=2,6
   19 PK(J)=PK(J)+PK(1)
      RETURN
      END
