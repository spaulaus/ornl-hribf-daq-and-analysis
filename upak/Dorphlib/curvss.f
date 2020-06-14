C$PROG CURVSS
C
      SUBROUTINE CURVSS (N,X,Y,D,ISW,S,EPS,YS,YSP,SIGMA,TD,
     &                   TSD1,HD,HSD1,HSD2,RD,RSD1,RSD2,V,
     &                   IERR)
C   
      INTEGER*4  N,ISW,IERR
C
      REAL*4     X(N),Y(N),D(N),S,EPS,YS(N),YSP(N),SIGMA,TD(N),
     &           TSD1(N),HD(N),HSD1(N),HSD2(N),RD(N),RSD1(N),
     &           RSD2(N),V(N)
C   
C     ==================================================================
C                            FROM FITPACK -- AUGUST 31, 1981
C                                 CODED BY ALAN KAYLOR CLINE
C                            DEPARTMENT OF COMPUTER SCIENCES
C                              UNIVERSITY OF TEXAS AT AUSTIN
C     ==================================================================
C   
C     THIS SUBROUTINE DETERMINES THE PARAMETERS NECESSARY TO
C     COMPUTE A SMOOTHING SPLINE UNDER TENSION. FOR A GIVEN
C     INCREASING SEQUENCE OF ABSCISSAE (X(I)), I = 1,..., N AND
C     ASSOCIATED ORDINATES (Y(I)), I = 1,..., N, THE FUNCTION
C     DETERMINED MINIMIZES THE SUMMATION FROM I = 1 TO N-1 OF
C     THE SQUARE OF THE SECOND DERIVATIVE OF F PLUS SIGMA
C     SQUARED TIMES THE DIFFERENCE OF THE FIRST DERIVATIVE OF F
C     AND (F(X(I+1))-F(X(I)))/(X(I+1)-X(I)) SQUARED, OVER ALL
C     FUNCTIONS F WITH TWO CONTINUOUS DERIVATIVES SUCH THAT THE
C     SUMMATION OF THE SQUARE OF (F(X(I))-Y(I))/D(I) IS LESS
C     THAN OR EQUAL TO A GIVEN CONSTANT S, WHERE (D(I)), I = 1,
C     ..., N ARE A GIVEN SET OF OBSERVATION WEIGHTS. THE
C     FUNCTION DETERMINED IS A SPLINE UNDER TENSION WITH THIRD
C     DERIVATIVE DISCONTINUITIES AT (X(I)), I = 2,..., N-1. FOR
C     ACTUAL COMPUTATION OF POINTS ON THE CURVE IT IS NECESSARY
C     TO CALL THE FUNCTION CURV2.
C     ==================================================================
C   
C     ON INPUT**********************************************************
C   
C     ------------------------------------------------------------------
C
C     N.......IS THE NUMBER OF VALUES TO BE SMOOTHED (N.GE.2).
C   
C     ------------------------------------------------------------------
C
C     X.......IS AN ARRAY OF THE N INCREASING ABSCISSAE OF THE
C     VALUES TO BE SMOOTHED.
C   
C     ------------------------------------------------------------------
C
C     Y.......IS AN ARRAY OF THE N ORDINATES OF THE VALUES TO BE
C     SMOOTHED, (I. E. Y(K) IS THE FUNCTIONAL VALUE
C     CORRESPONDING TO X(K) ).
C   
C     ------------------------------------------------------------------
C
C     D.......IS A PARAMETER CONTAINING THE OBSERVATION WEIGHTS.
C     THIS MAY EITHER BE AN ARRAY OF LENGTH N OR A SCALAR
C     (INTERPRETED AS A CONSTANT). THE VALUE OF D
C     CORRESPONDING TO THE OBSERVATION (X(K),Y(K)) SHOULD
C     BE AN APPROXIMATION TO THE STANDARD DEVIATION OF ERROR.
C
C     ------------------------------------------------------------------
C   
C     ISW.....CONTAINS A SWITCH INDICATING WHETHER THE PARAMETER
C     D IS TO BE CONSIDERED A VECTOR OR A SCALAR,
C             = 0 IF D IS AN ARRAY OF LENGTH N,
C             = 1 IF D IS A SCALAR.
C   
C     ------------------------------------------------------------------
C
C     S.......CONTAINS THE VALUE CONTROLLING THE SMOOTHING. THIS
C     MUST BE NON-NEGATIVE. FOR S EQUAL TO ZERO, THE
C     SUBROUTINE DOES INTERPOLATION, LARGER VALUES LEAD TO
C     SMOOTHER FUNTIONS. IF PARAMETER D CONTAINS STANDARD
C     DEVIATION ESTIMATES, A REASONABLE VALUE FOR S IS
C     FLOAT(N).
C   
C     ------------------------------------------------------------------
C
C     EPS.....CONTAINS A TOLERANCE ON THE RELATIVE PRECISION TO
C     WHICH S IS TO BE INTERPRETED. THIS MUST BE GREATER THAN
C     OR EQUAL TO ZERO AND LESS THAN EQUAL OR EQUAL TO ONE. A
C     REASONABLE VALUE FOR EPS IS SQRT(2./FLOAT(N)).
C   
C     ------------------------------------------------------------------
C
C     YS......IS AN ARRAY OF LENGTH AT LEAST N.
C   
C     ------------------------------------------------------------------
C
C     YSP.....IS AN ARRAY OF LENGTH AT LEAST N.
C   
C     ------------------------------------------------------------------
C
C     SIGMA...CONTAINS THE TENSION FACTOR. THIS VALUE INDICATES
C     THE DEGREE TO WHICH THE FIRST DERIVATIVE PART OF THE
C     SMOOTHING FUNCTIONAL IS EMPHASIZED. IF SIGMA IS NEARLY
C     ZERO (E. G. .001) THE RESULTING CURVE IS APPROXIMATELY A
C     CUBIC SPLINE. IF SIGMA IS LARGE (E. G. 50.) THE
C     RESULTING CURVE IS NEARLY A POLYGONAL LINE. IF SIGMA
C     EQUALS ZERO A CUBIC SPLINE RESULTS. A STANDARD VALUE FOR
C     SIGMA IS APPROXIMATELY 1.
C   
C     ------------------------------------------------------------------
C
C     AND
C   
C     ------------------------------------------------------------------
C
C     TD, TSD1, HD, HSD1, HSD2, RD, RSD1, RSD2, AND V ARE
C     ARRAYS OF LENGTH AT LEAST N WHICH ARE USED FOR SCRATCH
C     STORAGE.
C
C     ------------------------------------------------------------------
C   
C     ON OUTPUT*********************************************************
C   
C     ------------------------------------------------------------------
C
C     YS......CONTAINS THE SMOOTHED ORDINATE VALUES.
C   
C     ------------------------------------------------------------------
C
C     YSP.....CONTAINS THE VALUES OF THE SECOND DERIVATIVE OF THE
C     SMOOTHED CURVE AT THE GIVEN NODES.
C   
C     ------------------------------------------------------------------
C
C     IERR....CONTAINS AN ERROR FLAG,
C             = 0 FOR NORMAL RETURN,
C             = 1 IF N IS LESS THAN 2,
C             = 2 IF S IS NEGATIVE,
C             = 3 IF EPS IS NEGATIVE OR GREATER THAN ONE,
C             = 4 IF X-VALUES ARE NOT STRICTLY INCREASING,
C             = 5 IF A D-VALUE IS NON-POSITIVE.
C   
C     ------------------------------------------------------------------
C
C     N, X, Y, D, ISW, S, EPS, AND SIGMA ARE UNALTERED.
C   
C     ------------------------------------------------------------------
C
C     THIS SUBROUTINE REFERENCES PACKAGE MODULES TERMS AND
C     SNHCSH.
C
C     ==================================================================
      SAVE
C     ==================================================================
C   
      IF (N .LT. 2) GO TO 16
      IF (S .LT. 0.) GO TO 17
      IF (EPS .LT. 0. .OR. EPS .GT. 1.) GO TO 18
      IERR = 0
      P = 0.
      V(1) = 0.
      V(N) = 0.
      YSP(1) = 0.
      YSP(N) = 0.
      IF (N .EQ. 2) GO TO 14
      RSD1(1) = 0.
      RD(1) = 0.
      RSD2(N) = 0.
      RDIM1 = 0.
      YSPIM2 = 0.
C   
C DENORMALIZE TENSION FACTOR
C   
      SIGMAP = ABS(SIGMA)*FLOAT(N-1)/(X(N)-X(1))
C   
C FORM T MATRIX AND SECOND DIFFERENCES OF Y INTO YS
C   
      NM1 = N-1
      NM3 = N-3
      DELXI1 = 1.
      DELYI1 = 0.
      DIM1 = 0.
      DO 1 I = 1,NM1
        DELXI = X(I+1)-X(I)
        IF (DELXI .LE. 0.) GO TO 19
        DELYI = (Y(I+1)-Y(I))/DELXI
        YS(I) = DELYI-DELYI1
        CALL TERMS (DI,TSD1(I+1),SIGMAP,DELXI)
        TD(I) = DI+DIM1
        HD(I) = -(1./DELXI+1./DELXI1)
        HSD1(I+1) = 1./DELXI
        DELXI1 = DELXI
        DELYI1 = DELYI
    1   DIM1 = DI
C   
C CALCULATE LOWER AND UPPER TOLERANCES
C   
      SL = S*(1.-EPS)
      SU = S*(1.+EPS)
      IF (ISW .EQ. 1) GO TO 3
C   
C FORM H MATRIX - D ARRAY
C   
      IF (D(1) .LE. 0. .OR. D(2) .LE. 0.) GO TO 20
      BETAPP = 0.
      BETAP = 0.
      ALPHAP = 0.
      DO 2 I = 2,NM1
        ALPHA = HD(I)*D(I)*D(I)
        IF (D(I+1) .LE. 0.) GO TO 20
        BETA = HSD1(I+1)*D(I+1)*D(I+1)
        HD(I) = (HSD1(I)*D(I-1))**2+ALPHA*HD(I)
     *                             +BETA*HSD1(I+1)
        HSD2(I) = HSD1(I)*BETAPP
        HSD1(I) = HSD1(I)*(ALPHA+ALPHAP)
        ALPHAP = ALPHA
        BETAPP = BETAP
    2   BETAP = BETA
      GO TO 5
C   
C FORM H MATRIX - D CONSTANT
C   
    3 IF (D(1) .LE. 0.) GO TO 20
      SL = D(1)*D(1)*SL
      SU = D(1)*D(1)*SU
      HSD1P = 0.
      HDIM1 = 0.
      DO 4 I = 2,NM1
        HDI = HD(I)
        HD(I) = HSD1(I)*HSD1(I)+HDI*HDI+HSD1(I+1)*HSD1(I+1)
        HSD2(I) = HSD1(I)*HSD1P
        HSD1P = HSD1(I)
        HSD1(I) = HSD1P*(HDI+HDIM1)
    4   HDIM1 = HDI
C   
C TOP OF ITERATION
C CHOLESKY FACTORIZATION OF P*T+H INTO R
C   
    5 DO 6 I = 2,NM1
        RSD2I = HSD2(I)
        RSD1I = P*TSD1(I)+HSD1(I)-RSD2I*RSD1(I-1)
        RSD2(I) = RSD2I*RDIM1
        RDIM1 = RD(I-1)
        RSD1(I) = RSD1I*RDIM1
        RD(I) = 1./(P*TD(I)+HD(I)-RSD1I*RSD1(I)
     *                           -RSD2I*RSD2(I))
        YSP(I) = YS(I)-RSD1(I)*YSP(I-1)-RSD2(I)*YSPIM2
    6   YSPIM2 = YSP(I-1)
C   
C BACK SOLVE OF R(TRANSPOSE)* R * YSP = YS
C   
      YSP(NM1) = RD(NM1)*YSP(NM1)
      IF (N .EQ. 3) GO TO 8
      DO 7 IBAK = 1,NM3
        I = NM1-IBAK
    7   YSP(I) = RD(I)*YSP(I)-RSD1(I+1)*YSP(I+1)
     *                       -RSD2(I+2)*YSP(I+2)
    8 SUM = 0.
      DELYI1 = 0.
      IF (ISW .EQ. 1) GO TO 10
C   
C CALCULATION OF RESIDUAL NORM
C  - D ARRAY
C   
      DO 9 I = 1,NM1
        DELYI = (YSP(I+1)-YSP(I))/(X(I+1)-X(I))
        V(I) = (DELYI-DELYI1)*D(I)*D(I)
        SUM = SUM+V(I)*(DELYI-DELYI1)
    9   DELYI1 = DELYI
      V(N) = -DELYI1*D(N)*D(N)
      GO TO 12
C   
C CALCULATION OF RESIDUAL NORM
C  - D CONSTANT
C   
   10 DO 11 I = 1,NM1
        DELYI = (YSP(I+1)-YSP(I))/(X(I+1)-X(I))
        V(I) = DELYI-DELYI1
        SUM = SUM+V(I)*(DELYI-DELYI1)
   11   DELYI1 = DELYI
      V(N) = -DELYI1
   12 SUM = SUM-V(N)*DELYI1
C   
C TEST FOR CONVERGENCE
C   
      IF (SUM .LE. SU) GO TO 14
C   
C CALCULATION OF NEWTON CORRECTION
C   
      F = 0.
      G = 0.
      WIM2 = 0.
      WIM1 = 0.
      DO 13 I = 2,NM1
        TUI = TSD1(I)*YSP(I-1)+TD(I)*YSP(I)
     *                        +TSD1(I+1)*YSP(I+1)
        WI = TUI-RSD1(I)*WIM1-RSD2(I)*WIM2
        F = F+TUI*YSP(I)
        G = G+WI*WI*RD(I)
        WIM2 = WIM1
   13   WIM1 = WI
      H = F-P*G
      IF (H .LE. 0.) GO TO 14
C   
C UPDATE P - NEWTON STEP
C   
      STEP = (SUM-SQRT(SUM*SL))/H
      IF (SL .NE. 0.) STEP = STEP*SQRT(SUM/SL)
      P = P+STEP
      GO TO 5
C   
C STORE SMOOTHED Y-VALUES AND SECOND DERIVATIVES
C   
   14 DO 15 I = 1,N
        YS(I) = Y(I)-V(I)
   15   YSP(I) = P*YSP(I)
      RETURN
C   
C N LESS THAN 2
C   
   16 IERR = 1
      RETURN
C   
C S NEGATIVE
C   
   17 IERR = 2
      RETURN
C   
C EPS NEGATIVE OR GREATER THAN 1
C   
   18 IERR = 3
      RETURN
C   
C X-VALUES NOT STRICTLY INCREASING
C   
   19 IERR = 4
      RETURN
C   
C WEIGHT NON-POSITIVE
C   
   20 IERR = 5
      RETURN
      END
