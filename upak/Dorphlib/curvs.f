C$PROG CURVS
C
      SUBROUTINE CURVS (N,X,Y,D,ISW,S,EPS,YS,YSP,SIGMA,TEMP,
     &                  IERR)
C   
      INTEGER*4  N,ISW,IERR
C
      REAL*4     X(N),Y(N),D(N),S,EPS,YS(N),YSP(N),SIGMA,TEMP(N,9)
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
C     TO CALL THE FUNCTION CURV2. THE DETERMINATION OF THE CURVE
C     IS PERFORMED BY SUBROUTINE CURVSS, THE SUBROUTINE CURVS
C     ONLY DECOMPOSES THE WORKSPACE FOR CURVSS.
C     ------------------------------------------------------------------
C   
C     ON INPUT**********************************************************
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
C     OR EQUAL TO ZERO AND LESS THAN OR EQUAL TO ONE. A
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
C     TEMP....IS AN ARRAY OF LENGTH AT LEAST 9*N WHICH IS USED
C     FOR SCRATCH STORAGE.
C
C     ------------------------------------------------------------------
C
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
C     THIS SUBROUTINE REFERENCES PACKAGE MODULES CURVSS, TERMS,
C     AND SNHCSH.
C     ==================================================================
      SAVE
C     ==================================================================
C   
C     DECOMPOSE TEMP INTO NINE ARRAYS AND CALL CURVSS
C     ------------------------------------------------------------------
C
C   
      CALL CURVSS (N,X,Y,D,ISW,S,EPS,YS,YSP,SIGMA,TEMP(1,1),
     &             TEMP(1,2),TEMP(1,3),TEMP(1,4),TEMP(1,5),
     &             TEMP(1,6),TEMP(1,7),TEMP(1,8),TEMP(1,9),
     &             IERR)
      RETURN
      END
