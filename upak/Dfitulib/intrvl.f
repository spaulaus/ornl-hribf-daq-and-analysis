C$PROG INTRVL
C
      FUNCTION INTRVL (T,X,N)
C   
      INTEGER*4  N
C
      REAL*4     T,X(N)
C   
C                            FROM FITPACK -- AUGUST 31, 1981
C                       CODED BY A. K. CLINE AND R. J. RENKA
C                            DEPARTMENT OF COMPUTER SCIENCES
C                              UNIVERSITY OF TEXAS AT AUSTIN
C   
C THIS FUNCTION DETERMINES THE INDEX OF THE INTERVAL
C (DETERMINED BY A GIVEN INCREASING SEQUENCE) IN WHICH
C A GIVEN VALUE LIES.
C   
C ON INPUT--
C   
C   T IS THE GIVEN VALUE.
C   
C   X IS A VECTOR OF STRICTLY INCREASING VALUES.
C   
C AND
C   
C   N IS THE LENGTH OF X (N .GE. 2).
C   
C ON OUTPUT--
C   
C   INTRVL RETURNS AN INTEGER I SUCH THAT
C   
C          I = 1       IF             T .LT. X(2)  ,
C          I = N-1     IF X(N-1) .LE. T            ,
C          OTHERWISE       X(I)  .LE. T .LT. X(I+1),
C   
C NONE OF THE INPUT PARAMETERS ARE ALTERED.
C   
C-----------------------------------------------------------
      SAVE
C     ------------------------------------------------------
C   
      TT = T
      IF (TT .LT. X(2)) GO TO 4
      IF (TT .GE. X(N-1)) GO TO 5
      IL = 2
      IH = N-1
C   
C LINEAR INTERPOLATION
C   
    1 ITMP=FLOAT(IH-IL)*(TT-X(IL))/(X(IH)-X(IL))
C
      I = MIN0(IL+ITMP, IH-1)
C
      IF (TT .LT. X(I)) GO TO 2
      IF (TT .LT. X(I+1)) GO TO 3
C   
C TOO HIGH
C   
      IL = I+1
      GO TO 1
C   
C TOO LOW
C   
    2 IH = I
      GO TO 1
    3 INTRVL = I
      RETURN
C   
C LEFT END
C   
    4 INTRVL = 1
      RETURN
C   
C RIGHT END
C   
    5 INTRVL = N-1
      RETURN
      END
