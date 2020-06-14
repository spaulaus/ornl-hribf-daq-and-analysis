C$PROG USERFOX   - User supplied function for non-linear fitting
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 08/15/2000
C     ******************************************************************
C
      REAL*8 FUNCTION USERFOX(A,X)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/FTUSER/ NONEG(64),NV,IFUNK,UTIT
      INTEGER*4      NONEG,    NV,IFUNK
      CHARACTER*40                      UTIT
C     ------------------------------------------------------------------
      REAL*8         A(*),X,DENO,DABS
C     ------------------------------------------------------------------
C
      GO TO (10,20,30,40,50,60,70,80,90) IFUNK
C
   10 USERFOX=A(1)*X
      RETURN
C
   20 USERFOX=A(1) + A(2)*X
      RETURN
C
   30 USERFOX=A(1) + A(2)*X + A(3)*X*X
      RETURN
C
   40 USERFOX=A(1)*DSQRT(X)
      RETURN
C
   50 USERFOX=A(1) + A(2)*DSQRT(X)
      RETURN
C
   60 USERFOX=A(1) + A(2)*DSQRT(X) + A(3)*X
      RETURN
C
   70 USERFOX=DSQRT(A(1) + A(2)*X) + A(3)*X
      RETURN
C
   80 USERFOX=A(1) + A(2)*X + A(3)*DEXP(A(4)*X)
      RETURN
C
   90 DENO=A(3)-A(2)
      IF(DABS(DENO).LT.1.0E-9) DENO=-1.0E-9
      USERFOX=A(1)*(A(2)/DENO)*(DEXP(-A(2)*X)-DEXP(-A(3)*X))
C
      END
