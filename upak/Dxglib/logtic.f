C$PROG LOGTIC    - Specifies tic-mark locations for log axis
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
C     LOGTIC SPECIFIES TIC MARK LOCATIONS FOR LOG AXES
C     DEFINITION OF ARGUMENTS FOLLOW:
C
C     A    = MIN AXIS VALUE (FLOATING USER-UNITS)
C     B    = MAX AXIS VALUE (FLOATING USER-UNITS)
C     IA   = MIN AXIS VALUE (INTEGER  USER-UNITS)
C     IB   = MAX AXIS VALUE (INTEGER  USER-UNITS)
C     
C     JFIR = REFERENCE DECADE VALUE (USER UNITS) NORMALLY LESS
C            THAN OR EQUAL TO IA
C
C     MUL  = NUMBER BY WHICH JFIR IS TO BE MULTIPLIED SUCH THAT THE
C            RESULT IS JUST GREATER THAN (OR EQUAL TO) IA.
C            MUL = 1,2,3,4,5,6,7,8 OR 9
C
C     THE FIRST TICK TO BE DRAWN IS AT (MUL+0)*JFIR
C     THE NEXT  TICK TO BE DRAWN IS AT (MUL+1)*JFIR, ETC
C     
C     IF MUL=1, THEN WE DRAW A LONG TICK AND A DECADE LABEL
C     ------------------------------------------------------------------
C
      SUBROUTINE LOGTIC(A,B,JFIR,MUL)
C
      IMPLICIT NONE
C     ------------------------------------------------------------------
C
      REAL*4   A,B
C
      INTEGER*4  JFIR,MUL,IA,IB,IT,ITX,IT0,I
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IA=A+0.9
      IB=B+0.9
C
      IT=1
      DO 10 I=1,10
      IF(IT.GT.IA) GO TO 20
      IT=10*IT
   10 CONTINUE
C
   20 IT0=IT
      IF(IT0.GT.1) IT0=IT0/10
      DO 30 MUL=1,9
      ITX=MUL*IT0
      IF(ITX.GE.IA) GO TO 40
   30 CONTINUE
      MUL=9
   40 JFIR=IT0
      RETURN
      END
