C$PROG RAN       - Random number generator
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      FUNCTION RAN(ISEED)
C
      INTEGER*4 NCALL,GETRAND,MAX
C
      DATA      MAX/Z'7FFFFFFF'/
C
      DATA      NCALL/0/
C
      SAVE 
C
C     ------------------------------------------------------------------
C
      IF(NCALL.GT.0) GO TO 100
C
      CALL SETRAND(ISEED)
C
      FMAX=FLOAT(MAX)+1.0
C
      NCALL=1
C
  100 RAN=FLOAT(GETRAND())/FMAX
C
      RETURN
      END
