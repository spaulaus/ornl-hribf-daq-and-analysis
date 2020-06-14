C$PROG ZPEF      - Proton effective charge - from Ziegler
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/17/2000
C     ******************************************************************
C
      FUNCTION ZPEF(E)
C
C     ------------------------------------------------------------------
C     FUNCTION TO COMPUTE PROTON EFFECTIVE CHARGE (FROM J.F. ZIEGLER)
C     E IS IN KEV
C     ------------------------------------------------------------------
C
      R=E/1.0079
      EX=-(0.2*SQRT(R)+0.0012*R+1.443E-5*R*R)
      ZPEF=1.0-EXP(EX)
      RETURN
      END
