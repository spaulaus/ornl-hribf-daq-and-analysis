C$PROG ZPEF_ZS   - Computes proton effective charge - from Ziegler
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
      FUNCTION ZPEF_ZS(E)
C
      SAVE
C
C     ------------------------------------------------------------------
C     FUNCTION TO COMPUTE PROTON EFFECTIVE CHARGE (FROM J.F. ZIEGLER)
C     E IS IN KEV
C     ------------------------------------------------------------------
C
      R=E/1.0079
      EX=-(0.2*SQRT(R)+0.0012*R+1.443E-5*R*R)
      ZPEF_ZS=1.0-EXP(EX)
      RETURN
      END
