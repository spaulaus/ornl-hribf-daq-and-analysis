C$PROG SNUC_ZS   - Computes nuclear S(E) - from Ziegler
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      FUNCTION SNUC_ZS(A1,Z1,A2,Z2,E)
C
      SAVE
C
C     ------------------------------------------------------------------
C     FUNCTION TO CALCULATE NUCLEAR S(E) IN UNITS OF EV/(10**15 AT/CMSQ)
C     FORMULA FROM J.F. ZIEGLER
C     E IS IN KEV
C     A1,Z1,A2,Z2 = AP,ZP,AT,ZT
C     ------------------------------------------------------------------
C
      ZMF=(A1+A2)*SQRT(Z1**0.666666+Z2**0.666666)
      EP=32.53*A2*E/(Z1*Z2*ZMF)
      SN=0.5*ALOG(1.0+EP)/(EP+0.10718*EP**0.37544)
      SNUC_ZS=8.462*Z1*Z2*A1*SN/ZMF
      RETURN
      END
