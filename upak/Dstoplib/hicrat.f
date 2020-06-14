C$PROG HICRAT    - Computes ratio of heavy-ion frac-eff-Q to proton valu
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/17/2000
C     ******************************************************************
C
      FUNCTION HICRAT(A,Z,E)
      DATA PI/3.1415926/
C
C     ------------------------------------------------------------------
C     FUNCTION TO CALCULATE THE RATIO OF THE HEAVY ION FRACTIONAL
C     EFFECTIVE CHARGE TO THAT OF PROTONS AT THE SAME VELOCITY
C     FORMULA FROM J.F. ZIEGLER (HANDBOOK OF STOPPING CROSS SECTIONS
C     OF ENERGETIC IONS IN ALL ELEMENTS) 1980
C     ------------------------------------------------------------------
C
      BB=0.886*SQRT(E/(25.0*A))/Z**0.666666
      AA=BB+0.0378*SIN(0.5*PI*BB)
      F1=EXP(-AA)
      F2=(1.034-0.1777*EXP(-0.08114*Z))
      HICRAT=1.0-F1*F2
      RETURN
      END
