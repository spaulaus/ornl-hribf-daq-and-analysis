C$PROG ZEFCR     - Fract-eff-Q for He & Li ions - from Ziegler
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/17/2000
C     ******************************************************************
C
      FUNCTION ZEFCR(A1,Z1,Z2,E)
C
C     ------------------------------------------------------------------
C     FUNCTION TO CALCULATE THE FRACTIONAL EFFECTIVE CHARGE RATIO
C     FOR HE AND LI
C     E IS IN KEV
C     ------------------------------------------------------------------
C
      GAM=1.0+(0.007+0.00005*Z2)*EXP(-7.60-ALOG(E*E/(A1*A1)))
      IZ=Z1+0.5
      IF(IZ.EQ.2) GO TO 10
      IF(IZ.EQ.3) GO TO 20
      ZEFCR=0.0
      RETURN
C
C     ------------------------------------------------------------------
C     THE PROJECTILE IS A HE ION
C     ------------------------------------------------------------------
C
   10 R=ALOG(E/A1)
      EX=0.7446+0.1429*R+0.01562*R*R-0.00267*R**3+1.325E-6*R**8
      IF(EX.GT.20.0) EX=20.0
      ZEFCR=GAM*(1.0-EXP(-EX))
      RETURN
C
C     ------------------------------------------------------------------
C     THE PROJECTILE IS A LI ION
C     ------------------------------------------------------------------
C
   20 R=E/A1
      EX=0.7138+0.002797*R+1.348E-6*R*R
      IF(EX.GT.20.0) EX=20.0
      ZEFCR=GAM*(1.0-EXP(-EX))
      RETURN
      END
