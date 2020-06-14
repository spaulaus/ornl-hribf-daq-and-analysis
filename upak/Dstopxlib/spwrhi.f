C$PROG SPWRHI    - S(ELE) for heavy ion via Effective-Q method
C
C     ******************************************************************
C     BY T.C. Awes at HHIRF - LAST MODIFIED by W.T. Milner 04/25/2002
C     ******************************************************************
C
      REAL*4 FUNCTION SPWRHI(EA)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/PARTICLE/IZT,IZP,ZT,ZP,AT,AP,SPWRH
      INTEGER*4       IZT,IZP
      REAL*4                  ZT,ZP,AT,AP,SPWRH
C     ------------------------------------------------------------------
      REAL*4          EA
C
      REAL*4          X,EX,GAMMA,ZHEZH,ZLIZH,ZHIZH,A,B
C
      SAVE
C
C     ------------------------------------------------------------------
C
C     ROUTINE TO CALCULATE THE ELECTRONIC STOPPING POWER OF A HEAVY
C     ION USING THE HI'S EFFECTIVE CHARGE AND THE STOPPING POWER FOR
C     HYDROGEN.
C     ------------------------------------------------------------------
C
      SPWRHI=SPWRH
C
      IF(IZP.LT.2) RETURN
C
      IF(IZP-3) 30,30,100
C
   30 X=ALOG(EA)
C
      EX=(7.6-X)**2
C
      GAMMA=1.
C
      IF(EX.LT.20.) GAMMA=1.+(.007+.00005*ZT)*EXP(-EX)
C
      IF(IZP-3) 40,50,100
C
   40 EX=.7446+.1429*X+.01562*X*X-.00267*X**3+1.325E-06*X**8
C
      ZHEZH=2.*GAMMA
C
      IF(EX.LT.20.) ZHEZH=ZHEZH*(1.-EXP(-EX))
C
C     ------------------------------------------------------------------
C     STOPPING POWER FOR HE ION.
C     ------------------------------------------------------------------
C
      SPWRHI=SPWRH*ZHEZH*ZHEZH
C
      RETURN
C
   50 ZLIZH=3.*GAMMA
C
      EX=.7138+.002797*EA+1.348E-06*EA*EA
C
      IF(EX.LT.20.) ZLIZH=ZLIZH*(1.-EXP(-EX))
C
C     ------------------------------------------------------------------
C     STOPPING POWER FOR A LITHIUM ION.
C     ------------------------------------------------------------------
C
      SPWRHI=SPWRH*ZLIZH*ZLIZH
C
      RETURN
C
  100 B=.886/5.*SQRT(EA)*ZP**(-.666666)
C
      A=B+.0378*SIN(1.5708*B)
C
      ZHIZH=ZP
C
      IF(A.LT.20.) ZHIZH=ZP*(1.-EXP(-A)*(1.034-.1777*EXP(-.08114*ZP)))
C
C     ------------------------------------------------------------------
C     STOPPING POWER OF A HEAVY ION.
C     ------------------------------------------------------------------
C
      SPWRHI=SPWRH*ZHIZH*ZHIZH
C
      RETURN
      END
