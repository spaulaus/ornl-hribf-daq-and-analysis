C$PROG SHE_ZS    - Computes HI S(E) above 1.5 MeV/Amu - from Ziegler
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      FUNCTION SHE_ZS(A1,Z1,A2,Z2,E)
C
      DIMENSION XMIP(92)
C
      DATA (XMIP(I),I=1,92)/
     1 19.00,42.00,47.00,63.00,75.00,79.00,86.00,99.00,118.8,135.0,
     2 141.0,149.0,162.0,159.0,168.9,179.2,170.3,180.0,189.4,195.0,
     3 215.0,228.0,237.0,257.0,275.0,284.0,304.0,314.0,330.0,323.0,
     4 335.4,323.0,354.7,343.4,339.3,347.0,349.7,353.3,365.0,382.0,
     5 391.3,393.0,416.2,428.6,436.4,456.0,470.0,466.0,479.0,511.8,
     6 491.9,491.3,452.4,459.0,484.8,485.5,493.8,512.7,520.2,540.0,
     7 537.0,545.9,547.5,567.0,577.2,578.0,612.2,583.3,629.2,637.0,
     8 655.1,662.9,682.0,695.0,713.6,726.6,743.7,760.0,742.0,768.4,
     9 764.8,761.0,762.9,765.1,761.7,733.1,762.3,760.1,767.9,776.4,
     A 807.0,808.0/
C
      DATA ETO4,PI,XMC2/20.73567,3.1415926,511006.0/
C
      SAVE
C
C     ------------------------------------------------------------------
C     FUNCTION TO CALCULATE HEAVY ION S(E) ABOVE 1.5 MEV/AMU
C     FORMULA FROM J.F. ZIEGLER 1980 REPORT PAGE 20
C     S(E) IN UNITS OF EV/(10**15 ATOMS/CMSQ)
C
C     ETO4 = (ELECTRON CHARGE)**4 IN UNITS OF 10**15 (EV*CM)**2
C     XMC2 = REST ENERGY OF ELECTRON IN EV
C     EFC  = EFFECTIVE CHARGE OF PROJECTILE
C
C     SHELL CORRECTION TERM C/Z2 IS NOT INCLUDED (SET TO ZERO)
C
C     A1,Z1,A2,Z2 = AP,ZP,AT,ZT
C     E IS KEV
C     ------------------------------------------------------------------
C
      CONZ2=SHEL_ZS(E,Z2)
      EFC=Z1*HICR_ZS(A1,Z1,E)
      GSQ=1.0/(1.0+E/(931478*A1))**2
      BSQ=1.0-GSQ
      FOB=ALOG(2.0*XMC2*BSQ/GSQ)-BSQ
      IZ=Z2+0.5
      ALI=ALOG(XMIP(IZ))
      SHE_ZS=4.0*PI*EFC*EFC*Z2*ETO4*(FOB-ALI-CONZ2)/(BSQ*XMC2)
      RETURN
      END
