C$PROG SWARD     - S(ELE) from Ward's eff-Q & Zieglers proton S(E)
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/17/2000
C     ******************************************************************
C

      REAL*4 FUNCTION SWARD(AP,ZP,AT,ZT,EKEV)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      REAL*4     AP,ZP,AT,ZT,EKEV,APRO,ZPRO,GAM,EPN,EPRO
C
      REAL*4     EMEV,VP,V0,VRAT,F,G,H
C
      REAL*4     SZIEGL
C
      DATA       APRO/1.0079/
      DATA       ZPRO/1.0/
      DATA       V0  /0.2188/
C     ------------------------------------------------------------------
C
      EMEV=0.001*EKEV
      VP=SQRT(1.930*EMEV/AP)
      VRAT=VP/V0
      F=1.0-EXP(-VRAT)
      G=ALOG(1.035-0.4*EXP(-0.16*ZP))
      H=0.879*VRAT
C
      GAM=1.0-EXP(F*G)*EXP(-H/(ZP**0.65))            !Ward's gamma func
C
      EPN=EKEV/AP
      EPRO=APRO*EPN
C
      SWARD=(GAM*ZP)**2*SZIEGL(APRO,ZPRO,AT,ZT,EPRO)
C
      RETURN
      END
