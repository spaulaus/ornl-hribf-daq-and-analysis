C$PROG QMQEDROP  - Returns liquid droplet energy for arbitrary shapes
C
C     ******************************************************************
C     BY Volker Routh AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      REAL*8 FUNCTION QMQEDROP(IZ,IA,ALF2,ALF4,IDEG)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      DATA A1,A2,A3 / 15.96,20.69,0.0 /
      DATA FJ,FQ,FK,FL,FM,R0 / 36.8,17.0,240.0,100.0,0.0,1.18 /
      DATA C1,C2,C3,C4,C5 / .73219,.16302D-3,1.28846,.55911,.49274D-3 /
C
      SAVE
C
C     ------------------------------------------------------------------
C     THE NUCLEAR DROPLET FOR ARBITRARY SHAPES
C
C     W.D.MYERS AND W.J.SWIATECKI,ANNALS OF PHYSICS 84,186-210(1974)
C     ------------------------------------------------------------------
C
      AE(II,JJ)=DFLOAT(II)**(DFLOAT(JJ)/3.)
      BB=1.
      CC=1.
      IF (IDEG.EQ.2) THEN
      CC=0.
      ELSE IF (IDEG.EQ.3) THEN
      BB=0.
      END IF
C
C     SURFACE ENERGY DEFORMATION .......................................
C
      BS=1.+.2*ALF2**2*(2.*BB-4./21.*ALF2*CC)
      IF (ALF4.NE.0.) BS=BS-.377*ALF2**4-ALF4*(.114*ALF2**2-ALF4)
C
C     COULOMB ENERGY DEFORMATION........................................
C
      BC=1.-.2*ALF2**2*(BB+4./21.*ALF2*CC)
      IF (ALF4.NE.0.) BC=BC+.208*ALF2**4-ALF4*(.171*ALF2**2-.185*ALF4)
C
C     CURVATURE ENERGY DEFORMATION .....................................
C
      BK=1.+.2*ALF2**2*(2.*BB+16./21.*ALF2*CC)
      IF (ALF4.NE.0.) BK=BK-.469*ALF2**4+ALF4*(.057*ALF2**2+ALF4)
C
C     VOLUME REDISTRIBUTION ENERGY DEFORMATION .........................
C
      BR=1.+.2*ALF2**2*(2.*BB+16./21.*ALF2*CC)
      IF (ALF4.NE.0) BR=BR-2.526*ALF2**4+ALF4*(1.735*ALF2**2+1.493*ALF4)
C
C     NEUTRON SKIN ENERGY DEFORMATION ..................................
C
      BV=1.-.2*ALF2**2*(BB+2./21.*ALF2*CC)
      IF (ALF4.NE.0.) BV=BV-.207*ALF2**4-ALF4*(.038*ALF2**2+.444*ALF4)
C
C     SURFACE REDISTRIBUTION ENERGY DEFORMATION ........................
C
      BW=1.
      IF (ALF4.NE.0.) BW=BW-.992*ALF2**4+ALF4*(.571*ALF2**2+1.124*ALF4)
C
C     LOCAL RELATIVE NEUTRON EXCESS AVERAGE ............................
C
      ASY=1.-2.*IZ/IA
      DEL=ASY+BV*(3.*C1*IZ/(16.*FQ*AE(IA,2)))
      DEL=DEL/(1.+BS*(9*FJ*AE(IA,-1)/(4*FQ)))
C
C     BULK DENSITY AVERAGE DEVIATION ...................................
C
      EPS=(-BS*(2*A2*AE(IA,-1))+FL*DEL*DEL+BC*(C1*IZ*IZ*AE(IA,-4)))/FK
C
C     ------------------------------------------------------------------
C     DROPLET ENERGY
C     ------------------------------------------------------------------
C
      ELD=IA*(-A1+FJ*DEL*DEL-.5*FK*EPS*EPS+.5*FM*DEL**4)
      ELD=ELD+BS*AE(IA,2)*(A2+9*((FJ*DEL)**2)/(4*FQ))+BK*A3*AE(IA,1)
      ELD=ELD+IZ*IZ*(BC*(C1*AE(IA,-1))-BR*(C2*AE(IA,1))-BW*C5-C3/IA)
      QMQEDROP=ELD-.7937*C4*IZ
      END
