C$PROG QMQARAL   - Shell correction term for liquid drop mass excess
C
C     ******************************************************************
C     BY Volker Routh AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE QMQARAL(IZ,IN,DROP,ELD,SDEF,SHELL,FDEF,FSDL,ISW)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*4 MAGIC(10)
      REAL*8 S(29)
      DATA MAGIC / 0,2,8,14,28,50,82,126,184,258 /
      DATA A1,A2,C3,FKAP / 15.4941,17.9439,.7053,1.7826 /
      DATA GC,PC,AR0 / 5.8,.325,.444 /
      DATA S / -16.711,-7.296,-6.994,-3.448,-3.005,-4.101,-5.719,-3.356,
     &         -1.008,.301,-.373,-.515,-.877,-2.107,-2.635,-.9,-.983,
     &         .29,.124,.695,1.831,2.588,1.432,1.449,-.066,-.413,-1.902,
     &         -2.829,-2.526 /
C
      SAVE
C
C     ------------------------------------------------------------------
C
      AE(II,JJ)=DFLOAT(II)**(DFLOAT(JJ)/3.)
C
C     SHAPE DEPENDENT DROPLET ENERGY ***********************************
C
      Y(X)=QMQEDROP(IZ,IA,X,0.0D0,1)+SNZ*(1.-2.*(X/ALF0)**2)
     &*QMQSEXP(X/ALF0)-ELD
C
C     CUBIC ORDER EXPANSION ********************************************
C
C     Z0(X)=A*X*X-B*X*X*X+SNZ*(1.-2.*X*X)*QMQSEXP(X)
C
C     FIRST DERIVATIVE *************************************************
C
      Z1(X)=2.*X*(A-1.5*B*X-SNZ*(3.-2.*X*X)*QMQSEXP(X))
C
C     SECOND DERIVATIVE ************************************************
C
      Z2(X)=2.*(A-3.*B*X-SNZ*(3.-12.*X*X+4.*X**4)*QMQSEXP(X))
C
      IA=IZ+IN
C
      SHELL=0.0
      IF(MQMNLIM(IZ,IA).LE.0) GO TO 60
C
C     ------------------------------------------------------------------
C     SHELL-CORRECTION
C     ------------------------------------------------------------------
C
      DO 1 MZ=2,10
      IF (MAGIC(MZ).GT.IZ) GO TO 2
    1 CONTINUE
      MZ=10
    2 DO 3 MN=2,10
      IF (MAGIC(MN).GT.IN) GO TO 4
    3 CONTINUE
      MN=10
    4 IF (IZ.LE.29.AND.IN.LE.29) THEN
      SNZ=.7937*AE(IA,-2)*(AE(IN,2)*S(IN)+AE(IZ,2)*S(IZ))
      ELSE
      QZ=.6*(AE(MAGIC(MZ),5)-AE(MAGIC(MZ-1),5))/(MAGIC(MZ)-MAGIC(MZ-1))
      QN=.6*(AE(MAGIC(MN),5)-AE(MAGIC(MN-1),5))/(MAGIC(MN)-MAGIC(MN-1))
      FZ=QZ*(IZ-MAGIC(MZ-1))-.6*(AE(IZ,5)-AE(MAGIC(MZ-1),5))
      FN=QN*(IN-MAGIC(MN-1))-.6*(AE(IN,5)-AE(MAGIC(MN-1),5))
      SNZ=GC*(1.5874*(FZ+FN)/AE(IA,2)-PC*AE(IA,1))
      END IF
C
C     ------------------------------------------------------------------
C     EQUILIBRIUM DEFORMATION
C     ------------------------------------------------------------------
C
      DEF=0.
      FDEF=0.
      SHELL=0.
      FSDL=0.
      ISW=0
      C2=A2*(1.-FKAP*((IZ-IN)/IA)**2)
      XFIS=C3*IZ*IZ/(2.*C2*IA)
      ALF0=DSQRT(5.0D0)*AR0*AE(IA,-1)
      ALF1=DSQRT(3.0D0/2.0D0)*ALF0
      E=(QMQEDROP(IZ,IA,ALF1,0.0D0,2)-ELD)/(ALF1*ALF1)
      F=-(QMQEDROP(IZ,IA,ALF1,0.0D0,3)-ELD)/ALF1**3
      ALF2=2.*E/(3.*F)
      A=E*ALF0*ALF0
      B=F*ALF0*ALF0*ALF0
      ALF3=0.
      STEP=.02/XFIS
      UMIN=0.
      UMAX=ALF2
      IF (SNZ.LT.0.0) THEN
      UMAX=DMAX1(ALF1,ALF2)
      ELSE
      IF (ALF2.LT.ALF1) THEN
      NREP=-1
      XRAC=2.*ALF1/ALF0
    6 NREP=NREP+1
      IF(NREP.GT.10) GO TO 60
      XRAC=XRAC-Z1(XRAC)/Z2(XRAC)
      IF (DABS(Z1(XRAC)).GT.0.001) GO TO 6
      ALF3=ALF0*XRAC
      UMAX=ALF3
      END IF
      END IF
C
C     ------------------------------------------------------------------
C     FIND MAXIMUM (SADDLE MASS BARRIER)
C     ------------------------------------------------------------------
C
      DU=STEP
      U=UMAX
      NRED=0
   10 IF (U.LT.-ALF1) GO TO 60
      IF(U.GT.20.0*XFIS) GO TO 60
      Y0=Y(U)
      Y1=Y(U+DU)
      Y2=Y(U+2.*DU)
      YMAX=(DABS(Y0)+DABS(Y1)+DABS(Y2))/3.
      IF (Y2.GT.Y1.AND.Y1.GT.Y0) THEN
      U=U+DU
      GO TO 10
      ELSE IF (Y2.LT.Y1.AND.Y1.LT.Y0) THEN
      U=U-DU
      GO TO 10
      ELSE
      IF(DABS(YMAX-DABS(Y1)).LT.5.D-4*DMAX1(DABS(Y1),2.D0)) GO TO 20
      NRED=NRED+1
      IF(NRED.GT.5) GO TO 60
      DU=.5*DU
      U=U+DU
      GO TO 10
      END IF
   20 UMAX=U+DU
      NRED=0
      IF (UMAX.LE.2.*DABS(DU)) GO TO 60
      FDEF=UMAX
      FSDL=Y1
C  FIND MINIMUM
      IF (SNZ.LE.0.0) THEN
      ISW=2
      GO TO 50
      END IF
      DU=DMAX1(.02D0,STEP/10.0D0)
      U=UMIN
   30 IF (U.GT.DMAX1(FDEF,2.*ALF1)) GO TO 60
      IF(U.LT.0.0) GO TO 60
      Y0=Y(U)
      Y1=Y(U+DU)
      Y2=Y(U+2.*DU)
      YMIN=(DABS(Y0)+DABS(Y1)+DABS(Y2))/3.
      IF (Y2.LT.Y1.AND.Y1.LT.Y0) THEN
      U=U+DU
      GO TO 30
      ELSE IF (Y2.GT.Y1.AND.Y1.GT.Y0) THEN
      U=U-DU
      GO TO 30
      ELSE
      IF(DABS(YMIN-DABS(Y1)).LT.05.D-4*DMAX1(DABS(Y1),2.D0)) GO TO 40
      NRED=NRED+1
      IF(NRED.GT.5) GO TO 60
      DU=.5*DU
      U=U+DU
      GO TO 30
      END IF
   40 ISW=3
      UMIN=U+DU
   50 SDEF=UMIN
      SHELL=Y(UMIN)
      RETURN
C
   60 ISW=1
      RETURN
      END
