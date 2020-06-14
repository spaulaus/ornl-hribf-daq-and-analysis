C$PROG QMQMASX   - Liguid drop mass excess routine
C
C     ******************************************************************
C     BY Volker Routh AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      REAL*8 FUNCTION QMQMASX(IZ,IA,ISW,IERR)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      DATA EN0,EP0 / 8.07169,7.28922 /
C
      SAVE
C
C     ------------------------------------------------------------------
C     DROPLET MODEL MASS FORMULA
C     **************************
C     REF.1 : W.D.MYERS,DROPLET MODEL OF ATOMIC NUCLEI (PLENUM,1977)
C     REF.2 : W.D.MYERS AND W.J.SWIATECKI,ANN.PHYS. 84,186-210(1974)
C     REF.3 : W.D.MYERS AND W.J.SWIATECKI,ARK.FYS.36(1967)343-352
C     REF.4 : W.D.MYERS AND W.J.SWIATECKI,N.P.81(1966)1-60
C     ------------------------------------------------------------------
C
      IERR=0
      IF (IZ.LE.0.OR.IA.LT.1) THEN
      QMQMASX=0.
      IERR=1
      RETURN
      END IF
      IN=IA-IZ
      ASY=1.-2.*IZ/IA
C
C     ------------------------------------------------------------------
C     MASS TERM
C     ------------------------------------------------------------------
C
      EMASS=IZ*EP0+IN*EN0
C
C     ------------------------------------------------------------------
C     SHAPE TERM (SPHERICAL LIQUID DROP )
C     ------------------------------------------------------------------
C
      DEFORM=0.
      ELD=QMQEDROP(IZ,IA,DEFORM,0.0D0,1)
C
C     ------------------------------------------------------------------
C     EVEN-ODD TERM
C     ------------------------------------------------------------------
C
      BDEL=12.0D0/DSQRT(DFLOAT(IA))
      SDEL=20./DFLOAT(IA)
      EVOD=BDEL-.5*SDEL
      IF (IA.NE.2*(IA/2)) EVOD=SDEL/2.
      IF (IZ.EQ.2*(IZ/2).AND.IN.EQ.2*(IN/2)) EVOD=-BDEL+.5*SDEL
C
C     ------------------------------------------------------------------
C     WIGNER TERM
C     ------------------------------------------------------------------
C
      DELW=0.
      IF (IZ.EQ.IN.AND.IZ.NE.2*(IZ/2)) DELW=1./DFLOAT(IA)
      EWIG=30.*(DABS(ASY)+DELW)
C
C     ------------------------------------------------------------------
C     ATOMIC ELECTRON-BINDING TERM
C     ------------------------------------------------------------------
C
      ELEC=-.1433D-4*DFLOAT(IZ)**2.39
C
C     ------------------------------------------------------------------
C     LIQUID DROPLET MASS
C     ------------------------------------------------------------------
C
      DROP=EMASS+ELD+EVOD+EWIG+ELEC
C
C     ------------------------------------------------------------------
C     SHELL-CORRECTION TERM (DEFORMATION CORRECTED)
C     ------------------------------------------------------------------
C
      CALL QMQARAL(IZ,IN,DROP,ELD,SDEF,SHELL,FDEF,FSDL,ISW)
      ESHELL=SHELL
C
C     ------------------------------------------------------------------
C     TOTAL MASS EXCESS
C     ------------------------------------------------------------------
C
      QMQMASX=DROP+ESHELL
      RETURN
      END
