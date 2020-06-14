C$PROG LSQF      - Least-squares solver for GFIT
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C     From Saltmarsh & Halbert
C     ******************************************************************
C
C     SOLVE GNENERAL LEAST-SQUARES PROBLEM -- LSQF --
C     MLH    10-23-86    FOR GASPT -- CHANGE MESOUT TO MESSLOG, ETC.
C     CONVERTS GENERAL LEAST-SQUARES PROBLEM TO LINEAR FOR , & SOLVES THE
C     RESULTING SIMULTANEOUS EQUATIONS BY MATRIX INVERSION.
C     ------------------------------------------------------------------
C
      SUBROUTINE LSQF(SQSIG,DIAG,NM,NV,FL)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/GF01/ AM(180)
      COMMON/GF02/ YO(2048),SIGYO(2048),YC(2048),X(2048)
      COMMON/GF03/ P(18),KI(18),POLD(18),PD(18),NO,NP,IBOP,IPOP,IWOP
      COMMON/GF04/ PC(6,2048)
      DIMENSION V(18),DIAG(18),SQSIG(2),PK(6),DC(18),DV(18)
C     DIMENSION LOUT(12)       ! MLH  10-23-86
C
      SAVE
C
C     ------------------------------------------------------------------

C     INITIALISE THE MATRIX AND THE VECTOR
C     ------------------------------------------------------------------
C
    1 DO 2 I=1,NM
    2 AM(I)=0.
      DO 3 I=1,NV
    3 V(I)=0.
      SIG=0.
      DO 4 I=1,NO
C
C     ------------------------------------------------------------------
C     CALCULATE  FUNCTION AT  ALL VALUES OF X,AND CALCULATE THE
C     DERIVATIVE W.R.T. THE PARAMETERS.USE PRESENT P VALUES.
C     ------------------------------------------------------------------
C
      CALL CALC(X(I),Y,P,DC,PK,IBOP,IPOP,IWOP)
C
C     ------------------------------------------------------------------
C     CALCULATE PRESENT VALUE OF CHISQ.
C     ------------------------------------------------------------------
C
      SQRTW=1./SIGYO(I)
      DY=YO(I)-Y
      WDY=SQRTW*DY
      SIG=SIG+WDY*WDY
      J=1
      DO 5 K=1,18
      IF(KI(K))5,5,6
    6 DV(J)=SQRTW*DC(K)
      J=J+1
    5 CONTINUE
      JK=1
      DO 10 J=1,NV
      DVJ=DV(J)
      IF(DVJ)8,9,8
    9 JK=JK+NV+1-J
      GO TO 10
    8 DO 11 K=J,NV
      AM(JK)=AM(JK)+DVJ*DV(K)
      JK=JK+1
   11 CONTINUE
      V(J)=V(J)+DVJ*WDY
   10 CONTINUE
    4 CONTINUE
      Z=NO-NV
      SQSIG(1)=SQRT(SIG/Z)
    
      ISING=0
      II=1
      IID=NV
      DO 12 I=1,NV
      IF(AM(II))13,14,13
   14 ISING=1
      WRITE(CMSSG,101) I
  101 FORMAT('DIAG ELT=0, PARAM',I3,' OF THOSE VARIED')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
   13 AM(II)=(1.+FL)*AM(II)
C
C     ------------------------------------------------------------------
C     THIS IS THE MARQUARDT PRESCRIPTION TO ALLOW GOOD
C     CONVERGENCE FROM POINTS FAR FROM THE MINIMUM(LARGE FL)
C     AS WELL AS NEAR TO THE MINIMUM(SMALL FL)
C     ------------------------------------------------------------------
C
      II=II+IID
      IID=IID-1
   12 CONTINUE
    
      IF(ISING)15,16,15
   16 CALL MATINV(NV,ISING)
      IF(ISING)17,18,17
   17 WRITE(CMSSG,102)
  102 FORMAT('SINGULARITY FROM MATRIX INVERTER')
   15 CALL MESSLOG(LOGUT,LOGUP)
      RETURN
    
   18 DO 19 I=1,NV
      PDI=0.
C
C     ------------------------------------------------------------------
C     DO MATRIX MULTIPLICATION TO EXTRACT PARAMETER CHANGES
C     ------------------------------------------------------------------
C
      IJ=I
      IJD=NV-1
      DO 20 J=1,NV
      PDI=PDI+AM(IJ)*V(J)
      IF(J-I)21,22,23
   21 IJ=IJ+IJD
      IJD=IJD-1
      GO TO 20
   22 DIAG(I)=AM(IJ)
   23 IJ=IJ+1
   20 CONTINUE
      PD(I)=PDI
   19 CONTINUE
C
C     ------------------------------------------------------------------
C     CALCULATE NEW PARAMETERS,RETAIN OLD VALUES AS POLD
C     ------------------------------------------------------------------
C
      J=1
      DO 24 I=1,18
      POLD(I)=P(I)
      IF(KI(I))25,24,25
   25 P(I)=POLD(I)+PD(J)
      J=J+1
   24 CONTINUE
      SIGT=0.
      DO 27 I=1,NO
      CALL CALC(X(I),YC(I),P,DC,PK,IBOP,IPOP,IWOP)
      SIGT=SIGT+((YO(I)-YC(I))/SIGYO(I))**2
      DO 27 J=1,6
   27 PC(J,I)=PK(J)
      SQSIG(2)=SQRT(SIGT/Z)
      IF(SQSIG(2)-SQSIG(1))28,28,29
   29 FL=10.*FL
      DO 30 I=1,18
   30 P(I)=POLD(I)
      GO TO 1
   28 FL=FL/10.
      RETURN
      END
