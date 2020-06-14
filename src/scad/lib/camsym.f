C$PROG CAMSYM    - Simulates reading of  CAMAC scalers for RATE data
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 03/21/2000
C     ******************************************************************
C
      SUBROUTINE CAMSYM
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/DD12/ NRATSCA(16)
      INTEGER*4    NRATSCA
C     ------------------------------------------------------------------
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LAG(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LAG,      NSCA
C     ------------------------------------------------------------------
      INTEGER*4    VBUF(16),NCALL,MODE,I
C
      INTEGER*4    ISEED
C
      REAL*4       TLAST,TNOW,DELT,VLAST(16),VNOW(16),RATE
C
      REAL*4       FAC,RFAC,RAN,SECVLU
C
      REAL*4       SINFAC,DEG,DTOR
C
      DATA         DEG   /0.0/
      DATA         DTOR  /0.017453292/
      DATA         MODE  /1/
      DATA         NCALL /0/
      DATA         ISEED /-1/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(NCALL.GT.0) GO TO 100
      TLAST=SECVLU(0.0)
      NCALL=1
      DO 10 I=1,NSCA
      FAC=10.0*10.0**I
      RFAC=0.50*FAC
      VBUF(I)=FAC+RAN(ISEED)*RFAC
      VLAST(I)=VBUF(I)
   10 CONTINUE
      RETURN
C
  100 DEG=DEG+1.0
      SINFAC=0.5*(1.0+SIN(DTOR*DEG))
C
      TNOW=SECVLU(0.0)
      DELT=TNOW-TLAST
      TLAST=TNOW
C
      DO 160 I=1,NSCA
C
CX    FAC=SINFAC*10.0*10.0**I
      FAC=SINFAC*10.0**I
C
      RFAC=0.20*FAC
C
      VBUF(I)=VLAST(I)+FAC+RFAC*RAN(ISEED)
C
      VNOW(I)=VBUF(I)
C
      RATE=(VNOW(I)-VLAST(I))/DELT
C
      NRATSCA(I)=RATE
C
      VLAST(I)=VNOW(I)
C
  160 CONTINUE
C
      RETURN
      END
