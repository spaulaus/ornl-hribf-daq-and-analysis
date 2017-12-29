C$PROG CAMRED    - Reads CAMAC scalers for RATE data
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 03/21/2000
C     ******************************************************************
C
      SUBROUTINE CAMRED
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
      INTEGER*4    VBUF(16),STAT(16),NCALL,MODE,I
C
      REAL*4       TLAST,TNOW,DELT,VLAST(16),VNOW(16),RATE
C
      REAL*4       SECVLU
C
      DATA MODE/1/
      DATA NCALL/0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(NCALL.GT.0) GO TO 100
      TLAST=SECVLU(0.0)
      CALL CAMLIST(CC,NN,AA,FF,MODE,VBUF,NSCA,STAT)
      NCALL=1
      DO 10 I=1,NSCA
      VLAST(I)=VBUF(I)
   10 CONTINUE
      RETURN
C
  100 DO 110 I=1,NSCA
      STAT(I)=0
  110 CONTINUE
C
      TNOW=SECVLU(0.0)
      DELT=TNOW-TLAST
      TLAST=TNOW
C
      CALL CAMLIST(CC,NN,AA,FF,MODE,VBUF,NSCA,STAT)
C
      DO 150 I=1,NSCA
C
      IF(STAT(I).NE.0) CALL CAMERRR(CC(I),NN(I),AA(I),FF(I),STAT(I))
C
  150 CONTINUE
C
      DO 160 I=1,NSCA
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
