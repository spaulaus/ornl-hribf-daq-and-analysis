C$PROG CAMRED    - Reads CAMAC scalers for SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE CAMRED
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/SD01/ LA(3,512),CN(512),SN(512), A(512), F(512),TY(512),
     &                       KI(512),VN(512),VO(512),VD(512),PV(512),
     &                       LO(512),HI(512),NR,     NT,    NDD,
     &                       NORI, NORF
C
      INTEGER*4    LA,       CN,     SN,      A,      F,     TY,
     &                       KI,     VN,     VO,     VD,     PV,
     &                       LO,     HI,     NR,     NT,    NDD,
     &                       NORI
C
      REAL*4      NORF
C     ------------------------------------------------------------------
      COMMON/SD02/ POL(512),GOL(512),ECN(20),ESN(20),NPO,NEC
      INTEGER*4    POL,     GOL,     ECN,    ESN,    NPO,NEC
C     ------------------------------------------------------------------
      COMMON/SD09/ CC(512),NN(512),AA(512),FF(512),VBUF(512),NLIST
      INTEGER*4    CC,     NN,     AA,     FF,     VBUF,     NLIST
C     ------------------------------------------------------------------
      INTEGER*4 STAT(512)
C
      DATA MODE/1/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DO 10 I=1,NR
      STAT(I)=0
   10 CONTINUE
C
      CALL CAMLIST(CC,NN,AA,FF,MODE,VBUF,NLIST,STAT)
C
      DO 50 I=1,NR
C
      IF (STAT(I).NE.0) CALL CAMERRR(CC(I),NN(I),AA(I),FF(I),STAT(I))
C
   50 CONTINUE
      RETURN
      END
