C$PROG TABO      - Lists arrays for diagnostics only for SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE TABO
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
C
      SAVE
C
C     ------------------------------------------------------------------
C     THIS ROUTINE IS FOR DIAGNOSTICS ONLY
C     NORMALLY LOGICAL UNIT-7 IS ASSIGNED TO NULL:
C     ------------------------------------------------------------------
C
      DO 20 I=1,NT
C
      WRITE(7,10)(LA(J,I),J=1,3),CN(I),SN(I),A(I),F(I),TY(I),KI(I),
     &                           VN(I),VO(I),VD(I),PV(I),LO(I),HI(I)
   10 FORMAT(1H ,3A4,4I5,2X,A4,2X,A4,6I5)
   20 CONTINUE
C
      DO 50 I=1,NEC
      WRITE(7,30)ECN(I),ESN(I)
   30 FORMAT(1H ,2I5)
   50 CONTINUE
C
      DO 120 I=1,NPO
      WRITE(7,110)POL(I),GOL(I)
  110 FORMAT(1H ,2I10)
  120 CONTINUE
C
      DO 150 I=1,NLIST
      WRITE(7,140)CC(I),NN(I),AA(I),FF(I),VBUF(I)
  140 FORMAT(1H ,'C,N,A,F,DAT =',4I8,Z10)
  150 CONTINUE  
      RETURN
      END
