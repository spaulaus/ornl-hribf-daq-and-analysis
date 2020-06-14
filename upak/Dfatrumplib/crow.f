C$PROG CROW
      SUBROUTINE CROW (AM,E,TH,DTH,ZO,B,T,TMX,  RD,L,R,Q,NR,IF)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/AAA/ Z(4), NZ(30), CONST(50), ZZ(25)
C      CCC
C     INTEGER*4 ABSORB
      character*4 ABSORB
      DIMENSION Q(2), THA(2), THLA(2), E3R(2), DTFP(2), DEXIT(2)
     1, XI2PP(2), DK(2), ANOK(2), RD(2), AM(4)
      EQUIVALENCE (NZ(5),ABSORB), (CONST(34),XA)
C   
      EQUIVALENCE (TARGT ,ZZ(5)),
     &            (KAT   ,ZZ(6)),
     &            (ZT    ,ZZ(7)),
     &            (TBACKT,ZZ(9)),
     &            (ATB   ,ZZ(10)),
     &            (ZTB   ,ZZ(11)),
     &            (NORDER,ZZ(12))
      REAL*8 L
C   
C   
      TARGB=2*TBACKT
      IF=0
      DO 3 I=1,2
      CALL RELKIN  (AM(1),AM(2),AM(3),AM(4),E,TH,THA(I),THLA(I),E3R(I),Q
     1(I),IJ)
      IF (IJ.GT.0) GO TO 1
      IF=1
      RETURN
1     CONTINUE
      IF (TARGT.EQ.0.0) GO TO 2
      ZT = NZ(2)
      ZPOUT = NZ(3)
      CALL ELOSS(E3R(I),ZPOUT,AM(3),ZT,TARGT,ABSORB)
      IF (NORDER.EQ.2.AND.TBACKT.NE.0.0) CALL ELOSS (E3R(I),ZPOUT,AM(3),
     XZTB,TARGB,'SOLI')
c    XZTB,TARGB,4HSOLI)
2     CONTINUE
      S=TMX*DSQRT(E3R(I)**2+2.*T*E3R(I))/B
      RC=AM(1)+AM(2)-AM(3)-Q(I)/XA
      CALL OPTIC (S,E3R(I),AM(1),AM(3),E,TH,DTH,RC,DTFP(I),DEXIT(I),XI2P
     1P(I),DK(I),ANOK(I))
3     CONTINUE
      CALL WHERE (Q,2,DEXIT,DTFP,XI2PP,L,R,RD,IJ,2)
      RETURN
      END
