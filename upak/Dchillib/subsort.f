C$PROG SUBSORT
      SUBROUTINE SUBSORT(GLIB,IA,IB,IERR)
C
      INTEGER*2 GLIB(4096)
C
      INTEGER*4 IWD(200,2),LO(200),HI(200)
C
      INTEGER*4 IT(2)
C
      EQUIVALENCE (LO(1),IWD(1,1)),
     &            (HI(1),IWD(1,2))
C
      SAVE
C
      IERR=0
      I=IA-1
      NGA=(IB-IA+1)/2
      DO 10 N=1,NGA
      I=I+1
      IWD(N,1)=GLIB(I)
      I=I+1
      IWD(N,2)=GLIB(I)
   10 CONTINUE
C
      M=NGA
  100 M=M/2
      IF(M.EQ.0) GO TO 500
      K=NGA-M
      J=1
  200 I=J
  300 L=I+M
      IF(IWD(I,1).LE.IWD(L,1)) GO TO 400
      DO 310 II=1,2
      IT(II)=IWD(I,II)
  310 CONTINUE
      DO 320 II=1,2
      IWD(I,II)=IWD(L,II)
  320 CONTINUE
      DO 330 II=1,2
      IWD(L,II)=IT(II)
  330 CONTINUE
      I=I-M
      IF(I.GE.1) GO TO 300
  400 J=J+1
      IF(J.GT.K) GO TO 100
      GO TO 200
C
  500 DO 520 I=1,NGA
      ILO=LO(I)
      IHI=HI(I)
      IF(ILO.GT.IHI) GO TO 1000
C
      DO 510 J=1,NGA
      IF(J.EQ.I) GO TO 510
      IF(ILO.GE.LO(J).AND.ILO.LE.HI(J)) GO TO 1000
      IF(IHI.GE.LO(J).AND.IHI.LE.HI(J)) GO TO 1000
  510 CONTINUE
  520 CONTINUE
C
      I=IA-1
      DO 550 N=1,NGA
      I=I+1
      GLIB(I)=IWD(N,1)
      I=I+1
      GLIB(I)=IWD(N,2)
  550 CONTINUE
      RETURN
C
 1000 IERR=1
      RETURN
      END
