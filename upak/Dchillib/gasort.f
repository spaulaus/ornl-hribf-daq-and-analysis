C$PROG GASORT
      SUBROUTINE GASORT(IWD,NGA,IERR,MSER)
C
      INTEGER*4 IWD(2,200),IT(2)
C
      INTEGER*4 MESS(10,2),MSER(10)
      CHARACTER*40 MSC(2)
C
      EQUIVALENCE (MSC(1),MESS(1,1))
C
      DATA  MSC/'GATE-SET ERROR - LO-LIMIT.GT.HI-LIMIT  ',
     &          'GATE-SET ERROR - SOME GATES OVERLAP    '/
C
      SAVE
C
      IERR=0
C
C     **************************************************************
C     DO THE MIGHTY SHELL SORT ON LO-LIMITS ONLY
C     **************************************************************
C
      M=NGA
  100 M=M/2
      IF(M.EQ.0) GO TO 500
      K=NGA-M
      J=1
  200 I=J
  300 L=I+M
      IF(IWD(1,I).LE.IWD(1,L)) GO TO 400
      DO 310 II=1,2
      IT(II)=IWD(II,I)
  310 CONTINUE
      DO 320 II=1,2
      IWD(II,I)=IWD(II,L)
  320 CONTINUE
      DO 330 II=1,2
      IWD(II,L)=IT(II)
  330 CONTINUE
      I=I-M
      IF(I.GE.1) GO TO 300
  400 J=J+1
      IF(J.GT.K) GO TO 100
      GO TO 200
C
C     **************************************************************
C     TST FOR (LO-LIMIT).GT.(HI-LIMIT) AND GATE OVERLAP
C     **************************************************************
C
  500 DO 510 J=1,NGA
      IF(IWD(1,J).GT.IWD(2,J)) GO TO 610
  510 CONTINUE
      NDO=NGA-1
      DO 520 J=1,NDO
      IF(IWD(2,J).GE.IWD(1,J+1)) GO TO 620
  520 CONTINUE
      RETURN
C
  610 JJ=1
      GO TO 700
  620 JJ=2
C
  700 DO 710 I=1,10
      MSER(I)=MESS(I,JJ)
  710 CONTINUE
      IERR=JJ
      RETURN
      END
