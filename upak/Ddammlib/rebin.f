C$PROG REBIN     - Gain-shifts buffer-IB using JB for summing
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE REBIN(IB,XGS,NCGS,IERR)
C   
C     ------------------------------------------------------------------
      COMMON/MD00/ XBUF(33024),IOFF(2,2),KRUN,NUID,IPUS,IPUL
C     ------------------------------------------------------------------
      DIMENSION XGS(4),NCHN(2)
C   
      DIMENSION ADAT(16384),BDAT(16384)
      INTEGER*4 IDATF(16384),JDATF(16384),IHEDF(128),JHEDF(128)
      INTEGER*2 IDATH(16384),JDATH(16384),IHEDH(256),JHEDH(256)
C   
      EQUIVALENCE (IHEDF(1),XBUF(1)),(IHEDH(1),XBUF(1))
      EQUIVALENCE (IDATF(1),XBUF(129)),(ADAT(1),XBUF(129))
      EQUIVALENCE (IDATH(1),XBUF(8321))
      EQUIVALENCE (JHEDF(1),XBUF(16513)),(JHEDH(1),XBUF(16513))
      EQUIVALENCE (JDATF(1),XBUF(16641)),(BDAT(1),XBUF(16641))
      EQUIVALENCE (JDATH(1),XBUF(24833))
      EQUIVALENCE (IDI,IHEDF(1)),(IDJ,JHEDF(1))
      EQUIVALENCE (NCHI,IHEDF(12)),(NCHJ,JHEDF(12))
C
      SAVE
C   
C     ------------------------------------------------------------------
C     ROUTINE TO GAIN SHIFT BUFFER IB - USING JB TO SUM INTO
C     ------------------------------------------------------------------
C   
      IERR=0
      IOF=IOFF(2,IB)
      JB=3-IB
      JOF=IOFF(2,JB)
      CALL PUSPUL(IPUS,JB)
      NCHN(1)=NCHI
      NCHN(2)=NCHJ
      NA=NCHN(IB)
      NB=NCGS
      BB=(XGS(4)-XGS(3))/(XGS(2)-XGS(1))
      AA=XGS(3)-BB*XGS(1)
      IF(NB.EQ.0) NB=NA
      MAXC=AA+BB*FLOAT(NA)+0.9999999
      IF(NB.LT.0) NB=MAXC
      IF(NB.GT.16384) NB=16384
      DO 10 J=1,16384
      JDX=J+JOF
      XBUF(JDX)=0.0
   10 CONTINUE
      XX=-1.0
      DO 100 IX=1,NA
      XX=XX+1.0
      X1=AA+BB*XX
      X2=X1+BB
      IDX=IX+IOF
      CTS=XBUF(IDX)
      ILO=X1
      IHI=X2+0.9999999
      NBIV=IHI-ILO
      IF(NBIV.GT.1) GO TO 50
      IF(IHI.LT.1) GO TO 100
      IF(IHI.GT.NB) GO TO 110
      MDX=IHI+JOF
      XBUF(MDX)=XBUF(MDX)+CTS
      GO TO 100
   50 JX=ILO
      FNORM=1.0/(X2-X1)
      DO 60 N=1,NBIV
      IF(JX.GT.NB) GO TO 110
      JX=JX+1
      XJ=JX
      F=FNORM
      IF(N.EQ.1) F=FNORM*(XJ-X1)
      IF(N.EQ.NBIV) F=FNORM*(X2-(XJ-1.0))
      IF(JX.LT.1) GO TO 60
      IF(JX.GT.NB) GO TO 110
      JDX=JX+JOF
      XBUF(JDX)=XBUF(JDX)+F*CTS
   60 CONTINUE
  100 CONTINUE
  110 CONTINUE
C   
C     ------------------------------------------------------------------
C     LOAD IT BACK IN TO BUFFER FROM WHENCE IT CAME
C     ------------------------------------------------------------------
C   
      DO 120 I=1,NB
      IDX=I+IOF
      JDX=I+JOF
      XBUF(IDX)=XBUF(JDX)
  120 CONTINUE
C   
C     ------------------------------------------------------------------
C     SET NEW VALUE OF SPK LENGTH
C     ------------------------------------------------------------------
C   
      IF(IB.EQ.1) NCHI=NB
      IF(IB.EQ.2) NCHJ=NB
      CALL PUSPUL(IPUL,JB)
      RETURN
      END
