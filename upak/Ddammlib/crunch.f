C$PROG CRUNCH    - Sums channels together (for 1-D data)
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 04/30/90
C     ******************************************************************
C
      SUBROUTINE CRUNCH(JB,ICRUN)
C   
      COMMON/MD00/ XBUF(33024),IOFF(2,2),KRUN,NUID,IPUS,IPUL
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
C     ------------------------------------------------------------------
C     ROUTINE TO SUM CHANNELS TOGETHER
C     ------------------------------------------------------------------
C   
      IF(ICRUN.LE.1) RETURN
      NCH=NCHI
      IF(JB.EQ.2) NCH=NCHJ
      NDO=NCH/ICRUN
      IF(ICRUN*NDO.LT.NCH) NDO=NDO+1
      N=0
      DO 50 I=1,NDO
      SUM=0.0
      DO 20 J=1,ICRUN
      N=N+1
      IF(N.GT.NCH) GO TO 30
      IF(JB.EQ.2) GO TO 10
      SUM=SUM+ADAT(N)
      GO TO 20
   10 SUM=SUM+BDAT(N)
   20 CONTINUE
   30 GO TO (35,40),JB
   35 ADAT(I)=SUM
      GO TO 50
   40 BDAT(I)=SUM
   50 CONTINUE
      GO TO (60,80),JB
   60 NCHI=NDO
      IHEDF(10)=2*NDO
      IHEDF(14)=IHEDF(14)/ICRUN
      RETURN
   80 NCHJ=NDO
      JHEDF(10)=2*NDO
      JHEDF(14)=JHEDF(14)/ICRUN
      RETURN
      END
