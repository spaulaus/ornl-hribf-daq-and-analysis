C$PROG MEMIN     - Inputs data from memory-buffers 1/2
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE MEMIN(IB,KHEDF,KDATF,ILO,NCH,IERR)
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/MD00/ XBUF(33024),IOFF(2,2),KRUN,NUID,IPUS,IPUL
C     ------------------------------------------------------------------
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
      INTEGER*4 KHEDF(1),KDATF(1)
C
      SAVE
C
C     ==================================================================
C   
      IERR=0
      IHI=ILO+NCH-1
C   
      IF(IB.EQ.1) GO TO 100
      IF(IB.EQ.2) GO TO 200
                  GO TO 300
C   
  100 IF(NCHI.LE.0) GO TO 320
C   
      DO 110 I=1,32
      KHEDF(I)=IHEDF(I)
  110 CONTINUE
      IF(NCH.LE.0) RETURN
C   
      JHI=IHI
      IF(JHI.GT.NCHI) JHI=NCHI
      N=0
      DO 120 I=ILO,JHI
      N=N+1
      ADD=0.5
      IF(ADAT(I).LT.0.0) ADD=-0.5
      KDATF(N)=ADAT(I)+ADD
  120 CONTINUE
      RETURN
C   
  200 IF(NCHJ.LE.0) GO TO 320
C   
      DO 210 I=1,32
      KHEDF(I)=JHEDF(I)
  210 CONTINUE
      IF(NCH.LE.0) RETURN
C   
      JHI=IHI
      IF(JHI.GT.NCHJ) JHI=NCHJ
      N=0
      DO 220 I=ILO,IHI
      N=N+1
      ADD=0.5
      IF(BDAT(I).LT.0.0) ADD=-0.5
      KDATF(N)=BDAT(I)+ADD
  220 CONTINUE
      RETURN
C   
  300 WRITE(CMSSG,310)
  310 FORMAT('ILLEGAL MEMORY BUFFER REQUEST')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
C   
  320 WRITE(CMSSG,330)IB
  330 FORMAT('NO DATA IN REQUESTED MEMORY BUFFER -',I4)
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
