C$PROG GATE      - Generates gates on 2-D data
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE GATE(KGAT,ID,ILO,IHI,IERR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/DML1/ NAMFIL(20,20),KFIL(20),LUC(20),LIN,LCM,LCI
      INTEGER*4    NAMFIL,                LUC,    LIN,LCM,LCI
      CHARACTER*4                KFIL
C     ------------------------------------------------------------------
      COMMON/DIR/  KLOC(6),JHSP(4),LENG(4),ND,NHW,           !/DIR
     &             LENH,LENT,IOF,LDF,NHIS,LEND(4),           !/DIR
     &             LENS(4),MINC(4),MAXC(4),CONS(4),ITEX(20), !/DIR
     &             ITIT(10),LABX(3),LABY(3),MSER(10),KFILT   !/DIR 
C   
C     ------------------------------------------------------------------
      COMMON/MD00/ XBUF(33024),IOFF(2,2),KRUN,NUID,IPUS,IPUL
C     ------------------------------------------------------------------
      COMMON/MD01/ KODFIL
C     ------------------------------------------------------------------
      DIMENSION ADAT(16384),BDAT(16384)
      INTEGER*4 IDATF(16384),JDATF(16384),IHEDF(128),JHEDF(128)
      INTEGER*2 IDATH(16384),JDATH(16384),IHEDH(256),JHEDH(256)
      INTEGER*2 KDATH(16384)
C   
      EQUIVALENCE (IHEDF(1),XBUF(1)),(IHEDH(1),XBUF(1))
      EQUIVALENCE (IDATF(1),XBUF(129)),(ADAT(1),XBUF(129))
      EQUIVALENCE (IDATH(1),XBUF(8321))
      EQUIVALENCE (JHEDF(1),XBUF(16513)),(JHEDH(1),XBUF(16513))
      EQUIVALENCE (JDATF(1),XBUF(16641)),(BDAT(1),XBUF(16641))
      EQUIVALENCE (JDATH(1),XBUF(24833))
      EQUIVALENCE (IDI,IHEDF(1)),(IDJ,JHEDF(1))
      EQUIVALENCE (NCHI,IHEDF(12)),(NCHJ,JHEDF(12))
      EQUIVALENCE (IDATF(1),KDATH(1))
C
      CHARACTER*4  KGAT,KINF
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      IERR=0
C   
      KODE=KODFIL
      CALL LUGET(KODE,LUH,LUD,KINF,IERR)
C   
      IF(IERR.EQ.0.AND.KINF.EQ.'HIS ') GO TO 20
C   
      WRITE(CMSSG,10)
   10 FORMAT('REQUIRED INPUT HIS-FILE NOT OPEN')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
C   
C     ------------------------------------------------------------------
C     SAVE "JDAT" - - USED AS ACCUMULATION BUFFER
C     "GATE" WILL BE RETURNED IN "IDAT"
C     ------------------------------------------------------------------
C   
   20 CALL PUSPUL(IPUS,2)
C   
      DO 30 I=1,16384
      IDATF(I)=0
      JDATF(I)=0
   30 CONTINUE
C   
C     ------------------------------------------------------------------
C     INIT SLICER TO GET HISTOGRAM ATTRIBUTES
C     ------------------------------------------------------------------
C   
      CALL SLICER(LUD,LUH,ID,0,IDATF,IERR)
      IF(IERR.NE.0) GO TO 500
      NCH=MAXC(1)+1
      MCH=MAXC(2)+1
      IF(KGAT.EQ.'X   ') GO TO 200
      IF(KGAT.EQ.'Y   ') GO TO 100
C   
      WRITE(CMSSG,50)
   50 FORMAT('UNRECOGNIZED GATE TYPE')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      GO TO 500
C   
C     ------------------------------------------------------------------
C     GATE ON 'Y' - 2ND PARAMETER - "FAST DIRECTION"
C     ------------------------------------------------------------------
C   
  100 DO 150 JY=ILO,IHI
      CALL SLICER(LUD,LUH,ID,JY,IDATF,IERR)
      IF(IERR.NE.0) GO TO 500
C   
      DO 120 I=1,NCH
      JDATF(I)=JDATF(I)+IDATF(I)
  120 CONTINUE
  150 CONTINUE
      GO TO 300
C   
C     ------------------------------------------------------------------
C     GATE IS ON "X" - 1ST PARAMETER - "SLOW DIRECTION"
C     ------------------------------------------------------------------
C   
  200 DO 250 JY=1,MCH
      CALL SLICER(LUD,LUH,ID,JY,IDATF,IERR)
      IF(IERR.NE.0) GO TO 500
C   
      ISUM=0
      DO 240 I=ILO,IHI
      ISUM=ISUM+IDATF(I)
  240 CONTINUE
      JDATF(JY)=ISUM
  250 CONTINUE
C   
C     ------------------------------------------------------------------
C     LOAD "GATE" INTO "IDAT", SET UP HEADER & RESTORT "JDAT"
C     ------------------------------------------------------------------
C   
  300 NC=NCH
      IF(KGAT.EQ.'X   ') NC=MCH
      DO 310 I=1,NC
      ADAT(I)=FLOAT(JDATF(I))
  310 CONTINUE
C
      IST=NC+1
      DO 320 I=IST,16384
      ADAT(I)=0.0
  320 CONTINUE
C   
      CALL HEDZOT(IHEDF)
      DO 404 I=1,3
      IHEDF(I+1)=LABX(I)
      IF(KGAT.EQ.'X   ') IHEDF(I+1)=LABY(I)
      XBUF(I+17)=CONS(I)
  404 CONTINUE
      XBUF(21)=CONS(4)
      DO 406 I=1,6
      IHEDH(I+8)=KLOC(I)
  406 CONTINUE
      DO 408 I=1,10
      IHEDF(I+22)=ITIT(I)
  408 CONTINUE
C   
      IDI=ID
      IHEDF(8)=4
      IHEDF(9)=64
      IHEDF(10)=2*NC
      IHEDF(11)=1
      NCHI=NC
      IHEDF(13)=LEND(1)
      IHEDF(14)=LENS(1)
      IF(KGAT.EQ.'X   ') IHEDF(13)=LEND(2)
      IF(KGAT.EQ.'X   ') IHEDF(14)=LENS(2)
      IHEDF(15)=0
      IF(KRUN.GT.1) CALL CRUNCH(1,KRUN)
  500 CALL PUSPUL(IPUL,2)
      RETURN
      END
