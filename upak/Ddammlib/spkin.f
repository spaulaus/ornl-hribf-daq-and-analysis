C$PROG SPKIN     - Reads data from a spk-file - used by MILDO
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SPKIN(ID,NDX,MAXH,MAXD,IFXFL,IERR)
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/DIR/  KLOC(6),JHSP(4),LENG(4),ND,NHW,           !/DIR
     &             LENH,LENT,IOF,LDF,NHIS,LEND(4),           !/DIR
     &             LENS(4),MINC(4),MAXC(4),CONS(4),ITEX(20), !/DIR
     &             ITIT(10),LABX(3),LABY(3),MSER(10),KFILT   !/DIR 
C     ------------------------------------------------------------------
      COMMON/DML1/ NAMFIL(20,20),KFIL(20),LUC(20),LIN,LCM,LCI
      INTEGER*4    NAMFIL,                LUC,    LIN,LCM,LCI
      CHARACTER*4                KFIL
C     ------------------------------------------------------------------
      COMMON/DML6/ ISIGNF
      CHARACTER*4  ISIGNF
C     ------------------------------------------------------------------
      COMMON/MD00/ XBUF(33024),IOFF(2,2),KRUN,NUID,IPUS,IPUL
C     ------------------------------------------------------------------
      COMMON/MD01/ KODFIL
C     ------------------------------------------------------------------
C   
      DIMENSION ADAT(16384),BDAT(16384)
      INTEGER*4 IDATF(16384),JDATF(16384),IHEDF(128),JHEDF(128)
      INTEGER*4 KDATF(8192),NDX(4)
      INTEGER*2 IDATH(16384),JDATH(16384),IHEDH(256),JHEDH(256)
C   
      EQUIVALENCE (IHEDF(1),XBUF(1)),(IHEDH(1),XBUF(1))
      EQUIVALENCE (IDATF(1),XBUF(129)),(ADAT(1),XBUF(129))
      EQUIVALENCE (IDATH(1),XBUF(8321))
      EQUIVALENCE (KDATF(1),XBUF(8321))
      EQUIVALENCE (JHEDF(1),XBUF(16513)),(JHEDH(1),XBUF(16513))
      EQUIVALENCE (JDATF(1),XBUF(16641)),(BDAT(1),XBUF(16641))
      EQUIVALENCE (JDATH(1),XBUF(24833))
      EQUIVALENCE (IDI,IHEDF(1)),(IDJ,JHEDF(1))
      EQUIVALENCE (NCHI,IHEDF(12)),(NCHJ,JHEDF(12))
C
      CHARACTER*4  KINF,IFXFL
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      KODE=KODFIL
      CALL LUGET(KODE,LUH,LUD,KINF,IERR)
      LUS=LUH
C   
      IF(KINF.NE.'    ') GO TO 20
C   
   10 WRITE(CMSSG,12)
   12 FORMAT('INPUT FILE NOT ASSIGNED')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
C   
   20 IF(KINF.EQ.'SPK ') GO TO 200
      IF(KINF.EQ.'HIS ') GO TO 300
      GO TO 10
C   
C     ------------------------------------------------------------------
C     IT A SPK-FILE - READ HEADER TO GET # OF CHANNELS
C     ------------------------------------------------------------------
C   
  200 CALL SPKIO(1,LUS,ID,IHEDH,MAXH,KDATF,NDX,0,IERR)
      CALL SPKERR(IERR)
      IF(IERR.NE.0) RETURN
      NCH=NCHI
      CALL SPKIO(1,LUS,ID,IHEDH,MAXH,IDATF,NDX,NCH,IERR)
      CALL SPKERR(IERR)
      IF(IERR.NE.0) RETURN
      GO TO 410
C   
C     ------------------------------------------------------------------
C     IT IS A HIS-FILE - READ DIRECTORY TO GET # CHANNELS & # HWDS/CH
C     ------------------------------------------------------------------
C   
  300 CALL HISIN(LUD,LUH,ID,NDX,0,IDATF,IERR)
      CALL HISERR(IERR)
      IF(IERR.NE.0) RETURN
      NCH=LENG(1)
      GO TO (310,350),NHW
  310 CALL HISIN(LUD,LUH,ID,NDX,NCH,KDATF,IERR)
      CALL HISERR(IERR)
      IF(IERR.NE.0) RETURN
      IF(ISIGNF.NE.'USDA') GO TO 330
C   
      DO 320 I=1,NCH
      IDAF=IDATH(I)
      IDATF(I)=IAND(IDAF,Z'FFFF')
  320 CONTINUE
      GO TO 400
C   
  330 DO 340 I=1,NCH
      IDATF(I)=IDATH(I)
  340 CONTINUE
      GO TO 400
  350 CALL HISIN(LUD,LUH,ID,NDX,NCH,IDATF,IERR)
      CALL HISERR(IERR)
      IF(IERR.NE.0) RETURN
C   
  400 CALL HEDZOT(IHEDF)
      DO 404 I=1,3
      IHEDF(I+1)=LABX(I)
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
      IHEDF(10)=2*NCH
      NCHI=NCH
      IHEDF(13)=LEND(1)
      IHEDF(14)=LENS(1)
      IHEDF(15)=0
      IHEDF(16)=MINC(1)
      IHEDF(17)=MAXC(1)
  410 IF(IFXFL.EQ.'FIX ') RETURN
      NCH=NCHI
      DO 420 I=1,NCH
      ADAT(I)=FLOAT(IDATF(I))
  420 CONTINUE
      IF(KRUN.GT.1) CALL CRUNCH(1,KRUN)
      RETURN
      END
