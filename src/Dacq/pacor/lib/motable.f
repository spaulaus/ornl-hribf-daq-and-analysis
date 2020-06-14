C$PROG MOTABLE
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 06/02/2001
C                             for CAEN support
C     ************************************************************
C
      SUBROUTINE MOTABLE
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
      PARAMETER (MXI=2000)
      PARAMETER (MXR=10)
C
      COMMON/PAC1/ KINMOD,MODKOD,NAMOD(3),MODATA(2,12)
C
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C
      COMMON/PAC4/ LATN(MXI),MASK(MXI),KINS(MXI),
     &             TBLI(2,MXI),TBLN(2,MXI),NXTI(2,MXI),
     &             NUNDX(MXI),NTS
C
      COMMON/PAC5/ LABL(MXI),INST(MXI),NAMD(3,MXI),INDX(MXI),
     &             MSKI(MXI),IDES(MXI),NREA(MXI),ILOR(MXI),
     &             IHIR(MXI),JSORL(MXI),JEXPL(MXI),NCI
C
      COMMON/PAC6/ LABLIS(2,MXI),LABVAL(MXI),NLA 
C
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C
      COMMON/PAC8/ KLASID(50),KLASND(50),KLASMU(50),NKLAS,LKIND
C
      COMMON/PAC9/ NAMP(3,9),LATW(200,9),BITN(200,9),NPB(9),NPAT
C
      COMMON/PACA/ JCRA(MXI),JSLO(MXI),JSUB(MXI),JFRE(MXI),
     &             JIDN(MXI),NNAF
C
      COMMON/PACB/ CNAFI(200),DATAI(200),NNAFI,
     &             CNAFQ(200),DATAQ(200),NNAFQ,
     &             CNAFR(200),DATAR(200),NNAFR
C
      COMMON/PACD/ FERID(32,576),FERC(576),FERN(576),FERT(576),NFER,
     &             FERERID
C
      COMMON/PACE/ FASID(256,32),FASC(32), FASN(32), FAST(32), NFAS,
     &             FASERID
C
      COMMON/PACF/ CLRC(200),CLRN(200),CLRA(200),CLRF(200),NCLR
C
      COMMON/PACG/ CRATLST(50),NCRAT
C
      COMMON/PACN/ CAMID(32,32,8),CAMC(256),CAMN(256),CAMT(256),NCAM,
     &             CAMERID
C     ------------------------------------------------------------------
      COMMON/PACX/ XIAC(64),XIAN(64),XIAVSN(64),XIAGRP(64),NXIA,MXXIA
      INTEGER*4    XIAC,    XIAN,    XIAVSN,    XIAGRP,    NXIA,MXXIA
C     ------------------------------------------------------------------
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      BYTE BYTTST(4)
C
      EQUIVALENCE (IWDTST,BYTTST)
C
      CHARACTER*4  KIMO
C
      SAVE
C
C     ************************************************************
C     SET DEFAULT (ERROR) PARAMETER ID'S FOR FASTBUS & FERA
C     AND LOAD ID-TABLES WITH DEFAULT ID'S 
C     ************************************************************
C
      MAXID=0
      DO 30 I=1,NUMT
C
      IF(CAMERID.GT.0.AND.CAMERID.EQ.IDNM(I)) THEN
      WRITE(CMSSG,10)CAMERID
   10 FORMAT('SPECIFIED CAMAC ERROR-ID SAME AS NORMAL-ID =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
                                              ENDIF
C
      IF(FERERID.GT.0.AND.FERERID.EQ.IDNM(I)) THEN
      WRITE(CMSSG,15)FERERID
   15 FORMAT('SPECIFIED FERA ERROR-ID SAME AS NORMAL-ID =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
                                              ENDIF
C
      IF(FASERID.GT.0.AND.FASERID.EQ.IDNM(I)) THEN
      WRITE(CMSSG,20)FASERID
   20 FORMAT('SPECIFIED FASTBUS ERROR-ID SAME AS NORMAL-ID =',I8)
      CALL ERRLOG(LOGUT,LOGUP)
                                              ENDIF
      IF(IDNM(I).GT.MAXID) MAXID=IDNM(I)
   30 CONTINUE
C
      MAXXID=MAXID
      IF(CAMERID.GT.MAXXID) MAXXID=CAMERID
      IF(FASERID.GT.MAXXID) MAXXID=FASERID
      IF(FERERID.GT.MAXXID) MAXXID=FERERID
C
      IF(CAMERID.LE.0) CAMERID=MAXXID+1
      IF(FASERID.LE.0) FASERID=MAXXID+2
      IF(FERERID.LE.0) FERERID=MAXXID+3
C
      DO 50 K=1,8
      DO 45 J=1,32
      DO 40 I=1,32
      CAMID(I,J,K)=CAMERID
   40 CONTINUE
   45 CONTINUE
   50 CONTINUE
C
      DO 70 J=1,576
      DO 65 I=1,32
      FERID(I,J)=FERERID
   65 CONTINUE
   70 CONTINUE
C
      DO 80 J=1,32
      DO 75 I=1,256
      FASID(I,J)=FASERID
   75 CONTINUE
   80 CONTINUE
C
C     ************************************************************
C     BUILD CAMAC, FERA & FASTBUS MODULE TABLES AND
C     LOAD SPECIFIED ID'S INTO ID-TABLES
C     ************************************************************
C
      NCAM=0
      DO 100 I=1,NUMT
      IF(KIMO(I).NE.'$CAM') GO TO 100
      CALL CNTADD(CRAT(I),SLOT(I),MOTY(1,I),CAMC,CAMN,CAMT,NCAM)
      II=SUBA(I)+1
      JJ=SLOT(I)+1
      KK=CRAT(I)+1
      CAMID(II,JJ,KK)=IDNM(I)
  100 CONTINUE
C
      NFER=0
      DO 150 I=1,NUMT
      IF(KIMO(I).NE.'$FER') GO TO 150
      CALL CNTADD(CRAT(I),SLOT(I),MOTY(1,I),FERC,FERN,FERT,NFER)
      II=SUBA(I)+1
      JJ=32*CRAT(I)+SLOT(I)
      FERID(II,JJ)=IDNM(I)
  150 CONTINUE
C
      NFAS=0
      DO 200 I=1,NUMT
      IF(KIMO(I).NE.'$FAS') GO TO 200
      CALL CNTADD(CRAT(I),SLOT(I),MOTY(1,I),FASC,FASN,FAST,NFAS)
      II=SUBA(I)+1
      JJ=SLOT(I)+1
      FASID(II,JJ)=IDNM(I)
  200 CONTINUE
C
C     ************************************************************
C     BUILD CAMAC CLEAR-TABLES
C     ************************************************************
C
      NCLR=0
      DO 300 I=1,NUMT
C
      IF(KIMO(I).EQ.'$LAT') GO TO 250
      IF(KIMO(I).EQ.'$CAM') GO TO 250
      IF(KIMO(I).EQ.'$FER') GO TO 250
                            GO TO 300
C
  250 IF(FCLR(I).LT.0) GO TO 300
      IF(ACLR(I).EQ.-1) ACLR(I)=0
      IF(ACLR(I).LT.0) GO TO 300
C
      CALL CLRADD(CRAT(I),SLOT(I),ACLR(I),FCLR(I),
     &            CLRC,   CLRN,   CLRA,   CLRF,   NCLR)
C
  300 CONTINUE
C
C     ************************************************************
C     BUILD CAMAC CRATE-LIST
C     ************************************************************
C
      NCRAT=0
      DO 330 I=1,NUMT
C
      IF(KIMO(I).NE.'$LAT'.AND.
     &   KIMO(I).NE.'$CAM'.AND.
     &   KIMO(I).NE.'$FER') GO TO 330
C
      DO 320 J=1,NCRAT
      IF(CRAT(I).EQ.CRATLST(J)) GO TO 330
  320 CONTINUE
      NCRAT=NCRAT+1
      CRATLST(NCRAT)=CRAT(I)
  330 CONTINUE
C
      DO 350 I=1,NNAFI
      IWDTST=CNAFI(I)
      CTST=BYTTST(4)
      DO 340 J=1,NCRAT
      IF(CTST.EQ.CRATLST(J)) GO TO 350
  340 CONTINUE
      NCRAT=NCRAT+1
      CRATLST(NCRAT)=CTST
  350 CONTINUE
C
      DO 370 I=1,NNAFQ
      IWDTST=CNAFQ(I)
      CTST=BYTTST(4)
      DO 360 J=1,NCRAT
      IF(CTST.EQ.CRATLST(J)) GO TO 370
  360 CONTINUE
      NCRAT=NCRAT+1
      CRATLST(NCRAT)=CTST
  370 CONTINUE
C
      DO 390 I=1,NNAFR
      IWDTST=CNAFR(I)
      CTST=BYTTST(4)
      DO 380 J=1,NCRAT
      IF(CTST.EQ.CRATLST(J)) GO TO 390
  380 CONTINUE
      NCRAT=NCRAT+1
      CRATLST(NCRAT)=CTST
  390 CONTINUE
C
      DO 410 I=1,NXIA
      CTST=XIAC(I)
      DO 400 J=1,NCRAT
      IF(CTST.EQ.CRATLST(J)) GO TO 410
  400 CONTINUE
      NCRAT=NCRAT+1
      CRATLST(NCRAT)=CTST
  410 CONTINUE
C
C
      RETURN
      END
