C$PROG CMPMILDO  - Command processor for mildo   operations
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE CMPMILDO(IDONE,IERR)
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/DML1/ NAMFIL(20,20),KFIL(20),LUC(20),LIN,LCM,LCI
      INTEGER*4    NAMFIL,                LUC,    LIN,LCM,LCI
      CHARACTER*4                KFIL
C     ------------------------------------------------------------------
      COMMON/MD00/ XBUF(33024),IOFF(2,2),KRUN,NUID,IPUS,IPUL
C     ------------------------------------------------------------------
      COMMON/MD01/ KODFIL
C     ------------------------------------------------------------------
      REAL*8 SUM,SUMA,SUMB
C
      INTEGER*4 KBUF(33024)
      INTEGER*4 LIST(78),NDX(4)
C     ------------------------------------------------------------------
      CHARACTER*4  IDONE,KMD
C   
      DIMENSION XGS(4),KMDC(4),GSX(4),QBUF(6)
      DIMENSION ADAT(16384),BDAT(16384)
C   
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
      EQUIVALENCE (KMD,LWD(1,1))
      EQUIVALENCE (LIST(1),LWD(1,2))
      EQUIVALENCE (KBUF(1),XBUF(1))
C   
      DATA NCALL,NUID,NCGS,NCYC,NCFS,JPRZ,IHI,IDSTEP/7*0,1/
C   
      integer*4 iflo
      character*4 ciflo, cipus, cipul
      equivalence (ciflo, iflo), (cipus, ipus), (cipul,ipul)
      DATA cIPUS,cIPUL,cIFLO/'PUSH','PULL','FLO '/
C
      CHARACTER*4  KGAT,KIPL,KACU,KODFIL
C
      INTEGER*4    X41,X53,X4F
C
      DATA         X41,X53,X4F/Z'41',Z'53',Z'4F'/
C
      EXTERNAL     UNPACK
C
      SAVE
C   
C     ------------------------------------------------------------------
C     XBUF STRUCTURE
C   
C     --FULL-WORD--
C         1 -   128  -- IHED  -   128 FULL-WORDS
C       129 - 16512  -- IDATF - 16384 FULL-WDS
C      8321 - 16512  -- IDATH -  8192 FULL-WORDS
C     16513 - 16640  -- JHED  -   128 FULL-WORDS
C     16641 - 32024  -- JDATA - 16384 FULL-WORDS
C     24833 - 32024  -- JDATH -  8192 FULL-WORDS
C     ------------------------------------------------------------------
C   
      IF(NCALL.NE.0) GO TO 20
      NCALL=1
C   
      CALL HEDZOT(IHEDF)
      CALL HEDZOT(JHEDF)
C   
      IOFF(1,1)=0             !FULL-WD OFFSET OF HEADER-1 IN XBUF
      IOFF(2,1)=128           !FULL-WD OFFSET OF DATA-1   IN XBUF
      IOFF(1,2)=16512         !FULL-WD OFFSET OF HEADER-2 IN XBUF
      IOFF(2,2)=16640         !FULL-WD OFFSET OF DATA-2   IN XBUF
C   
      DO 10 I=1,4
      NDX(I)=1
      XGS(I)=0.0
   10 CONTINUE
C   
   20 IERR=0
      IDONE='    '
C   
C     ------------------------------------------------------------------
C     PROCESS COMMAND
C     ------------------------------------------------------------------
C   
      IF(KMD.EQ.'IDST') GO TO 200
      IF(KMD.EQ.'NUID') GO TO 210
      IF(KMD.EQ.'PLRZ') GO TO 220
      IF(KMD.EQ.'SKRZ') GO TO 230
C   
      IF(KMD.EQ.'GASP') GO TO 360
      IF(KMD.EQ.'CRUN') GO TO 370
C   
      IF(KMD.EQ.'GEN ') GO TO 420
      IF(KMD.EQ.'COMP') GO TO 430
      IF(KMD.EQ.'M1  ') GO TO 460
      IF(KMD.EQ.'M2  ') GO TO 460
      IF(KMD.EQ.'C1  ') GO TO 500
      IF(KMD.EQ.'C2  ') GO TO 500
      IF(KMD.EQ.'Z1  ') GO TO 540
      IF(KMD.EQ.'Z2  ') GO TO 560
      IF(KMD.EQ.'S1  ') GO TO 580
      IF(KMD.EQ.'S2  ') GO TO 580
C
      IF(KMD.EQ.'O1  ') GO TO 605
      IF(KMD.EQ.'O2  ') GO TO 610
      IF(KMD.EQ.'MX12') GO TO 615
      IF(KMD.EQ.'MX21') GO TO 620
      IF(KMD.EQ.'MN12') GO TO 625
      IF(KMD.EQ.'MN21') GO TO 630
C
      IF(KMD.EQ.'A12 ') GO TO 660
      IF(KMD.EQ.'A21 ') GO TO 660
      IF(KMD.EQ.'SWAP') GO TO 700
      IF(KMD.EQ.'M2D1') GO TO 720
C   
      IF(KMD.EQ.'I   ') GO TO 800
      IF(KMD.EQ.'IS  ') GO TO 800
      IF(KMD.EQ.'IA  ') GO TO 800
      IF(KMD.EQ.'ISA ') GO TO 800
      IF(KMD.EQ.'IO  ') GO TO 850
      IF(KMD.EQ.'ISO ') GO TO 850
C   
      IF(KMD.EQ.'GY  ') GO TO 1000
      IF(KMD.EQ.'GYS ') GO TO 1000
      IF(KMD.EQ.'GYA ') GO TO 1000
      IF(KMD.EQ.'GYSA') GO TO 1000
      IF(KMD.EQ.'GYO ') GO TO 1000
      IF(KMD.EQ.'GYSO') GO TO 1000
C   
      IF(KMD.EQ.'GX  ') GO TO 1010
      IF(KMD.EQ.'GXS ') GO TO 1010
      IF(KMD.EQ.'GXA ') GO TO 1010
      IF(KMD.EQ.'GXSA') GO TO 1010
      IF(KMD.EQ.'GXO ') GO TO 1010
      IF(KMD.EQ.'GXSO') GO TO 1010
C   
      IF(KMD.EQ.'PJ  ') GO TO 1015
      IF(KMD.EQ.'PJS ') GO TO 1015
      IF(KMD.EQ.'PJA ') GO TO 1015
      IF(KMD.EQ.'PJSA') GO TO 1015
      IF(KMD.EQ.'PJO ') GO TO 1015
      IF(KMD.EQ.'PJSO') GO TO 1015
C   
      IF(KMD.EQ.'SET1') GO TO 1200
      IF(KMD.EQ.'SET2') GO TO 1200
      IF(KMD.EQ.'ADD1') GO TO 1200
      IF(KMD.EQ.'ADD2') GO TO 1200
C
      IF(KMD.EQ.'MSK1') GO TO 1310
      IF(KMD.EQ.'MSK2') GO TO 1310
C   
      IF(KMD.EQ.'PR1 ') GO TO 1550
      IF(KMD.EQ.'PR2 ') GO TO 1550
      IF(KMD.EQ.'D1  ') GO TO 1550
      IF(KMD.EQ.'D2  ') GO TO 1550
C   
      IF(KMD.EQ.'PLG1') GO TO 1600
      IF(KMD.EQ.'PLG2') GO TO 1602
      IF(KMD.EQ.'PLN1') GO TO 1600
      IF(KMD.EQ.'PLN2') GO TO 1602
      IF(KMD.EQ.'PLG ') GO TO 1650
      IF(KMD.EQ.'PLN ') GO TO 1652
C   
      IF(KMD.EQ.'SUM1') GO TO 1800
      IF(KMD.EQ.'SUM2') GO TO 1802
C   
      IF(KMD.EQ.'PJAL') GO TO 1900
C   
      RETURN
C   
C     READ IN IDSTEP TO USE IN IMPLIED LOOPS ***********************
C   
  200 CALL IVALU(LIST,IDSTEP,IERR)
      IF(IERR.NE.0) GO TO 2000
      IF(IDSTEP.LE.0) IDSTEP=1
      GO TO 2500
C   
C     READ IN NEXT SEQUENCE NUMBER TO USE - NUID *******************
C   
  210 CALL IVALU(LIST,NUID,IERR)
      IF(IERR.NE.0) GO TO 2000
      GO TO 2500
C   
C     SET REPEATED-ZERO-FLAG FOR PRINTER PLOTS *********************
C   
  220 JPRZ=1
      GO TO 2500
  230 JPRZ=2
      GO TO 2500
C   
C     READ IN GAIN SHIFT PARAMETERS ********************************
C   
  360 JJ=1
      DO 362 I=1,4
      CALL MILV(LIST(JJ),IDUM,XGS(I),KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
      JJ=JJ+2
  362 CONTINUE
      CALL MILV(LIST(JJ),NCGS,XDUM,KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
      GO TO 2500
C   
C     READ IN "STANDARD CRUNCH" SPECIFICATION **********************
C   
  370 CALL IVALU(LIST,KRUN,IERR)
      IF(IERR.NE.0) GO TO 2000
      GO TO 2500
C   
C     READ SPECS FOR GENERATED SPECTRUM - AND DO IT ****************
C   
  420 CALL IVALU(LIST(1),ID,IERR)
      IF(IERR.NE.0) GO TO 2000
      CALL IVALU(LIST(3),KO,IERR)
      IF(IERR.NE.0) GO TO 2000
      CALL IVALU(LIST(5),KX,IERR)
      IF(IERR.NE.0) GO TO 2000
      CALL IVALU(LIST(7),NC,IERR)
      IF(IERR.NE.0) GO TO 2000
      CALL HEDZOT(IHEDF)
      IHEDF(1)=ID
      IHEDF(8)=4
      IHEDF(9)=64
      IHEDF(10)=2*NC
      IHEDF(11)=1
      NCHI=NC
      DO 426 I=1,NC
      ADAT(I)=KO+KX*I
  426 CONTINUE
      CALL HEDCOP(IHEDF,IHEDF)
      GO TO 2500
C   
  430 CALL IVALU(LIST,NC,IERR)
      IF(IERR.NE.0) GO TO 2000
      NERR=0
      SUMA=0.0
      SUMB=0.0
      DO 434 I=1,NC
      SUMA=SUMA+ADAT(I)
      SUMB=SUMB+BDAT(I)
      IF(ADAT(I).NE.BDAT(I)) NERR=NERR+1
  434 CONTINUE
      WRITE(CMSSG,436)NC,SUMA,SUMB,NERR
  436 FORMAT('NC,SUMA,SUMB,NERR =',I5,2F15.2,I7)
      CALL MESSLOG(6,7)
      GO TO 2500
C   
C     MULTIPLY BUFFER BY A CONSTANT FACTOR *************************
C   
  460 CALL MILV(LIST,IDUM,XM,KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
      IF(KMD.EQ.'M2  ') GO TO 480
      NDO=NCHI
      DO 470 I=1,NDO
      ADAT(I)=XM*ADAT(I)
  470 CONTINUE
      CALL HEDCOP(IHEDF,IHEDF)
      GO TO 2500
C   
  480 NDO=NCHJ
      DO 490 I=1,NDO
      BDAT(I)=XM*BDAT(I)
  490 CONTINUE
      CALL HEDCOP(JHEDF,JHEDF)
      GO TO 2500
C   
C     "CRUNCH" BY SUMMING ICRUN CHANNELS TOGETHER ******************
C   
  500 CALL IVALU(LIST,ICRUN,IERR)
      IF(IERR.NE.0) GO TO 2000
C   
      IF(KMD.EQ.'C1  ') JB=1
      IF(KMD.EQ.'C2  ') JB=2
      CALL CRUNCH(JB,ICRUN)
      GO TO 2500
C   
C     SET BUFFER TO ZERO *******************************************
C   
  540 CALL HEDZOT(IHEDF)
      DO 550 I=1,16384
      ADAT(I)=0.0
  550 CONTINUE
      GO TO 2500
C   
  560 CALL HEDZOT(JHEDF)
      DO 570 I=1,16384
      BDAT(I)=0.0
  570 CONTINUE
      GO TO 2500
C   
C     DO A MIGHTY GAIN SHIFT ***************************************
C   
  580 IF(NF.GT.4) GO TO 590
      NCH=NCGS
      DO 585 I=1,4
      GSX(I)=XGS(I)
  585 CONTINUE
      GO TO 600
C   
  590 JJ=1
      DO 592 I=1,4
      CALL MILV(LIST(JJ),IDUM,GSX(I),KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
      JJ=JJ+2
  592 CONTINUE
      CALL MILV(LIST(JJ),NCH,XDUM,KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
C   
  600 IF(KMD.EQ.'S1  ') CALL REBIN(1,GSX,NCH,IERR)
      IF(KMD.EQ.'S2  ') CALL REBIN(2,GSX,NCH,IERR)
      GO TO 2500
C   
C     OUTPUT SOMEBODY **********************************************
C   
  605 CALL SPKOUT(1,IERR)
      GO TO 2500
C   
  610 CALL SPKOUT(2,IERR)
      GO TO 2500
C
C     PROCESS - MX12, MX21, MN12, MN21 *****************************
C
  615 NDO=NCHI
      IF(NCHI.GT.NCHJ) NCHJ=NCHI
      DO 616 I=1,NDO
      IF(ADAT(I).GT.BDAT(I)) BDAT(I)=ADAT(I)
  616 CONTINUE
      CALL HEDCOP(IHEDF,JHEDF)
      GO TO 2500
C
  620 NDO=NCHJ
      IF(NCHJ.GT.NCHI) NCHI=NCHJ
      DO 621 I=1,NDO
      IF(BDAT(I).GT.ADAT(I)) ADAT(I)=BDAT(I)
  621 CONTINUE
      CALL HEDCOP(JHEDF,IHEDF)
      GO TO 2500
C
  625 NDO=NCHI
      IF(NCHI.GT.NCHJ) NCHJ=NCHI
      DO 626 I=1,NDO
      IF(ADAT(I).LT.BDAT(I)) BDAT(I)=ADAT(I)
  626 CONTINUE
      CALL HEDCOP(IHEDF,JHEDF)
      GO TO 2500
C
  630 NDO=NCHJ
      IF(NCHJ.GT.NCHI) NCHI=NCHJ
      DO 631 I=1,NDO
      IF(BDAT(I).LT.ADAT(I)) ADAT(I)=BDAT(I)
  631 CONTINUE
      CALL HEDCOP(JHEDF,IHEDF)
      GO TO 2500
C   
C     ADD FAC*ADAT TO BDAT OR FAC*BDAT TO ADAT *********************
C   
  660 FAC=1.0
      IF(NF.LE.1) GO TO 662
      CALL MILV(LIST,IDUM,FAC,KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
C   
  662 IF(KMD.EQ.'A21 ') GO TO 680
      NDO=NCHI
      IF(NCHI.GT.NCHJ) NCHJ=NCHI
      DO 670 I=1,NDO
      BDAT(I)=BDAT(I)+FAC*ADAT(I)
  670 CONTINUE
      CALL HEDCOP(IHEDF,JHEDF)
      GO TO 2500
C   
  680 NDO=NCHJ
      IF(NCHJ.GT.NCHI) NCHI=NCHJ
      DO 690 I=1,NDO
      ADAT(I)=ADAT(I)+FAC*BDAT(I)
  690 CONTINUE
      CALL HEDCOP(JHEDF,IHEDF)
      GO TO 2500
C   
C     SWAP BUFFERS (I & J) (A & B) (1 & 2) ALL SAME ****************
C   
  700 CALL PUSPUL(IPUS,1)
      DO 710 I=1,16512
      KBUF(I)=KBUF(I+16512)
  710 CONTINUE
      CALL PUSPUL(IPUL,2)
      GO TO 2500
C   
C     BDAT = FAC * BDAT / ADAT *************************************
C   
  720 CALL MILV(LIST,IDUM,FAC,KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
      NDO=NCHJ
      DO 725 I=1,NDO
      BDAT(I)=FAC*BDAT(I)
  725 CONTINUE
      CALL HEDCOP(JHEDF,JHEDF)
      MDO=NCHI
      IF(MDO.GT.NDO) MDO=NDO
      DO 730 I=1,MDO
      DIV=ADAT(I)
      IF(DIV.LT.1.0E-8) DIV=1.0E-8
      BDAT(I)=BDAT(I)/DIV
  730 CONTINUE
      CALL HEDCOP(JHEDF,JHEDF)
      GO TO 2500
C   
C     DO AN INPUT TO ADAT ******************************************
C   
  800 CALL LOCCOD(LIST,NF)
C
      CALL MILV(LIST(1),ID,XDUM,KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
      CALL MILV(LIST(3),IDUM,FAC,KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
C   
      DO 804 I=1,4
      NDX(I)=1
  804 CONTINUE
C   
      CALL SPKIN(ID,NDX,256,16384,IFLO,IERR)
      IF(IERR.NE.0) GO TO 2500
C   
      IF(KMD.EQ.'I   ') GO TO 2500
      IF(KMD.EQ.'IA  ') GO TO 820
      CALL REBIN(1,XGS,NCGS,IERR)
      IF(IERR.NE.0) GO TO 2500
C   
      IF(KMD.EQ.'IS  ') GO TO 2500
  820 IF(NCHI.GT.NCHJ) NCHJ=NCHI
      NDO=NCHI
      IF(FAC.NE.0.0) GO TO 835
      DO 830 I=1,NDO
      BDAT(I)=BDAT(I)+ADAT(I)
  830 CONTINUE
      CALL HEDCOP(IHEDF,JHEDF)
      GO TO 2500
C   
  835 DO 840 I=1,NDO
      BDAT(I)=BDAT(I)+FAC*ADAT(I)
  840 CONTINUE
      CALL HEDCOP(IHEDF,JHEDF)
      GO TO 2500
C   
C     INPUT TO "ADAT" AND OUTPUT ***********************************
C   
C
  850 CALL LOCCOD(LIST,NF)
C
      IF(NF.EQ.3) GO TO 860
      IF(ITYP(2).EQ.1) GO TO 2500
      CALL IVALU(LIST,IDA,IERR)
      IF(IERR.NE.0) GO TO 2000
C   
      IDB=IDA
      GO TO 870
C   
  860 CALL IVALU(LIST(1),IDA,IERR)
      IF(IERR.NE.0) GO TO 2000
      CALL IVALU(LIST(3),IDB,IERR)
      IF(IERR.NE.0) GO TO 2000
C   
  870 DO 900 ID=IDA,IDB,IDSTEP
C   
      CALL SPKIN(ID,NDX,256,16384,IFLO,IERR)
      IF(IERR.NE.0) GO TO 2500
C   
      IF(KMD.NE.'ISO ') GO TO 880
      CALL REBIN(1,XGS,NCGS,IERR)
      IF(IERR.NE.0) GO TO 2500
C   
  880 CALL SPKOUT(1,IERR)
      IF(IERR.NE.0) GO TO 2500
C   
  900 CONTINUE
      GO TO 2500
C   
C     ------------------------------------------------------------------
C     PROCESS Y-GATE, X-GATE OR PROJECTION
C     ------------------------------------------------------------------
C   
 1000 KGAT='Y   '
      GO TO 1020
 1010 KGAT='X   '
      GO TO 1020
 1015 KGAT='P   '
C   
 1020 CALL LOCCOD(LIST,NF)
C
 1022 JJ=1
      DO 1024 I=1,6
      CALL MILV(LIST(JJ),IDUM,QBUF(I),KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
      JJ=JJ+2
 1024 CONTINUE
C   
      CALL UNPACK(KMD,KMDC,4)
      KACU='    '
      IF(KMDC(3).EQ.X41.OR.KMDC(4).EQ.X41)KACU='ADD '
      IF(KGAT.EQ.'P   ') GO TO 1028
C   
C     IT IS A GATE - GX OR GY **************************************
C   
      MIDA=1
      MIDB=1
      NFMAX=5
      IF(KACU.EQ.'ADD ') NFMAX=6
      IF(NF.GE.NFMAX) GO TO 1026
C   
C     SET UP FOR G--- ID,ILO,IHI,FAC *******************************
C   
      IDA=QBUF(1)+0.5
      IDB=IDA
      ILO=QBUF(2)+1.5
      IHI=QBUF(3)+1.5
      FAC=QBUF(4)
      GO TO 1032
C   
C     SET UP FOR G--- IDA,IDB,ILO,IHI,FAC **************************
C   
 1026 IDA=QBUF(1)+0.5
      IDB=QBUF(2)+0.5
      ILO=QBUF(3)+1.5
      IHI=QBUF(4)+1.5
      FAC=QBUF(5)
      GO TO 1032
C   
C     IT IS A PROJECTION *******************************************
C   
 1028 NFMAX=6
      IF(KACU.EQ.'ADD ') NFMAX=7
      IF(NF.GE.NFMAX) GO TO 1030
C   
C     SET UP FOR PJ-- ID,MID,DEGA,FAC ******************************
C   
      IDA=QBUF(1)+0.5
      IDB=IDA
      MIDA=QBUF(2)+0.5
      MIDB=MIDA
      DEGA=QBUF(3)
      FAC=QBUF(4)
      GO TO 1032
C   
C     SET UP FOR PJ-- IDA,IDB,MIDA,MIDB,DEGA,FAC *******************
C   
 1030 IDA=QBUF(1)+0.5
      IDB=QBUF(2)+0.5
      MIDA=QBUF(3)+0.5
      MIDB=QBUF(4)+0.5
      DEGA=QBUF(5)
      FAC=QBUF(6)
C   
C     DO THE IMPLIED LOOPS ON ID'S AND MID'S ***********************
C   
 1032 DO 1120 IDM=MIDA,MIDB
      DO 1110 ID=IDA,IDB,IDSTEP
      IF(KGAT.NE.'P   ') CALL GATE(KGAT,ID,ILO,IHI,IERR)
      IF(KGAT.EQ.'P   ') CALL PROJE(ID,IDM,DEGA,IERR)
      IF(IERR.NE.0) GO TO 2500
C   
C     TST FOR TO BE GAIN SHIFTED (I.E. TEST FOR S) *****************
C   
      IF(KMDC(3).EQ.X53) CALL REBIN(1,XGS,NCGS,IERR)
      IF(IERR.NE.0) GO TO 2500
C   
C     TEST FOR "ADD" (I.E. TEST FOR A) *****************************
C   
      IF(KACU.EQ.'    ') GO TO 1100
      NCH=NCHI
      IF(NCHJ.LT.NCHI) NCHJ=NCHI
      IF(FAC.NE.0.0) GO TO 1050
      DO 1040 I=1,NCH
      BDAT(I)=BDAT(I)+ADAT(I)
 1040 CONTINUE
      CALL HEDCOP(IHEDF,JHEDF)
      GO TO 1110
C   
 1050 DO 1060 I=1,NCH
      BDAT(I)=BDAT(I)+FAC*ADAT(I)
 1060 CONTINUE
      CALL HEDCOP(IHEDF,JHEDF)
      GO TO 1110
C   
C     ARE WE TO DO "OUTPUT" (TEST FOR O) ***************************
C   
 1100 IF(KMDC(3).NE.X4F.AND.KMDC(4).NE.X4F) GO TO 1110
      CALL SPKOUT(1,IERR)
      IF(IERR.NE.0) GO TO 2500
C   
 1110 CONTINUE
 1120 CONTINUE
      GO TO 2500
C   
C     SET UP FOR SET1, SET2, ADD1, ADD2, MSK1, MSK2 CMDS ***********
C   
 1200 IF(NF.EQ.3) GO TO 1220
      IF(NF.EQ.4) GO TO 1230
      IF(NF.EQ.5) GO TO 1240
C   
 1210 WRITE(CMSSG,1212)
      CALL MESSLOG(LOGUT,LOGUP)
 1212 FORMAT('SYNTAX ERROR')
      GO TO 2500
C   
 1220 CALL MILV(LIST(1),ILOC,XDUM,KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
      CALL MILV(LIST(3),IDUM,YA,  KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
C   
      IHIC=ILOC
      YB=YA
      GO TO 1250
C   
 1230 CALL MILV(LIST(1),ILOC,XDUM,KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
      CALL MILV(LIST(3),IHIC,XDUM,KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
      CALL MILV(LIST(5),IDUM,YA,  KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
C   
      YB=YA
      GO TO 1250
C   
 1240 CALL MILV(LIST(1),ILOC,XDUM,KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
      CALL MILV(LIST(3),IHIC,XDUM,KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
      CALL MILV(LIST(5),IDUM,YA,  KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
      CALL MILV(LIST(7),IDUM,YB,  KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
C   
 1250 ILO=ILOC+1
      IHI=IHIC+1
      IF(ILO.LT.1)    GO TO 1210
      IF(IHI.GT.16384) GO TO 1210
      IF(ILO.GT.IHI)  GO TO 1210
      SLOPE=0.0
      IF(IHI.GT.ILO) SLOPE=(YB-YA)/FLOAT(IHI-ILO)
C   
      IF(KMD.EQ.'SET1') GO TO 1260
      IF(KMD.EQ.'SET2') GO TO 1270
      IF(KMD.EQ.'ADD1') GO TO 1280
      IF(KMD.EQ.'ADD2') GO TO 1290
      GO TO 1210
C   
 1260 DO 1265 I=ILO,IHI
      ADAT(I)=YA+SLOPE*FLOAT(I-ILO)
 1265 CONTINUE
      CALL HEDCOP(IHEDF,IHEDF)
      GO TO 2500
C   
 1270 DO 1275 I=ILO,IHI
      BDAT(I)=YA+SLOPE*FLOAT(I-ILO)
 1275 CONTINUE
      CALL HEDCOP(JHEDF,JHEDF)
      GO TO 2500
C   
 1280 DO 1285 I=ILO,IHI
      ADAT(I)=ADAT(I)+YA+SLOPE*FLOAT(I-ILO)
 1285 CONTINUE
      CALL HEDCOP(IHEDF,IHEDF)
      GO TO 2500
C   
 1290 DO 1295 I=ILO,IHI
      BDAT(I)=BDAT(I)+YA+SLOPE*FLOAT(I-ILO)
 1295 CONTINUE
      CALL HEDCOP(JHEDF,JHEDF)
      GO TO 2500
C
 1310 CALL MILV(LIST(1),ILOC,XDUM,KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
      CALL MILV(LIST(3),IHIC,XDUM,KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
      CALL HEXVAL(LIST(5),MSKV,IERR)
      IF(IERR.NE.0) GO TO 2000
      ILO=ILOC+1
      IHI=IHIC+1
      IF(ILO.LT.1)     GO TO 2000
      IF(IHI.GT.16384) GO TO 2000
      IF(ILO.GT.IHI)   GO TO 2000
C
      IF(KMD.EQ.'MSK2') GO TO 1320
C
      DO 1315 I=ILO,IHI
      ADD=0.5
      IF(ADAT(I).LT.0.0) ADD=-0.5
      ITMP=ADAT(I)+ADD
      ITMP=IAND(ITMP,MSKV)
      ADAT(I)=ITMP
 1315 CONTINUE
      GO TO 2500
C
 1320 DO 1325 I=ILO,IHI
      ADD=0.5
      IF(BDAT(I).LT.0.0) ADD=-0.5
      ITMP=BDAT(I)+ADD
      ITMP=IAND(ITMP,MSKV)
      BDAT(I)=ITMP
 1325 CONTINUE
      GO TO 2500
C   
C     LIST/DISPLAY MEMORY BUFFERS **********************************
C   
 1550 CALL IVALU(LIST(1),ILOC,IERR)
      IF(IERR.NE.0) GO TO 2000
      CALL IVALU(LIST(3),IHIC,IERR)
      IF(IERR.NE.0) GO TO 2000
C   
      CALL LISTUM(KMD,ILOC,IHIC)
      GO TO 2500
C   
C     ------------------------------------------------------------------
C     DO THE SLAP PLOT BIT
C     ------------------------------------------------------------------
C   
 1600 JB=1
      GO TO 1605
 1602 JB=2
 1605 IDXD=IOFF(2,JB)+1
      IDXH=IOFF(1,JB)+1
C   
      CALL IVALU(LIST(1),ILOC,IERR)
      IF(IERR.NE.0) GO TO 2000
      CALL IVALU(LIST(3),IHIC,IERR)
      IF(IERR.NE.0) GO TO 2000
      CALL IVALU(LIST(5),NCCC,IERR)
      IF(IERR.NE.0) GO TO 2000
C   
      ILO=ILOC+1
      MAXC=NCHI
      IF(JB.EQ.2) MAXC=NCHJ
      IF(IHIC.GT.0) IHI=IHIC+1
      IF(IHIC.LE.0) IHI=MAXC
      IF(IHI.GT.MAXC) IHI=MAXC
C   
C     TEST FOR "G" INDICATING LOG PLOT *****************************
C   
      IF(KMD.EQ.'PLG1'.OR.KMD.EQ.'PLG2')GO TO 1620
      KIPL='LIN '
      NCYC=0
      NCFS=NCCC
      GO TO 1630
 1620 KIPL='LOG '
      NCYC=NCCC
      NCFS=0
 1630 ID=KBUF(IDXH)
      CALL SLAPIT(XBUF(IDXD),ID,KIPL,NCYC,NCFS,ILO,IHI,JPRZ)
      GO TO 2500
C   
C     READ DATA FROM INPUT FILE AND DO PRINTER PLOT ****************
C   
 1650 KIPL='LOG '
      GO TO 1660
 1652 KIPL='LIN '
C   
 1660 JJ=1
      DO 1662 I=1,6
      CALL MILV(LIST(JJ),IDUM,QBUF(I),KIND,IERR)
      IF(IERR.NE.0) GO TO 2000
      JJ=JJ+2
 1662 CONTINUE
C   
      IF(NF.GE.6) GO TO 1670
C   
C     IT IS A SINGLE ID ********************************************
C   
      IDA=QBUF(1)+0.5
      IDB=IDA
      ILO=QBUF(2)+1.5
      IHIC=QBUF(3)+0.5
      NCCC=QBUF(4)+0.5
      GO TO 1680
C   
C     IT IS A LIST OF ID'S *****************************************
C   
 1670 IDA=QBUF(1)+0.5
      IDB=QBUF(2)+0.5
      ILO=QBUF(3)+1.5
      IHIC=QBUF(4)+0.5
      NCCC=QBUF(5)+0.5
 1680 NCYC=0
      NCFS=0
      IF(KIPL.EQ.'LIN ') NCFS=NCCC
      IF(KIPL.EQ.'LOG ') NCYC=NCCC
C   
      DO 1700 ID=IDA,IDB,IDSTEP
      CALL SPKIN(ID,NDX,256,16384,IFLO,IERR)
      IF(IERR.NE.0) GO TO 2500
C   
      IF(IHIC.NE.0) IHI=IHIC+1
      IF(IHIC.EQ.0) IHI=NCHI
      IF(IHI.GT.NCHI) IHI=NCHI
      CALL SLAPIT(XBUF(129),ID,KIPL,NCYC,NCFS,ILO,IHI,JPRZ)
 1700 CONTINUE
      GO TO 2500
C   
C     ------------------------------------------------------------------
C     PROCESS  -  SUM ILOC,IHIC
C     ------------------------------------------------------------------
C   
 1800 IA=1
      IB=NCHI
      NCHH=NCHI
      GO TO 1805
C   
 1802 IA=1
      IB=NCHJ
      NCHH=NCHJ
C   
 1805 IF(NF.LT.2) GO TO 1810
      CALL LIMIV(LWD(1,2),0,16383,IA,IERR)
      IF(IERR.NE.0) GO TO 2000
      IA=IA+1
      IF(NF.LT.3) GO TO 1810
      CALL LIMIV(LWD(1,3),IA-1,16383,IB,IERR)
      IF(IERR.NE.0) GO TO 2000
      IB=IB+1
 1810 IF(IA.LT.1)    IA=1
      IF(IB.GT.NCHH) IB=NCHH
      IF(IA.GT.IB)   IA=IB
      SUM=0.0
      IF(KMD.EQ.'SUM2') GO TO 1820
C   
      DO 1815 I=IA,IB
      SUM=SUM+ADAT(I)
 1815 CONTINUE
      GO TO 1830
C   
 1820 DO 1825 I=IA,IB
      SUM=SUM+BDAT(I)
 1825 CONTINUE
C   
 1830 IA=IA-1
      IB=IB-1
      WRITE(CMSSG,1835)NCHI,IA,IB,SUM
 1835 FORMAT('NCH,ILOC,IHIC,SUM=',3I6,F16.0)
      CALL MESSLOG(6,7)
      GO TO 2500
C   
 1900 KODFIL='N   '
      CALL PROJALL
      GO TO 2500
C   
 2000 WRITE(CMSSG,2010)
      CALL MESSLOG(LOGUT,LOGUP)
 2010 FORMAT('ILLEGAL COMMAND OR SYNTAX ERROR')
C   
 2500 IDONE='YES '
      RETURN
      END
