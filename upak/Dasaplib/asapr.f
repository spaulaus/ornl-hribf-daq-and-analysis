C$PROG ASAPR
C
      SUBROUTINE ASAPR(IDONE,IERR)
C
C     ******************************************************************
C     ASAP - AUTOMATIC SPECTRUM ANALYSIS PROGRAM
C     ******************************************************************
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
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/ML03/ ISYN(100),ISYV(100),NSYM
C     ------------------------------------------------------------------
      COMMON/HEPF/ IHEPF
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
      COMMON/III/ LIN,LCM,LCI
C     ------------------------------------------------------------------
C
      COMMON/MAINV8/ A(50,50),B(50,50),DET,IFS
      REAL*8         A        B        DET
C
      COMMON/AAX/ SPECT(256,19),ISPK(16384),IBAK(16384),ICAL(16384)
      COMMON/BBB/ XAG(250,7),NPK
      COMMON/CCC/ BGD(256),WT(256),YCAL(256),GAU(200),NG(16)
      COMMON/DDD/ IHOL(16),XP(16),AREA(16),BETA(18),LOCA(250),BIAS
      COMMON/EEE/ ID,ILO,IHI,KWD,FWHMA,FWHMB,EOOO,GAINN,WDNC,DELCH,BSTD
      COMMON/FFF/ HWID(16),QFN,QFLO,IFBGD,NTRY,MSN,NCH,KPK,I1,I2
      COMMON/GGG/ MXNPK,MXRGL,MXPIS,NBNF
      COMMON/HHH/ IGAUS,KNBAK,KNVAR,KVW
      CHARACTER*4                   KVW
      COMMON/JJJ/ NAMFIL(20),ICN
      COMMON/SSS/ LUS,LUH,LUD,LUT,KFIL
      CHARACTER*4                 KFIL
      COMMON/TTT/ ITITL(19)
C
      INTEGER*4 IHELP(20,100)
C
      INTEGER*4     LIST(78),LWDL(10)
      CHARACTER*4  CLIST(78)
C
      INTEGER*4 NAMCMD(20),DATIM(5)
C
      DIMENSION AOUT(7)
C
      CHARACTER*4  KMD,KMX,IDONE,NAMFIT,IWARN,NAMBAK
C
      EQUIVALENCE (LIST(1),LWD(1,2)),(LWDL(1),LWD(1,1)),(KMD,LWD(1,1))
C
      EQUIVALENCE (KMX,LWD(1,2))
C
      DATA ILOO,IHII,ITPF/0,0,Z'0C202020'/
C
      SAVE
C
C     ------------------------------------------------------------------
C     XAG(I,1)  CONTAINS SUM-AREA
C     XAG(I,2)  CONTAINS SUM-AREA ERROR
C     XAG(I,3)  CONTAINS GAUSS-AREA
C     XAG(I,4)  CONTAINS GAUSS-AREA ERROR
C     XAG(I,5)  CONTAINS FWHM
C     XAG(I,6)  CONTAINS ADJUSTED POSITION
C     XAG(I,7)  CONTAINS CENTROID
C     ------------------------------------------------------------------
C
      IERR=0
      IDONE='NO  '
C
      IF(KMD.EQ.'IN  ') GO TO 300
      IF(KMD.EQ.'CMD ') GO TO 310
C
      IF(NTER.NE.0) GO TO 700
C
C     **************************************************************
C     GO OFF AND DECODE THE NAME-LISTS
C     **************************************************************
C
      IF(KMD.EQ.'END ') STOP
      IF(KMD.EQ.'TIT ') GO TO 290
      IF(KMD.EQ.'HELP') GO TO 270
      IF(KMD.EQ.'H   ') GO TO 270
C
      IF(KMD.EQ.'DSYM') GO TO 80
      IF(KMD.EQ.'LON ') GO TO 90
      IF(KMD.EQ.'LOF ') GO TO 90
      IF(KMD.EQ.'BIAS') GO TO 100
      IF(KMD.EQ.'WCAL') GO TO 120
      IF(KMD.EQ.'KWD ') GO TO 130
      IF(KMD.EQ.'FIT ') GO TO 140
      IF(KMD.EQ.'DEL ') GO TO 150
      IF(KMD.EQ.'ECAL') GO TO 160
      IF(KMD.EQ.'KFIT') GO TO 190
      IF(KMD.EQ.'KBAK') GO TO 200
      IF(KMD.EQ.'MPIS') GO TO 210
      IF(KMD.EQ.'BSTD') GO TO 220
      IF(KMD.EQ.'VW  ') GO TO 240
C
      RETURN
C
C     **************************************************************
C     PROCESS CMD DATA-LISTS
C     **************************************************************
C
   80 WRITE(LOGUT,85)(ISYV(I),ISYN(I),I=1,NSYM)
   85 FORMAT(1H ,5(I8,'=',A4))
      GO TO 2500
C
   90 LISFLG=KMD
      GO TO 2500
C
  100 CALL LIMXV(LIST(1),1.0,100.0,BIAS,IERR)
      IF(IERR.NE.0) GO TO 700
      GO TO 2500
C
  120 CALL MILV(LIST(1),IV,FWHMA,KIND,IERR)
      IF(IERR.NE.0) GO TO 700
      CALL MILV(LIST(3),IV,FWHMB,KIND,IERR)
      IF(IERR.NE.0) GO TO 700
      GO TO 2500
C
  130 CALL LIMIV(LIST(1),2,100,KWD,IERR)
      IF(IERR.NE.0) GO TO 700
      GO TO 2500
C
  140 IF(NF.EQ.1) GO TO 400
      IF(NF.EQ.2) THEN
                  CALL IVALU(LIST(1),ICN,IERR)
                  IF(IERR.NE.0) GO TO 700
                  GO TO 400
                  ENDIF
C
      IF(NF.GT.2) THEN
                  CALL IVALU(LIST(1),IV,IERR)
                  IF(IERR.NE.0) GO TO 700
                  CALL LIMIV(LIST(3),0,16383,JV,IERR)
                  IF(IERR.NE.0) GO TO 700
                  CALL LIMIV(LIST(5),0,16383,KV,IERR)
                  IF(IERR.NE.0) GO TO 700
                  ICN=IV
                  ILOO=JV
                  IHII=KV
                  GO TO 400
                  ENDIF
C
  150 CALL LIMXV(LIST(1),0.0,10.0,XV,IERR)
      IF(IERR.NE.0) GO TO 700
      CALL LIMXV(LIST(3),0.0,0.20,YV,IERR)
      IF(IERR.NE.0) GO TO 700
      DELCH=XV
      WDNC=YV
      GO TO 2500
C
  160 CALL MILV(LIST(1),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 700
      CALL MILV(LIST(3),IV,YV,KIND,IERR)
      IF(IERR.NE.0) GO TO 700
      EOOO=XV
      GAINN=YV
      GO TO 2500
C
  190 NAMFIT=KMX
      IF(NAMFIT.EQ.'GAUS') IGAUS=1
      IF(NAMFIT.EQ.'SUMA') IGAUS=0
      GO TO 2500
C
  200 NAMBAK=KMX
      IF(NAMBAK.EQ.'ZERO') KNBAK=0
      IF(NAMBAK.EQ.'FULL') KNBAK=1
      IF(NAMBAK.EQ.'FAST') KNBAK=2
      GO TO 2500
C
  210 CALL LIMIV(LIST(1),1,16,MXPIS,IERR)
      IF(IERR.NE.0) GO TO 700
      GO TO 2500
C
  220 CALL LIMXV(LIST(1),0.5,5.0,BSTD,IERR)
      IF(IERR.NE.0) GO TO 700
      GO TO 2500
C
  240 IF(KMX.EQ.'FREE') GO TO 250
      IF(KMX.EQ.'LOCK') GO TO 250
      IF(KMX.EQ.'NONE') GO TO 250
      GO TO 700
  250 KVW=KMX
      GO TO 2500
C
C     **************************************************************
C     DISPLAY HELP MESSAGES
C     **************************************************************
C
  270 CALL HELPMANU(IWD,1,IHELP,100,20,IHEPF)
      GO TO 2500
C
C     **************************************************************
C     SET UP A TITLE
C     **************************************************************
C
  290 DO 292 I=1,19
      ITITL(I)=IWD(I+1)
  292 CONTINUE
      GO TO 2500
C
C     **************************************************************
C     OPEN NEW DATA-FILE  OR  CMD-FILE
C     **************************************************************
C
  300 CALL FILOPEN(IWDRAW,NAMFIL,LUS,LUD,LUH,KFIL)
      GO TO 2500
C
  310 CALL CMDOPEN(IWDRAW,NAMCMD,LCI,LIN,LCM,IERR)
      GO TO 2500
C
C     **************************************************************
C     DO THE MIGHTY FIT
C     **************************************************************
C
  400 CONTINUE
      KWDFLG=0
      IF(KWD.EQ.0) KWDFLG=1
      IF(KWD.EQ.0) KWD=8
C
C     READ IN THE SPECTRUM
C
      CALL SPKIN(KFIL,LUS,LUH,LUD,ICN,ISPK,NCH,IERR)
      IF(IERR.NE.0) GO TO 2500
      NCHS=NCH
C
      IF(IHII.EQ.0.AND.KNBAK.NE.0) IHII=NCH-3*KWD
      IF(IHII.EQ.0.AND.KNBAK.EQ.0) IHII=NCH-KWD
      IHI=IHII+1
      INEED=IHI+6*KWD
      IF(NCH.GT.INEED) NCH=INEED
      DO 510 I=1,MXNPK
      DO 510 J=1,7
      XAG(I,J)=0.0
  510 CONTINUE
      DO 520 I=1,MXRGL
      DO 520 J=2,5
      SPECT(I,J)=I**(J-2)
  520 CONTINUE
C
      CALL CALBAK(ILOO,IHI)
C
      CALL SUMMER
C
      IF(IGAUS.EQ.0) GO TO 530
C
      DO 522 I=1,16384
      IBAK(I)=0
  522 CONTINUE
C
      CALL SETTER(KWDFLG)
C
  530 NLN=0
      N5=0
C
      CALL MILDATIM(DATIM)
C
      IF(NPK.LT.1) GO TO 2500
      DO 610 I=1,NPK
      IF(NLN.GT.0) GO TO 570
C
      WRITE(7,532)ITPF
  532 FORMAT(1H ,A4)
      WRITE(7,534)DATIM,NAMFIL
  534 FORMAT(1H ,4X,5A4,4X,20A4,7X,'FIL$'/)
      WRITE(7,536)ITITL
  536 FORMAT(1H ,4X,19A4,35X,'TIT$'/)
      WRITE(7,540)
  540 FORMAT(1H ,'    ID   ILO   IHI   KWD    BIAS  KEV/CH      ',
     &'EO      FW     FWB    WDNC      DX      VW    BSTD  ',
     &'MAXPPS'/)
      WRITE(7,550)ICN,ILOO,IHII,KWD,BIAS,GAINN,EOOO,FWHMA,FWHMB,
     1WDNC,DELCH,KVW,BSTD,MXPIS
  550 FORMAT(1H ,4I6,7F8.4,4X,A4,F8.4,I8,11X,'DAT$'/)
      WRITE(7,560)
  560 FORMAT(1H ,'     SAREA       ERR     GAREA       ERR     ',
     &'AFWHM        XG  CENTROID      EGAM   100(S-G)/S        ',
     &' ID'/)
  570 CONTINUE
      AOUT(1)=XAG(I,1)
      AOUT(2)=XAG(I,2)
      AOUT(3)=XAG(I,3)
      AOUT(4)=XAG(I,4)
      DO 580 L=5,7
      AOUT(L)=XAG(I,L)
  580 CONTINUE
      EGAM=EOOO+GAINN*AOUT(7)
      IF(IGAUS.NE.0) EGAM=EOOO+GAINN*AOUT(6)
      IWARN='    '
      DEV=0.0
      IF(IGAUS.EQ.0) GO TO 590
      RATER= SQRT(AOUT(2)**2+AOUT(4)**2)
      AOUT3=AOUT(3)
      IF(AOUT3.LT.1.0) AOUT3=1.0
      DEV=100.0*(AOUT(1)-AOUT(3))/AOUT3
      IF(DEV.GT.RATER) IWARN='****'
  590 WRITE(7,600)AOUT,EGAM,DEV,IWARN,ICN
  600 FORMAT(1H ,2(F10.0,F10.1),5F10.2,1X,A2,3X,I8,11X,'SAP$')
      NLN=NLN+1
      N5=N5+1
      IF(N5.LT.5) GO TO 605
      WRITE(7,602)
  602 FORMAT(1H )
      N5=0
      NLN=NLN+1
C
  605 IF(NLN.LT.50) GO TO 610
      NLN=0
  610 CONTINUE
      WRITE(7,615)
  615 FORMAT(1H ,115X,'QIT$')
      GO TO 2500
C
C     **************************************************************
C     REPORT SYNTAX ERRORS
C     **************************************************************
C
  700 WRITE(6,710)
  710 FORMAT(1H ,'SYNTAX ERROR OR ILLEGAL VALUE - CMD IGNORED')
      IERR=1
C
 2500 IDONE='YES '
      RETURN
      END
