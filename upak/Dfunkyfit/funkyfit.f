C$PROG FUNKYFIT
C
      COMMON/HEPF/ IHEPF
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
      COMMON/III/ LIN,LCM,LCI
C     ------------------------------------------------------------------
C
      INTEGER*4   IWDRED(20)
C
      INTEGER*4    NAMHEP(6)
      CHARACTER*24 CNAMHEP
      EQUIVALENCE (CNAMHEP,NAMHEP)
      DATA CNAMHEP/'funkyfit.hep            '/
C
      CHARACTER*4  KMD,KMI,IHEPF,IDONE
C
      EQUIVALENCE (KMD,LWD(1,1)),(KMI,IWD(1))
C
      DATA         LCI,LIN,LCM/5,5,3/
C
      character*4  cnamprog(2)
      equivalence  (cnamprog, namprog)
      DATA        cNAMPROG/'FUNK','YFIT'/
C
      SAVE
C
C     ------------------------------------------------------------------
C     GENERAL PURPOSE LEAST SQUARES FITTING PROGRAM - FUNKYFIT
C                                                   - UNIX VERSION
C     ------------------------------------------------------------------
C
      CMSSG=' '
      LOGUT=6
      LOGUP=7
      LISFLG='LOF '
      MSGF='    '
C
      CALL HELPOPEN(13,NAMHEP,IHEPF)              !OPEN HELP-FILE
C
      OPEN(UNIT      = LOGUP,                     !OPEN/CREATE LOG-FILE
     &     FILE      = 'funkyfit.log',
     &     STATUS    = 'UNKNOWN',
     &     IOSTAT    = IOS)
C
c     CLOSE(UNIT=LOGUP,DISP='DELETE')             !Delete it
      CLOSE(UNIT=LOGUP)             !Delete it
C
      OPEN(UNIT      = LOGUP,                     !CREATE NEW LOG-FILE
     &     FILE      = 'funkyfit.log',
     &     STATUS    = 'REPLACE',
     &     IOSTAT    = IOS)
C
      IF(IHEPF.NE.'YES ') GO TO 100               !TST FOR ON-LINE HELP
C
      WRITE(LOGUT,30)
      WRITE(LOGUT,35)
      WRITE(LOGUT,40)
   30 FORMAT(1H ,
     &'Type: h       - for list of HELP code-words & subjects')
   35 FORMAT(1H ,
     &'Type: h all   - for a more detailed help directory')
   40 FORMAT(1H ,
     &'Type: h code  - for command list for associated subject')
C
      GO TO 100
C   
   50 IF(LIN.NE.LCI) THEN
      WRITE(CMSSG,60)
   60 FORMAT('INPUT SWITCHED TO VDT ******')
      CALL MESSLOG(LOGUT,LOGUP)
                     ENDIF
      LIN=LCI
C   
  100 IF(LIN.EQ.LCI) WRITE(6,105)
  105 FORMAT(' FUNKY->',$)
      READ(LIN,110,END=50,ERR=50)IWD
  110 FORMAT(20A4)
      MSGF='    '
C
      DO 115 I=1,20
      IWDRAW(I)=IWD(I)
      IWDRED(I)=IWD(I)
  115 CONTINUE
      CALL CASEUP1(IWDRAW)
      CALL CASEUP(IWD)
C
      IF(KMI.EQ.'LON ') GO TO 120
      IF(KMI.EQ.'LOF ') GO TO 120
C
      IF(KMI.EQ.'    ') GO TO 130
      IF(KMI.EQ.'COM ') GO TO 130
      GO TO 140
C
  120 LISFLG=KMI
      GO TO 100
C   
  130 WRITE(LOGUP,135)IWDRED
  135 FORMAT(1H ,20A4)
C
      IF(LIN.NE.LCI) THEN
                     WRITE(CMSSG,110)IWDRED
                     CALL MESSLOG(LOGUT,0)
                     GO TO 100
                     ENDIF
      GO TO 100
C   
  140 WRITE(CMSSG,110)IWD
      IF(LIN.EQ.LCI) CALL MESSLOG(0,LOGUP)
      IF(LIN.NE.LCI) CALL MESSLOG(LOGUT,LOGUP)
C   
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C   
      IF(KMD.EQ.'END ') STOP
      IF(KMD.EQ.'LOOP') THEN
                        CALL LOOPER(LIN,LCI)
                        GO TO 100
                        ENDIF
      IDONE='    '
      IERR=0
      CALL CALLER(IDONE,IERR)
      IF(IERR.NE.0)       GO TO 50
      IF(IDONE.EQ.'YES ') GO TO 100
C   
      WRITE(CMSSG,200)
  200 FORMAT('COMMAND NOT RECOGNIZED ******')
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 100
      END
C$PROG CALLER
      SUBROUTINE CALLER(IDONE,IERR)
C
      CHARACTER*4  IDONE
C
      IF(IDONE.NE.'YES ') CALL EQUATE(IDONE,IERR)
C   
      IF(IDONE.NE.'YES ') CALL MODFIN(IERR)
      IF(IERR.NE.0)       RETURN
C   
      IF(IDONE.NE.'YES ') CALL LWDMOD
C   
      IF(IDONE.NE.'YES ') CALL FUNKYRU(IDONE,IERR)
C   
      RETURN
      END
C$PROG FUNKY
      REAL*8 FUNCTION FUNKY(X,NDX)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON/TTT/ ITSP(20),KINT(20),NTERMS
      DIMENSION XTSP(20)
      EQUIVALENCE (ITSP(1),XTSP(1))
C
C     **************************************************************
C     RETURNS TERM VALUE AS FUNCTION OF X AND NDX
C     **************************************************************
C
      KIND=KINT(NDX)
      GO TO (100,200,300),KIND
C
C     POWER SERIES TERMS *******************************************
C
  100 EX=XTSP(NDX)
      FUNKY=X**EX
      RETURN
C
C     LEGENDRE POLYNOMIAL TERMS ************************************
C
  200 N=ITSP(NDX)
      FUNKY=PL(N,X)
      RETURN
C
C     CONSTANT TERMS ***********************************************
C
  300 FUNKY=1.0
      RETURN
      END
C$PROG FUNKYRU
C
      SUBROUTINE FUNKYRU(IDONE,IERR)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MXDAT=500)
C
      REAL*4 GRAN12,SV
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
      COMMON/HEPF/ IHEPF
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
      COMMON/AAA/ XIN(MXDAT),YIN(MXDAT),UIN(MXDAT),NDAT
C
      COMMON/MAINV8/ A(50,50),B(50,50),DET,IFS
C
      COMMON/TTT/ITSP(20),KINT(20),NTERMS
C
      DIMENSION XTSP(20),KTV(20),YTAB(10)
      DIMENSION X(MXDAT),Y(MXDAT),U(MXDAT),W(MXDAT),YCAL(MXDAT)
      DIMENSION TABDAT(3),XLIM(2),ILIM(2)
      DIMENSION COMP(MXDAT,20),BETA(20),BERF(20),BERE(20)
      DIMENSION XHED(10)
C
      COMMON/III/  LIN,LCM,LCI
C
      COMMON/JJJ/  LWD4(4,20)
C
      INTEGER*2 IUSE(500)
      INTEGER*4 IHELP(20,200),IFH(19),IFD(19),ITIT(8,4),NAMT(5)
      INTEGER*4 NAMCMD(20)
C
      CHARACTER*38 CIFH(2),CIFD(2)
      CHARACTER*20 CNAMT
      CHARACTER*32 CITIT(4)
C
      CHARACTER*4  KMD,KMI,IDONE,INDI,IBAD
      integer*4  isetw
      character*4 cisetw
C
      EQUIVALENCE (KMD,LWD(1,1)),(KMI,IWD(1))
      EQUIVALENCE (XTSP(1),ITSP(1)),(B(1,1),BETA(1))
      EQUIVALENCE (XMIN,XLIM(1)),(XMAX,XLIM(2))
      EQUIVALENCE (IMIN,ILIM(1)),(IMAX,ILIM(2))
      EQUIVALENCE (YTAB(1),YCAL(1))
      EQUIVALENCE (XA,TABDAT(1)),(XB,TABDAT(2)),(DX,TABDAT(3))
      EQUIVALENCE (CIFH,IFH),(CIFD,IFD),(CNAMT,NAMT)
      EQUIVALENCE (CITIT,ITIT),(cisetw,isetw)
C
      DATA CITIT/
     1'Y=F1(X)+F2(X)+F3(X)+------------',
     2'Y=F1(LOG(X))+F2(LOG(X))+--------',
     3'LOG(Y)=F1(X)+F2(X)+F3(X)+-------',
     4'LOG(Y)=F1(LOG(X))+F2(LOG(X))+---'/
C
      DATA CNAMT/'XPOWLPOL            '/
C
      DATA CIFD/'(1H ,11F10.3/)                        ',
     1          '                                      '/
C
      DATA CIFH/'(1H ,10X,10F10.3/)                    ',
     1          '                                      '/
C
      DATA TABDAT,XMIN,XMAX,IMIN,IMAX/5*0.0,0,0/
C
      DATA ISEED/-1/
      DATA cISETW/'SETW'/
      DATA LDA/1/
      DATA LGX/0/
      DATA LGY/0/
      DATA NTERMS/0/
      DATA KINDUU/2/
      DATA KINDU/3/
      DATA NXLM/0/
      DATA NILM/0/
      DATA NTAB/0/
      DATA IWTY/1/
      DATA USUN/5.0/
      DATA ALUN/5.0/
      DATA UMUL/1.0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IDONE='NO  '
      IERR=0
C
      CALL GREAD(IWD,LWD4,ITYP,NF,ISETW,16,NTER)
C
C     **************************************************************
C     KINDUU= 1 SAYS ALL UNCERT = ALUN (%)
C     KINDUU= 2 SAYS UNSPECIFIED UNCERT = USUN (%)
C     KINDU = 1 SAYS COMPUTE UNCERT AS COUNTING STATISTICS
C     KINDU = 2 SAYS UNCERT GIVEN BY "UIN" IS ABSOLUTE
C     KINDU = 3 SAYS UNCERT GIVEN BY "UIN" IS IN %
C     **************************************************************
C
      IF(KMD.EQ.'    ') GO TO 2500
      IF(KMD.EQ.'DFMT') GO TO 70
      IF(KMD.EQ.'HFMT') GO TO 90
      IF(KMD.EQ.'CMD ') GO TO 110
      IF(KMD.EQ.'CMDF') GO TO 110
      IF(KMD.EQ.'DATF') GO TO 120
      IF(KMD.EQ.'IN  ') GO TO 120
      IF(KMD.EQ.'DSYM') GO TO 130
C
      GO TO 100
   70 DO 75 I=1,19
      IFD(I)=IWD(I+1)
   75 CONTINUE
      GO TO 2500
   90 DO 95 I=1,19
      IFH(I)=IWD(I+1)
   95 CONTINUE
      GO TO 2500
  100 CALL GREAD(IWD,LWD4,ITYP,NF,1,80,NTER)
C
      IF(KMD.EQ.'END ') STOP
      IF(KMD.EQ.'LINX') GO TO 200
      IF(KMD.EQ.'LOGX') GO TO 210
      IF(KMD.EQ.'LINY') GO TO 220
      IF(KMD.EQ.'LOGY') GO TO 230
      IF(KMD.EQ.'NUFU') GO TO 240       !SET NTERMS=0
      IF(KMD.EQ.'CSUN') GO TO 250       !USE COUNTING STAT UNCERT
      IF(KMD.EQ.'ABUN') GO TO 260       !GIVEN UNCERT ARE ABSOLUTE
      IF(KMD.EQ.'PCUN') GO TO 270       !GIVEN UNCERT ARE IN %
C
      IF(KMD.EQ.'H   ') GO TO 280
      IF(KMD.EQ.'HELP') GO TO 280
C
      IF(KMD.EQ.'FIT ') GO TO 600       !DO   WEIGHTED FIT
      IF(KMD.EQ.'FITU') GO TO 610       !DO UNWEIGHTED FIT
C
      IF(KMD.EQ.'DATA') GO TO 400       !DATA TABLE FOLLOWS
      IF(KMD.EQ.'TABL') GO TO 420       !TABLE SPEC
      IF(KMD.EQ.'XLIM') GO TO 440       !FIT RANGE - XMIN,XMAX
      IF(KMD.EQ.'ILIM') GO TO 450       !FIT RANGE - IMIN,IMAX
      IF(KMD.EQ.'ALUN') GO TO 460       !VALUE FOR ALL UNCERT IN %
      IF(KMD.EQ.'USUN') GO TO 470       !UNSPECIFIED UNCERT IN %
      IF(KMD.EQ.'MULU') GO TO 480       !MULTIPLIER FOR ALL UNCERT
      IF(KMD.EQ.'XPOW') GO TO 490       !X-POWER LIST
      IF(KMD.EQ.'LPOL') GO TO 500       !LEGENDRE POL TERMS LIST
C
      GO TO 2510
C
C     **************************************************************
C     OPEN NEW CMD-FILE & NEW DATA FILE
C     **************************************************************
C
  110 CALL CMDOPEN(IWDRAW,NAMCMD,LCI,LIN,LCM,IERR)
      GO TO 2500
C
  120 CALL NUDAF(IWDRAW,LDA,LIN,IERR)
      IF(IERR.NE.0) GO TO 2400
      CALL GETDATX(-1,IERR)
      GO TO 2500
C
  130 CALL SYMLOG('LSYM')
      GO TO 2500
C
C     **************************************************************
C     PROCESS DIRECTIVES
C     **************************************************************
C
  200 LGX=0
      GO TO 2500
  210 LGX=1
      GO TO 2500
  220 LGY=0
      GO TO 2500
  230 LGY=1
      GO TO 2500
  240 NTERMS=0
      GO TO 2500
  250 KINDU=1
      GO TO 2500
  260 KINDU=2
      GO TO 2500
  270 KINDU=3
      GO TO 2500
  280 CALL GREAD(IWD,LWD4,ITYP,NF,ISETW,8,NTER)
      CALL HELPMANU(IWD,13,IHELP,200,20,IHEPF)
      GO TO 2500
C
C     **************************************************************
C     PROCESS LABELED LISTS
C     **************************************************************
C
C
C     READ IN DATA UNTIL YOU HIT AN "ENDATA" ***********************
C
  400 NDAT=0
  405 READ(LIN,406,END=2200,ERR=2210)IWD
  406 FORMAT(20A4)
      CALL CASEUP(IWD)
      IF(KMI.NE.'ENDA') GO TO 410
      GO TO 2500
  410 CALL GREAD(IWD,LWD4,ITYP,NF,1,80,NTER)
      IF(NTER.NE.0) GO TO 2010
      NDAT=NDAT+1
      IF(NDAT.GT.MXDAT) GO TO 405
      CALL MILV4(LWD4(1,1),IV,XIN(NDAT),KIND,IERR)
      IF(IERR.NE.0) GO TO 2010
      CALL MILV4(LWD4(1,2),IV,YIN(NDAT),KIND,IERR)
      IF(IERR.NE.0) GO TO 2010
      CALL MILV4(LWD4(1,3),IV,UIN(NDAT),KIND,IERR)
      IF(IERR.NE.0) GO TO 2010
      GO TO 405
C
C     PROCESS TABLE SPECIFICATION **********************************
C
  420 NTAB=0
      IF(NF.NE.4) GO TO 2010
      DO 425 N=2,4
      CALL MILV4(LWD4(1,N),IV,TABDAT(N-1),KIND,IERR)
      IF(IERR.NE.0) GO TO 2010
  425 CONTINUE
      NTAB=3
      GO TO 2500
C
C     PROCESS XLIM-LIST ********************************************
C
  440 NXLM=0
      IF(NF.NE.3) GO TO 2010
      DO 445 N=2,3
      CALL MILV4(LWD4(1,N),IV,XLIM(N-1),KIND,IERR)
      IF(IERR.NE.0) GO TO 2010
  445 CONTINUE
      NXLM=2
      GO TO 2500
C
C     PROCESS ILIM-LIST ********************************************
C
  450 NILM=0
      IF(NF.NE.3) GO TO 2010
      DO 455 N=2,3
      CALL MILV4(LWD4(1,N),ILIM(N-1),XV,KIND,IERR)
  455 CONTINUE
      NILM=2
      GO TO 2500
C
C     PICK UP "ALUN" ***********************************************
C
  460 CALL MILV4(LWD4(1,2),IV,ALUN,KIND,IERR)
      IF(IERR.NE.0) GO TO 2010
      KINDUU=1
      GO TO 2500
C
C     PICK UP "USUN" ***********************************************
C
  470 CALL MILV4(LWD4(1,2),IV,USUN,KIND,IERR)
      IF(IERR.NE.0) GO TO 2010
      KINDUU=2
      GO TO 2500
C
C     PICK UP "UMUL" - UNCERT MULTIPLIER ***************************
C
  480 CALL MILV4(LWD4(1,2),IV,UMUL,KIND,IERR)
      IF(IERR.NE.0) GO TO 2010
      GO TO 2500
C
C     PROCESS XPOW-LIST ********************************************
C
  490 DO 495 N=2,NF
      MM=N
      NTERMS=NTERMS+1
      IF(NTERMS.GT.20) GO TO 2050
      CALL MILV4(LWD4(1,N),IV,XTSP(NTERMS),KIND,IERR)
      IF(IERR.NE.0) GO TO 2010
      KINT(NTERMS)=1
      KTV(NTERMS)=1
  495 CONTINUE
      GO TO 2500
C
C     PROCESS LPOL-LIST ********************************************
C
  500 DO 505 N=2,NF
      MM=N
      NTERMS=NTERMS+1
      IF(NTERMS.GT.20) GO TO 2050
      CALL MILV4(LWD4(1,N),IV,TEMP,KIND,IERR)
      IF(IERR.NE.0) GO TO 2010
      ITST=TEMP+0.5
      IF(ITST.GT.6) GO TO 2060
      ITSP(NTERMS)=ITST
      KINT(NTERMS)=2
      KTV(NTERMS)=2
  505 CONTINUE
      GO TO 2500
C
C     **************************************************************
C     FIT REQUEST - IS NEXT FIELD NUMERIC INDICATING DATA SET #
C     **************************************************************
C
  600 IWTY=1
      GO TO 620
  610 IWTY=0
  620 IF(NF.LT.2) GO TO 650
      CALL MILV(LWD(1,2),IDN,SV,KIND,IERR)
      IF(IERR.NE.0) GO TO 2010
      CALL GETDATX(IDN,IERR)
      IF(IERR.EQ.0) GO TO 650
      WRITE(6,640)IDN
  640 FORMAT(' DATA SET #',I6,'  NOT FOUND')
      GO TO 2500
C
C     FLAG ANY POINTS THAT ARE OUTSIDE RANGE OF FIT ****************
C     SET ALL WEIGHTS TO 1.0 IN CASE FIT IS UNWEIGHTED *************
C
  650 DO 670 I=1,NDAT
      IUSE(I)=1
      W(I)=1.0
      IF(NXLM.NE.2) GO TO 660
      IF(XIN(I).LT.XMIN.OR.XIN(I).GT.XMAX) IUSE(I)=0
  660 IF(NILM.NE.2) GO TO 670
      IF(I.LT.IMIN.OR.I.GT.IMAX) IUSE(I)=0
  670 CONTINUE
      ITDX=1+LGX+2*LGY
C
C     COMPUTE LOG OF XIN IF REQUIRED *******************************
C
      IF(LGX.EQ.1) GO TO 720
      DO 710 I=1,NDAT
      X(I)=XIN(I)
      IF(X(I).EQ.0.0) X(I)=1.0E-20
  710 CONTINUE
      GO TO 735
  720 DO 730 I=1,NDAT
      X(I)=DLOG(XIN(I))
  730 CONTINUE
  735 DO 740 I=1,NDAT
      Y(I)=YIN(I)
  740 CONTINUE
      IF(IWTY.EQ.0) GO TO 855
C
C     COMPUTE THE UNCERT U(I) BY REQUIRED METHOD *******************
C
      IF(KINDUU.EQ.1) GO TO 750
      IF(KINDU.EQ.1)  GO TO 770
      GO TO 785
  750 DO 760 I=1,NDAT
      U(I)=ALUN
  760 CONTINUE
      GO TO 840
  770 DO 780 I=1,NDAT
      YY=YIN(I)
      IF(YY.LT.1.0) YY=1.0
      U(I)=100.0*DSQRT(YY)/YY*UMUL
  780 CONTINUE
      GO TO 840
  785 DO 800 I=1,NDAT
      IF(UIN(I).EQ.0.0) GO TO 790
      IF(KINDU.EQ.2) U(I)=100.0*UIN(I)/YIN(I)*UMUL
      IF(KINDU.EQ.3) U(I)=UIN(I)*UMUL
      GO TO 800
  790 U(I)=USUN
  800 CONTINUE
C
C     COMPUTE WEIGHT FACTORS "W" TO BE USED IN FIT *****************
C
  840 DO 850 I=1,NDAT
      ERR=0.01*U(I)*Y(I)
      IF(DABS(ERR).LT.1.0E-6) ERR=1.0E-6
      W(I)=1.0/(ERR*ERR)
  850 CONTINUE
  855 IF(LGY.EQ.0) GO TO 900
      DO 860 I=1,NDAT
      ERR=DSQRT(1.0/W(I))/Y(I)
      Y(I)=DLOG(Y(I))
      W(I)=1.0/(ERR*ERR)
  860 CONTINUE
C
C     CALCULATE COMPONENTS TO BE USED IN FIT ***********************
C
  900 DO 920 I=1,NDAT
      DO 910 J=1,NTERMS
      COMP(I,J)=FUNKY(X(I),J)
  910 CONTINUE
  920 CONTINUE
C
C     CALCULATE MATRIX ELEMENTS ************************************
C
      DO 950 NN=1,NTERMS
      SUM=0.0
      DO 940 I=1,NDAT
      IF(IUSE(I).EQ.0) GO TO 940
      SUM=SUM+W(I)*Y(I)*COMP(I,NN)
  940 CONTINUE
      BETA(NN)=SUM
  950 CONTINUE
      DO 1000 I=1,NTERMS
      DO 1000 J=1,I
      SUM=0.0
      DO 990 K=1,NDAT
      IF(IUSE(K).EQ.0) GO TO 990
      SUM=SUM+W(K)*COMP(K,I)*COMP(K,J)
  990 CONTINUE
      A(I,J)=SUM
      A(J,I)=SUM
 1000 CONTINUE
C
C     **************************************************************
C     DO THE FIT
C     **************************************************************
C
      CALL DMAINV8(NTERMS,1)
C
C     CALCULATE THE FITTED POINTS - YCAL ***************************
C
      DO 1030 I=1,NDAT
      SUM=0.0
      DO 1020 J=1,NTERMS
      SUM=SUM+BETA(J)*COMP(I,J)
 1020 CONTINUE
      YCAL(I)=SUM
 1030 CONTINUE
      IF(LGY.EQ.0) GO TO 1045
      DO 1040 I=1,NDAT
      YCAL(I)=DEXP(YCAL(I))
 1040 CONTINUE
C
C     CALCULATE CHISQ AND QFN **************************************
C
 1045 RESQF=0.0
      RESQT=0.0
      CHISQF=0.0
      CHISQT=0.0
      CHISQE=0.0
      NPTS=0
      NPTE=0
      DO 1050 I=1,NDAT
      RADD=(YCAL(I)-YIN(I))**2
      CADD=W(I)*RADD
      RESQT=RESQT+RADD
      CHISQT=CHISQT+CADD
      IF(IUSE(I).EQ.0) GO TO 1050
      RESQF=RESQF+RADD
      CHISQF=CHISQF+CADD
      NPTS=NPTS+1
      FAC=0.01*U(I)*Y(I)/2.12
      DO 1046 II=1,10
      YADD=FAC*GRAN12(ISEED)
      CHISQE=CHISQE+W(I)*(YCAL(I)-YIN(I)-YADD)**2
      NPTE=NPTE+1
 1046 CONTINUE
 1050 CONTINUE
      DENO=NPTS-NTERMS
      IF(DENO.LT.1.0) DENO=1.0
      QFNF=CHISQF/DENO
      QFNT=CHISQT/DENO
      DENO=NPTE-NTERMS
      IF(DENO.LT.1.0) DENO=1.0
      QFNE=CHISQE/DENO
C
C     CALCULATE COEFFICIENT UNCERTS ********************************
C
      DO 1120 I=1,NTERMS
      BERF(I)=100.0*DSQRT(DABS(QFNF*A(I,I)))/DABS(BETA(I))
      BERE(I)=100.0*DSQRT(DABS(QFNE*A(I,I)))/DABS(BETA(I))
 1120 CONTINUE
C
C     **************************************************************
C     LIST CHISQ, QFN, COEFFICIENTS, ETC
C     **************************************************************
C
      IF(NPTS.EQ.NDAT) WRITE(6,1130)CHISQF,QFNF
 1130 FORMAT(' CHISQ,QFN =',2F12.3)
C
      IF(NPTS.NE.NDAT) WRITE(6,1132)CHISQF,CHISQT,QFNF,QFNT
 1132 FORMAT(' CHISQF,CHISQT,QFNF,QFNT =',2F10.2,2F10.3)
C
      ITPF=Z'0C202020'
      WRITE(7,1140)ITPF,(ITIT(I,ITDX),I=1,8)
 1140 FORMAT(1H ,A4,'FUNKY FIT TO THE FORM -  ',8A4/)
C
      WRITE(7,1142)
 1142 FORMAT(1H ,' SUM(RES)**2       CHISQ         QFN'/)
C
      IF(NPTS.EQ.NDAT) WRITE(7,1143)RESQF,CHISQF,QFNF
      IF(NPTS.NE.NDAT) WRITE(7,1144)RESQF,CHISQF,QFNF
      IF(NPTS.NE.NDAT) WRITE(7,1146)RESQT,CHISQT,QFNT
 1143 FORMAT(1H ,3F12.3/)
 1144 FORMAT(1H ,3F12.3,' FOR FITTED DATA POINTS'/)
 1146 FORMAT(1H ,3F12.3,' FOR ALL    DATA POINTS'/)
C
      IF(KINDUU.EQ.1) WRITE(7,1148)ALUN
      IF(KINDUU.EQ.2) WRITE(7,1150)USUN
 1148 FORMAT(1H ,'ALL UNCERTAINTIES (ERR(%)) SET TO ',F8.2/)
 1150 FORMAT(1H ,'UNSPECIFIED UNCERTAINTIES ARE SET TO',F8.2,' %')
C
      IF(KINDUU.EQ.1) GO TO 1155
C
      IF(KINDU.EQ.1) WRITE(7,1151)UMUL
      IF(KINDU.EQ.2) WRITE(7,1152)UMUL
      IF(KINDU.EQ.3) WRITE(7,1153)UMUL
 1151 FORMAT(1H ,'ERR(%) IS ',F8.3,' TIMES "COUNTING STATISTICS"'/)
 1152 FORMAT(1H ,'ERR(%) IS ',F8.3,' TIMES GIVEN ABSOLUTE VALUES'/)
 1153 FORMAT(1H ,'ERR(%) IS ',F8.3,' TIMES GIVEN VALUES(%)'/)
C
 1155 WRITE(7,1160)
 1160 FORMAT(1H ,'TERM-# -TYPE  -SPECIFIER  COEF-VALUE  FIT-ERR(%)  ',
     1'EST-ERR(%)'/)
C
      DO 1180 I=1,NTERMS
      NDX=KINT(I)
      IGO=KTV(I)
      GO TO (1165,1170),IGO
 1165 WRITE(7,1168)I,NAMT(NDX),XTSP(I),BETA(I),BERF(I),BERE(I)
 1168 FORMAT(1H ,I6,2X,A4,F12.5,D12.5,2F12.2/)
      GO TO 1180
 1170 WRITE(7,1172)I,NAMT(NDX),ITSP(I),BETA(I),BERF(I),BERE(I)
 1172 FORMAT(1H ,I6,2X,A4,I12,D12.5,2F12.2/)
 1180 CONTINUE
C
C     **************************************************************
C     LIST THE RESULTS OF THE FIT
C     **************************************************************
C
      WRITE(7,1190)
 1190 FORMAT(1H ,/)
      WRITE(7,1210)
 1210 FORMAT(1H ,'           X           Y   GIVEN-ERR      ERR(%)',
     1'        YCAL      YCAL-Y   100(YC-Y)/YC'/)
C
      DO 1240 I=1,NDAT
      INDI='    '
      IF(IUSE(I).EQ.0) INDI='****'
      DIF=YCAL(I)-YIN(I)
      PCD=100.0*DIF/YIN(I)
      WRITE(7,1220)XIN(I),YIN(I),UIN(I),U(I),YCAL(I),DIF,PCD,INDI
 1220 FORMAT(1H ,6D12.4,F15.4,2X,A4/)
 1240 CONTINUE
C
      IF(NTAB.LT.3.OR.TABDAT(3).LE.0.0) GO TO 2500
C
C     **************************************************************
C     PRINT OUT CALCULATED TABLE IF REQUESTED
C     **************************************************************
C
      SUM=0.0
      DO 1250 I=1,10
      XHED(I)=SUM
      SUM=SUM+DX
 1250 CONTINUE
C
      WRITE(7,1140)(ITIT(I,ITDX),I=1,8)
      WRITE(7,1255)
 1255 FORMAT(1H ,'CALCULATED VALUES OF Y VS X'/)
      WRITE(7,cIFH)XHED
C
      XX=XA
 1260 IF(XX.GT.XB) GO TO 2500
      XLO=XX
      DO 1280 I=1,10
      SUM=0.0
      ZZ=XX
      IF(ZZ.EQ.0.0) ZZ=1.0E-20
      IF(LGX.NE.0) ZZ=DLOG(ZZ)
      DO 1270 J=1,NTERMS
      SUM=SUM+BETA(J)*FUNKY(ZZ,J)
 1270 CONTINUE
      IF(LGY.EQ.0) YTAB(I)=SUM
      IF(LGY.NE.0) YTAB(I)=DEXP(SUM)
      XX=XX+DX
 1280 CONTINUE
C
      WRITE(7,cIFD)XLO,YTAB
      GO TO 1260
C
C     **************************************************************
C     LIST DIAGNOSTIC MESSAGES
C     **************************************************************
C
 2010 WRITE(7,2012)
      WRITE(6,2012)
 2012 FORMAT(1H ,'SYNTAX ERROR OR ILLEGAL VALUE')
      GO TO 2400
C
 2050 IBAD='TERM'
      GO TO 2100
 2060 IBAD='LPOL'
 2100 WRITE(7,2110)LWD4(1,MM),LWD4(2,MM),IBAD
 2110 FORMAT(1H ,2A4,' IS TOO MANY ENTRIES IN',2X,A4,'-LIST')
      WRITE(6,2110)LWD4(1,MM),LWD4(2,MM),IBAD
      GO TO 2400
C
 2200 WRITE(7,2202)
      WRITE(6,2202)
 2202 FORMAT(1H ,'END-OF-FILE ENCOUNTERED ON DATA-FILE')
      GO TO 2400
C
 2210 WRITE(7,2212)
      WRITE(6,2212)
 2212 FORMAT(1H ,'READ ERROR - READING DATA-FILE')
      GO TO 2400
C
 2400 IERR=1
 2500 IDONE='YES '
 2510 CALL GREAD(IWD,LWD4,ITYP,NF,ISETW,8,NTER)
      RETURN
      END
C$PROG GETDATX
C
      SUBROUTINE GETDATX(IDN,IERR)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MXDAT=500)
C
      COMMON/AAA/ XIN(MXDAT),YIN(MXDAT),UIN(MXDAT),NDAT
C
      INTEGER*4   IWD(20),LWD(4,20),ITYP(40),IBLN(101)
C
      CHARACTER*4  KMI
C
      EQUIVALENCE (KMI,IWD(1))
C
      DATA LU/1/
C
      SAVE
C
C
C     **************************************************************
C     READS DATA-SETS FORM DATA FILE - LU=1
C     **************************************************************
C
      IERR=0
      IF(IDN.NE.-1) GO TO 100
C
C     CONSTRUCT DIRECTORY IF IDN=-1 ********************************
C
      NB=0
      NSET=0
C
   10 NB=NB+1
      READ(LU,15,END=50,ERR=210)IWD
   15 FORMAT(20A4)
      CALL CASEUP(IWD)
      IF(KMI.EQ.'DATA') GO TO 20
      GO TO 10
   20 NSET=NSET+1
      IF(NSET.GT.100) NSET=100
      IBLN(NSET)=NB
      GO TO 10
   50 RETURN
C
C     READ REQUESTED DATA-SET **************************************
C
  100 IF(IDN.LT.0.OR.IDN.GT.NSET) GO TO 200
      NB=IBLN(IDN)
      NDAT=0
C
      REWIND LU
      DO 105 I=1,NB
      READ(LU,15,END=210,ERR=210)IWD
  105 CONTINUE
C
  110 READ(LU,15,END=210,ERR=210)IWD
      CALL CASEUP(IWD)
      IF(KMI.EQ.'ENDA') RETURN
C
      NDAT=NDAT+1
C
      WRITE(6,115)NDAT,(IWD(I),I=1,10)
  115 FORMAT(1H ,I4,'-  ',10A4)
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
      IF(NTER.NE.0) GO TO 210
C
      CALL MILV4(LWD(1,1),IV,XIN(NDAT),KIND,JERR)
      IF(JERR.NE.0) GO TO 220
      CALL MILV4(LWD(1,2),IV,YIN(NDAT),KIND,JERR)
      IF(JERR.NE.0) GO TO 220
      CALL MILV4(LWD(1,3),IV,UIN(NDAT),KIND,JERR)
      IF(JERR.NE.0) GO TO 220
      IF(NDAT.GE.MXDAT) RETURN
      GO TO 110
  200 IERR=1
      RETURN
  210 IERR=2
      RETURN
  220 IERR=3
      RETURN
      END
C$PROG GRAN12
      FUNCTION GRAN12(ISEED)
C
      SUM=0.0
C
      DO 10 I=1,12
      SUM=SUM+RAN(ISEED)
   10 CONTINUE
      GRAN12=2.0*SUM-12.0
      RETURN
      END
C$PROG NUDAF
      SUBROUTINE NUDAF(IWD,LDAT,LCMD,IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      INTEGER*4 IWD(20),NAMF(20)
C
      CHARACTER*80 CNAMF
C
      EQUIVALENCE (CNAMF,NAMF)
C
      IERR=0
C
      CLOSE(UNIT=LDAT)
      IS=NXBL(IWD,1,80)
      IF(IS.LE.0) GO TO 120
      IA=NXNB(IWD,IS,80)
      IF(IA.LE.0) GO TO 120
      IB=LSNB(IWD,IA,80)
      IF(IB.LE.0) GO TO 120
C
      DO 20 I=1,20
      NAMF(I)=0
   20 CONTINUE
C
      CALL LODUP(IWD,IA,IB,NAMF,1)
C
      OPEN(UNIT     = LDAT,
     &     FILE     = CNAMF,
     &     STATUS   = 'OLD',
     &     ACCESS   = 'SEQUENTIAL',
     &     FORM     = 'FORMATTED',
     &     IOSTAT   = STAT)
C
      IF(STAT.NE.0) GO TO 100
      RETURN
C
  100 WRITE(6,110)STAT
  110 FORMAT(1H ,'ERROR TRYING TO OPEN DATA FILE - STAT =',I8)
      GO TO 150
C
  120 WRITE(6,130)
  130 FORMAT(1H ,'SYNTAX ERROR IN DATA FILE SPECIFICATION')
C
  150 WRITE(6,160)
  160 FORMAT(1H ,'CONTROL RETURNED TO VDT')
      IERR=1
      LCMD=5
      RETURN
      END
C$PROG PL
      REAL*8 FUNCTION PL(N,X)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     **************************************************************
C     RETURNS LEGENDRE POLY TERM VALUE AS FUNCTION OF (N,X)
C     **************************************************************
C
      JGO=N+1
      GO TO (1,2,3,4,5,6,7),JGO
    1 PL=1.0
      RETURN
    2 PL=X
      RETURN
    3 PL=1.5*X*X-0.5
      RETURN
    4 PL=2.5*X**3-1.5*X
      RETURN
    5 PL=4.375*X**4-3.75*X*X+0.375
      RETURN
    6 PL=7.875*X**5-8.75*X**3+1.875*X
      RETURN
    7 PL=14.4375*X**6-19.6875*X**4+6.5625*X**2-0.3125
      RETURN
      END
