C$PROG CMPFIT    - Command processor for Linear Fit operations
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
C
      SUBROUTINE CMPFIT(IDONE,IERR)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MXDAT=500)
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
      COMMON/FT01/ IHEPF
      INTEGER*4    IHEPF
C     ------------------------------------------------------------------
      COMMON/FT02/ LIN,LCM,LCI
      INTEGER*4    LIN,LCM,LCI
C     ------------------------------------------------------------------
      COMMON/FT03/ XIN(MXDAT),YIN(MXDAT),UIN(MXDAT),NDAT
      REAL*8       XIN,       YIN,       UIN
      INTEGER*4                                     NDAT
C     ------------------------------------------------------------------
      COMMON/FT04/ ITSP(20),KINT(20),NTERMS
      INTEGER*4    ITSP,    KINT,    NTERMS
C     ------------------------------------------------------------------
      COMMON/FT05/ LWD4(4,20)
      INTEGER*4    LWD4
C     ------------------------------------------------------------------
      COMMON/FT08/ NAMF(20)
      INTEGER*4    NAMF
C     ------------------------------------------------------------------
      COMMON/FT10/ KINDUU,KINDU,USUN,ALUN,UMUL
      INTEGER*4    KINDUU,KINDU
      REAL*8                    USUN,ALUN,UMUL
C     ------------------------------------------------------------------
      COMMON/FT12/ TITLA,TITLB
      CHARACTER*80 TITLA,TITLB
C     ------------------------------------------------------------------
      COMMON/MAINV/ A(20,20),B(20,1),DET,IFS
      REAL*8        A,       B,      DET
      INTEGER*4                          IFS
C     ------------------------------------------------------------------
      CHARACTER*80 CNAMF
      EQUIVALENCE (CNAMF,NAMF)
C     ------------------------------------------------------------------
      INTEGER*4    STRLEN,STRAPPEND,LENG
C
      CHARACTER*40 CTERMS
C
      CHARACTER*4  CTERMN,CTERMV,CTERML
C     ------------------------------------------------------------------
      REAL*4       XLIN(MXDAT),YLIN(MXDAT)
C
      INTEGER*4    NLIN
C     ------------------------------------------------------------------
C
      REAL*4 GRAN12,SV
C
      DIMENSION XTSP(20),KTV(20),YTAB(10)
      DIMENSION X(MXDAT),Y(MXDAT),U(MXDAT),W(MXDAT),YCAL(MXDAT)
      DIMENSION YII(MXDAT),YCC(MXDAT)
C
      DIMENSION TABDAT(3),XLIM(2),ILIM(2)
      DIMENSION COMP(MXDAT,20),BETA(20),BERF(20),BERE(20)
      DIMENSION XHED(10)
C
      INTEGER*2 IUSE(500)
      INTEGER*4 IHELP(20,200),IFH(19),IFD(19),ITIT(8,4),NAMT(5)
      INTEGER*4 NAMCMD(20)
C
      CHARACTER*38 CIFH(2),CIFD(2)
      CHARACTER*20 CNAMT
      CHARACTER*32 CITIT(4)
      integer*4    isetw
      character*4  cisetw
C
      CHARACTER*4  KMD,IDONE,IWD1,IBAD,INDI
C
      EQUIVALENCE (KMD,LWD(1,1))
      EQUIVALENCE (XTSP(1),ITSP(1)),(B(1,1),BETA(1))
      EQUIVALENCE (XMIN,XLIM(1)),(XMAX,XLIM(2))
      EQUIVALENCE (IMIN,ILIM(1)),(IMAX,ILIM(2))
      EQUIVALENCE (YTAB(1),YCAL(1))
      EQUIVALENCE (XA,TABDAT(1)),(XB,TABDAT(2)),(DX,TABDAT(3))
      EQUIVALENCE (CIFH,IFH),(CIFD,IFD),(CNAMT,NAMT)
      EQUIVALENCE (CITIT,ITIT), (cisetw, isetw)
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
      DATA LGX/0/
      DATA LGY/0/
      DATA NTERMS/0/
      DATA NXLM/0/
      DATA NILM/0/
      DATA NTAB/0/
      DATA IWTY/1/
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
C     ------------------------------------------------------------------
C     KINDUU= 1 SAYS ALL UNCERT = ALUN (%)
C     KINDUU= 2 SAYS UNSPECIFIED UNCERT = USUN (%)
C     KINDU = 1 SAYS COMPUTE UNCERT AS COUNTING STATISTICS
C     KINDU = 2 SAYS UNCERT GIVEN BY "UIN" IS ABSOLUTE
C     KINDU = 3 SAYS UNCERT GIVEN BY "UIN" IS IN %
C     ------------------------------------------------------------------
C
      IF(KMD.EQ.'    ') GO TO 2500
      IF(KMD.EQ.'DFMT') GO TO 70
      IF(KMD.EQ.'HFMT') GO TO 90
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
C
      IF(KMD.EQ.'FIT ') GO TO 600       !DO   WEIGHTED FIT
      IF(KMD.EQ.'FITU') GO TO 610       !DO UNWEIGHTED FIT
C
      IF(KMD.EQ.'DATA') GO TO 400       !DATA TABLE FOLLOWS
      IF(KMD.EQ.'TABL') GO TO 420       !TABLE SPEC
      IF(KMD.EQ.'XLIM') GO TO 440       !FIT RANGE - XMIN,XMAX
      IF(KMD.EQ.'ILIM') GO TO 450       !FIT RANGE - IMIN,IMAX
      IF(KMD.EQ.'XPOW') GO TO 490       !X-POWER LIST
      IF(KMD.EQ.'LPOL') GO TO 500       !LEGENDRE POL TERMS LIST
C
      GO TO 2510
C
C     ------------------------------------------------------------------
C     PROCESS DIRECTIVES
C     ------------------------------------------------------------------
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
C
C     ------------------------------------------------------------------
C     PROCESS LABELED LISTS
C     ------------------------------------------------------------------
C
C
C     ------------------------------------------------------------------
C     READ IN DATA UNTIL YOU HIT AN "ENDATA" 
C     ------------------------------------------------------------------
C
  400 NDAT=0
  405 READ(LIN,406,END=2200,ERR=2210)IWD
  406 FORMAT(20A4)
      CALL CASEUP(IWD)
      IF(IWD1.NE.'ENDA') GO TO 410
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
C     ------------------------------------------------------------------
C     PROCESS TABLE SPECIFICATION 
C     ------------------------------------------------------------------
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
C     ------------------------------------------------------------------
C     PROCESS XLIM-LIST
C     ------------------------------------------------------------------
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
C     ------------------------------------------------------------------
C     PROCESS ILIM-LIST
C     ------------------------------------------------------------------
C
  450 NILM=0
      IF(NF.NE.3) GO TO 2010
      DO 455 N=2,3
      CALL MILV4(LWD4(1,N),ILIM(N-1),XV,KIND,IERR)
  455 CONTINUE
      NILM=2
      GO TO 2500
C
C     ------------------------------------------------------------------
C     PROCESS XPOW-LIST
C     ------------------------------------------------------------------
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
C     ------------------------------------------------------------------
C     PROCESS LPOL-LIST 
C     ------------------------------------------------------------------
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
C     ------------------------------------------------------------------
C     FIT REQUEST - IS NEXT FIELD NUMERIC INDICATING DATA SET #
C     ------------------------------------------------------------------
C
  600 IWTY=1                                  !Weighted   fit
      GO TO 620
C
  610 IWTY=0                                  !Unweighted fit
C
  620 IF(NF.LT.2) GO TO 2020
      CALL MILV(LWD(1,2),IDN,SV,KIND,IERR)
      IF(IERR.NE.0) GO TO 2010
      CALL GETDATX(IDN,IERR)
      IF(IERR.EQ.0) GO TO 650
      WRITE(6,640)IDN
  640 FORMAT(' DATA SET #',I6,'  NOT FOUND')
      GO TO 2500
C
C     ------------------------------------------------------------------
C     FLAG ANY POINTS THAT ARE OUTSIDE RANGE OF FIT 
C     SET ALL WEIGHTS TO 1.0 IN CASE FIT IS UNWEIGHTED
C     ------------------------------------------------------------------
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
C     ------------------------------------------------------------------
C     COMPUTE LOG OF XIN IF REQUIRED
C     ------------------------------------------------------------------
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
CX    IF(IWTY.EQ.0) GO TO 855
C
C     ------------------------------------------------------------------
C     COMPUTE THE UNCERT U(I) BY REQUIRED METHOD 
C     ------------------------------------------------------------------
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
C     ------------------------------------------------------------------
C     COMPUTE WEIGHT FACTORS "W" TO BE USED IN FIT
C     ------------------------------------------------------------------
C
  840 IF(IWTY.EQ.0) GO TO 855
C
      DO 850 I=1,NDAT
      ERR=0.01*U(I)*Y(I)
      IF(DABS(ERR).LT.1.0E-6) ERR=1.0E-6
      W(I)=1.0/(ERR*ERR)
  850 CONTINUE
C
  855 IF(LGY.EQ.0) GO TO 900
C
      DO 860 I=1,NDAT
      ERR=DSQRT(1.0/W(I))/Y(I)
      YMD=Y(I)
      Y(I)=DLOG(Y(I))
C
      IF(IWTY.EQ.1) THEN
      ERR=0.01*U(I)*YMD
      YLO=YMD-ERR
      YHI=YMD+ERR
      IF(YLO.LE.1.0) YLO=1.0
      YLO=DLOG(YLO)
      YHI=DLOG(YHI)
      ERR=(YHI-YLO)/2.0
      W(I)=1.0/(ERR*ERR)
      ENDIF
C
  860 CONTINUE
C
C     ------------------------------------------------------------------
C     CALCULATE COMPONENTS TO BE USED IN FIT
C     ------------------------------------------------------------------
C
  900 DO 920 I=1,NDAT
      DO 910 J=1,NTERMS
      COMP(I,J)=FUNKY(X(I),J)
  910 CONTINUE
  920 CONTINUE
C
C     ------------------------------------------------------------------
C     CALCULATE MATRIX ELEMENTS
C     ------------------------------------------------------------------
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
C     ------------------------------------------------------------------
C     DO THE FIT
C     ------------------------------------------------------------------
C
      CALL DMAINV(NTERMS,1)
C
C     ------------------------------------------------------------------
C     CALCULATE THE FITTED POINTS - YCAL
C     AND VALUES (YII & YCC) TO BE USED IN QFN CALCULATIONS
C     ------------------------------------------------------------------
C
      DO 1030 I=1,NDAT
      SUM=0.0
      DO 1020 J=1,NTERMS
      SUM=SUM+BETA(J)*COMP(I,J)
 1020 CONTINUE
      YCAL(I)=SUM
      YCC(I) =SUM
      YII(I) =YIN(I)
 1030 CONTINUE
      IF(LGY.EQ.0) GO TO 1045
      DO 1040 I=1,NDAT
      YCC(I) =YCAL(I)
      YII(I) =DLOG(YIN(I))
      YCAL(I)=DEXP(YCAL(I))
 1040 CONTINUE
C
C     ------------------------------------------------------------------
C     CALCULATE CHISQ AND QFN 
C     ------------------------------------------------------------------
C
 1045 RESQF=0.0
      RESQT=0.0
      CHISQF=0.0
      CHISQT=0.0
      CHISQE=0.0
      NPTS=0
      NPTE=0
      DO 1050 I=1,NDAT
      RADD=(YCC(I)-YII(I))**2
      CADD=W(I)*RADD
      RESQT=RESQT+RADD
      CHISQT=CHISQT+CADD
      IF(IUSE(I).EQ.0) GO TO 1050
      RESQF=RESQF+RADD
      CHISQF=CHISQF+CADD
      NPTS=NPTS+1
      ERR=DSQRT(1.0/W(I))
      FAC=ERR/2.12
      DO 1046 II=1,10
      YADD=FAC*GRAN12(ISEED)
      CHISQE=CHISQE+W(I)*(YCC(I)-YII(I)-YADD)**2
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
C     ------------------------------------------------------------------
C     CALCULATE COEFFICIENT UNCERTS
C     ------------------------------------------------------------------
C
      DO 1120 I=1,NTERMS
      BERF(I)=100.0*DSQRT(DABS(QFNF*A(I,I)))/DABS(BETA(I))
      BERE(I)=100.0*DSQRT(DABS(QFNE*A(I,I)))/DABS(BETA(I))
 1120 CONTINUE
C
C     ------------------------------------------------------------------
C     LIST CHISQ, QFN, COEFFICIENTS, ETC
C     ------------------------------------------------------------------
C
      ITPF=Z'0C202020'
C
      WRITE(7,1125)ITPF
 1125 FORMAT(1H ,A4)
C
      WRITE(6,1130)
      WRITE(7,1130)
C
 1130 FORMAT(1H ,15('-----'),'--')
C
      WRITE(6,1135)IDN,CNAMF(1:STRLEN(CNAMF))
      WRITE(7,1135)IDN,CNAMF(1:STRLEN(CNAMF))
C
 1135 FORMAT(1H ,'LINEAR FIT, ID=',I4,' FILE= ',A)
C
      WRITE(6,1140)(ITIT(I,ITDX),I=1,8)
      WRITE(7,1140)(ITIT(I,ITDX),I=1,8)
C
 1140 FORMAT(1H ,'FIT TO THE FORM:  ',8A4/)
C
      WRITE(6,1142)
      WRITE(7,1142)
C
 1142 FORMAT(1H ,' SUM(RES)**2       CHISQ         QFN')
C
      IF(NPTS.EQ.NDAT) THEN
      WRITE(6,1143)RESQF,CHISQF,QFNF
      WRITE(7,1143)RESQF,CHISQF,QFNF
      ENDIF
C
      IF(NPTS.NE.NDAT) THEN
      WRITE(6,1144)RESQF,CHISQF,QFNF
      WRITE(6,1146)RESQT,CHISQT,QFNT
      WRITE(7,1144)RESQF,CHISQF,QFNF
      WRITE(7,1146)RESQT,CHISQT,QFNT
      ENDIF
C
 1143 FORMAT(1H ,3F12.3/)
 1144 FORMAT(1H ,3F12.3,' FOR FITTED DATA POINTS')
 1146 FORMAT(1H ,3F12.3,' FOR ALL    DATA POINTS')
C
      WRITE(6,1130)
      WRITE(7,1130)
C
      IF(KINDUU.EQ.1) THEN
      WRITE(6,1148)ALUN
      WRITE(7,1148)ALUN
      ENDIF
C
      IF(KINDUU.EQ.2) THEN
      WRITE(6,1150)USUN
      WRITE(7,1150)USUN
      ENDIF
C
 1148 FORMAT(1H ,'ALL UNCERTAINTIES (ERR(%)) SET TO ',F8.2)
 1150 FORMAT(1H ,'UNSPECIFIED UNCERTAINTIES ARE SET TO',F8.2,' %')
C
      IF(KINDUU.EQ.1) GO TO 1155
C
      WRITE(6,1130)
      WRITE(7,1130)
C
      IF(KINDU.EQ.1) THEN
      WRITE(6,1151)UMUL
      WRITE(7,1151)UMUL
      ENDIF
C
      IF(KINDU.EQ.2) THEN
      WRITE(6,1152)UMUL
      WRITE(7,1152)UMUL
      ENDIF
C
      IF(KINDU.EQ.3) THEN
      WRITE(6,1153)UMUL
      WRITE(7,1153)UMUL
      ENDIF
C
 1151 FORMAT(1H ,'ERR(%) IS ',F8.3,' TIMES "COUNTING STATISTICS"')
 1152 FORMAT(1H ,'ERR(%) IS ',F8.3,' TIMES GIVEN ABSOLUTE VALUES')
 1153 FORMAT(1H ,'ERR(%) IS ',F8.3,' TIMES GIVEN VALUES(%)')
C
 1155 WRITE(6,1130)
      WRITE(7,1130)
C
      WRITE(6,1160)
      WRITE(7,1160)
C
 1160 FORMAT(1H ,'TERM-# -TYPE  -SPECIFIER   COEF-VALUE  FIT-ERR(%)  ',
     &'EST-ERR(%)')
C
      DO 1180 I=1,NTERMS
      NDX=KINT(I)
      IGO=KTV(I)
      GO TO (1165,1170),IGO
C
 1165 WRITE(6,1168)I,NAMT(NDX),XTSP(I),BETA(I),BERF(I),BERE(I)
      WRITE(7,1168)I,NAMT(NDX),XTSP(I),BETA(I),BERF(I),BERE(I)
 1168 FORMAT(1H ,I6,2X,A4,F12.5,1PD13.5,0P2F12.2)
      GO TO 1180
C
 1170 WRITE(6,1172)I,NAMT(NDX),ITSP(I),BETA(I),BERF(I),BERE(I)
      WRITE(7,1172)I,NAMT(NDX),ITSP(I),BETA(I),BERF(I),BERE(I)
 1172 FORMAT(1H ,I6,2X,A4,I12,1PD13.5,0P2F12.2)
C
 1180 CONTINUE
C
      CTERMS=' '
      CTERML=' '
      DO 1190 I=1,NTERMS
      CTERMV=' '
      CTERMN=' '
      IGO=KTV(I)
      IF(IGO.EQ.1) CTERMN='-XP='
      IF(IGO.EQ.2) CTERMN='-LP='
      IF(IGO.EQ.1) WRITE(CTERMV,1182)XTSP(I)
      IF(IGO.EQ.2) WRITE(CTERMV,1184)ITSP(I)
 1182 FORMAT(F4.2)
 1184 FORMAT(I4)
      IF(CTERMN.NE.CTERML) LOST=STRAPPEND(CTERMS,CTERMN)
      IF(CTERMN.EQ.CTERML) LOST=STRAPPEND(CTERMS,' ,')
      CTERML=CTERMN
      LOST=STRAPPEND(CTERMS,CTERMV)
      LENG=STRLEN(CTERMS)
      IF(LENG.GE.40) GO TO 1195
 1190 CONTINUE
C
 1195 CONTINUE
C
C     ------------------------------------------------------------------
C     LIST THE RESULTS OF THE FIT
C     ------------------------------------------------------------------
C
      WRITE(6,1130)
      WRITE(7,1130)
C
      WRITE(6,1210)
      WRITE(7,1210)
C
 1210 FORMAT(1H ,'  I          X          Y  GIVEN-ERR  ERR(%)',
     &'       YCAL     YCAL-Y   %DIFF')
C
 1215 FORMAT(1H )
C
      NLN=0
      DO 1240 I=1,NDAT
      NLN=NLN+1
      IF(NLN.GT.5) THEN
      WRITE(6,1215)
      WRITE(7,1215)
      NLN=1
      ENDIF
      INDI='    '
      IF(IUSE(I).EQ.0) INDI='****'
      DIF=YCAL(I)-YIN(I)
      PCD=100.0*DIF/YIN(I)
      WRITE(6,1220)I,XIN(I),YIN(I),UIN(I),U(I),YCAL(I),DIF,PCD,INDI
      WRITE(7,1220)I,XIN(I),YIN(I),UIN(I),U(I),YCAL(I),DIF,PCD,INDI
 1220 FORMAT(1H ,I3,3(1PD11.3),0PF8.2,2(1PD11.3),0PF8.2,1X,A2)
 1240 CONTINUE
C
      CALL GREAD(IWD,LWD4,ITYP,NF,ISETW,8,NTER)
C
      LENG=STRLEN(CNAMF)
C
      IF(LENG.GT.20) LENG=20
C
      WRITE(TITLA,1245)IDN,CNAMF(1:LENG),CTERMS(1:STRLEN(CTERMS))
C
 1245 FORMAT('Lfit_ID=',I3,' of ',A,' - ',A)
C
      XLO=1.0E10
      XHI=-1.0E10
      DO 1250 I=1,NDAT
      IF(XIN(I).LT.XLO) XLO=XIN(I)
      IF(XIN(I).GT.XHI) XHI=XIN(I)
 1250 CONTINUE
      IF(LGX.NE.0) THEN
      XLO=DLOG(XLO)
      XHI=DLOG(XHI)
      ENDIF
      DELX=(XHI-XLO)/100.0
      XX=XLO-DELX
      DO 1270 I=1,101
      XX=XX+DELX
      IF(XX.EQ.0.0) XX=1.0E-20
      XLIN(I)=XX
      IF(LGX.NE.0)  XLIN(I)=DEXP(XX)
      SUM=0.0
      DO 1260 J=1,NTERMS
      SUM=SUM+BETA(J)*FUNKY(XX,J)
 1260 CONTINUE
      YLIN(I)=SUM
      IF(LGY.NE.0) YLIN(I)=DEXP(SUM)
 1270 CONTINUE
      NLIN=101
C
CX    CALL PLOTFIT(1,XIN,YIN,YCAL,NDAT,XLIN,YLIN,NLIN)
C
      IF(NTAB.LT.3.OR.TABDAT(3).LE.0.0) GO TO 2500
C
C     ------------------------------------------------------------------
C     PRINT OUT CALCULATED TABLE IF REQUESTED
C     ------------------------------------------------------------------
C
      SUM=0.0
      DO 1350 I=1,10
      XHED(I)=SUM
      SUM=SUM+DX
 1350 CONTINUE
C
      WRITE(7,1140)(ITIT(I,ITDX),I=1,8)
      WRITE(7,1355)
 1355 FORMAT(1H ,'CALCULATED VALUES OF Y VS X'/)
      WRITE(7,cIFH)XHED
C
      XX=XA
 1360 IF(XX.GT.XB) GO TO 2500
      XLO=XX
      DO 1380 I=1,10
      SUM=0.0
      ZZ=XX
      IF(ZZ.EQ.0.0) ZZ=1.0E-20
      IF(LGX.NE.0) ZZ=DLOG(ZZ)
      DO 1370 J=1,NTERMS
      SUM=SUM+BETA(J)*FUNKY(ZZ,J)
 1370 CONTINUE
      IF(LGY.EQ.0) YTAB(I)=SUM
      IF(LGY.NE.0) YTAB(I)=DEXP(SUM)
      XX=XX+DX
 1380 CONTINUE
C
      WRITE(7,cIFD)XLO,YTAB
      GO TO 1360
C
C     ------------------------------------------------------------------
C     LIST DIAGNOSTIC MESSAGES
C     ------------------------------------------------------------------
C
 2010 WRITE(CMSSG,2015)
 2015 FORMAT('Syntax error or illegal value - command not processed')
      GO TO 2400
C
 2020 WRITE(CMSSG,2025)
 2025 FORMAT('Data ID number not entered - no longer supported')
      GO TO 2400
C
 2050 IBAD='TERM'
      GO TO 2100
 2060 IBAD='LPOL'
 2100 WRITE(CMSSG,2110)LWD4(1,MM),LWD4(2,MM),IBAD
 2110 FORMAT(2A4,' is too many entries in',2X,A4,'-LIST')
      GO TO 2400
C
 2200 WRITE(CMSSG,2205)
 2205 FORMAT('End-of-file encountered on data-file')
      GO TO 2400
C
 2210 WRITE(CMSSG,2215)
 2215 FORMAT('Read error - reading data-file')
      GO TO 2400
C
 2400 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
C
 2500 IDONE='YES '
 2510 CALL GREAD(IWD,LWD4,ITYP,NF,ISETW,8,NTER)
      RETURN
      END
