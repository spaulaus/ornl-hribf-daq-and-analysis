C$PROG DISPHANS  - Processes display related requiest for 1-D fits
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/12/2003
C     ******************************************************************
C
      SUBROUTINE DISPHANS(IERR)
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
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/SM01/ I1,I2,NPK,NCOMP,IFBGD,QFN,QFLO,NVLU,NVLUI,DXFAC
C   
      COMMON/SM02/COMP(2048,44),YCAL(2048),BGD(2048),DATA(2048),WT(2048)
C   
      COMMON/SM04/ KXF(4,44),KFUN(44),BETA(50),IVF(4)
C   
      COMMON/SM06/ NUMITS,ICON
C     ------------------------------------------------------------------
      COMMON/SM11/ MDYHMS(5),KINFD,MINSY,MAXSY
      CHARACTER*4            KINFD
C     ------------------------------------------------------------------
      COMMON/SM05/ PAT(500,15),NPAT,MAXPAT,NUPAT,FWA,FWB,FWC,ASLO,ASHI
C   
C     ------------------------------------------------------------------
      COMMON/SM08/ MARFS(4,20),JLOX,JHIX,JLOF,JHIF,SLOX,SHIX,SLOF,SHIF
C
      CHARACTER*4  MARFS
      INTEGER*4                JLOX,JHIX,JLOF,JHIF
      REAL*4                                       SLOX,SHIX,SLOF,SHIF
C     ------------------------------------------------------------------
C
      COMMON/SM07/ WINL(4),KSCAL,LASIDW
C     ------------------------------------------------------------------
      COMMON/SM21/ NDXI(4),JDATE(3),JTIME(2),TITLE(20)
      CHARACTER*80                          CTITLE
      EQUIVALENCE (CTITLE,TITLE)
C     ------------------------------------------------------------------
      COMMON/SM22/ IDATA(2048),IDAT(2048,3),ISKF(2048)
C     ------------------------------------------------------------------
      COMMON/SM23/ JYLOG,JDSP,KDSP,MXDL,MXFR,MAXH,
     &             NOP,NBC,KINBAK,MARKS,ECA,ECB,ECC,
     &             DEL,DELFAC,NMUL,XSTEP,DXMAX,FWLO,FWHI,FALO,FAHI,
     &             MAXNC,MAXVP,KFUNS,NWOOD,KPPL,ASUM,ID,NSKIP,RESOK,
     &             ILO,IHI,SETW
      CHARACTER*4  JDSP,KDSP,KINBAK,NWOOD,KPPL,MARKS,SETW,RESOK
C     ------------------------------------------------------------------
      COMMON/SM25/ DATX(16384),IFI,NCH
C
      DIMENSION    DAT(2048,3),ISPK(16384)
      DIMENSION    LIV(5),XV(5)
      INTEGER*4    LAT(500,15),LLWD(80),LWDL(10),LIST(78)
      INTEGER*4    IDATE(3),ITIME(2)
      INTEGER*4    LWDS(2,40)
C
      CHARACTER*12 IDASK
C     ------------------------------------------------------------------
      COMMON/SM26/ KSORFIT, KFITFIL(20)
      CHARACTER*4  KSORFIT, KFITFIL
C     ------------------------------------------------------------------
      COMMON/SM27/ LTITL(20),KSOR,IDD,KDISPOK,JDSPL,NCHD
      CHARACTER*4            KSOR,    KDISPOK,JDSPL
      CHARACTER*80 CTITL
      EQUIVALENCE (CTITL,LTITL)
C     ------------------------------------------------------------------
C
      CHARACTER*4  KMD
C   
      EQUIVALENCE (DAT,IDAT)
      EQUIVALENCE (DATX,ISPK)
      EQUIVALENCE (LAT(1,1),PAT(1,1))
      EQUIVALENCE (LIST(1),LWD(1,2))
      EQUIVALENCE (KMD    ,LWD(1,1)),
     &            (LWDL(1),LWD(1,1)),
     &            (LLWD(1),LWD(1,1))
C   
      EQUIVALENCE (XLO ,WINL(1)),(XHI ,WINL(2)),
     &            (YMIN,WINL(3)),(YMAX,WINL(4))
C   
C
C     ------------------------------------------------------------------
C   
      INTEGER*4   BLACK,WHITE,RED,GREEN,BLUE,CYAN,MAGENTA,YELLOW
      character*4 cBLACK,cWHITE,cRED,cGREEN,cBLUE,
     &            cCYAN,cMAGENTA,cYELLOW
      equivalence (cBLACK,black),(cWHITE,white),(cRED,red),
     &            (cGREEN,green),(cBLUE,blue),(cCYAN,cyan),
     &            (cMAGENTA,magenta),(cYELLOW,yellow)
      DATA   cBLACK,cWHITE,cRED,cGREEN,
     &       cBLUE,cCYAN,cMAGENTA,cYELLOW/
     &      'ERAS','WHIT','RED ','GREE',
     &      'BLUE','GRBL','RDBL','RDGR'/
C   
      CHARACTER*4  INIT,PLOT
      DATA         INIT,PLOT,KDISPOK/'INIT','PLOT','NO  '/
C   
      CHARACTER*4  HIST,CVEC
      DATA         HIST,CVEC/'HIST','CVEC'/
      DATA         IDWD/1/
      DATA         LASN1,LASN2/0,0/
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      IERR=0
C
      DO 10 I=1,5
      LIV(I)=0
   10 CONTINUE
C
      IF(KMD.EQ.'DS  ') GO TO 100
      IF(KMD.EQ.'DSX ') GO TO 100
                        GO TO 200
C
  100 IF(NF.LT.1) GO TO 150
C
      DO 110 J=1,40
      LWDS(1,J)=LWD(1,J)
      LWDS(2,J)=LWD(2,J)
  110 CONTINUE
      NFS=NF
C
  150 CALL SORGET(LWDS(1,2),KSOR,IOF)
      NIV=NFS-IOF-1
      IF(NIV.GT.4) NIV=4
C
      DO 160 I=1,NIV
      JDX=I+IOF+1
      CALL MILV(LWDS(1,JDX),LIV(I),XV(I),KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
  160 CONTINUE
C   
      IF(KMD.EQ.'DSX ') GO TO 1220
      IF(KMD.EQ.'DS  ') GO TO 1230
      RETURN
C
  200 NIV=NF-1
      IF(NIV.GT.4) NIV=4
C
      DO 210 I=1,NIV
      JDX=I+1
      CALL MILV(LWD(1,JDX),LIV(I),XV(I),KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
  210 CONTINUE
C   
      IF(KMD.EQ.'WIN ') GO TO 1100
      IF(KMD.EQ.'DL  ') GO TO 1200
C
      IF(WINFLG(1,IDWD).EQ.0) GO TO 3090
C
      IF(KMD.EQ.'DF  ') GO TO 1800
      IF(KMD.EQ.'DC  ') GO TO 1800
      GO TO 3000
C   
 1100 IT=LIV(1)
      IF(IT.GT.NUMWIN) GO TO 3080
      IF(IT.LT.1)      GO TO 3080
      IDWD=IT
      RETURN
C
 1200 LASN1=0
      LASN2=0
      IF(NIV.NE.2) RETURN
      LASN1=LIV(1)+1
      LASN2=LIV(2)+1
      RETURN
C   
C     ------------------------------------------------------------------
C     DISPLAY DATA, PEAK-MARKERS & FIXED BACKGROUND
C     ------------------------------------------------------------------
C   
 1220 IF(WINFLG(1,IDWD).EQ.0) GO TO 3090
C
      CALL LUGET(KSOR,IDUM,JFI,IDUM,IERR)
      JFI=JFI-1
C
      IF(NIV.GE.1) IDD=LIV(1)
      IF(JHIX.LE.0) GO TO 3000
      N1=JLOX+1
      N2=JHIX+1
      GO TO 1240
C   
 1230 IF(WINFLG(1,IDWD).EQ.0) GO TO 3090
C
      CALL LUGET(KSOR,IDUM,JFI,IDUM,IERR)
      JFI=JFI-1
      N1=LASN1
      N2=LASN2
      IF(NIV.LT.1) GO TO 1235
      IDD=LIV(1)
      IF(NIV.LT.2) GO TO 1235
      N1=LIV(2)+1
      IF(NIV.LT.3) GO TO 1235
      N2=LIV(3)+1
C   
 1235 NDXI(1)=1
      IF(IDD.LE.0) GO TO 3100
      CALL SPKINS(KSOR,IDD,ISPK,NDXI,0,NCHD,IERR)
      IF(IERR.NE.0) RETURN
      NCALL=1
      IF(N1.LT.1)       N1=1
      IF(N2.LT.1)       N2=NCHD
      IF(N2-N1.GT.16383) N2=N1+16383
      LASN1=N1
      LASN2=N2
C   
 1240 IF(N1.LT.0.OR.N1.GE.N2) GO TO 3000
      NCH=N2-N1+1
      IF(NCH.GT.16384) GO TO 3070
      JDSPL=JDSP
C   
      CALL MILDATE2(IDATE)
      CALL MILTIME(ITIME)
C
      WRITE(IDASK,1242)IDD
 1242 FORMAT('ID=',I9)
      CALL SQUEZL(IDASK,1,12)
C
      CTITL= ' '
C
      IF(KSOR.NE.'M   ') THEN
      WRITE(CTITL,1245)(NAMFIL(I,JFI),I=1,8),IDATE,ITIME,IDASK
                         ENDIF
C
      IF(KSOR.EQ.'M   ') THEN
      WRITE(CTITL,1250)IDATE,ITIME,IDASK
                         ENDIF
C
 1245 FORMAT('SAM-',8A4,2A4,A1,1X,2A4,'  ',A)
 1250 FORMAT('SAM- MEMORY-BUFFER   ',2A4,A1,1X,2A4,'  ',A)
C
      CALL PLOTUMS(IDWD,N1,NCH,KDSP)
C
      RETURN
C
C     ------------------------------------------------------------------
C     DISPLAY THE FIT
C     ------------------------------------------------------------------
C
 1800 WRITE(IDASK,1242)ID
      CALL SQUEZL(IDASK,1,12)   
C
      CTITLE=' '
C
      IF(KSOR.NE.'M   ') THEN
      WRITE(CTITLE,1802)(KFITFIL(I),I=1,8),MDYHMS,IDASK,QFN
                         ENDIF
C
      IF(KSOR.EQ.'M   ') THEN
      WRITE(CTITLE,1804)MDYHMS,IDASK,QFN
                         ENDIF
C
 1802 FORMAT(8A4,5A4,A,' QFN=',F7.2,'    ')
 1804 FORMAT('MEMORY-BUFFER  ',5A4,A,' QFN=',F7.2)
C   
      IF(ILO.LT.0.OR.ILO.GE.IHI) GO TO 3000
C   
      KDISPOK='NO  '
C   
      IF(RESOK.NE.'YES ') GO TO 3010
C   
      IF(KMD.EQ.'DC  ') GO TO 2000
C   
 1805 IF(NUPAT.EQ.0)  CALL SWAPAT(1)
C   
      N1=ILO+1
      N2=IHI+1
      NCH=N2-N1+1
C   
      L1=N1
      L2=N2
      NCHX=NCH
C   
      IF(NIV.EQ.2) THEN
                   L1=LIV(1)+1
                   L2=LIV(2)+1
                   ENDIF
C   
      IF(L1.GT.N1) L1=N1
      IF(L2.LT.N2) L2=N2
      NCHX=L2-L1+1
      IF(NCHX.GT.16384) GO TO 3070
C   
      NDXI(1)=L1
C
C  
      CALL SPKINS(KSORFIT,ID,ISPK,NDXI,0,   NCHD,IERR)
C
C
      CALL SPKINS(KSORFIT,ID,ISPK,NDXI,NCHX,NCHD,IERR)
C   
      IF(L2.GT.NCHD) THEN
                     L2=NCHD
                     NCHX=L2-L1+1
                     ENDIF
C
      DO 1815 I=1,NCHX
      DATX(I)=ISPK(I)
 1815 CONTINUE
C   
      IF(IFBGD.GT.0) GO TO 1825
C   
      DO 1820 I=I1,I2
      DAT(I,1)=YCAL(I)
      DAT(I,2)=BGD(I)
 1820 CONTINUE
      GO TO 1840
C   
 1825 DO 1830 I=I1,I2
      DAT(I,1)=YCAL(I)+BGD(I)
      DAT(I,2)=BGD(I)
 1830 CONTINUE
C   
 1840 YMINI=1.0E7
      YMAXI=1.0
      CALL DATMM(DATX(1),1,NCHX,YMINI,YMAXI,YMIN,YMAX)
      YMINI=YMIN
      YMAXI=YMAX
      CALL DATMM(DAT(1,1),1,NCH,YMINI,YMAXI,YMIN,YMAX)
      YMINI=YMIN
      YMAXI=YMAX
      CALL DATMM(DAT(1,2),1,NCH,YMINI,YMAXI,YMIN,YMAX)
C   
      CALL DISTY(YMIN,JDSP,KDSP,KSCAL)
C   
      XLO=FLOAT(L1-1)
      XHI=XLO+FLOAT(NCHX)-1.0
C
      CALL BOXIT(IDWD,KDSP,TITLE,XLO,YMIN,XHI,YMAX)
C
      WINFLC(3,IDWD)='FIT '
C   
      CALL PLOTY(IDWD,HIST,WHITE,XLO,1.0,DATX,NCHX)
C   
      TLO=FLOAT(N1-1)
C   
      CALL PLOTY(IDWD,CVEC,GREEN,TLO,1.0,DAT(1,1),NCH)
C   
      IF(IFBGD.GT.0) GO TO 1850
C   
      CALL PLOTY(IDWD,CVEC,YELLOW,TLO,1.0,DAT(1,2),NCH)
      GO TO 1860
C   
 1850 CALL PLOB(IDWD,INIT)
      CALL PLOB(IDWD,PLOT)
C   
 1860 IF(KINFD.EQ.'FIT ') GO TO 1880
C   
      DO 1870 KPK=1,NPK
      CALL GETBETA(KPK,KPX,BETAK,ADUM)
      DO 1865 I=1,NCH
      DATX(I)=COMP(I,KPX)*BETAK
      IF(KINFD.EQ.'PPB ') DATX(I)=DATX(I)+BGD(I)
 1865 CONTINUE
C   
      CALL PLOTY(IDWD,CVEC,GREEN,TLO,1.0,DATX(1),NCH)
C   
 1870 CONTINUE
C   
 1880 IF(MARKS.EQ.'ON  ') CALL DMAR(IDWD)
C   
      IF(NUPAT.EQ.0) CALL SWAPAT(2)
      RETURN
C   
C     ------------------------------------------------------------------
C     PLOT ONE COMPONENT AND IT'S RESIDUALS
C     ------------------------------------------------------------------
C   
 2000 IF(NUPAT.EQ.0) CALL SWAPAT(1)
C   
      N1=ILO+1
      N2=IHI+1
      NCH=N2-N1+1
      NA=LIV(1)
      NB=LIV(2)
      IF(NA.LE.0) RETURN
      IF(NB.EQ.0) NB=NA
      IF(NB.GT.NPK) NB=NPK
      DO 2035 N=NA,NB
      DO 2020 I=I1,I2
      SUM=0.0
      DO 2010 J=1,NCOMP
      IF(J.EQ.N) GO TO 2010
      CALL GETBETA(J,JX,BETAJ,ADUM)
      SUM=SUM+BETAJ*COMP(I,JX)
 2010 CONTINUE
      DAT(I,1)=DATA(I)-SUM
      CALL GETBETA(N,NX,BETAN,ADUM)
      DAT(I,2)=BETAN*COMP(I,NX)
 2020 CONTINUE
C   
      YMINI=1.0E7
      YMAXI=1.0
      CALL DATMM(DAT(1,1),1,NCH,YMINI,YMAXI,YMIN,YMAX)
      YMINI=YMIN
      YMAXI=YMAX
      CALL DATMM(DAT(1,2),1,NCH,YMINI,YMAXI,YMIN,YMAX)
C   
      CALL DISTY(YMIN,JDSP,KDSP,KSCAL)
C   
      XLO=FLOAT(N1-1)
      XHI=XLO+FLOAT(NCH)-1.0
C   
      CALL BOXIT(IDWD,KDSP,TITLE,XLO,YMIN,XHI,YMAX)
C
      WINFLC(3,IDWD)='FIT '
C   
      CALL PLOTY(IDWD,HIST,WHITE,XLO,1.0,DAT(1,1),NCH)
C   
      CALL PLOTY(IDWD,CVEC,GREEN,XLO,1.0,DAT(1,2),NCH)
C   
      CALL DMAR(IDWD)
C   
 2035 CONTINUE
      IF(NUPAT.EQ.0) CALL SWAPAT(2)
      RETURN
C   
C     ------------------------------------------------------------------
C     SEND ERROR MESSAGES  ETC
C     ------------------------------------------------------------------
C   
 3000 WRITE(CMSSG,3005)
 3005 FORMAT('SYNTAX ERROR OR ILLEGAL CMD')
      GO TO 3500
C   
 3010 WRITE(CMSSG,3015)
 3015 FORMAT('NO VALID FIT RESULTS')
      GO TO 3500
C   
 3070 WRITE(CMSSG,3072)
 3072 FORMAT('MAX # OF CHANNELS YOU CAN DISPLAY IS 16384')
      GO TO 3500
C
 3080 WRITE(CMSSG,3082)
 3082 FORMAT('ILLEGAL WINDOW NUMBER REQUEST - IGNORED')
      GO TO 3500
C
 3090 WRITE(CMSSG,3092)IDWD
 3092 FORMAT('WINDOW-',I2,'  IS NOT OPEN  -  COMMAND IGNORED')
      GO TO 3500
C
 3100 WRITE(CMSSG,3105)IDD
 3105 FORMAT('ILLEGAL ID SPECIFICATION - ID =',I8,'  - IGNORED')
C
 3500 CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
