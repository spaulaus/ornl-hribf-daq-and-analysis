C$PROG CMPGEN    - General command processor for damm
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 08/20/2005
C     ******************************************************************
C
      SUBROUTINE CMPGEN(IDONE,IERR)
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
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/HEPF/ IHEPF
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
      COMMON/DML1/ NAMFIL(20,20),KFIL(20),LUC(20),LIN,LCM,LCI
      INTEGER*4    NAMFIL,                LUC,    LIN,LCM,LCI
      CHARACTER*4                KFIL
C     ------------------------------------------------------------------
      COMMON/DML6/ ISIGNF
      CHARACTER*4  ISIGNF
C     ------------------------------------------------------------------
      COMMON/GN01/ NDLINES,IBELL
      INTEGER*4    NDLINES,IBELL
C     ------------------------------------------------------------------
      INTEGER*4    FIGNIT(20),KFIN(20)
      character*4  cfignit(20), ckfin(20)
      equivalence (cfignit,fignit), (ckfin,kfin)
C
      INTEGER*4    IHELP(1000,20)
C
      CHARACTER*4  IDONE,KMD
C
      EQUIVALENCE (KMD,LWD(1,1))
C
      DATA LUM,NDLINES,IBELL/3,16,Z'07070707'/
C   
      DATA cKFIN/7*'    ','N   ','    ','O   ','    ','Q   ','    ',
     &                   'R   ','    ','S   ','    ','P   ','BAN ',
     &                   '    '/
C
      DATA NCALL/0/
C
      DATA MSDLAF/1000/
C
      DATA cFIGNIT/'FIG ',' 1  ',18*'    '/
C
      SAVE
C
C     ==================================================================
C   
      IF(NCALL.EQ.0) THEN
                     NCALL=1
                     CALL NEWFIG(LUM,FIGNIT,IERR)
                     ENDIF
      IERR=0
C   
      IF(KMD.EQ.'TOF ') GO TO 50
      IF(KMD.EQ.'C   ') GO TO 60
      IF(KMD.EQ.'WAIT') GO TO 70
      IF(KMD.EQ.'WO  ') GO TO 80
      IF(KMD.EQ.'DLNS') GO TO 90
      IF(KMD.EQ.'LON ') GO TO 100
      IF(KMD.EQ.'LOF ') GO TO 100
      IF(KMD.EQ.'HELP') GO TO 110
      IF(KMD.EQ.'H   ') GO TO 110
      IF(KMD.EQ.'DIR ') GO TO 120
      IF(KMD.EQ.'LDIR') GO TO 120
      IF(KMD.EQ.'DSYM') GO TO 130
      IF(KMD.EQ.'LSYM') GO TO 130
      IF(KMD.EQ.'ZSYM') GO TO 130
      IF(KMD.EQ.'DFIL') GO TO 140
      IF(KMD.EQ.'SIDA') GO TO 170
      IF(KMD.EQ.'USDA') GO TO 170
C
      IF(KMD.EQ.'FIND') GO TO 180
      IF(KMD.EQ.'NOFI') GO TO 180
      IF(KMD.EQ.'DL  ') GO TO 190
      IF(KMD.EQ.'DMM ') GO TO 190
      IF(KMD.EQ.'LIN ') GO TO 190
      IF(KMD.EQ.'LOG ') GO TO 190
C   
      IF(KMD.EQ.'WIN ') GO TO 210
      IF(KMD.EQ.'NULO') GO TO 220
      IF(KMD.EQ.'CLO ') GO TO 250
C   
      IF(KMD.EQ.'CMAP') GO TO 310
      IF(KMD.EQ.'REVV') GO TO 320
C
      IF(KMD.EQ.'DLAF') GO TO 325
C
      IF(KMD.EQ.'FIGI') GO TO 330
      IF(KMD.EQ.'FIGF') GO TO 330
      IF(KMD.EQ.'FIG ') GO TO 340
C
      IF(KMD.EQ.'Z   ') GO TO 350
C
      RETURN
C   
   50 IF(LOGUP.NE.0) WRITE(LOGUP,55)
   55 FORMAT(1H1)
      GO TO 400
C
   60 CALL WAIT(1000,1,ISTAT)
      CALL XX_EVENTMAN
      GO TO 400
C
   70 CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IV.LT.1)     IV=1
      IF(IV.GT.60000) IV=60000
      CALL WAIT(IV,1,ISTAT)
      GO TO 400
C
   80 WRITE(LOGUT,82)
   82 FORMAT(' Type: [RETURN] to continue',$)
      READ(LCI,84,END=400,ERR=400)IXX
   84 FORMAT(A4)
      GO TO 400
C
   90 CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IV.LT.1)  IV=1
      IF(IV.GT.50) IV=50
      NDLINES=IV
      GO TO 400
C
  100 LISFLG=KMD
      GO TO 400
C   
  110 CALL HELPMANU(IWD,1,IHELP,999,NDLINES,IHEPF)
      GO TO 400
C   
  120 CALL SHOID
      GO TO 400
C   
  130 CALL SYMLOG(KMD)
      GO TO 400
C
  140 DO 160 J=1,20
      IF(KFIL(J).EQ.'SPK ') GO TO 145
      IF(KFIL(J).EQ.'HIS ') GO TO 145
      IF(KFIL(J).EQ.'BAN ') GO TO 145
      GO TO 160
  145 WRITE(LOGUT,150)KFIN(J),(NAMFIL(I,J),I=1,18)
  150 FORMAT(1H ,A4,'- ',18A4)
  160 CONTINUE
      GO TO 400
C   
  170 ISIGNF=KMD
      GO TO 400
C   
  180 CALL CMPPLO(IDONE,IERR)
      CALL FINDX(IERR)
      GO TO 400
C
  190 CALL CMPPLO(IDONE,IERR)
      CALL CMPSAM(IDONE,IERR)
      GO TO 400
C
  210 CALL CMPPLO(IDONE,IERR)
      CALL CMPLABL(IDONE,IERR)
      CALL DISPHANS(IERR)
      GO TO 400
C
  220 CALL NEWLOG
      GO TO 400
C
  250 CALL CLOSUM
      GO TO 400
C
  310 CALL COLRSET(LUM,IWDRAW,IERR)
      GO TO 400
C
  320 CALL REVV
      GO TO 400
C
  325 CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IV.LE.0)     IV=1000
      IF(IV.LT.10)    IV=10
      IF(IV.GT.20000) IV=20000
      MSDLAF=IV
      GO TO 400
C
  330 CALL NEWFIG(LUM,IWDRAW,IERR)
      GO TO 400
C
  340 CALL NEWFIG(LUM,LWD,IERR)
      KMD='FIGG'
      CALL XX_WINMAN(KMD,0)
      CALL WAIT(MSDLAF,1,ISTAT)
      GO TO 400
C
  350 CALL ZOTMAN
      GO TO 400
C
  400 IDONE='YES '
      RETURN
      END
