C$PROG DISPHAN2  - Processes display related requests for 2-D displays
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/12/2003
C     ******************************************************************
C
      SUBROUTINE DISPHAN2(IDW)
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
      COMMON/DML2/ IDATF(65536),IHEDF(32),MAXCH
      INTEGER*4    IDATF,       IHEDF,    MAXCH
C     ------------------------------------------------------------------
      COMMON/DML3/ IDL(33,20),KFL(33,20),CNO(33,20),         !/DML3
     &             NCHL(33,20),KOLR(33,20),NNID(20),KDST(20) !/DML3
      REAL*4       CNO                                       !/DML3
      INTEGER*4    IDL,NCHL,KOLR,NNID                        !/DML3
      CHARACTER*4             KFL,                  KDST     !/DML3
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/PL04/ ILOCX(20),IHICX(20),ILOCY(20),IHICY(20),  !/PL04
     &             KLOCX(20),KHICX(20),KLOCY(20),KHICY(20),  !/PL04
     &             KDDP(20),MINZZ,MAXZZ                      !/PL04
      CHARACTER*4  KDDP
C     ------------------------------------------------------------------
      COMMON/PL10/ IXOF(25),JYOF(25),SYMPO(25),KOLOFF,KINZMAP
      CHARACTER*4                                     KINZMAP
C     ------------------------------------------------------------------
C
      CHARACTER*4  KMD,KPLO,KINF,KODF,ITST,CLWD(2,40)
C
      INTEGER*4    LIST(2)
C   
      EQUIVALENCE (KMD,LWD(1,1)),(CLWD,LWD)
C   
      DATA         JLOCX,JHICX,JLOCY,JHICY,ID/0,0,0,0,0/
C   
      DATA         KLOCX,KHICX,KLOCY,KHICY/80*-1/
C   
      DATA         KODF/'N   '/
C   
      DATA         KRX,KRY/1,1/
      DATA         KPLO/'LOG '/
C
      DATA         KOLOFF,KINZMAP/10,'COLR'/
C
      DATA         IXOF/0,1,-1,0,0,-1,1,1,-1,0,0,-2,2,-1,1,1,
     &                 -1,-2,2,2,-2,-2,2,-2,2/
C
      DATA         JYOF/0,0,0,-1,1,-1,1,-1,1,-2,2,0,0,-2,2,-2,
     &                  2,-1,1,-1,1,-2,2,2,-2/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(KMD.EQ.'ZINT') GO TO 180
      IF(KMD.EQ.'DD  ') GO TO 200
      IF(KMD.EQ.'DDX ') GO TO 200
C   
      DO 120 I=1,2
      CALL MILV(LWD(1,I+1),LIST(I),XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 500
  120 CONTINUE
C   
      IF(KMD.EQ.'XC  ') GO TO 130
      IF(KMD.EQ.'YC  ') GO TO 140
      IF(KMD.EQ.'ZMM ') GO TO 150
      IF(KMD.EQ.'ZLOG') GO TO 160
      IF(KMD.EQ.'ZLIN') GO TO 165
      IF(KMD.EQ.'CRNX') GO TO 170
      IF(KMD.EQ.'CRNY') GO TO 175
C   
      GO TO 500
C
  130 IF(LIST(1).GT.LIST(2).AND.LIST(2).GT.0) GO TO 500   
      JLOCX=LIST(1)
      JHICX=LIST(2)
      RETURN
  140 IF(LIST(1).GT.LIST(2).AND.LIST(2).GT.0) GO TO 500
      JLOCY=LIST(1)
      JHICY=LIST(2)
      RETURN
  150 MINZZ=LIST(1)
      MAXZZ=LIST(2)
      RETURN
  160 KPLO='LOG '
      RETURN
  165 KPLO='LIN '
      RETURN
C
  170 KRX=LIST(1)
      RETURN
  175 KRY=LIST(1)
      RETURN
C   
  180 ITST=CLWD(1,2)
      IF(ITST.EQ.'    ') ITST='COLR'
      IF(ITST.EQ.'COLO') ITST='COLR'
      IF(ITST.EQ.'GRAY') ITST='GREY'
      IF(ITST.EQ.'COLR') GO TO 185
      IF(ITST.EQ.'GREY') GO TO 185
      IF(ITST.EQ.'DOTS') GO TO 185
      GO TO 500
  185 KINZMAP=ITST
      KOLOFF=10
      IF(KINZMAP.EQ.'GREY') KOLOFF=20
      RETURN
C
  200 IF(WINFLG(1,IDW).EQ.0) GO TO 530
      IF(NF.GT.3) GO TO 500
      IF(NF.EQ.1) GO TO 215
      JJ=2
C   
      IF(NF.EQ.2) THEN
                  KODF='N   '
                  JJ=2
                  ENDIF
C   
      IF(NF.EQ.3) THEN
                  KODF=CLWD(1,2)
                  JJ=3
                  ENDIF
C   
      CALL MILV(LWD(1,JJ),ID,XV,KIND,IERR)
      IF(IERR.NE.0)      GO TO 500
      CALL LUGET(KODF,LUH2,LUD2,KINF,IERR)
      IF(IERR.NE.0)      GO TO 520
      IF(KINF.NE.'HIS ') GO TO 520
C   
  215 IDL(1,IDW)=ID
      KFL(1,IDW)=KODF
      NNID(IDW)=1
C   
      IF(KMD.EQ.'DDX ') GO TO 250
C   
      CALL PLOTUM2(IDW,ID,KPLO,JLOCX,JHICX,JLOCY,JHICY,KRX,KRY)
C
CX    IF(JLOCX.GT.0.OR.JLOCY.GT.0) THEN
CX    CALL MARKIT2(IDW,'WHIT','LL  ',JLOCX,JLOCY)
CX    ENDIF
C
      IF(JHICX.GT.0.OR.JHICY.GT.0) THEN
      CALL MARKIT2(IDW,'WHIT','UR  ',JHICX,JHICY)
      ENDIF
C
      RETURN
C   
  250 LLOCX=KLOCX(IDW)
      LHICX=KHICX(IDW)
      LLOCY=KLOCY(IDW)
      LHICY=KHICY(IDW)
      IF(LLOCX.LT.0) GO TO 510
      IF(LHICX.LT.0) GO TO 510
C   
      CALL PLOTUM2(IDW,ID,KPLO,LLOCX,LHICX,LLOCY,LHICY,KRX,KRY)
C
CX    IF(JLOCX.GT.0.OR.JLOCY.GT.0) THEN
CX    CALL MARKIT2(IDW,'WHIT','LL  ',JLOCX,JLOCY)
CX    ENDIF
C
      IF(JHICX.GT.0.OR.JHICY.GT.0) THEN
      CALL MARKIT2(IDW,'WHIT','UR  ',JHICX,JHICY)
      ENDIF
C
      RETURN
C
C     ------------------------------------------------------------------
C     Send error messages
C     ------------------------------------------------------------------
C   
  500 WRITE(CMSSG,505)
  505 FORMAT('SYNTAX ERROR OR ILLEGAL REQUEST')
      GO TO 600
C   
  510 WRITE(CMSSG,515)
  515 FORMAT('EXPAND REGION UN-DEFINED')
      GO TO 600
C   
  520 WRITE(CMSSG,525)
  525 FORMAT('REQUESTED FILE NOT OPEN OR NOT A HIS-FILE')
      GO TO 600
C
  530 WRITE(CMSSG,535)IDW
  535 FORMAT('WINDOW-',I2,'  UNDEFINED - COMMAND IGNORED')
C
  600 CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
