C$PROG XYMAN     - Reads ASCII X-Y files and draws on 1D & 2D disps
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE XYMAN(IDW,IERR)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
C
      INTEGER*4 MXID,MXXY,MXPQ
C
      PARAMETER (MXID=2048)
      PARAMETER (MXXY=256000)
      PARAMETER (MXPQ=500)
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
      COMMON/DML3/ IDL(33,20),KFL(33,20),CNO(33,20),         !/DML3
     &             NCHL(33,20),KOLR(33,20),NNID(20),KDST(20) !/DML3
      REAL*4       CNO
      INTEGER*4    IDL,KFL,NCHL,KOLR,NNID
      CHARACTER*4                                   KDST     !/DML3
C     ------------------------------------------------------------------
      COMMON/XLBB/ AA(2,20),BB(2,20),PLOTYP(20)
      REAL*4       AA,      BB
      CHARACTER*4                    PLOTYP
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/PL04/ ILOCX(20),IHICX(20),ILOCY(20),IHICY(20),  !/PL04
     &             KLOCX(20),KHICX(20),KLOCY(20),KHICY(20),  !/PL04
     &             KDDP(20), MINZZ,    MAXZZ                 !/PL04
C
      INTEGER*4    ILOCX,    IHICX,    ILOCY,    IHICY
      INTEGER*4    KLOCX,    KHICX,    KLOCY,    KHICY
      INTEGER*4    KDDP,     MINZZ,    MAXZZ
C     ------------------------------------------------------------------
      COMMON/PL16/ LENHIS(4,20)
      INTEGER*4    LENHIS
C     ------------------------------------------------------------------
C
      INTEGER*4    IDW,IERR
C
      INTEGER*4    IDN(MXID),LOC(MXID),NUM(MXID),IDLST(40)
C
      REAL*4       XLN(MXID),YLN(MXID)
C
      REAL*4       XL(MXXY),YL(MXXY),P(MXPQ),Q(MXPQ)
C   
      INTEGER*4    NAMF(20),JWD(20),MWD(3,40),JTYP(40)
C
      CHARACTER*4   IDST(2)
C
      CHARACTER*80  CNAMF
C
      CHARACTER*240 CMWD
C
      CHARACTER*8   CIDS
C
      CHARACTER*4   KMD,KOL,IRDY,KOLT,CLWD(2,40),CJWD(20)
C
      EQUIVALENCE (CNAMF,NAMF),(CJWD,JWD),(CMWD,MWD),(CIDS,IDST)
C
      EQUIVALENCE (KMD,LWD(1,1)),(CLWD,LWD)
C
      INTEGER*4    NID,NXY,LU,ISET,NUMD
      character*4  cISET
      equivalence  (ISET,cISET)
C
      DATA         NID,NXY,LU,cISET/0,0,3,'SETW'/
C
      DATA         KOL,IRDY,NUMD/'OGRE','NO  ',0/
C
      INTEGER*4    I,IOS,KIND,MF,MTER,N,K,IA,IB,NDO,NN,IV,ID,IX,JY
C
      REAL*4       XLNT,YLNT,X,Y,XV,CENX,CENY,CX,CY,XLEN,YLEN,XM,YM
C
      REAL*4       PX,PY,DSQ,DSQT
C
      SAVE
C
C     ------------------------------------------------------------------
C     XYMAN - READS IN ASCII XY-FILES & DRAWS LINES ON 1D & 2D displays
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(KMD.EQ.'XYC ') GO TO 50
      IF(KMD.EQ.'XYF ') GO TO 100
C
      IF(IRDY.NE.'YES ')GO TO 920
C 
      IF(KMD.EQ.'XYI ') GO TO 180
      IF(KMD.EQ.'XYD ') GO TO 200
      IF(KMD.EQ.'XYP ') GO TO 200
C
      RETURN
C
C     *************************************************************
C     SPECIFY COLOR (GRAPHICS CONTEXT) FOR XY-DISPLAY
C     *************************************************************
C
   50 KOLT=CLWD(1,2)
      IF(KOLT.EQ.'WHIT') GO TO 60
      IF(KOLT.EQ.'RED ') GO TO 61
      IF(KOLT.EQ.'GREE') GO TO 62
      IF(KOLT.EQ.'BLUE') GO TO 63
      IF(KOLT.EQ.'RG  ') GO TO 64
      IF(KOLT.EQ.'RB  ') GO TO 65
      IF(KOLT.EQ.'GB  ') GO TO 66
      GO TO 910
   60 KOL='OWHI'
      RETURN
   61 KOL='ORED'
      RETURN
   62 KOL='OGRE'
      RETURN
   63 KOL='OBLU'
      RETURN
   64 KOL='ORG '
      RETURN
   65 KOL='ORB '
      RETURN
   66 KOL='OGB '
      RETURN
C
C     *************************************************************
C     OPEN THE XY-FILE AND READ IN ALL DATA
C     *************************************************************
C
  100 DO 105 I=1,20
      NAMF(I)=0
  105 CONTINUE
      CALL LODNX(IWDRAW,1,80,NAMF,1)
C
      CLOSE(UNIT=LU)
C
      IRDY='NO  '
C
      OPEN(UNIT    = LU,
     &     FILE    = CNAMF,
     &     STATUS  = 'OLD',
     &     IOSTAT  = IOS)
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   IERR=1
                   RETURN
                   ENDIF
C
      IRDY='YES '
      NID=0
      NXY=0
      CALL GREAD(JWD,MWD,JTYP,MF,ISET,12,MTER)
C
  110 READ(LU,115,END=170)JWD
  115 FORMAT(20A4)
      CALL CASEUP(JWD)
      IF(CJWD(1).NE.'XYDA') GO TO 110
C
      CALL GREAD(JWD,MWD,JTYP,MF,1,80,MTER)
      READ(CMWD,120,ERR=150)ID,XLNT,YLNT
  120 FORMAT(12X,I12,2E12.0)
      NID=NID+1
      IF(NID.GT.MXID) GO TO 140
      IDN(NID)=ID
      XLN(NID) =XLNT
      YLN(NID) =YLNT
      LOC(NID)=NXY+1
      NUM(NID)=0
C
  130 READ(LU,115,END=170)JWD
      CALL CASEUP(JWD)
      IF(CJWD(1).EQ.'ENDA') GO TO 110
      IF(CJWD(1).EQ.'XYDA') THEN
                            BACKSPACE LU
                            GO TO 110
                            ENDIF
C
      CALL GREAD(JWD,MWD,JTYP,MF,1,80,MTER)
      READ(CMWD,135,ERR=150)X,Y
  135 FORMAT(2E12.0)
      NXY=NXY+1
      IF(NXY.GT.MXXY) GO TO 160
      XL(NXY)=X
      YL(NXY)=Y
      NUM(NID)=NUM(NID)+1
      GO TO 130
C
  140 WRITE(CMSSG,145)MXID
  145 FORMAT('MAX# OF XY-IDs EXCEEDED - MAX =',I8)
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 170
C
  150 WRITE(CMSSG,155)
  155 FORMAT('ERROR READING XY-FILE')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      GO TO 170
C
  160 WRITE(CMSSG,165)MXXY
  165 FORMAT('MAX# XY-ENTRIES EXCEEDED - MAX =',I8)
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1 
  170 CALL GREAD(JWD,MWD,JTYP,MF,ISET,8,MTER)
      RETURN

C
C     *************************************************************
C     DISPLAY DIRECTORY OF XY-LIBRARY
C     *************************************************************
C
  180 DO 190 I=1,NID
      WRITE(LOGUT,185)IDN(I),XLN(I),YLN(I)
  185 FORMAT(1H ,'ID,XL,YL=',I8,2F8.0)
  190 CONTINUE
      RETURN
C
  200 NDO=NF-1
      IF(NDO.LE.0) GO TO 250
      DO 220 I=1,NDO
      CALL MILV(LWD(1,I+1),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 900
      IDLST(I)=IV
  220 CONTINUE
      NUMD=NDO
C
  250 IF(NUMD.LE.0) GO TO 930
C
      IF(KDST(IDW).NE.'2D  ') GO TO 400

C
C     *************************************************************
C     DRAW LINES ON 2D-DISPLAYS
C     *************************************************************
C
      CENX=(ILOCX(IDW)+IHICX(IDW))/2.0
      CENY=(ILOCY(IDW)+IHICY(IDW))/2.0
C
      DO 300 N=1,NUMD
      ID=IDLST(N)
      DO 260 I=1,NID
      IF(IDN(I).EQ.ID) GO TO 270
  260 CONTINUE
      WRITE(LOGUT,265)ID
  265 FORMAT(1H ,'ID NOT FOUND =',I8)
      GO TO 300
C
  270 K=I
      IA=LOC(K)
      IB=LOC(K)+NUM(K)-1
      CX=1.0
      CY=1.0
      XLEN=LENHIS(1,IDW)
      YLEN=LENHIS(2,IDW)
      IF(XLEN.GT.0.0.AND.XLN(K).GT.0.0) CX=XLEN/XLN(K)
      IF(YLEN.GT.0.0.AND.YLN(K).GT.0.0) CY=YLEN/YLN(K)
      NN=0
      DSQ=1.0E12
      XM=CX*XL(1)
      YM=CY*YL(1)
      DO 290 I=IA,IB
      IF(NN.GT.MXPQ) THEN
                      NN=MXPQ
                      WRITE(LOGUT,280)MXPQ,ID
                      ENDIF
  280 FORMAT(1H ,'MAX# POINTS (',I4,') EXCEEDED FOR ID =',I8)
      NN=NN+1
      P(NN)=CX*XL(I)
      Q(NN)=CY*YL(I)
      DSQT=(P(NN)-CENX)**2+(Q(NN)-CENY)**2
      IF(DSQT.LT.DSQ) THEN
                      DSQ=DSQT
                      XM=P(NN)
                      YM=Q(NN)
                      ENDIF
  290 CONTINUE
      IF(KMD.EQ.'XYP ') CALL PLOTXY (IDW,KOL,P,Q,NN)
      IF(KMD.EQ.'XYD ') CALL PLOTXYL(IDW,KOL,P,Q,NN)
C
      IDST(1)='    '
      IDST(2)='    '   
      WRITE(IDST,295)ID
  295 FORMAT(I8)
      CALL SQUEZL(IDST,1,8)
C
      IX=AA(1,IDW)+BB(1,IDW)*XM
      JY=AA(2,IDW)+BB(2,IDW)*YM-15              
      CALL TEXOUT(IDW,CIDS,KOL,IX,JY)
  300 CONTINUE
      RETURN

C
C     *************************************************************
C     DRAW LINES ON 1D DISPLAYS
C     *************************************************************
C
  400 CENX=WINDAT(1,IDW)+0.5*WINDAT(3,IDW)
      CENY=WINDAT(2,IDW)-0.5*WINDAT(4,IDW)
C
      DO 600 N=1,NUMD
      ID=IDLST(N)
      DO 510 I=1,NID
      IF(IDN(I).EQ.ID) GO TO 520
  510 CONTINUE
      WRITE(LOGUT,265)ID
      GO TO 600
C
  520 K=I
      IA=LOC(K)
      IB=LOC(K)+NUM(K)-1
      CX=1.0
      XLEN=LENHIS(1,IDW)
      IF(XLEN.GT.0.0.AND.XLN(K).GT.0.0) CX=XLEN/XLN(K)
      CY=1.0
      NN=0
      DSQ=1.0E12
      IX=100
      JY=100
      DO 540 I=IA,IB
      NN=NN+1
      IF(NN.GT.MXPQ) THEN
                      NN=MXPQ
                      WRITE(LOGUT,280)MXPQ,ID
                      ENDIF
      P(NN)=CX*XL(I)
      Q(NN)=CY*YL(I)
      IF(PLOTYP(IDW).EQ.'LOG '.AND.Q(NN).LT.1.0) Q(NN)=1.0
C
      PX=AA(1,IDW)+BB(1,IDW)*P(NN)
      IF(PLOTYP(IDW).EQ.'LIN ') PY=AA(2,IDW)+BB(2,IDW)*Q(NN)
      IF(PLOTYP(IDW).EQ.'LOG ') PY=AA(2,IDW)+BB(2,IDW)*ALOG(Q(NN))
      DSQT=(PX-CENX)**2+(PY-CENY)**2
      IF(DSQT.LT.DSQ) THEN
                      DSQ=DSQT
                      IX=PX
                      JY=PY
                      ENDIF
  540 CONTINUE
      IF(KMD.EQ.'XYP ') CALL PLOTXY (IDW,KOL,P,Q,NN)
      IF(KMD.EQ.'XYD ') CALL PLOTXYL(IDW,KOL,P,Q,NN)
C
      IDST(1)='    '
      IDST(2)='    '   
      WRITE(IDST,545)ID
  545 FORMAT(I8)
      CALL SQUEZL(IDST,1,8)
C
      JY=JY-15              
      IF(JY.LT.20) JY=20
      IF(JY.GT.WINDAT(2,IDW)-20) JY=CENY
      CALL TEXOUT(IDW,CIDS,KOL,IX,JY)
  600 CONTINUE
      RETURN
C
  900 WRITE(LOGUT,905)
  905 FORMAT(1H ,'SYNTAX ERROR DECODING REQUEST')
      IERR=1
      RETURN
  910 WRITE(LOGUT,915)
  915 FORMAT(1H ,'ILLEGAL XY-COLOR SPECIFICATION')
      IERR=1
      RETURN
  920 WRITE(LOGUT,925)
  925 FORMAT(1H ,'XY-FILE NOT PROCESSED - COMMAND IGNORED')
      IERR=1
  930 WRITE(LOGUT,935)
  935 FORMAT(1H ,'ID-LIST UNDEFINED - COMMAND IGNORED')
      IERR=1
 1000 RETURN
      END
