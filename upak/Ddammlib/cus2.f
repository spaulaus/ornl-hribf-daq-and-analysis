C$PROG CUS2      - Command processor for 2-D cursor input
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/12/2003
C     ******************************************************************
C
      SUBROUTINE CUS2(IDW,IX,JY,X,Y,KEY,KRETN)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/DML1/ NAMFIL(20,20),KFIL(20),LUC(20),LIN,LCM,LCI
      INTEGER*4    NAMFIL,                LUC,    LIN,LCM,LCI
      CHARACTER*4                KFIL
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
      COMMON/DML7/ NUD(20),NUB
      CHARACTER*4  NUD,    NUB
C     ------------------------------------------------------------------
      COMMON/PL03/ IDAT(4096),MINCN(2,20),MAXCN(2,20),MINZ,MAXZ
C     ------------------------------------------------------------------
      COMMON/PL04/ ILOCX(20),IHICX(20),ILOCY(20),IHICY(20),  !/PL04
     &             KLOCX(20),KHICX(20),KLOCY(20),KHICY(20),  !/PL04
     &             KDDP(20),MINZZ,MAXZZ                      !/PL04
C     ------------------------------------------------------------------
      COMMON/PL08/ BIAS,IFWHM,IFINDF,IPLOGF
      CHARACTER*4                    IPLOGF
C     ------------------------------------------------------------------
      COMMON/PL09/ XL(64,21),YL(64,21),NXYL(21),IBL(21),
     &             IBWN(21),LNBAN(4,21),MAXXYL
C     ------------------------------------------------------------------
      COMMON/PL13/ MARF(4,20),MARF2(2,20)
      CHARACTER*4  MARF,      MARF2
C     ------------------------------------------------------------------
      COMMON/XLPP/ IPROMP
      CHARACTER*4  IPROMP
C     ------------------------------------------------------------------
C
      CHARACTER*4  KEY,KRETN,MODE,ISTAT,ISAV,IDONE,KEYSAV,LISFSAV,ACF
C   
      DIMENSION    XC(64),YC(64)
C   
      INTEGER*4    BLACK,WHITE,RED,GREEN,BLUE,CYAN,MAGENTA,YELLOW
      character*4  cBLACK,cWHITE,cRED,cGREEN,cBLUE,
     &             cCYAN,cMAGENTA,cYELLOW
      equivalence  (BLACK,BLACK),(WHITE,WHITE),(RED,RED),
     &             (GREEN,GREEN),(BLUE,BLUE),(CYAN,CYAN),
     &             (MAGENTA,MAGENTA),(YELLOW,YELLOW)
      DATA        cBLACK,cWHITE,cRED,
     &            cGREEN,cBLUE,cCYAN,
     &            cMAGENTA,cYELLOW
     &  /'ERAS','WHIT','RED ','GREE','BLUE','GRBL','RDBL','RDGR'/
C   
      INTEGER*4    READ,RECL,STOR,REPL,DELE
      character*4  cREAD,cRECL,cSTOR,cREPL,cDELE
      equivalence  (cREAD,READ),(cRECL,RECL),(cSTOR,STOR),
     &             (cREPL,REPL),(cDELE,DELE)
      DATA cREAD,cRECL,cSTOR,cREPL,cDELE
     &      /'READ','RECL','STOR','REPL','DELE'/
C   
      INTEGER*4    DIFX,DIFY
C
      EQUIVALENCE (XC(1),XL(1,21)),
     &            (YC(1),YL(1,21)),
     &            (NXY,NXYL(21)),
     &            (IDOPN,IBL(21))
C   
C   
C
      DATA ACF,IPROMP/'OFF ','NO  '/
C
      CHARACTER*4  VONOF(20)
      DATA         VONOF/20*'ON  '/
C
      SAVE
C
C     ------------------------------------------------------------------
C
C     KLOCX(IDW) = "EXPAND REGION" X-LO-LIMIT (CHANNEL#)
C     KHICX(IDW) = "EXPAND REGION" X-HI-LIMIT (CHANNEL#)
C     KLOCY(IDW) = "EXPAND REGION" Y-LO-LIMIT (CHANNEL#)
C     KHICY(IDW) = "EXPAND REGION" Y-HI-LIMIT (CHANNEL#)
C     ------------------------------------------------------------------
C   
C     KLOCX(IDW)=-1
C     KHICX(IDW)=-1
C     KLOCY(IDW)=-1
C     KHICY(IDW)=-1
C   
      KRETN='    '
C
      DIFX=IHICX(IDW)-ILOCX(IDW)
      DIFY=IHICY(IDW)-ILOCY(IDW)
C
      IF(NUB.EQ.'YES ') GO TO 740
      IF(NUD(IDW).EQ.'YES ') GO TO 740
C
  100 NUD(IDW)='NO  '
      NUB='NO  '
C   
      IF(X.LT.FLOAT(ILOCX(IDW))) X=ILOCX(IDW)
      IF(X.GT.FLOAT(IHICX(IDW))) X=IHICX(IDW)
      IF(Y.LT.FLOAT(ILOCY(IDW))) Y=ILOCY(IDW)
      IF(Y.GT.FLOAT(IHICY(IDW))) Y=IHICY(IDW)
C   
      IF(ACF.EQ.'ON  ')GOTO 790 !TST FOR BAN-ID ACCUMULATE FLG
      IF(KEY.EQ.'G   ')GOTO 780 !TST FOR GET-ID REQUEST
      IF(KEY.EQ.'S   ')GOTO 785 !TST FOR SAV-ID REQUEST
C
      IF(KEY.EQ.'LF-A')GOTO 200 !< - "EXPAND REGION" LO-LIMIT
      IF(KEY.EQ.',   ')GOTO 200 !, - "EXPAND REGION" LO-LIMIT
      IF(KEY.EQ.'RT-A')GOTO 220 !> - "EXPAND REGION" HI-LIMIT
      IF(KEY.EQ.'.   ')GOTO 220 !. - "EXPAND REGION" HI-LIMIT
      IF(KEY.EQ.'V   ')GOTO 230 !V - TOGGLE MARKERS ON/OFF
      IF(KEY.EQ.'J   ')GOTO 260 !J - JUNK (DELETE) all MARKERS
C   
      IF(KEY.EQ.'1   ')GOTO 310 !MOVE PIC SO CURSOR AT LO-LEFT
      IF(KEY.EQ.'2   ')GOTO 320 !MOVE PIC SO CURSOR AT HI-LEFT
      IF(KEY.EQ.'3   ')GOTO 330 !MOVE PIC SO CURSOR AT HI-RIGHT
      IF(KEY.EQ.'4   ')GOTO 340 !MOVE PIC SO CURSOR AT LO-RIGHT
C   
      IF(KEY.EQ.'Z   ')GOTO 600 !ZERO BAN-LIST
      IF(KEY.EQ.'A   ')GOTO 610 !ADD POINT TO BAN-LIST
      IF(KEY.EQ.'D   ')GOTO 620 !DEL NEAREST POINT FROM BAN-LIST
      IF(KEY.EQ.'M   ')GOTO 630 !MOVE NEAREST POINT TO CUR-X,Y
      IF(KEY.EQ.'I   ')GOTO 640 !INSERT BEFORE NEAREST POINT
      IF(KEY.EQ.'L   ')GOTO 650 !LIST BAN-LIST
C   
      IF(KEY.EQ.'O   ')GOTO 810 !OPEN NEAREST BAN FOR MODIFICATION
      IF(KEY.EQ.'R   ')GOTO 830 !STOR NEAREST BAN WITH ORIGINAL ID
      IF(KEY.EQ.'F   ')GOTO 840 !REMOVE NEAREST BAN FROM LIB & ERAS
      IF(KEY.EQ.'K   ')GOTO 850 !DELE NEAREST BAN FROM LIB & DISK
      IF(KEY.EQ.'T   ')GOTO 1000!TOTALIZE COUNTS IN NEAREST BAN
      IF(KEY.EQ.'P   ')GOTO 1000!TOTALIZE COUNTS & SAVE PROJECTION
C   
      IF(KEY.EQ.'E   ')GOTO 500 !E - SAYS EXIT/EXPAND
      IF(KEY.EQ.'?   ')GOTO 550 !/ - DISPLAY  CURSOR COORDINATES
      IF(KEY.EQ.'''   ')GOTO 570!' - DISP/LOG CURS COORDINATES
      IF(KEY.EQ.'/   ')GOTO 550 !/ - DISPLAY  CURSOR COORDINATES
      RETURN
C   
C     ------------------------------------------------------------------
C     SET "EXPAND REGION"
C     ------------------------------------------------------------------
C   
  200 IF(MARF2(1,IDW).EQ.'ON  ') THEN
      CALL MARKIT2(IDW,'OGRE','LL  ',KLOCX(IDW),KLOCY(IDW))
      ENDIF
      KLOCX(IDW)=X+0.5
      KLOCY(IDW)=Y+0.5
      CALL MARKIT2(IDW,'OGRE','LL  ',KLOCX(IDW),KLOCY(IDW))
      MARF2(1,IDW)='ON  '
      RETURN
C   
  220 IF(MARF2(2,IDW).EQ.'ON  ') THEN
      CALL MARKIT2(IDW,'OGRE','UR  ',KHICX(IDW),KHICY(IDW))
      ENDIF
      KHICX(IDW)=X+0.5
      KHICY(IDW)=Y+0.5
      CALL MARKIT2(IDW,'OGRE','UR  ',KHICX(IDW),KHICY(IDW))
      MARF2(2,IDW)='ON  '
      RETURN
C
  230 IF(VONOF(IDW).EQ.'ON  ') GO TO 240     !Tst for markers now ON
      IF(VONOF(IDW).EQ.'OFF ') GO TO 250     !Tst for markers now OFF
      RETURN
C
  240 CALL MARKOF2(IDW)                      !Turn OFF markers
      VONOF(IDW)='OFF '                      !Set markers OFF flag
      RETURN
C
  250 CALL MARKON2(IDW)                      !Turn ON  markers
      VONOF(IDW)='ON  '                      !Set markers ON  flag
      RETURN
C
  260 CALL MARKIL2(IDW)                      !Kill markers
      VONOF(IDW)='ON  '                      !Set markers ON flag
      RETURN
C
  310 KLOCX(IDW)=X+0.5                   !MOVE CUR-POS TO LO-LEFT
      KLOCY(IDW)=Y+0.5
C   
      KHICX(IDW)=KLOCX(IDW)+DIFX
      KHICY(IDW)=KLOCY(IDW)+DIFY
C   
      IF(KHICX(IDW).GT.MAXCN(1,IDW)) KHICX(IDW)=MAXCN(1,IDW)
      IF(KHICY(IDW).GT.MAXCN(2,IDW)) KHICY(IDW)=MAXCN(2,IDW)
C   
      KLOCX(IDW)=KHICX(IDW)-DIFX
      KLOCY(IDW)=KHICY(IDW)-DIFY
C   
      GO TO 505
C   
  320 KLOCX(IDW)=X+0.5                    !MOVE CUR-POS TO UP-LEFT
      KHICY(IDW)=Y+0.5
C
      KHICX(IDW)=KLOCX(IDW)+DIFX
      KLOCY(IDW)=KHICY(IDW)-DIFY
C
      IF(KHICX(IDW).GT.MAXCN(1,IDW)) KHICX(IDW)=MAXCN(1,IDW)
      IF(KLOCY(IDW).LT.MINCN(2,IDW)) KLOCY(IDW)=MINCN(2,IDW)
C
      KLOCX(IDW)=KHICX(IDW)-DIFX
      KHICY(IDW)=KLOCY(IDW)+DIFY
C   
      GO TO 505
C   
  330 KHICX(IDW)=X+0.5                    !MOVE CUR-POS TO UP-RIGHT
      KHICY(IDW)=Y+0.5
C   
      KLOCX(IDW)=KHICX(IDW)-DIFX
      KLOCY(IDW)=KHICY(IDW)-DIFY
C   
      IF(KLOCX(IDW).LT.MINCN(1,IDW)) KLOCX(IDW)=MINCN(1,IDW)
      IF(KLOCY(IDW).LT.MINCN(2,IDW)) KLOCY(IDW)=MINCN(2,IDW)
C   
      KHICX(IDW)=KLOCX(IDW)+DIFX
      KHICY(IDW)=KLOCY(IDW)+DIFY
C   
      GO TO 505
C   
  340 KHICX(IDW)=X+0.5                    !MOVE CUR-POS TO LO-RIGHT
      KLOCY(IDW)=Y+0.5
C   
      KLOCX(IDW)=KHICX(IDW)-DIFX
      KHICY(IDW)=KLOCY(IDW)+DIFY
C   
      IF(KLOCX(IDW).LT.MINCN(1,IDW)) KLOCX(IDW)=MINCN(1,IDW)
      IF(KHICY(IDW).GT.MAXCN(2,IDW)) KHICY(IDW)=MAXCN(2,IDW)
C   
      KHICX(IDW)=KLOCX(IDW)+DIFX
      KLOCY(IDW)=KHICY(IDW)-DIFY
C   
      GO TO 505
C   
  500 IF(KLOCX(IDW).LT.0) GO TO 520
      IF(KHICX(IDW).LT.0) GO TO 520
C   
  505 CALL EXPAN2(IDW)
      KRETN='RES '
      RETURN
C   
  520 WRITE(CMSSG,525)
  525 FORMAT('EX UNDEFINED')
      CALL MESSLOG(LOGUT,0)
      GO TO 1200
C
  550 IF(IPLOGF.EQ.'PLON') GO TO 570
C   
      WRITE(CMSSG,555)X
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,560)Y
      CALL MESSLOG(LOGUT,LOGUP)
  555 FORMAT('X=',F8.1)
  560 FORMAT('Y=',F8.1)
      GO TO 1200
C
  570 CALL LUGET(KFL(1,IDW),IDUM,JDX,JDUM,IERR)
      JDX=JDX-1
      WRITE(CMSSG,575)IDL(1,IDW),X,Y,
     &                (NAMFIL(II,JDX),II=1,16)
  575 FORMAT('MAR2',I10,2F10.2,2X,16A4)
      LISFSAV=LISFLG
      LISFLG='LON '
      CALL MESSLOG(0,LOGUP)
      LISFLG=LISFSAV
C
      WRITE(CMSSG,555)X
      CALL MESSLOG(LOGUT,0)
      WRITE(CMSSG,560)Y
      CALL MESSLOG(LOGUT,0)
      GO TO 1200
C   
C     ------------------------------------------------------------------
C     PROCESS BAN-LIST COMMANDS
C     ------------------------------------------------------------------
C   
  600 MODE='ZERO'
      CALL BANOCK('TEST',IDW,ISTAT)
      IF(ISTAT.NE.'GOOD') RETURN
      GO TO 700
  610 MODE='ADD '
      CALL BANOCK('CHEK',IDW,ISTAT)
      IF(ISTAT.NE.'GOOD') RETURN
      GO TO 700
  620 MODE='DEL '
      CALL BANOCK('TEST',IDW,ISTAT)
      IF(ISTAT.NE.'GOOD') RETURN
      GO TO 700
  630 MODE='REP '
      CALL BANOCK('TEST',IDW,ISTAT)
      IF(ISTAT.NE.'GOOD') RETURN
      GO TO 700
  640 MODE='INS '
      CALL BANOCK('TEST',IDW,ISTAT)
      IF(ISTAT.NE.'GOOD') RETURN
      GO TO 700
  650 MODE='LIST'
      CALL BANOCK('TEST',IDW,ISTAT)
      IF(ISTAT.NE.'GOOD') RETURN
C   
  700 CALL BANHAN(IDW,MODE,X,Y)
      IF(NXY.LE.0) CALL BANOCK('CLOS',IDW,ISTAT)
      RETURN
C   
C     ------------------------------------------------------------------
C     DRAW ALL BANANAS - CURRENT AND THOSE IN BAN-LIB
C     ------------------------------------------------------------------
C   
  740 IF(NUD(IDW).EQ.'NO  ') GO TO 750
      NUD(IDW)='NO  '
      CALL BANOCK('TEST',IDW,ISTAT)  !TST FOR OPEN BANANA
      IF(ISTAT.EQ.'GOOD') THEN       !IF IN THIS WINDOW
      CALL BANDRA(IDW,IDOPN,YELLOW,  !NEW DISP - DRAW OPEN BAN
     &XC,YC,NXY)
                          ENDIF
      IF(NUB.EQ.'YES ')  GO TO 770   !IF NEW BANF, RESET INT LIB
      DO 745 I=1,MAXXYL              !OTHERWISE, LOOP TO
      IF(IBL(I).LE.0)    GO TO 745   !TST ID
      IF(IBWN(I).NE.IDW) GO TO 745   !TST FOR IN WINDOW
      CALL BANDRA(IDW,IBL(I),RED,    !AND DRAW IT CLOSED
     &XL(1,I),YL(1,I),NXYL(I))       !
  745 CONTINUE
      GO TO 100
C   
  750 IF(NUB.EQ.'NO  ')  GO TO 100   !NO NEW DISP OR BANF
C
      DO 760 I=1,MAXXYL              !IF NEW BANF, LOOP TO ERASE
      IF(IBL(I).LE.0)    GO TO 760   !TST BANLIB ID,
      IF(IBWN(I).NE.IDW) GO TO 760   !TST FOR IN WINDOW
      CALL BANDRA(IDW,IBL(I),RED,    !ERASE IT CLOSED
     &XL(1,I),YL(1,I),NXYL(I))       !
  760 CONTINUE
C
  770 DO 775 I=1,MAXXYL              !LOOP TO RESET BANLIB ID'S
      IBL(I)=-1
      IBWN(I)=0 
  775 CONTINUE
      NUB='NO  '                     !SET NO NEW BANF
      GO TO 100                      !AND GO HOME
C   
C     ------------------------------------------------------------------
C     PROMPT FOR & ACCUMULATE BAN-ID FOR "GET" & "STOR" REQUEST
C     ------------------------------------------------------------------
C   
  780 KEYSAV=KEY
      GO TO 790
  785 KEYSAV=KEY
      KEY='G   '
      IF(NXY.LE.0) RETURN               !SAVE - TST FOR EMPTY
      IF(KFIL(20).NE.'BAN ') GO TO 910  !TST FOR BAN-FILE OPEN
C
  790 IPROMP='YES '
      CALL GETINTEGER(ACF,KEY,IDONE,IBN)!PROMPT FOR ID & ACCUMULATE
      IF(IDONE.NE.'YES ')  RETURN
      IPROMP='NO  '
C
      IF(IBN.GT.9999)      GO TO 920
C
      IF(KEYSAV.EQ.'G   ') GO TO 800
      IF(KEYSAV.EQ.'S   ') GO TO 820
C   
C     ------------------------------------------------------------------
C     PROCESS BAN-STORE, RECALL, OPEN, ETC COMMANDS
C     ------------------------------------------------------------------
C   
  800 IF(IBN.LE.0)     RETURN           !TST FOR ERROR
      IF(KFIL(20).NE.'BAN ') GO TO 910  !TST FOR BAN-FILE OPEN
      CALL BANOCK('TEST',IDW,ISTAT)     !TST FOR ANY BAN OPEN IN
      IF(ISTAT.NE.'GOOD') GO TO 805     !THIS WINDOW
      IF(IBN.NE.IDOPN) GO TO 805        !TST FOR SAME ID OPEN
      MODE='ZERO'                       !IF IT IS, THEN
      CALL BANHAN(IDW,MODE,X,Y)         !ZOT IT
  805 CALL BANX(READ,IDW,IBN,NDX,IERR)  !READ IN AND STOR IN LIB
      IF(IERR.NE.0) GO TO 1200          !TST FOR ERROR
      CALL BANDRA(IDW,IBN,RED,          !DRAW BAN IN RED
     &XL(1,NDX),YL(1,NDX),NXYL(NDX))
      RETURN
C
  810 IF(NXY.NE.0) THEN
      IF(ISTAT.NE.'GOOD') RETURN   
                   ENDIF
      CALL NEARBAN(IDW,1,X,Y,IBN,NDX)   !OPEN - GET ID,NDX OF NEAREST
      IF(NDX.LE.0) RETURN               !TST FOR ERROR
      CALL PPBAN(RECL,IDW,IBN,NDX,IERR) !COPY TO CURRENT (OPEN) BAN
      IF(IERR.NE.0) GO TO 1200          !TST FOR ERROR
      CALL BANDRA(IDW,IBN,RED,          !DRAW IN RED TO ERASE
     &XL(1,NDX),YL(1,NDX),NXYL(NDX))
      CALL BANDRA(IDW,IBN,YELLOW,       !DRAW IT IN YELLOW
     &XL(1,NDX),YL(1,NDX),NXYL(NDX))
      IBL(NDX)=-1                       !DELETE BAN-LIB ENTRY
      IDOPN=IBN                         !SET BAN-ID
      CALL BANOCK('OPEN',IDW,ISTAT)     !SET OPEN FLAG
      RETURN
C   
  820 IF(IBN.LE.0) RETURN                    !TST FOR ERROR
      IF(KFIL(20).NE.'BAN ') GO TO 910       !TST FOR BAN-FILE OPEN
      CALL BANX(STOR,IDW,IBN,NDX,IERR)       !SAVE ON DISK & BAN-LIB
      IF(IERR.NE.0) GO TO 1200               !TST FOR ERROR
      CALL BANDRA(IDW,IDOPN,BLACK,XC,YC,NXY) !ERASE CURRENT BAN
      CALL BANDRA(IDW,IBN,RED,               !DRAW IT IN RED
     &XL(1,NDX),YL(1,NDX),NXYL(NDX))
      NXY=0                             !DELETE CURRENT BAN
      IDOPN=0                           !SET CURRENT-ID TO 0
      CALL BANOCK('CLOS',IDW,ISTAT)     !SET CLOSED FLAG
      RETURN
C   
  830 IF(NXY.LE.0)   RETURN             !REPLACE - TST FOR EMPTY
      IF(IDOPN.LE.0) RETURN             !REPLACE - TST FOR NEW
      IF(KFIL(20).NE.'BAN ') GO TO 910  !TST FOR BAN-FILE OPEN
C   
      CALL BANX(REPL,IDW,IDOPN,NDX,IERR)     !REPLACE ON DISK & BAN-LIB
      IF(IERR.NE.0) GO TO 1200               !TST FOR ERROR
      CALL BANDRA(IDW,IDOPN,BLACK,XC,YC,NXY) !ERASE CURRENT BAN
      CALL BANDRA(IDW,IDOPN,RED,             !DRAW IT IN RED
     &XL(1,NDX),YL(1,NDX),NXYL(NDX))
      NXY=0                             !DELETE CURRENT BAN
      IDOPN=0                           !SET CURRENT-ID TO 0
      CALL BANOCK('CLOS',IDW,ISTAT)     !SET CLOSED FLAG
      RETURN
C   
  840 CALL NEARBAN(IDW,1,X,Y,IBN,NDX)   !FORGET - NEAREST BAN IN LIB
      IF(IBN.LE.0) RETURN               !TST FOR ERROR
      CALL BANDRA(IDW,IBN,RED,          !ERASE IT
     &XL(1,NDX),YL(1,NDX),NXYL(NDX))
      IBL(NDX)=-1                       !DELETE IT FROM LIB
      RETURN
C   
  850 CALL NEARBAN(IDW,1,X,Y,IBN,NDX)   !KILL - NEAREST BAN
      IF(IBN.LE.0) RETURN               !TST FOR ERROR
      CALL BANX(DELE,IDW,IBN,NDX,IERR)  !DELETE FROM DISK
      IF(IERR.NE.0) GO TO 1200          !TST FOR ERROR
      CALL BANDRA(IDW,IBN,RED,          !ERASE IT
     &XL(1,NDX),YL(1,NDX),NXYL(NDX))
      IBL(NDX)=-1                       !DELETE BAN-LIB ENTRY
      RETURN
C   
  910 WRITE(LOGUT,915)
  915 FORMAT(1H ,'BAN-FILE NOT OPEN')
      RETURN
  920 WRITE(LOGUT,925)IBN
  925 FORMAT(1H ,'ILLEGAL BAN-ID (.GT.9999) =',I8)
      RETURN
C   
 1000 CALL NEARBAN(IDW,2,X,Y,IBN,NDX)
      IF(IBN.LT.0) RETURN
      ISAV='NO  '
      IF(KEY.EQ.'P   ') ISAV='YES '
C   
      CALL BANSUM(IDW,IBN,XL(1,NDX),YL(1,NDX),NXYL(NDX),ISAV)
C   
 1200 CALL MESSLOG(LOGUT,0)
      RETURN
      END
