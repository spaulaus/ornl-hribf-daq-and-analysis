C$PROG CUSS      - Command processor for 1-D cursor input for fitting
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/12/2003
C     ******************************************************************
C
      SUBROUTINE CUSS(IDW,IX,JY,X,Y,KEY,IRETN)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/SM02/COMP(2048,44),YCAL(2048),BGD(2048),DATA(2048),WT(2048)
C
      COMMON/SM05/ PAT(500,15),NPAT,MAXPAT,NUPAT,FWA,FWB,FWC,ASLO,ASHI
C   
      COMMON/SM07/ WINL(4),KSCAL,LASIDW
C   
C     ------------------------------------------------------------------
      COMMON/SM08/ MARFS(4,20),JLOX,JHIX,JLOF,JHIF,SLOX,SHIX,SLOF,SHIF
C
      CHARACTER*4  MARFS
      INTEGER*4                JLOX,JHIX,JLOF,JHIF
      REAL*4                                       SLOX,SHIX,SLOF,SHIF
C     ------------------------------------------------------------------
C
      COMMON/SM09/ XYP(50,2),NBXY
C
      COMMON/SM25/ DATX(16384),IFI,NCH
C
C     ------------------------------------------------------------------
      INTEGER*4 IORD(500),ITEM(500)
      INTEGER*4 LAT(500,15)
      REAL*4    PPOS(500),DAT(1)
C     ------------------------------------------------------------------
C
      CHARACTER*4  KEY,IRETN,LASKEY
C
      INTEGER*4 BLACK,WHITE,RED,GREEN,BLUE,CYAN,MAGENTA,YELLOW
      character*4 cBLACK,cWHITE,cRED,cGREEN,cBLUE,cCYAN,
     &            cMAGENTA,cYELLOW
      INTEGER*4 INIT,PLOT,ON,OFF,ERAS,REP
      character*4 cINIT,cPLOT,cON,cOFF,cERAS,cREP
C   
      EQUIVALENCE (PPOS(1),PAT(1,1))
      EQUIVALENCE (LAT(1,1),PAT(1,1))
      EQUIVALENCE (DAT,DATX)
C   
      DATA   cBLACK,cWHITE,cRED,cGREEN,cBLUE,cCYAN,
     &       cMAGENTA,cYELLOW
     &   /'ERAS','WHIT','RED ','GREE','BLUE','GRBL','RDBL','RDGR'/
C   
      DATA cINIT,cPLOT,cON,cOFF,cERAS,cREP/
     &     'INIT','PLOT','ON  ','OFF ','ERAS','REP '/
C   
      DATA YLOF,YHIF/0.0,0.0/
C
C     ------------------------------------------------------------------
      CHARACTER*4  VONOF(20)
      DATA         VONOF/20*'ON  '/
C     ------------------------------------------------------------------
C
      SAVE
C   
C     ------------------------------------------------------------------
C     PPOS(I)  = POSITION OF ITH PEAK (CHAN # + 1)
C   
C     LAT(I,6) = 0/1 SAYS VARY/FIX POSITION OF PEAK-I
C     LAT(I,7) = 0/1 SAYS VARY/FIX WIDTH    OF PEAK-I
C     LAT(I,8) = 0/1 SAYS VARY/FIX LO-ASYM  OF PEAK-I
C     LAT(I,9) = 0/1 SAYS VARY/FIX HI-ASYM  OF PEAK-I
C     LAT(I,10)= 1/0 SAYS PEAK-I ON/OFF
C     PAT(I,11)- POSITION (CHAN#) OF ITH PEAK
C   
C     XYP(1,J) = X-COORDINATE OF JTH BGD POINT ("CH#+1" UNITS)
C     XYP(2,J) = Y-COORDINATE OF JTH BGD POINT ("COUNTS")
C   
C     JLOX = "EXPAND REGION" LO-LIMIT (ARRAY INDEX)
C     JHIX = "EXPAND REGION" HI-LIMIT (ARRAY INDEX)
C     SLOX = "EXPAND REGION" LO-LIMIT (REAL  CHAN#)
C     SHIX = "EXPAND REGION" HI-LIMIT (REAL  CHAN#)
C   
C     JLOF = "FIT REGION" LO-LIMIT (ARRAY INDEX)
C     JHIF = "FIT REGION" HI-LIMIT (ARRAY INDEX)
C     SLOF = "FIT REGION" LO-LIMIT (REAL  CHAN#)
C     SHIF = "FIT REGION" HI-LIMIT (REAL  CHAN#)
C   
C     NPAT = # OF ENTRIES IN PEAK POSITION LIST
C     NBXY = # OF ENTRIES IN BGD  POINT    LIST
C     ------------------------------------------------------------------
C   
      IRETN='    '
C
      IF(IDW.NE.LASIDW) RETURN
C
      IF(KEY.EQ.'E   ')GOTO 780   !E - SAYS EXIT/EXPAND
      IF(KEY.EQ.'Q   ')GOTO 780   !Q - SAYS EXIT
      IF(KEY.EQ.'T   ')GOTO 820   !T - TEST (SHO IX,JY,X,Y)
C   
      IF(X.LE.WINL(1).OR.X.GE.WINL(2))GOTO 800
      IF(Y.LT.WINL(3).OR.Y.GE.WINL(4))GOTO 800
C
      IF(KEY.EQ.'J   ')GOTO 100   !J - JUNK (DELETE) ALL MARKERS
      IF(KEY.EQ.'V   ')GOTO 120   !V - TOGGLE MARKERS ON/OFF
C   
      IF(KEY.EQ.'P   ')GOTO 200   !P - ADD TO PEAK POS LIST
      IF(KEY.EQ.'M   ')GOTO 250   !M - MOVE NEAREST PEAK TO CURSOR
      IF(KEY.EQ.'B   ')GOTO 300   !B - ADD TO BGD POINT LIST
      IF(KEY.EQ.'LF-A')GOTO 400   !< - "EXPAND REGION" LO-LIMIT
      IF(KEY.EQ.',   ')GOTO 400   !, - "EXPAND REGION" LO-LIMIT
      IF(KEY.EQ.'RT-A')GOTO 420   !> - "EXPAND REGION" HI-LIMIT
      IF(KEY.EQ.'.   ')GOTO 420   !. - "EXPAND REGION" HI-LIMIT
      IF(KEY.EQ.'[   ')GOTO 500   ![ - "FIT    REGION" LO-LIMIT
      IF(KEY.EQ.']   ')GOTO 520   !] - "FIT    REGION" HI-LIMIT
      IF(KEY.EQ.'X   ')GOTO 600   !X    - FIX  PEAK POSITION
      IF(KEY.EQ.'W   ')GOTO 620   !W    - FIX  PEAK WIDTH
      IF(KEY.EQ.'L   ')GOTO 640   !L    - FIX  ASLO
      IF(KEY.EQ.'H   ')GOTO 660   !H    - FIX  ASHI
      IF(KEY.EQ.'O   ')GOTO 680   !O    - TURN PEAK "ON"
      IF(KEY.EQ.'/   ')GOTO 900   !? - DISPLAY X,Y-VALUE OF CURSOR
      IF(KEY.EQ.'S   ')GOTO 950   !S - DISPLAY SUM (JLOF TO JHIF)
      IF(KEY.EQ.'A   ')GOTO 950   !A - DISP NET SUM(ILO-IHI) & CENT
C   
      GO TO 2000
C
C     ------------------------------------------------------------------
C     TURN MARKER DISPLAY ON & OFF
C     ------------------------------------------------------------------
C
  100 CALL MARKILS(IDW)                      !Kill markers
      VONOF(IDW)='ON  '                      !Set markers ON flag
      RETURN
C
  110 CALL MARKONS(IDW)                      !Turn markers ON
      VONOF(IDW)='ON  '                      !Set markers flag ON
      RETURN
C
  120 IF(VONOF(IDW).EQ.'ON  ') GO TO 130     !Tst for markers now ON
      IF(VONOF(IDW).EQ.'OFF ') GO TO 140     !Tst for markers now OFF
      RETURN
C
  130 CALL MARKOFS(IDW)                      !Turn OFF markers
      VONOF(IDW)='OFF '                      !Set markers OFF flag
      RETURN
C
  140 CALL MARKONS(IDW)                      !Turn ON  markers
      VONOF(IDW)='ON  '                      !Set markers ON  flag
      RETURN
C   
C     ------------------------------------------------------------------
C     ADD TO PEAK LIST AND MARK IT
C     ------------------------------------------------------------------
C   
  200 IF(LASKEY.EQ.'U   ') GO TO 220
      IF(NPAT.GE.MAXPAT)   GO TO 2000
      NPAT=NPAT+1
      PAT(NPAT,1)=X
      PAT(NPAT,2)=FWA+FWB*SQRT(X)+FWC*X
      PAT(NPAT,3)=ASLO
      PAT(NPAT,4)=ASHI
C   
      LAT(NPAT,5)=0
      LAT(NPAT,6)=0
      LAT(NPAT,7)=0
      LAT(NPAT,8)=0
      LAT(NPAT,9)=0
      LAT(NPAT,10)=1
      PAT(NPAT,11)=X
      LAT(NPAT,12)=0
      CALL PMAR(IDW,NPAT)
      NUPAT=1
      GO TO 2000
C   
C     ------------------------------------------------------------------
C     DELETE NEAREST PEAK AND DRAW BLACK LINE
C     ------------------------------------------------------------------
C   
  220 IF(NPAT.LT.1) GO TO 2000
      IP=NERP(X)
      IF(IP.EQ.0) GO TO 2000
      CALL PMAR(IDW,IP)
      IA=IP+1
      IF(IA.GT.NPAT) GO TO 240
      DO 235 I=IA,NPAT
      DO 232 J=1,15
      LAT(I-1,J)=LAT(I,J)
  232 CONTINUE
  235 CONTINUE
  240 NPAT=NPAT-1
      NUPAT=1
      GO TO 2000
C   
C     ------------------------------------------------------------------
C     MOVE NEAREST PEAK (ON SCREEN) TO CURSOR LOC (SAME ATTRIBUTES)
C     ------------------------------------------------------------------
C   
  250 IP=NERP(X)
      IF(IP.EQ.0) GO TO 2000
      CALL PMAR(IDW,IP)
      PAT(IP,1)=X
      PAT(IP,11)=X
      CALL PMAR(IDW,IP)
      NUPAT=1
      GO TO 2000
C   
C     ------------------------------------------------------------------
C     ADD BACKGROUND POINT AND REDRAW THE WHOLE THING
C     ------------------------------------------------------------------
C   
  300 IF(LASKEY.EQ.'U   ') GO TO 350
      NBXY=NBXY+1
      IF(NBXY.GT.50) NBXY=50
      XYP(NBXY,1)=X
      XYP(NBXY,2)=Y
C   
  310 IF(NBXY.EQ.1) GO TO 315
C   
      CALL FINORD(XYP(1,1),IORD,ITEM,NBXY)
      CALL IORDER(XYP(1,1),IORD,ITEM,NBXY)
      CALL IORDER(XYP(1,2),IORD,ITEM,NBXY)
C   
  315 CALL PLOB(IDW,ERAS)
      CALL PLOB(IDW,PLOT)
      GO TO 2000
C   
C     ------------------------------------------------------------------
C     DELETE BACKGROUND POINT AND REDRAW
C     ------------------------------------------------------------------
C   
  350 IF(NBXY.LT.1) GO TO 2000
      IP=0
      DSQ=1.0E36
      DO 360 I=1,NBXY
      XT=XYP(I,1)
      IF(XT.LT.WINL(1)) GO TO 360
      IF(XT.GT.WINL(2)) GO TO 360
      DSQT=ABS(X-XT)
      IF(DSQ.LT.DSQT) GO TO 360
      IP=I
      DSQ=DSQT
  360 CONTINUE
C   
      IF(IP.LE.0) GO TO 2000
      IA=IP+1
      IF(IA.GT.NBXY) GO TO 375
C   
      DO 370 I=IA,NBXY
      XYP(I-1,1)=XYP(I,1)
      XYP(I-1,2)=XYP(I,2)
  370 CONTINUE
C   
  375 NBXY=NBXY-1
      GO TO 310
C   
C     ------------------------------------------------------------------
C     SET MARKERS FOR "EXPAND REGION"
C     ------------------------------------------------------------------
C   
  400 IF(MARFS(1,IDW).EQ.'ON  ') CALL MARKIT1(IDW,'OBLU',1,SLOX)
      SLOX=X
      JLOX=X+0.5
      CALL MARKIT1(IDW,'OBLU',1,SLOX)
      MARFS(1,IDW)='ON  '
      GO TO 2000
C   
  420 IF(MARFS(2,IDW).EQ.'ON  ') CALL MARKIT1(IDW,'OBLU',1,SHIX)
      SHIX=X
      JHIX=X+0.5
      CALL MARKIT1(IDW,'OBLU',1,SHIX)
      MARFS(2,IDW)='ON  '
      GO TO 2000
C   
C     ------------------------------------------------------------------
C     SET MARKERS FOR "FIT-REGION"
C     ------------------------------------------------------------------
C   
  500 IF(MARFS(3,IDW).EQ.'ON  ') CALL MARKIT1(IDW,'OBLU',2,SLOF)
      SLOF=X
      JLOF=X+0.5
      YLOF=Y
      CALL MARKIT1(IDW,'OBLU',2,SLOF)
      MARFS(3,IDW)='ON  '
      GO TO 2000
C   
  520 IF(MARFS(4,IDW).EQ.'ON  ') CALL MARKIT1(IDW,'OBLU',2,SHIF)
      SHIF=X
      JHIF=X+0.5
      YHIF=Y
      CALL MARKIT1(IDW,'OBLU',2,SHIF)
      MARFS(4,IDW)='ON  '
      GO TO 2000
C   
C     ------------------------------------------------------------------
C     DO THE FIX AND VARY BIT
C     ------------------------------------------------------------------
C   
  600 IF(LASKEY.EQ.'U   ') GO TO 610
      LDX=6                            !FIX  PEAK POSITION
      IFLG=1
      GO TO 750
C   
  610 LDX=6                            !VARY PEAK POSITION
      IFLG=0
      GO TO 750
C   
  620 IF(LASKEY.EQ.'U   ') GO TO 630
      LDX=7                            !FIX  PEAK WIDTH
      IFLG=1
      GO TO 750
C   
  630 LDX=7                            !VARY PEAK WIDTH
      IFLG=0
      GO TO 750
C   
  640 IF(LASKEY.EQ.'U   ') GO TO 650
      LDX=8                            !FIX  ASLO
      IFLG=1
      GO TO 750
C   
  650 LDX=8                            !VARY ASLO
      IFLG=0
      GO TO 750
C   
  660 IF(LASKEY.EQ.'U   ') GO TO 670
      LDX=9                            !FIX  ASHI
      IFLG=1
      GO TO 750
C   
  670 LDX=9                            !VARY ASHI
      IFLG=0
      GO TO 750
C   
  680 IF(LASKEY.EQ.'U   ') GO TO 690
      LDX=10                           !TURN PEAK "ON"
      IFLG=1
      GO TO 750
C   
  690 LDX=10                           !TURN PEAK "OFF"
      IFLG=0
      GO TO 750
C   
  750 IP=NERP(X)
      IF(IP.EQ.0) GO TO 2000
      CALL PMAR(IDW,IP)
      LAT(IP,LDX)=IFLG
      CALL PMAR(IDW,IP)
      GO TO 2000
C   
C     ------------------------------------------------------------------
C     RETURN & EXPAND OR JUST RETURN
C     ------------------------------------------------------------------
C   
C     SORT THE PEAK-POSITION AND ASSOCIATED TABLES
C     ------------------------------------------------------------------
C   
  780 IF(NPAT.LE.1) GO TO 795
      CALL FINORD(PPOS,IORD,ITEM,NPAT)
      DO 790 J=1,15
      CALL IORDER(PAT(1,J),IORD,ITEM,NPAT)
  790 CONTINUE
C
  795 IF(KEY.EQ.'E   '.AND.JHIX.GT.0) THEN
                                      CALL EXPANS(IDW)
                                      IRETN='RES '
                                      ENDIF
      GO TO 2000
C   
  800 WRITE(CMSSG,810)X,WINL(1),WINL(2),Y,WINL(3),WINL(4)
  810 FORMAT('WER-X,XL,Y,YL=', 3F8.1,3E11.3)
      GO TO 1100
C   
  820 WRITE(CMSSG,825)IX,JY,X,Y
  825 FORMAT('IX,JY,X,Y=',2I6,2F10.1)
      GO TO 1100
C   
C     ------------------------------------------------------------------
C     DISPLAY CHANNEL# AND CONTENTS TO WHICH CURSOR CORRESPONDS
C     ------------------------------------------------------------------
C   
  900 NC=X+0.5
      N1=NC-IFI+2
      IDAT=DAT(N1)+0.5
      WRITE(CMSSG,910)X,Y,IDAT
  910 FORMAT('C#,CURY,CNT =',2F8.1,I8)
      GO TO 1100
C   
C     ------------------------------------------------------------------
C     DISPLAY SUM OF CHANNEL CONTENTS BETWEEN CURSORS (JLOF,JHIF)
C          OR NET SUM OF COUNTS (JLOF-JHIF) AD CENTROID
C     ------------------------------------------------------------------
C   
  950 N1=JLOF-IFI+2
      N2=JHIF-IFI+2
      IF(N1.LT.1)   GO TO 2000
      IF(N1.GT.N2)  GO TO 2000
      IF(N2.GT.NCH) GO TO 2000
      XL=JLOF
C   
      IF(KEY.EQ.'A   ') THEN
                        YL=YLOF
                        YR=YHIF
                        ENDIF
C   
      IF(KEY.EQ.'S   ') THEN
                        YL=DAT(N1)
                        YR=DAT(N2)
                        ENDIF
C   
      CALL SSUM(DAT,N1,N2,XL,YL,YR,SRAW,SNET,CRAW,CNET,WRAW,WNET)
C   
      ISUMR=SRAW+0.5
      ISUMN=SNET+0.5
      IF(KEY.EQ.'S   ') GO TO 960
C
      WRITE(CMSSG,970)JLOF,JHIF,ISUMR,ISUMN,CNET,WNET
      GO TO 1100
  960 WRITE(CMSSG,975)JLOF,JHIF,ISUMR,ISUMN,CNET,WNET
      GO TO 1100
C
  970 FORMAT('LO,HI,SRAW,SNET,CEN,FWHM=',2I6,2I9,F8.1,F7.2,
     &       '  CUR-BG')
  975 FORMAT('LO,HI,SRAW,SNET,CEN,FWHM=',2I6,2I9,F8.1,F7.2,
     &       '  DAT-BG')
C   
C     ------------------------------------------------------------------
C     OUTPUT A LINE OF TEXT TO DIALOG WINDOW
C     ------------------------------------------------------------------
C   
 1100 CALL MESSLOG(LOGUT,LOGUP)
      CALL MESSLOG(LOGUT,0)
C
 2000 LASKEY=KEY
      RETURN
      END
