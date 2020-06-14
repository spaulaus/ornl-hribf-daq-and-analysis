C$PROG CUS1      - Command processor for 1-D cursor input
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/12/2003
C     ******************************************************************
C
      SUBROUTINE CUS1(IDW,IX,JY,X,Y,KEY,KRETN)
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
      COMMON/PL01/ ILOX(20),IHIX(20),ILOF(20),IHIF(20),      !/PL01
     &             FLOX(20),FHIX(20),FLOF(20),FHIF(20),      !/PL01
     &             GWID,MOPL,ECAL(3)                         !/PL01
      CHARACTER*4       MOPL
C     ------------------------------------------------------------------
      COMMON/PL08/ BIAS,IFWHM,IFINDF,IPLOGF
      CHARACTER*4                    IPLOGF
C     ------------------------------------------------------------------
      COMMON/PL13/ MARF(4,20),MARF2(2,20)
      CHARACTER*4  MARF,      MARF2
C     ------------------------------------------------------------------
C
      CHARACTER*4  KEY,KRETN
C   
      INTEGER*4 BLACK,WHITE,RED,GREEN,BLUE,CYAN,MAGENTA,YELLOW,READ
C   
      INTEGER*4 INIT,PLOT,ON,OFF,ERAS,REP,IDAT(1)
C   
      CHARACTER*8 CENE
C
      REAL*8 SRAW,SNET
C
      INTEGER*4 ANUM(3),BNUM(3),ANUM2(2),BNUM2(2)
C
      EQUIVALENCE (ANUM2,ANUM),(BNUM2,BNUM)
      EQUIVALENCE (IDAT,IDATF)
C   
      character*4 cblack, cwhite, cred, cgreen, cblue, ccyan,
     &            cmagenta, cyellow
      equivalence (cblack, black), (cwhite, white), (cred,red),
     &            (cgreen, green), (cblue, blue), (ccyan, cyan)
      DATA   cBLACK,cWHITE,cRED,cGREEN,cBLUE,
     &       cCYAN,cMAGENTA,cYELLOW/
     &      'ERAS','WHIT','RED ','GREE','BLUE','GRBL','RDBL','RDGR'/
C   
      character*4 cINIT,cPLOT,cON,cOFF,cERAS,cREP
      equivalence (cINIT,INIT), (cPLOT,PLOT), (cON,ON), 
     &            (cOFF,OFF), (cERAS,ERAS), (cREP,REP)
      DATA cINIT,cPLOT,cON,cOFF,cERAS,cREP/'INIT','PLOT',
     &           'ON  ','OFF ','ERAS','REP '/
C   
      DATA YLOF,YHIF/0.0,0.0/
      character*4 cREAD
      equivalence (cread, read)
      DATA cREAD/'READ'/
C
      CHARACTER*4 LISFSAV
C
      CHARACTER*4  VONOF(20)
      DATA         VONOF/20*'ON  '/
C
      SAVE
C
C     ------------------------------------------------------------------

C   
C     ------------------------------------------------------------------
C     ILOX = "EXPAND REGION" LO-LIMIT (ARRAY INDEX)
C     IHIX = "EXPAND REGION" HI-LIMIT (ARRAY INDEX)
C     FLOX = "EXPAND REGION" LO-LIMIT (REAL  CHAN#)
C     FHIX = "EXPAND REGION" HI-LIMIT (REAL  CHAN#)
C   
C     ILOF = "SUM    REGION" LO-LIMIT (ARRAY INDEX)
C     IHIF = "SUM    REGION" HI-LIMIT (ARRAY INDEX)
C     FLOF = "SUM    REGION" LO-LIMIT (REAL  CHAN#)
C     FHIF = "SUM    REGION" HI-LIMIT (REAL  CHAN#)
C     ------------------------------------------------------------------
C   
      KRETN='    '
C
      IFI=WINDAT(5,IDW)+1.5
      NCH=WINDAT(7,IDW)-WINDAT(5,IDW)+1.5
C
      IF(KEY.EQ.'E   ') GO TO 780  !E - SAYS EXIT/EXPAND
      IF(KEY.EQ.'T   ') GO TO 820  !T - TEST (SHO IX,JY,X,Y)
C   
      IF(KEY.EQ.'M   ') GO TO 210  !M - MARKERS ON
      IF(KEY.EQ.'V   ') GO TO 220  !V - TOGGLE MARKERS ON/OFF
      IF(KEY.EQ.'J   ') GO TO 200  !J - JUNK (DELETE) ALL MARKERS
C   
      IF(KEY.EQ.'LF-A') GO TO 400  !< - "EXPAND REGION" LO-LIMIT
      IF(KEY.EQ.',   ') GO TO 400  !, - "EXPAND REGION" LO-LIMIT
      IF(KEY.EQ.'RT-A') GO TO 420  !> - "EXPAND REGION" HI-LIMIT
      IF(KEY.EQ.'.   ') GO TO 420  !. - "EXPAND REGION" HI-LIMIT
C   
      IF(KEY.EQ.'UP-A') GO TO 430  !PAN UP   (SEE HIGHER CHANNELS)
      IF(KEY.EQ.'DN-A') GO TO 440  !PAN DOWN (SEE LOWER  CHANNELS)
C   
      IF(KEY.EQ.'[   ') GO TO 500  ![ - "SUM    REGION" LO-LIMIT
      IF(KEY.EQ.'L   ') GO TO 500  !L - "SUM    REGION" LO-LIMIT
      IF(KEY.EQ.']   ') GO TO 520  !] - "SUM    REGION" HI-LIMIT
      IF(KEY.EQ.'H   ') GO TO 520  !H - "SUM    REGION" HI-LIMIT
C   
      IF(KEY.EQ.'G   ') GO TO 500  !G - "SUM    REGION" LO & HI
C   
      IF(KEY.EQ.'P   ') GO TO 830  !P -DRAW PEAK MARKER & DISP ENER
      IF(KEY.EQ.'C   ') GO TO 830  !C -DRAW PEAK MARKER & DISP CHAN
      IF(KEY.EQ.'?   ') GO TO 900  !/ - DISPLAY X,Y-VALUE OF CURSOR
      IF(KEY.EQ.'/   ') GO TO 900  !/ - DISPLAY X,Y-VALUE OF CURSOR
      IF(KEY.EQ.'''   ')GO TO 910  !' - DISP/LOG X,Y-VALUE OF CURS
      IF(KEY.EQ.'S   ') GO TO 920  !S - DISPLAY SUM (ILOF TO IHIF)
      IF(KEY.EQ.'A   ') GO TO 920  !A - DISPLAY SUM (ILOF TO IHIF)
C   
      RETURN
C   
C     ------------------------------------------------------------------
C     TURN MARKER DISPLAY ON & OFF
C     ------------------------------------------------------------------
C
  200 CALL MARKIL1(IDW)                      !Kill markers
      VONOF(IDW)='ON  '                      !Set markers ON flag
      RETURN
C
  210 CALL MARKON1(IDW)                      !Turn markers ON
      VONOF(IDW)='ON  '                      !Set markers flag ON
      RETURN
C
  220 IF(VONOF(IDW).EQ.'ON  ') GO TO 230     !Tst for markers now ON
      IF(VONOF(IDW).EQ.'OFF ') GO TO 240     !Tst for markers now OFF
      RETURN
C
  230 CALL MARKOF1(IDW)                      !Turn OFF markers
      VONOF(IDW)='OFF '                      !Set markers OFF flag
      RETURN
C
  240 CALL MARKON1(IDW)                      !Turn ON  markers
      VONOF(IDW)='ON  '                      !Set markers ON  flag
      RETURN
C   
C     ------------------------------------------------------------------
C     SET MARKERS FOR "EXPAND REGION"
C     ------------------------------------------------------------------
C   
  400 IF(MARF(1,IDW).EQ.'ON  ') CALL MARKIT1(IDW,'OBLU',1,FLOX(IDW))
      FLOX(IDW)=X
      ILOX(IDW)=X+0.5
      CALL MARKIT1(IDW,'OBLU',1,FLOX(IDW))
      MARF(1,IDW)='ON  '
      RETURN
C   
  420 IF(MARF(2,IDW).EQ.'ON  ') CALL MARKIT1(IDW,'OBLU',1,FHIX(IDW))
      FHIX(IDW)=X
      IHIX(IDW)=X+0.5
      CALL MARKIT1(IDW,'OBLU',1,FHIX(IDW))
      MARF(2,IDW)='ON  '
      RETURN
C   
C     ------------------------------------------------------------------
C     PAN LEFT OR RIGHT
C     ------------------------------------------------------------------
C   
  430 IF(FLOX(IDW).LT.0.0) GO TO 450
      IF(FHIX(IDW).LE.0.0) GO TO 450
C   
      DFX=FHIX(IDW)-FLOX(IDW)
      FHIX(IDW)=X+DFX
      XUP=MAXCH-1
      IF(FHIX(IDW).GT.XUP) FHIX(IDW)=XUP
      FLOX(IDW)=FHIX(IDW)-DFX
      IF(FLOX(IDW).LT.0.0) FLOX(IDW)=0.0
      ILOX(IDW)=FLOX(IDW)+0.5
      IHIX(IDW)=FHIX(IDW)+0.5
      CALL EXPAN1(IDW)
      KRETN='RES '
      RETURN
C   
  440 IF(FLOX(IDW).LT.0.0) GO TO 450
      IF(FHIX(IDW).LE.0.0) GO TO 450
C   
      DFX=FHIX(IDW)-FLOX(IDW)
      FLOX(IDW)=X-DFX
      IF(FLOX(IDW).LT.0.0) FLOX(IDW)=0.0
      FHIX(IDW)=FLOX(IDW)+DFX
      XUP=MAXCH-1
      IF(FHIX(IDW).GT.XUP) FHIX(IDW)=XUP
      ILOX(IDW)=FLOX(IDW)+0.5
      IHIX(IDW)=FHIX(IDW)+0.5
      CALL EXPAN1(IDW)
      KRETN='RES '
      RETURN
C   
  450 WRITE(CMSSG,455)
  455 FORMAT('EXPAND REGION MUST BE SPECIFIED FOR PAN')
      GO TO 1100
C   
C     ------------------------------------------------------------------
C     SET MARKERS FOR "SUM-REGION"
C     ------------------------------------------------------------------
C   
  500 IF(MARF(3,IDW).EQ.'ON  ') CALL MARKIT1(IDW,'OBLU',2,FLOF(IDW))
      FLOF(IDW)=X
      ILOF(IDW)=X+0.5
      YLOF=Y
      CALL MARKIT1(IDW,'OBLU',2,FLOF(IDW))
      MARF(3,IDW)='ON  '
      IF(GWID.LT.1.0)   RETURN
      IF(KEY.NE.'G   ') RETURN
C   
      X=X+GWID-1.0
C   
  520 IF(MARF(4,IDW).EQ.'ON  ') CALL MARKIT1(IDW,'OBLU',2,FHIF(IDW))
      FHIF(IDW)=X
      IHIF(IDW)=X+0.5
      YHIF=Y
      CALL MARKIT1(IDW,'OBLU',2,FHIF(IDW))
      MARF(4,IDW)='ON  '
C
      RETURN
C   
  780 CALL EXPAN1(IDW)
      KRETN='RES '
      RETURN
C   
  820 WRITE(CMSSG,825)IX,JY,X,Y
  825 FORMAT('IX,JY,X,Y=',2I6,2F10.1)
      GO TO 1100
C   
C     ------------------------------------------------------------------
C     DRAW PEAK MARKER AND DISPLAY ENERGY
C     ------------------------------------------------------------------
C
  830 KENE=X+0.5   
      IF(KEY.EQ.'P   ') KENE=ECAL(1)+ECAL(2)*X+ECAL(3)*X*X+0.5
      WRITE(CENE,835)KENE
  835 FORMAT(I8)
      CALL SQUEZL(CENE,1,8)
C
      CALL MARKUM1(IDW,'OGRE',X,JY)
C
      CALL TEXOUT(IDW,CENE,'OGRE',IX,JY-6)
C
      RETURN
C   
C     ------------------------------------------------------------------
C     DISPLAY/LOG CHANNEL# AND CONTENTS TO WHICH CURSOR CORRESPONDS
C     ------------------------------------------------------------------
C
  900 IF(IPLOGF.EQ.'PLON') GO TO 910
C   
      NC=X+0.5
      N1=NC-IFI+2
      ENER=ECAL(1)+ECAL(2)*X+ECAL(3)*X*X
C   
      WRITE(CMSSG,905)X,Y,ENER
  905 FORMAT('X-CUR,Y-CUR,ENERGY =',F10.1,F13.1,F10.1)
      GO TO 1100
C   
  910 NC=X+0.5
      N1=NC-IFI+2
      ENER=ECAL(1)+ECAL(2)*X+ECAL(3)*X*X
C
      CALL LUGET(KFL(1,IDW),IDUM,JDX,JDUM,IERR)
      JDX=JDX-1
      WRITE(CMSSG,915)IDL(1,IDW),X,ENER,
     &                    (NAMFIL(II,JDX),II=1,16)
  915 FORMAT('MARK',I10,2F10.2,2X,16A4)
      LISFSAV=LISFLG
      LISFLG='LON '
      CALL MESSLOG(0,LOGUP)
      LISFLG=LISFSAV
      WRITE(CMSSG,905)X,Y,ENER
      CALL MESSLOG(LOGUT,0)
      GO TO 1200   
C   
C     ------------------------------------------------------------------
C     DISPLAY SUM OF CHANNEL CONTENTS BETWEEN CURSORS (ILOF,IHIF)
C          OR NET SUM OF COUNTS (ILOF-IHIF) AD CENTROID
C     ------------------------------------------------------------------
C   
  920 IF(KEY.EQ.'S   ')  GO TO 950
      IF(MOPL.EQ.'OV  ') GO TO 950
C   
      WRITE(CMSSG,925)
  925 FORMAT('ILLEGAL REQUEST IN STACK-MODE')
      GO TO 1100
C   
  950 N1=ILOF(IDW)-IFI+2
      N2=IHIF(IDW)-IFI+2
      IF(N1.LT.1)   RETURN
      IF(N1.GT.N2)  RETURN
      IF(N2.GT.NCH) RETURN
C   
      XL=ILOF(IDW)
C   
      IF(KEY.EQ.'S   ') WRITE(CMSSG,952)
      IF(KEY.EQ.'A   ') WRITE(CMSSG,954)
C   
  952 FORMAT('      ID    LO    HI     SRAW----CRAW----WRAW',
     &                   '     SNET----CNET----WNET  ENERGY')
C   
  954 FORMAT('      ID    LO    HI     SRAW----CRAW----WRAW',
     &                   '    SNETA---CNETA---WNETA  ENERGY')
      CALL MESSLOG(LOGUT,LOGUP)
C   
      IUP=IFI+NCH-1
C   
      NID=NNID(IDW)
      DO 970 N=1,NID
C   
      CALL SPKIN1(READ,KFL(N,IDW),IDL(N,IDW),CNO(N,IDW),
     &            IFI,IUP,NCHL(N,IDW),IERR)
C   
      IF(KEY.EQ.'A   ') THEN
                        YL=YLOF
                        YR=YHIF
                        ENDIF
C   
      IF(KEY.EQ.'S   ') THEN
                        YL=IDATF(N1)
                        YR=IDATF(N2)
                        ENDIF
C   
      CALL SSUM1(N1,N2,XL,YL,YR,SRAW,SNET,CRAW,CNET,WRAW,WNET)
C
      CALL DFASCII(SRAW,ANUM,8)
      CALL DFASCII(SNET,BNUM,8)
C   
      ENER=ECAL(1)+ECAL(2)*CNET+ECAL(3)*CNET*CNET
C   
      WRITE(CMSSG,960)KFL(N,IDW),IDL(N,IDW),ILOF(IDW),IHIF(IDW),
     &ANUM2,CRAW,WRAW,BNUM2,CNET,WNET,ENER
  960 FORMAT(A3,I5,2I6,1X,2A4,2F8.2,1X,2A4,2F8.2,F8.1)
      CALL MESSLOG(LOGUT,LOGUP)
  970 CONTINUE
      GO TO 1200
C   
C     ------------------------------------------------------------------
C     OUTPUT A LINE OF TEXT TO DIALOG WINDOW
C     ------------------------------------------------------------------
C   
 1100 CALL MESSLOG(LOGUT,LOGUP)
C
 1200 CALL MESSLOG(LOGUT,0)
      RETURN
      END
