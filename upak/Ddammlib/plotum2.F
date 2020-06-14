C$PROG PLOTUM2   - Display routine for 2-D
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE PLOTUM2(IDW,ID,KPLO,JLOCX,JHICX,JLOCY,JHICY,
     &                   KRX,KRY)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
#if (PTRSIZE == 64)
      INTEGER*8    DPY,WDID                                 !Alpha STAR8
#else
      INTEGER*4    DPY,WDID                                 !Alpha STAR8
#endif
      INTEGER*4                 XN,    YN,              WN
      CHARACTER*4                             NUWIN
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
      COMMON/XLEE/ MASKEV,MODE_OR,MODE_PL
      INTEGER*4    MASKEV,MODE_OR,MODE_PL
C     ------------------------------------------------------------------
      COMMON/XLFF/ GCOR(7),GCON(35)
#if (PTRSIZE == 64)
      INTEGER*8    GCOR,   GCON                             !Alpha STAR8
#else
      INTEGER*4    GCOR,   GCON                             !Alpha STAR8
#endif
C     ------------------------------------------------------------------
      COMMON/XLHH/ ITITL(20),IBANNER
      INTEGER*4    ITITL
      CHARACTER*4            IBANNER
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
      COMMON/DML4/ LABLX(3),LABLY(3),LABTIT(10)
C     ------------------------------------------------------------------
      COMMON/DML5/ TITI(20),FILI(6),KPARI(9),IHI,DGI,        !/DML5
     &             TITO(20),FILO(6),KPARO(9),IHO,DGO         !/DML5
      INTEGER*4    TITI,    FILI,   KPARI,   IHI,DGI
      INTEGER*4    TITO,    FILO,   KPARO,   IHO,DGO
C     ------------------------------------------------------------------
      COMMON/DML7/ NUD(20),NUB
      CHARACTER*4  NUD
C     ------------------------------------------------------------------
      COMMON/DIR/  KLOC(6),JHSP(4),LENG(4),ND,NHW,           !/DIR
     &             LENH,LENT,IOF,LDF,NHIS,LEND(4),           !/DIR
     &             LENS(4),MINC(4),MAXC(4),CONS(4),ITEX(20), !/DIR
     &             ITIT(10),LABX(3),LABY(3),MSER(10),KFILT   !/DIR 
C     ------------------------------------------------------------------
      COMMON/PL03/ IDAT(4096),MINCN(2,20),MAXCN(2,20),MINZ,MAXZ
C     ------------------------------------------------------------------   
      COMMON/PL04/ ILOCX(20),IHICX(20),ILOCY(20),IHICY(20),  !/PL04
     &             KLOCX(20),KHICX(20),KLOCY(20),KHICY(20),  !/PL04
     &             KDDP(20),MINZZ,MAXZZ                      !/PL04
      CHARACTER*4  KDDP
C     ------------------------------------------------------------------
C
      COMMON/PL05/ NFX,NFY,PPCX,PPCY,BPPCX,BPPCY,KRUNX,KRUNY
C
      COMMON/PL06/ NPXX,NPXY,NUPX,NUPY
C   
      COMMON/PL09/ XL(64,21),YL(64,21),NXYL(21),IBL(21),
     &             IBWN(21),LNBAN(4,21),MAXXYL
C
C     ------------------------------------------------------------------
      COMMON/PL10/ IXOF(25),JYOF(25),SYMPO(25),KOLOFF,KINZMAP
      CHARACTER*4                                     KINZMAP
C     ------------------------------------------------------------------
C
      COMMON/PL11/ NZADD
C
      COMMON/PL12/ KZSCAL,NZLEV
C
      COMMON/PL16/ LENHIS(4,20)
C
      COMMON/PL18/ KHISDAT(9,20)
C     ------------------------------------------------------------------
#if (PTRSIZE == 64)
      INTEGER*8                        IGC              !Alpha STAR8 
#else
      INTEGER*4                        IGC              !Alpha STAR8
#endif
C     ------------------------------------------------------------------
      REAL*8    SUMI
C
      INTEGER*4 ANUM(3)
C
      INTEGER*4 PPCX,PPCY,BPPCX,BPPCY
C
      INTEGER*4 TLOCX,THICX,TLOCY,THICY
C
      INTEGER*4 PXXL(2048),PXYL(2048),KOF(20)
C
      INTEGER*4 XDOTS,YDOTS,DOTSIZ,SYMPO
C
      INTEGER*2 RECS(4,20480),DOTS(2,5000)
C
      CHARACTER*4  MODE,KPLO
C
      SAVE
C
C     ------------------------------------------------------------------
C
      MSGF='    '
C
C     ------------------------------------------------------------------
C     DO INITIAL ACCESS OF REQUESTED ID TO GET HISTOGRAM ATTRIBUTES
C     ------------------------------------------------------------------
C
      WINFLC(3,IDW)='2D  '
      KDDP(IDW)=KPLO
      IDL(1,IDW)=ID
C
      CALL LUGET(KFL(1,IDW),LUH,LUD,KINF,IERR)
      IF(IERR.NE.0) RETURN
C
      MODE='INIT'
      CALL SLICE2(IDW,MODE,LUH,LUD,ID,IXA,IXB,1,1,1,IERR)
      IF(IERR.NE.0) RETURN
      ID2=ID
C
      LENHIS(1,IDW)=LENS(1)  !SAVE FOR POSSIBLE USE BY XYMAN
      LENHIS(2,IDW)=LENS(2)
      LENHIS(3,IDW)=LENS(3)
      LENHIS(4,IDW)=LENS(4)
C
      DO 5 I=1,3
      LABLX(I)=LABX(I)
      LABLY(I)=LABY(I)
    5 CONTINUE
      DO 6 I=1,10
      LABTIT(I)=ITIT(I)
    6 CONTINUE
C
      CALL TITLO(ITITL,KFL(1,IDW),ID)
C   
      DO 10 I=1,10           !SAVE TITLE FOR BAN-FILE USE
      TITO(I)=ITIT(I)
   10 CONTINUE
      DO 15 I=11,20
      TITO(I)=Z'20202020'
   15 CONTINUE
      DO 20 I=1,6            !SAVE FILENAME FOR BAN-FILE USE
      FILO(I)=NAMFIL(I,8)
   20 CONTINUE
C                            !
      IHO=ID                 !SAVE OTHER STUFF FOR BANFILE USE
      DGO=0
      KHISDAT(1,IDW)=JHSP(1)
      KHISDAT(2,IDW)=JHSP(2)
      KHISDAT(3,IDW)=LEND(1)
      KHISDAT(4,IDW)=LENS(1)
      KHISDAT(5,IDW)=LEND(2)
      KHISDAT(6,IDW)=LENS(2)
      KHISDAT(7,IDW)=0
      KHISDAT(8,IDW)=0
      KHISDAT(9,IDW)=0
C
      DO 22 I=1,9
      KPARO(I)=KHISDAT(I,IDW)
   22 CONTINUE
C   
C     ------------------------------------------------------------------
C     ADJUST DISPLAY REQUEST TO CHANNELS ACTUALLY AVAILABLE
C     IF MAX-CHANNEL NUMBERS ARE ENTERED AS 0, DISPLAY ALL CHANS
C     ------------------------------------------------------------------
C
      TLOCX=JLOCX        
      THICX=JHICX
      TLOCY=JLOCY
      THICY=JHICY
C   
      IF(TLOCX.LE.0)            TLOCX=MINCN(1,IDW)
      IF(TLOCY.LE.0)            TLOCY=MINCN(2,IDW)
C
      IF(THICX.LE.0)            THICX=MAXCN(1,IDW)
      IF(THICY.LE.0)            THICY=MAXCN(2,IDW)
C   
      IF(TLOCX.GT.MAXCN(1,IDW)) TLOCX=MAXCN(1,IDW)
      IF(TLOCY.GT.MAXCN(2,IDW)) TLOCY=MAXCN(2,IDW)
C
      IF(TLOCX.LT.MINCN(1,IDW)) TLOCX=MINCN(1,IDW)
      IF(TLOCY.LT.MINCN(2,IDW)) TLOCY=MINCN(2,IDW)
C
      IF(THICX.GT.MAXCN(1,IDW)) THICX=MAXCN(1,IDW)
      IF(THICY.GT.MAXCN(2,IDW)) THICY=MAXCN(2,IDW)
C
      IF(THICX.LT.TLOCX)        THICX=TLOCX
      IF(THICY.LT.TLOCY)        THICY=TLOCY
C   
C     ------------------------------------------------------------------
C     COMPUTE #PIXELS/CHANNEL (X & Y), REQUIRED CRUNCH-FACTORS, ETC
C     ------------------------------------------------------------------
C
      NCHX=THICX-TLOCX+1     !# OF RAW X-CHANS TO DISPLAY
      NCHY=THICY-TLOCY+1     !# OF RAW Y-CHANS TO DISPLAY
C
      CALL PJUST2(IDW,NCHX,NCHY,KRX,KRY,IERR)
      IF(IERR.NE.0) RETURN
C
C     PPCX                   !# LIT   PIXELS/CHAN IN X-DIRECTION
C     PPCY                   !# LIT   PIXELS/CHAN IN Y-DIRECTION
C     BPPCX                  !# BLANK PIXELS/CHAN IN X-DIRECTION
C     BPPCY                  !# BLANK PIXELS/CHAN IN Y-DIRECTION
C     KRUNX                  !FINAL X-CRUNCH (FROM USER OR PJUST2)
C     KRUNY                  !FINAL Y-CRUNCH (FROM USER OR PJUST2)
C
C     ------------------------------------------------------------------
C     COMPUTE Z-SCALE FACTORS , AZ & BZ
C     ------------------------------------------------------------------
C
      IXA=TLOCX-MINCN(1,IDW)+1   !MIN X-INDEX (NOT CHAN#) TO DISP
      IXB=THICX-MINCN(1,IDW)+1   !MAX X-INDEX (NOT CHAN#) TO DISP
C   
      MINY=TLOCY-MINCN(2,IDW)+1  !MIN Y-INDEX (NOT CHAN#) TO DISP
      MAXY=THICY-MINCN(2,IDW)+1  !MAX Y-INDEX (NOT CHAN#) TO DISP
C
      MINZ=MINZZ             !INIT MINZ - SCALE2 MAY ADJUST
      MAXZ=MAXZZ             !INIT MAXZ - SCALE2 MAY ADJUST
C
      IF(KINZMAP.NE.'DOTS') GO TO 30
C
      XDOTS=PPCX+BPPCX
      YDOTS=PPCY+BPPCY
      DOTSIZ=XDOTS
      IF(YDOTS.LT.DOTSIZ) DOTSIZ=YDOTS
C
      CALL SYMLIS2(DOTSIZ,NZLEV,NZVAL,SYMPO)
      GO TO 35
C
   30 NZVAL=NZLEV
      IF(NZVAL.LE.0)  NZVAL=10
      IF(NZVAL.GT.10) NZVAL=10
      IF(NZVAL.LT.3)  NZVAL=3
C
   35 CALL SCALE2(IDW,LUH,LUD,ID,KPLO,NZVAL,IXA,IXB,
     &             MINY,MAXY,KRUNX,KRUNY,AZ,BZ,IERR)
      IF(IERR.NE.0) RETURN
C   
      WRITE(CMSSG,40)MINZ       !MIN-Z (FROM USER OR SCALE2)
      CALL MESSLOG(LOGUT,0)
      WRITE(CMSSG,45)MAXZ       !MAX-Z (FROM USER OR SCALE2)
      CALL MESSLOG(LOGUT,0)
   40 FORMAT('MIN=',I11)
   45 FORMAT('MAX=',I11)
C   
C     ------------------------------------------------------------------
C     SET UP SOME STUFF
C     ------------------------------------------------------------------
C
      SUMI=0.0D0                          !INIT TOTAL COUNT SUM
      MODE='GET '                     !SET SLICE MODE TO READ
      KDOX=(THICX-TLOCX+KRUNX)/KRUNX  !# OF X-CHANS POST CRUNCH
      KDOY=(MAXY -MINY +KRUNY)/KRUNY  !# OF Y-CHANS POST CRUNCH
      NBYX=PPCX+BPPCX                 !TOTAL PIX/CHAN IN X-DIR
      NBYY=PPCY+BPPCY                 !TOTAL PIX/CHAN IN Y-DIR
      JYB=MINY-1                      !INIT INPUT SLICE
C
      DO 50 I=1,20480                 !LOOP TO SET RECTANGLE
      RECS(1,I)=0
      RECS(2,I)=0
      RECS(3,I)=PPCX                  !WIDTH AND
      RECS(4,I)=PPCY                  !HEIGHT
   50 CONTINUE                        !ONCE & FOR ALL
C
      ILOCX(IDW)=TLOCX
      IHICX(IDW)=THICX
      ILOCY(IDW)=TLOCY
      IHICY(IDW)=THICY
C
      TCH=THICX-TLOCX+1
      FPU=FLOAT(NUPX)/FLOAT(NPXX)
      FNCHX=TCH/FPU
      TCH=THICY-TLOCY+1
      FPU=FLOAT(NUPY)/FLOAT(NPXY)
      FNCHY=TCH/FPU
C
      IF(WINFLG(1,IDW).EQ.0) RETURN
C
crlv      CALL XX_WINMAN('ERAS',IDW)
C
      WINFLG(1,IDW)=0
      WINDAT(5,IDW)=TLOCX-0.5
      WINDAT(6,IDW)=TLOCY-0.5
      WINDAT(7,IDW)=TLOCX+FNCHX-1.5
      WINDAT(8,IDW)=TLOCY+FNCHY-1.5
C
      CALL XX_WINMAN('WIN ',IDW)
C
      CALL XX_SYNC(DPY,.TRUE.)
C
      FIXN=AA(1,IDW)+BB(1,IDW)*TLOCX  !PIX-ORG FOR MIN-X
      FJYN=AA(2,IDW)+BB(2,IDW)*TLOCY  !PIX-ORG FOR MIN-Y
      IF(KINZMAP.NE.'DOTS') THEN
      FIXN=FIXN-0.5*FLOAT(NBYX)
      FJYN=FJYN-0.5*FLOAT(NBYY)
                            ENDIF
C
      IXN=FIXN+0.5
      JYN=FJYN+0.5
C
      DO 60 I=1,2048                  !LOOP TO SET REC-LOCATION
      PXXL(I)=IXN                     !FOR ALL X-CHANNELS 
      PXYL(I)=JYN                     !AND ALL Y-CHANNELS
      IXN=IXN+NBYX                    !INC X-PIXEL POINTER
      JYN=JYN-NBYY                    !INC Y-PIXEL POINTER
   60 CONTINUE
C
      IF(KINZMAP.EQ.'DOTS') GO TO 250
C
C     ------------------------------------------------------------------
C     LOOP ON HIST SLICES & DISPLAY DATA IN COLOR OR GREY-SCALE
C     ------------------------------------------------------------------
C
      DO 200 JJ=1,KDOY                !LOOP ON INPUT-SLICES
C
      IF(MSGF.NE.'    ') GO TO 410    !TST FOR INTERRUPT REQUEST
      JYA=JYB+1                       !SET MIN-SLICE
      JYB=JYA+KRUNY-1                 !SET MAX-SLICE
      IF(JYB.GT.MAXY) JYB=MAXY        !SET MAX-SLICE
C   
      CALL SLICE2(IDW,MODE,LUH,LUD,   !READ IN DATA-SLICE
     &ID,IXA,IXB,KRUNX,JYA,JYB,IERR)  !ALREADY CRUNCHED IN X & Y
C
      IF(IERR.NE.0) GO TO 410         !TST FOR ERROR
C
      II=0                            !INIT COLOR-OFFSET POINTERS
      DO 70 I=1,10                    !LOOP ON 10 COLORS
      KOF(I)=II                       !STORE OFFSET
      II=II+2048                      !INC POINTER
   70 CONTINUE
C   
      IF(KPLO.EQ.'LOG ') GO TO 110    !TST FOR LOG Z-SCALE
C
      DO 100 KK=1,KDOX                !DO LINEAR Z-SCALE
C
      IF(IDAT(KK).EQ.0.OR.            !SKIP IF DATA IS ZERO OR
     &   IDAT(KK).LT.MINZ.OR.         !OUT OF RANGE
     &   IDAT(KK).GT.MAXZ) GO TO 100
C
      SUMI=SUMI+IDAT(KK)
      JC=AZ+BZ*FLOAT(IDAT(KK))        !GET COLOR INDEX
      IF(JC.GT.NZVAL) JC=NZVAL        !MAKE SURE INDEX IS LEGAL
      IF(JC.LT.1    ) JC=1
      KOF(JC)=KOF(JC)+1               !INC STRUCT OFFSET
      NN=KOF(JC)                      !GET STRUCT INDEX
      RECS(1,NN)=PXXL(KK)             !STORE X-COOR IN REC-STRUCT
      RECS(2,NN)=PXYL(JJ)             !STORE Y-COOR IN REC-STRUCT
  100 CONTINUE
      GO TO 125                       !GO DISPLAY IT
C
  110 DO 120 KK=1,KDOX                !DO LOG Z-SCALE
C
      IF(IDAT(KK).EQ.0.OR.            !SKIP IF DATA IS ZERO
     &   IDAT(KK).LT.MINZ.OR.         !OR OUT OF RANGE
     &   IDAT(KK).GT.MAXZ) GO TO 120
C
      SUMI=SUMI+IDAT(KK)
      IDATZ=IDAT(KK)+NZADD
      IF(IDATZ.LE.0) GO TO 120        !SKIP 0-DATA
      JC=AZ+BZ*ALOG(FLOAT(IDATZ))     !GET COLOR INDEX
      IF(JC.GT.NZVAL) JC=NZVAL        !MAKE SURE INDEX IS LEGAL
      IF(JC.LT.1    ) JC=1
      KOF(JC)=KOF(JC)+1               !INC STRUCT OFFSET
      NN=KOF(JC)                      !INDEX IN STRUCT
      RECS(1,NN)=PXXL(KK)             !STORE X-COOR IN REC-STRUCT
      RECS(2,NN)=PXYL(JJ)             !STORE Y-COOR IN REC-STRUCT
  120 CONTINUE
C
  125 NR=-2047                        !DISPLAY SLICE
      DO 130 I=1,10                   !LOOP ON # OF COLORS
      NR=NR+2048                      !INC OFFSET
      IGC=GCON(KOLOFF+I)              !SET GRAPHICS CONTEXT (COLOR)
      NREC=KOF(I)-2048*(I-1)          !# OF RECS TO DISPLAY
      IF(NREC.LT.1) GO TO 130         !TEST FOR NONE
C
      CALL XX_FILLRECTANGLES(DPY,     !DRAW RECTANGLES
     &                     WDID(IDW), !WINDOW-ID
     &                     IGC,       !GRAPHICS CONTEXT
     &                     RECS(1,NR),!REC ARRAY 
     &                     NREC,      !NUMBER OF POINTS
     &                     MODE_OR)   !ORIGIN-MODE
  130 CONTINUE
C
      CALL XX_FILLRECTANGLES(DPY,     !DRAW RECTANGLES
     &                     WDID(IDW), !WINDOW-ID
     &                     IGC,       !GRAPHICS CONTEXT
     &                     RECS(1,NR),!REC ARRAY 
     &                     NREC,      !NUMBER OF POINTS
     &                     MODE_OR)   !ORIGIN-MODE
C
      CALL XX_SYNC(DPY,.TRUE.)
C
  200 CONTINUE
C
C
      GO TO 410
C
C     ------------------------------------------------------------------
C     LOOP ON HIST SLICES & DISPLAY DATA IN "DOT-MODE"
C     ------------------------------------------------------------------
C
  250 NP=0                            !INIT DOT COUNTER
      DO 400 JJ=1,KDOY                !LOOP ON INPUT-SLICES
C
      IF(MSGF.NE.'    ') GO TO 410    !TST FOR INTERRUPT REQUEST
      JYA=JYB+1                       !SET MIN-SLICE
      JYB=JYA+KRUNY-1                 !SET MAX-SLICE
      IF(JYB.GT.MAXY) JYB=MAXY        !SET MAX-SLICE
C   
      CALL SLICE2(IDW,MODE,LUH,LUD,   !READ IN DATA-SLICE
     &ID,IXA,IXB,KRUNX,JYA,JYB,IERR)  !ALREADY CRUNCHED IN X & Y
C
      IF(IERR.NE.0) GO TO 410         !TST FOR ERROR
C
      IF(KPLO.EQ.'LOG ') GO TO 310    !TST FOR LOG Z-SCALE
C
      DO 300 KK=1,KDOX                !DO LINEAR Z-SCALE
C
      IF(IDAT(KK).EQ.0.OR.            !SKIP IF DATA IS ZERO OR 
     &   IDAT(KK).LT.MINZ.OR.         !OUT OF RANGE
     &   IDAT(KK).GT.MAXZ) GO TO 300
C
      SUMI=SUMI+IDAT(KK)
      JC=AZ+BZ*FLOAT(IDAT(KK))        !GET COLOR INDEX
      IF(JC.GT.NZVAL) JC=NZVAL        !MAKE SURE INDEX IS LEGAL
      IF(JC.LT.1    ) JC=1
      NDO=SYMPO(JC)
      DO 280 I=1,NDO
      NP=NP+1
      DOTS(1,NP)=PXXL(KK)+IXOF(I)
      DOTS(2,NP)=PXYL(JJ)+JYOF(I)
      IF(NP.LT.2000) GO TO 280
      CALL XX_DRAWPOINTS(DPY,WDID(IDW),GCON(2),DOTS,NP,MODE_OR)
      CALL XX_SYNC(DPY,.TRUE.)
      NP=0
  280 CONTINUE
  300 CONTINUE
      GO TO 350                       !GO OUTPUT WHATS LEFT
C
  310 DO 320 KK=1,KDOX                !DO LOG Z-SCALE
C
      IF(IDAT(KK).EQ.0.OR.            !SKIP IF DATA IS ZERO OR
     &   IDAT(KK).LT.MINZ.OR.         !OUT OF RANGE
     &   IDAT(KK).GT.MAXZ) GO TO 320
C
      SUMI=SUMI+IDAT(KK)
      IDATZ=IDAT(KK)+NZADD
      IF(IDATZ.LE.0) GO TO 320        !SKIP 0-DATA
      JC=AZ+BZ*ALOG(FLOAT(IDATZ))     !GET COLOR INDEX
      IF(JC.GT.NZVAL) JC=NZVAL        !MAKE SURE INDEX IS LEGAL
      IF(JC.LT.1    ) JC=1
      NDO=SYMPO(JC)
      DO 305 I=1,NDO
      NP=NP+1
      DOTS(1,NP)=PXXL(KK)+IXOF(I)
      DOTS(2,NP)=PXYL(JJ)+JYOF(I)
      IF(NP.LT.2000) GO TO 305
      CALL XX_DRAWPOINTS(DPY,WDID(IDW),GCON(2),DOTS,NP,MODE_OR)
      CALL XX_SYNC(DPY,.TRUE.)
      NP=0
  305 CONTINUE
  320 CONTINUE
C
  350 IF(NP.LE.0) GO TO 400
      CALL XX_DRAWPOINTS(DPY,WDID(IDW),GCON(2),DOTS,NP,MODE_OR)
      CALL XX_SYNC(DPY,.TRUE.)
C
  400 CONTINUE
C
  410 CALL DOZSCAL(IDW,KPLO,AZ,BZ,NZADD,NZVAL)
C
      CALL DMAR2(IDW)
C
      CALL XX_SYNC(DPY,.TRUE.)
C
      CALL DFASCII(SUMI,ANUM,11)
      WRITE(CMSSG,415)ANUM
      CALL MESSLOG(LOGUT,0)
  415 FORMAT('SUM=',2A4,A3)
C
      WRITE(CMSSG,420)ID
  420 FORMAT(' ID=',I11)
      CALL MESSLOG(LOGUT,0)
      NUD(IDW)='YES '
      RETURN
      END
