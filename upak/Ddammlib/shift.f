C$PROG SHIFT     - Gain-shifts 2-D IDI and stores in IDO
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SHIFT
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
C     ------------------------------------------------------------------
      COMMON/TDX2/ LUDI,LUHI,LUDO,LUHO
C     ------------------------------------------------------------------
      COMMON/TDX3/ XGS(4),YGS(4),IGSX,IGSY
      CHARACTER*4                IGSX,IGSY
C     ------------------------------------------------------------------
      COMMON/TDX7/ KMDS,FACI,FACO
C     ------------------------------------------------------------------
      INTEGER*4 IDAT(8192),JDAT(8192)
C
      DIMENSION   ABUF(8192),BBUF(8192)
C   
      INTEGER*4 HEDF(32),MSER(10),NDX(4)
C   
      INTEGER*2 HEDH(64),MINC(4),MAXC(4),ND,NHW
C
      EQUIVALENCE (IDAT,IDATF(49153)),(JDAT,IDATF(57345))
C   
      EQUIVALENCE (ABUF(1),IDAT(1)),(BBUF(1),JDAT(1))
C   
      EQUIVALENCE (HEDH(1),HEDF(1)),
     &            (ND     ,HEDH(1)),
     &            (NHW    ,HEDH(2)),
     &            (MINC(1),HEDH(15)),
     &            (MAXC(1),HEDH(19))
C   
      DATA NDX/1,1,0,0/
      DATA AA,BB,CNOR/0.0,1.0,1.0/
C
      CHARACTER*4  IGSYSAV
C
      SAVE
C   
C     ------------------------------------------------------------------
C     SHIFTS IDI ACCORDING TO XGS & YGS AND STORES IN IDO
C     ------------------------------------------------------------------
C   
      IERR=0
      IGSYSAV=IGSY
      KMDS=IWD(1)
C   
      CALL IVALU(LWD(1,2),IDI,IERR)
      IF(IERR.NE.0) GO TO 300
      CALL IVALU(LWD(1,3),IDO,IERR)
      IF(IERR.NE.0) GO TO 300
C   
      FACI=1.0
      FACO=1.0
      IF(NF.GE.4) CALL MILV(LWD(1,4),IV,FACI,KIND,IERR)
      IF(IERR.NE.0) GO TO 300
      IF(NF.GE.5) CALL MILV(LWD(1,5),IV,FACO,KIND,IERR)
      IF(IERR.NE.0) GO TO 300
C   
      CALL HISIO('INIT',LUDI,LUHI,IDI,NDX,0,HEDF,IDAT,IERR,MSER)
      CALL HISIOER(IERR,MSER)
      IF(IERR.NE.0) GO TO 400
      CALL HISIO('READ',LUDI,LUHI,IDI,NDX,0,HEDF,IDAT,IERR,MSER)
      CALL HISIOER(IERR,MSER)
      IF(IERR.NE.0) GO TO 400
C   
      IF(ND.LT.2) IGSY='NO  '
C   
      IF(IGSY.EQ.'NO  ') GO TO 10
C   
      BB=(YGS(4)-YGS(3)+1.)/(YGS(2)-YGS(1)+1.) !GAIN-SHIFT COEF
      AA=YGS(3)-BB*YGS(1)                      !GAIN-SHIFT COEF
      CNOR=1.0/BB                              !COUNT NORM-FACTOR
C   
   10 NDI=ND
      NHWI=NHW
      NCXI=MAXC(1)-MINC(1)+1
      NCYI=MAXC(2)-MINC(2)+1
C   
      MINXI=MINC(1)+1
      MAXXI=MAXC(1)+1
      MINYI=MINC(2)+1
      MAXYI=MAXC(2)+1
C   
      CALL HISIO('INIT',LUDO,LUHO,IDO,NDX,0,HEDF,IDAT,IERR,MSER)
      CALL HISIOER(IERR,MSER)
      IF(IERR.NE.0) GO TO 400
      CALL HISIO('READ',LUDO,LUHO,IDO,NDX,0,HEDF,IDAT,IERR,MSER)
      CALL HISIOER(IERR,MSER)
      IF(IERR.NE.0) GO TO 400
C   
      NDO=ND
      NHWO=NHW
      NCXO=MAXC(1)-MINC(1)+1
C   
      MINXO=MINC(1)+1
      MAXXO=MAXC(1)+1
      MINYO=MINC(2)+1
      MAXYO=MAXC(2)+1
C   
C     ------------------------------------------------------------------
C     DO COPY AND GAIN SHIFT
C     ------------------------------------------------------------------
C   
      CALL SLICIA(LUDI,LUHI,IDI,0,IDAT,IERR)      !INIT IN-SLICE
      IF(IERR.NE.0) GO TO 400
      CALL  SLICO(LUDO,LUHO,IDO,0,JDAT,IERR)      !INIT OU-SLICE
      IF(IERR.NE.0) GO TO 400
C   
      DO 20 I=1,NCXO                              !ZOT OU-BUFF
      BBUF(I)=0.0
   20 CONTINUE
C   
      CALL SLOUT('INIT',IDO,0,0,0.0,IERR)         !INIT SLOUT
      IF(IERR.NE.0) GO TO 400
C   
      YY=MINYI-2                                  !INIT Y-CHAN CNTR
C   
      DO 200 JY=MINYI,MAXYI                       !LOOP ON IN-Y-CHAN
C   
      CALL SLICIA(LUDI,LUHI,IDI,JY,IDAT,IERR)     !INPUT SLICE
      IF(IERR.NE.0) GO TO 400
C   
      DO 30 I=1,NCXI                              !FLOAT IN-SLICE
      ABUF(I)=IDAT(I)
   30 CONTINUE
C   
      IF(IGSX.EQ.'YES ') CALL REBINIT(MINXI,MAXXI,MINXO,MAXXO)
C   
      IF(IGSX.NE.'YES ') CALL SLIDE(MINXI,MAXXI,MINXO,MAXXO)
C   
      IF(IGSY.EQ.'YES ') GO TO 50
C   
      IF(JY.LT.MINYO) GO TO 200
      IF(JY.GT.MAXYO) GO TO 210
C   
      F=1.0
      KY=JY
      CALL SLOUT('STOR',IDO,KY,NCXO,F,IERR)       !ADD ALL COUNTS
      IF(IERR.NE.0) GO TO 400
      GO TO 200
C   
C   
   50 YY=YY+1.0                                   !INC IN Y-CHAN-CTR
      Y1=AA+BB*YY                                 !OU-CHAN LO-VAL
      Y2=Y1+BB                                    !OU-CHAN HI-VAL
      JLO=Y1                                      !OU-INDEX LO-VAL
      JHI=Y2+0.99                                 !OU-INDEX HI-VAL
      NBIN=JHI-JLO                                !# OU-BINS
C   
      IF(NBIN.GT.1) GO TO 60                      !TST FOR SPLIT
      IF(JHI.LT.MINYO) GO TO 100                  !TST LO-LIMIT
      IF(JHI.GT.MAXYO) GO TO 200                  !TST HI-LIMIT
C   
      F=1.0
      KY=JHI
      CALL SLOUT('STOR',IDO,KY,NCXO,F,IERR)       !ADD ALL COUNTS
      IF(IERR.NE.0) GO TO 400
      GO TO 100
C   
C     ********************************** SPLIT ACROSS OUT-CHANS
C   
   60 KY=JLO                                      !INIT OU BIN-NDX
      DO 80 N=1,NBIN                              !LOOP ON OU-BINS
      KY=KY+1                                     !INC BIN-INDEX
C   
      IF(KY.LT.MINYO) GO TO 80                    !TST LO-LIMIT
      IF(KY.GT.MAXYO) GO TO 200                   !TST HI-LIMIT
C   
      YJ=KY                                       !FLOAT IT
      F=CNOR                                      !COUNT NORM-FACTOR
C   
      IF(N.EQ.1)    F=CNOR*(YJ-Y1)                !ADJUST FIRST BIN?
      IF(N.EQ.NBIN) F=CNOR*(Y2-(YJ-1.0))          !ADJUST LAST  BIN?
C   
      CALL SLOUT('STOR',IDO,KY,NCXO,F,IERR)       !ADD CNTS & STOR?
      IF(IERR.NE.0) GO TO 400
C   
   80 CONTINUE
  100 CONTINUE
C   
  200 CONTINUE
C   
  210 CALL SLOUT('FLUS',IDO,KY,NCXO,F,IERR)       !FLUSH SLICO
      GO TO 400
C   
C     ------------------------------------------------------------------
C     REPORT ERROR MESSAGES
C     ------------------------------------------------------------------
C   
  300 WRITE(CMSSG,305)
  305 FORMAT('SYNTAX ERROR OR ILLEGAL VALUE - CMD IGNORED')
      CALL MESSLOG(6,7)
      IERR=1
C   
  400 IGSY=IGSYSAV
      RETURN
      END
