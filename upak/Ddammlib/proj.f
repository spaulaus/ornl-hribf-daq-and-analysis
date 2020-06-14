C$PROG PROJ      - Does banana projections from 2-D histograms
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE PROJ(LUD,LUH,ID,IDAT,ADAT,KX,KY,NP,LXG,LYG,DEGA,NC,
     &                IERR)
C
C     ------------------------------------------------------------------
      COMMON/DML2/ IDATF(65536),IHEDF(32),MAXCH
      INTEGER*4    IDATF,       IHEDF,    MAXCH
C     ------------------------------------------------------------------
      COMMON/DIR/  KLOC(6),JHSP(4),LENG(4),ND,NHW,           !/DIR
     &             LENH,LENT,IOF,LDF,NHIS,LEND(4),           !/DIR
     &             LENS(4),MINC(4),MAXC(4),CONS(4),ITEX(20), !/DIR
     &             ITIT(10),LABX(3),LABY(3),MSER(10),KFILT   !/DIR 
C     ------------------------------------------------------------------
      DIMENSION IDAT(4096),ADAT(4096),BDAT(4096),B(64)
C   
      INTEGER*2 MASK(4096)
C   
      INTEGER*4 IX(64),IY(64),KX(64),KY(64)
C
      EQUIVALENCE (BDAT,IDATF(16385)),
     &            (MASK,IDATF(20481))
C   
      DATA DTOR,MAXL/0.0174532925,4096/
C
      CHARACTER*4  IRECF
C
      SAVE
C   
C     ------------------------------------------------------------------
C     KX(I) = ITH X-COOR IN CXY-LIST (AS READ IN - CHAN# UNITS)
C     KY(I) = ITH Y-COOR IN CXY-LIST (AS READ IN - CHAN# UNITS)
C     IX(I) = ITH X-COOR IN CXY-LIST SHIFTED TO MATCH DATA (INDEX UNITS)
C     IY(I) = ITH Y-COOR IN CXY-LIST SHIFTED TO MATCH DATA (INDEX UNITS)
C     B(I) GIVES SLOPE OF LINE CONNECTING POINTS I AND (I+1)
C     MINY, MAXY ARE MIN AND MAX VALUES OF "IY"
C     MINX, MAXX ARE MIN AND MAX VALUES OF "IX"
C     NP   = # OF POINTS IN LIST (POINT (NP+1) IS SET = POINT 1)
C     MAXL = MAX NO. OF X-CHANNELS SUPPORTED
C     ------------------------------------------------------------------
C   
      IERR=0
C   
      DO 20 I=1,MAXL
      BDAT(I)=0.0
      IDAT(I)=0
   20 CONTINUE
C   
C     ------------------------------------------------------------------
C     READ IN DIRECTORY TO GET DIMENSIONS AND BYTES/CH OF HISTOGRAM
C     ------------------------------------------------------------------
C   
      CALL SLICER(LUD,LUH,ID,0,IDAT,IERR)
      IF(IERR.NE.0) RETURN
      NCX=MAXC(1)+1
      NCY=MAXC(2)+1
      XNCX=NCX
      YNCY=NCY
C   
C     ------------------------------------------------------------------
C     FIND XMIN AND XMAX FOR UNTRANSFORMED PROJECTION
C     ------------------------------------------------------------------
C   
      IDEGA=DEGA+0.5
      IRECF='NO  '
      IF(IDEGA.EQ.0.OR.IDEGA.EQ.90) IRECF='YES '
      RAD=DTOR*DEGA
      SINA=SIN(RAD)
      COSA=COS(RAD)
      IF(ABS(SINA).LT.1.0E-5) SINA=0.0
      IF(ABS(COSA).LT.1.0E-5) COSA=0.0
      IF(COSA.GE.0.0.AND.SINA.GE.0.0) GO TO 30
      IF(COSA.LE.0.0.AND.SINA.LE.0.0) GO TO 35
      IF(COSA.LE.0.0.AND.SINA.GE.0.0) GO TO 40
      IF(COSA.GE.0.0.AND.SINA.LE.0.0) GO TO 45
   30 XMIN=0.0
      XMAX=COSA*XNCX+SINA*YNCY
      NQUAD=1
      GO TO 50
   35 XMAX=0.0
      XMIN=COSA*XNCX+SINA*YNCY
      NQUAD=3
      GO TO 50
   40 XMIN=COSA*XNCX
      XMAX=SINA*YNCY
      NQUAD=2
      GO TO 50
   45 XMIN=SINA*YNCY
      XMAX=COSA*XNCX
      NQUAD=4
   50 IAA=-XMIN
      AA=IAA+1
      IF(IRECF.EQ.'YES ') AA=0.0
C   
C     ------------------------------------------------------------------
C     CALCULATE SLOPE OF BOUNDRY SEGMENTS, MIN AND MAX OF X AND Y
C     ------------------------------------------------------------------
C   
      NBSX=0
      NBSY=0
      IF(LXG.LE.0) GO TO 60
C   
      NBSX=LOGRAT2(LENS(1),LXG)         !#BITS TO SHIFT X-COOR
      NBSY=LOGRAT2(LENS(2),LYG)         !#BITS TO SHIFT Y-COOR
C   
   60 DO 65 I=1,NP
      IX(I)=ISHFT(KX(I),NBSX)+1         !SHIFT X-COOR TO MATCH HIST
      IY(I)=ISHFT(KY(I),NBSY)+1         !SHIFT Y-COOR TO MATCH HIST
   65 CONTINUE
C   
      CALL ROTA(IX,IY,NP)
      IX(NP+1)=IX(1)
      IY(NP+1)=IY(1)
      MINX=IX(1)
      MAXX=IX(1)
      MINY=IY(1)
      MAXY=IY(1)
      DO 70 I=1,NP
      IF(IX(I).LT.MINX) MINX=IX(I)
      IF(IX(I).GT.MAXX) MAXX=IX(I)
      IF(IY(I).LT.MINY) MINY=IY(I)
      IF(IY(I).GT.MAXY) MAXY=IY(I)
      B(I)=0.0
      IF(IX(I).EQ.IX(I+1)) GO TO 70
      IF(IY(I).EQ.IY(I+1)) GO TO 70
      B(I)=FLOAT(IX(I+1)-IX(I))/FLOAT(IY(I+1)-IY(I))
   70 CONTINUE
C   
C     READ IN ALL APPROPRIATE SLICES AND LAY MASK ON EACH
C   
      MLO=MINY
      MHI=MAXY
      IF(MLO.LT.1) MLO=1
      IF(MHI.GT.NCY) MHI=NCY
C   
C     ------------------------------------------------------------------
C     MAIN LOOP - READ SLICES, SET UP MASK, LAY ON MASK, ETC
C     ------------------------------------------------------------------
C   
      DO 500 II=MLO,MHI
      JY=II
C   
C     SET UP THE "SLICE MASK"
C   
      DO 90 I=1,NCX
      MASK(I)=0
   90 CONTINUE
C   
C     ------------------------------------------------------------------
C     SET UP MASK FOLLOWING SCOTT'S RULES
C   
C     ADD +1 IF BOUNDRY SEGMENT IS ABOVE SLICE AND IA.LT.IB
C     ADD -1 IF BOUNDRY SEGMENT IS BELOW SLICE AND IA.LT.IB
C     ADD -1 IF BOUNDRY SEGMENT IS ABOVE SLICE AND IA.GT.IB
C     ADD +1 IF BOUNDRY SEGMENT IS BELOW SLICE AND IA.GT.IB
C     ADD +1 IF BOUNDRY SEGMENT IS ON SLICE (EITHER CASE)
C   
C     I CAN IGNORE "VERTICAL" SEGMENTS
C     ------------------------------------------------------------------
C   
      LDIR=0
      DO 220 N=1,NP
      IA=IX(N)
      IB=IX(N+1)
      JA=IY(N)
      JB=IY(N+1)
      IDIR=0
C   
      IF(IA.NE.IB) GO TO 95
C   
      IF(JA.LE.JY.AND.JB.GE.JY) MASK(IA)=MASK(IA)+10    !VERTICAL
      IF(JA.GE.JY.AND.JB.LE.JY) MASK(IA)=MASK(IA)+10
      GO TO 215
C   
   95 IDIR=1
      IF(IA.GT.IB) IDIR=-1                              !SET DIR
C   
      IF(JA.EQ.JY.AND.JB.EQ.JY) GO TO 100               !COINC
C   
      IF(JA.GE.JY.AND.JB.GE.JY) GO TO 110               !ABOVE
C   
      IF(JA.LE.JY.AND.JB.LE.JY) GO TO 120               !BELOW
C   
      IF(JA.LT.JY.AND.JB.GT.JY) GO TO 130               !CROSS +
C   
      IF(JA.GT.JY.AND.JB.LT.JY) GO TO 140               !CROSS -
C   
C                                       COINCIDENT
  100 IAD=1
      GO TO 200
C                                       ALL ABOVE, ENTER -, EXIT +
  110 IAD=IDIR
      IF(JA.EQ.JY) MASK(IA)=MASK(IA)+10                  !  EXIT +
      IF(JB.EQ.JY) MASK(IB)=MASK(IB)+10                  ! ENTER -
      GO TO 200
C                                       ALL BELOW, ENTER +, EXIT -
  120 IAD=-IDIR
      IF(JA.EQ.JY) MASK(IA)=MASK(IA)+10                  !  EXIT -
      IF(JB.EQ.JY) MASK(IB)=MASK(IB)+10                  ! ENTER +
      GO TO 200
C                                       CROSS GOING +
  130 ICROS=IA+B(N)*(JY-JA)+0.5
      KA=ICROS
      KB=IB
      IF(IDIR.GT.0) GO TO 132
      ITMP=KA
      KA=KB
      KB=ITMP
  132 IF(KA.GT.KB) GO TO 136
      DO 135 I=KA,KB
      MASK(I)=MASK(I)+IDIR
  135 CONTINUE
  136 IAD=-IDIR
      IB=ICROS-IDIR
      GO TO 200
C                                       CROSS GOING -
  140 ICROS=IA+B(N)*(JY-JA)+0.5
      KB=ICROS
      KA=IA
      IF(IDIR.EQ.LDIR) KA=KA+IDIR
      IF(IDIR.GT.0) GO TO 142
      ITMP=KA
      KA=KB
      KB=ITMP
  142 IF(KA.GT.KB) GO TO 146
      DO 145 I=KA,KB
      MASK(I)=MASK(I)+IDIR
  145 CONTINUE
  146 IA=ICROS+IDIR
      LDIR=0
      IAD=-IDIR
C   
  200 IF(IDIR.EQ.LDIR) IA=IA+IDIR
      IF(IDIR.GT.0) GO TO 205
      ITMP=IA
      IA=IB
      IB=ITMP
  205 IF(IA.GT.IB) GO TO 215
      DO 210 I=IA,IB
      MASK(I)=MASK(I)+IAD
  210 CONTINUE
  215 LDIR=IDIR
  220 CONTINUE
C   
C     ------------------------------------------------------------------
C     READ IN SLICES AND ACCUMULATE DATA WHICH FALL WITHIN MASK
C     ------------------------------------------------------------------
C   
      CALL SLICER(LUD,LUH,ID,II,IDAT,IERR)
      IF(IERR.NE.0) RETURN
      DO 400 K=1,NCX
      NCT=IDAT(K)
      IF(NCT.EQ.0.OR.MASK(K).LE.0) GO TO 400
      CTS=NCT
      GO TO (321,322,323,324),NQUAD
  321 X1=AA+COSA*FLOAT(K-1)+SINA*FLOAT(II-1)
      X2=AA+COSA*FLOAT(K  )+SINA*FLOAT(II  )
      GO TO 240
  322 X1=AA+COSA*FLOAT(K  )+SINA*FLOAT(II-1)
      X2=AA+COSA*FLOAT(K-1)+SINA*FLOAT(II  )
      GO TO 240
  323 X1=AA+COSA*FLOAT(K  )+SINA*FLOAT(II  )
      X2=AA+COSA*FLOAT(K-1)+SINA*FLOAT(II-1)
      GO TO 240
  324 X1=AA+COSA*FLOAT(K-1)+SINA*FLOAT(II  )
      X2=AA+COSA*FLOAT(K  )+SINA*FLOAT(II-1)
  240 ILO=X1
      IHI=X2+0.999
      NBIV=IHI-ILO
      IF(NBIV.GT.1) GO TO 250
      IF(IHI.LT.1) GO TO 300
      IF(IHI.GT.MAXL) GO TO 310
      BDAT(IHI)=BDAT(IHI)+CTS
      GO TO 300
  250 JX=ILO
      FNORM=1.0/(X2-X1)
      DO 260 N=1,NBIV
      IF(JX.GT.MAXL) GO TO 310
      JX=JX+1
      XJ=JX
      F=FNORM
      IF(N.EQ.1) F=FNORM*(XJ-X1)
      IF(N.EQ.NBIV) F=FNORM*(X2-(XJ-1.0))
      IF(JX.LT.1) GO TO 260
      IF(JX.GT.MAXL) GO TO 310
      BDAT(JX)=BDAT(JX)+F*CTS
  260 CONTINUE
  300 CONTINUE
  310 CONTINUE
  400 CONTINUE
  500 CONTINUE
C   
C     ------------------------------------------------------------------
C     LOAD IT BACK INTO ADAT
C     ------------------------------------------------------------------
C   
      NC=XMAX-XMIN+0.99
      IF(IRECF.EQ.'NO  ') NC=NC+1
      DO 510 I=1,NC
      ADAT(I)=BDAT(I)
  510 CONTINUE
      RETURN
      END
