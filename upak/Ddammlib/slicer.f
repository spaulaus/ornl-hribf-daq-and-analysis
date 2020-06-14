C$PROG SLICER    - Returns slices of 2-D data for GATE & PROJ
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SLICER(LUD,LUH,ID,IY,IDAT,IERR)
C   
C     ------------------------------------------------------------------
      COMMON/DIR/  KLOC(6),JHSP(4),LENG(4),ND,NHW,           !/DIR
     &             LENH,LENT,IOF,LDF,NHIS,LEND(4),           !/DIR
     &             LENS(4),MINC(4),MAXC(4),CONS(4),ITEX(20), !/DIR
     &             ITIT(10),LABX(3),LABY(3),MSER(10),KFILT   !/DIR 
C     ------------------------------------------------------------------
      COMMON/DML2/ IDATF(65536),IHEDF(32),MAXCH
C     ------------------------------------------------------------------
      COMMON/DML6/ ISIGNF
      CHARACTER*4  ISIGNF
C     ------------------------------------------------------------------
      INTEGER*4    JDATF(16384),NDX(4),IDAT(*)
C   
      INTEGER*2    JDATH(32768)
C   
      EQUIVALENCE (JDATF,IDATF),(JDATH,IDATF)
C   
      DATA NDX/1,1,0,0/
C
      DATA NSLI,ISLO,ISHI,NCH,MINX,MAXX,MINY,MAXY/8*0/
C
      SAVE
C   
C     ------------------------------------------------------------------
C     RETURNS ONE SLICE AT A TIME FROM A 2-D HISTOGRAM
C     PROPERLY POSITIONED IN THE FULL-WORD ARRAY "IDAT"
C     ------------------------------------------------------------------
C     IY = 0 SAYS INIT - DO 1ST HISIN CALL TO GET ATTRIBUTES
C     IY > 0 SAYS RETURN SLICE - IY
C     ------------------------------------------------------------------
C   
      IERR=0
C   
      IF(IY.GT.0) GO TO 100
C   
      NDX(1)=1
      NDX(2)=1
      CALL HISIN(LUD,LUH,ID,NDX,0,IDAT,IERR)
      CALL HISERR(IERR)
      IF(ND.NE.2) IERR=6
      CALL HISERR(IERR)
      IF(IERR.NE.0) RETURN
C   
      MINX=MINC(1)+1                        !MIN X-INDEX AVAILABLE
      MAXX=MAXC(1)+1                        !MAX X-INDEX AVAILABLE
      MINY=MINC(2)+1                        !MIN Y-INDEX AVAILABLE
      MAXY=MAXC(2)+1                        !MAX Y-INDEX AVAILABLE
      IDX=MINX                              !START INDEX IN IDAT
      NCH=MAXC(1)-MINC(1)+1                 !# OF X-CHANS AVAILABLE
      NWPS=NHW*NCH                          !# HALF-WDS/SLICE
      NSLI=32768/NWPS                       !MAX # SLICES IN-CORE
      ISLO=0                                !INIT MIN IN-CORE SLICE
      ISHI=0                                !INIT MAX IN-CORE SLICE
      RETURN
C   
  100 DO 110 I=1,MAXX
      IDAT(I)=0
  110 CONTINUE
C   
      IF(IY.LT.MINY.OR.IY.GT.MAXY) RETURN
      IF(IY.GE.ISLO.AND.IY.LE.ISHI) GO TO 150
C   
      ISLO=IY                               !1ST SLICE TO READ
      ISHI=ISLO+NSLI-1                      !MAX SLICE TO READ
      IF(ISHI.GT.MAXY) ISHI=MAXY            !LIMIT TO # AVAILABLE
      NDX(2)=ISLO-MINY+1                    !SET NDX(2) FOR HISIN
      NC=NCH*(ISHI-ISLO+1)                  !# CHANS TO READ
C   
C   
      CALL HISIN(LUD,LUH,ID,NDX,NC,JDATF,IERR)
      CALL HISERR(IERR)
      IF(ND.NE.2) IERR=6
      CALL HISERR(IERR)
      IF(IERR.NE.0) RETURN
C   
  150 N=NCH*(IY-ISLO)
      IF(NHW.EQ.1) GO TO 200
      DO 160 I=MINX,MAXX
      N=N+1
      IDAT(I)=JDATF(N)
  160 CONTINUE
      RETURN
C   
  200 IF(ISIGNF.NE.'USDA') GO TO 220
C   
      DO 210 I=MINX,MAXX
      N=N+1
      JDAF=JDATH(N)
      IDAT(I)=IAND(JDAF,Z'FFFF')
  210 CONTINUE
      RETURN
C   
  220 DO 230 I=MINX,MAXX
      N=N+1
      IDAT(I)=JDATH(N)
  230 CONTINUE
      RETURN
      END
