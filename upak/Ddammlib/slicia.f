C$PROG SLICIA    - Returns slices of 2-D data for COPAD & SHIFT
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SLICIA(LUD,LUH,ID,IY,IDAT,IERR)
C
C     ------------------------------------------------------------------
      COMMON/DML2/ IDATF(65536),IHEDF(32),MAXCH
C     ------------------------------------------------------------------
      COMMON/DML6/ ISIGNF
      CHARACTER*4  ISIGNF
C     ------------------------------------------------------------------
      INTEGER*4    JDATF(16384),NDX(4),IDAT(*)
      INTEGER*4    JHEDF(32),MSER(10)
C   
      INTEGER*2    JDATH(32768)
      INTEGER*2    JHEDH(64),MINC(4),MAXC(4),ND,NHW
C   
      EQUIVALENCE (JDATF(1),IDATF(1))
C
      EQUIVALENCE (JDATH(1),JDATF(1)),
     &            (JHEDH(1),JHEDF(1)),
     &            (ND      ,JHEDH(1)),
     &            (NHW     ,JHEDH(2)),
     &            (MINC(1) ,JHEDH(15)),
     &            (MAXC(1) ,JHEDH(19))
C   
      DATA NDX/1,1,0,0/
C
      DATA NSLI,ISLO,ISHI,NCH,MINX,MAXX,MINY,MAXY/8*0/
C
      DATA NHW/1/
C
      SAVE
C   
C     ------------------------------------------------------------------
C     RETURNS ONE SLICE AT A TIME FROM A 2-D HISTOGRAM
C     THE FIRST "AVAILABLE" ELEMENT LOADED INTO IDAT(1), ETC
C     I.E. NOT "PROPERLY POSITIONED"
C     ------------------------------------------------------------------
C     IY = 0 SAYS INIT - DO 1ST HISIO CALL TO GET ATTRIBUTES
C     IY > 0 SAYS RETURN SLICE - IY
C     ------------------------------------------------------------------
C   
      IERR=0
C   
      IF(IY.GT.0) GO TO 100
C   
      NDX(1)=1
      NDX(2)=1
      CALL HISIO('INIT',LUD,LUH,ID,NDX,0,JHEDF,IDAT,IERR,MSER)
      CALL HISIOER(IERR,MSER)
      IF(IERR.NE.0) RETURN
      CALL HISIO('READ',LUD,LUH,ID,NDX,0,JHEDF,IDAT,IERR,MSER)
      CALL HISIOER(IERR,MSER)
      IF(IERR.NE.0) RETURN
C   
      MINX=MINC(1)+1                        !MIN X-INDEX AVAILABLE
      MAXX=MAXC(1)+1                        !MAX X-INDEX AVAILABLE
      MINY=MINC(2)+1                        !MIN Y-INDEX AVAILABLE
      MAXY=MAXC(2)+1                        !MAX Y-INDEX AVAILABLE
C   
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
C   
      IF(IY.GE.ISLO.AND.IY.LE.ISHI) GO TO 150
C   
      ISLO=IY                               !1ST SLICE TO READ
      ISHI=ISLO+NSLI-1                      !MAX SLICE TO READ
      IF(ISHI.GT.MAXY) ISHI=MAXY            !LIMIT TO # AVAILABLE
      NDX(2)=ISLO-MINY+1                    !SET NDX(2) FOR HISIO
      NC=NCH*(ISHI-ISLO+1)                  !# CHANS TO READ
C   
      CALL HISIO('INIT',LUD,LUH,ID,NDX,NC,JHEDF,JDATF,IERR,MSER)
      CALL HISIOER(IERR,MSER)
      IF(IERR.NE.0) RETURN
      CALL HISIO('READ',LUD,LUH,ID,NDX,NC,JHEDF,JDATF,IERR,MSER)
      CALL HISIOER(IERR,MSER)
      IF(IERR.NE.0) RETURN
C   
  150 N=NCH*(IY-ISLO)
      NDO=MAXX-MINX+1
      IF(NHW.EQ.1) GO TO 200
      DO 160 I=1,NDO                        !LOAD FULL-WORD DATA
      N=N+1
      IDAT(I)=JDATF(N)
  160 CONTINUE
      RETURN
C
  200 IF(ISIGNF.EQ.'SIDA') GO TO 250
C   
      DO 210 I=1,NDO                        !LOAD HALF-WORD UNSIGNED
      N=N+1
      JDAF=JDATH(N)
      IDAT(I)=IAND(JDAF,Z'FFFF')
  210 CONTINUE
      RETURN
C
  250 DO 260 I=1,NDO                        !LOAD HALF-WORD SIGNED
      N=N+1
      IDAT(I)=JDATH(N)
  260 CONTINUE
      RETURN
      END
