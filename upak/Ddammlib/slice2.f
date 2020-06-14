C$PROG SLICE2    - Returns slices of 2-D data for PLOTUM2 & SCALE2
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SLICE2(IDW,MODE,LUH,LUD,ID,IXA,IXB,KRUNX,
     &                 JYA,JYB,IERR)
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
      COMMON/PL03/ IDAT(4096),MINCN(2,20),MAXCN(2,20),MINZ,MAXZ
C     ------------------------------------------------------------------
      CHARACTER*4  MODE
C   
      INTEGER*4    INBUF(16384)
      INTEGER*2    INBUH(32768)
C   
      INTEGER*4    NDX(4)
C   
      EQUIVALENCE (INBUH,IDATF),(INBUF,IDATF)
C   
      DATA NDX/4*0/
      DATA MAXNJ,JLOIN,JHIIN/0,0,0/
C
      SAVE
C
C     ==================================================================
C   
C     MODE = 'INIT' SAYS INITIALIZE, CALL "HISIN" TO GET ATTRIBUTES
C     MODE = 'GET ' SAYS RETURN 1ST PARM IXA THRU IXB IN "IDAT"
C                          (SUM 2ND PARM JYA THRU JYB)
C   
C     JLOIN = "LOWEST  2ND INDEX" CURRENTLY IN-CORE
C     JHIIN = "HIGHEST 2ND INDEX" CURRENTLY IN-CORE
C     MAXNJ = MAX # OF 2ND INDEX VALUES WHICH CAN BE READ IN AT ONE TIME
C     ------------------------------------------------------------------
C   
      IERR=0
C   
      IF(MODE.EQ.'GET ') GO TO 100
C   
      NDX(1)=1
      NDX(2)=1
      CALL HISIN(LUD,LUH,ID,NDX,0,INBUF,IERR)
      IF(IERR.NE.0) CALL HISERR(IERR)
      IF(IERR.NE.0) RETURN
      IF(ND.EQ.2) GO TO 50
C   
      WRITE(LOGUT,20)
   20 FORMAT(1H ,'REQUESTED ID NOT 2-D DATA')
      IERR=1
      RETURN
C   
   50 JLOIN=0
      JHIIN=0
      MAXNJ=32768/(NHW*LENG(1))
C   
      MINCN(1,IDW)=MINC(1)
      MINCN(2,IDW)=MINC(2)
      MAXCN(1,IDW)=MAXC(1)
      MAXCN(2,IDW)=MAXC(2)
      RETURN
C   
C     ------------------------------------------------------------------
C     MODE = 'GET ' - LOAD REQUESTED DATA INTO "IDAT"
C     ------------------------------------------------------------------
C   
  100 DO 105 I=1,4096                           !ZERO OUTPUT BUFFER
      IDAT(I)=0
  105 CONTINUE
C   
      J=JYA-1                                   !INIT 2ND INDEX
      NJJ=JYB-JYA+1
      DO 200 JJ=1,NJJ                           !LOOP ON 2ND PARM
      J=J+1
      IF(J.GE.JLOIN.AND.J.LE.JHIIN) GO TO 120   !TEST FOR INPUT NEED
C   
      JLOIN=J
      JHIIN=JLOIN+MAXNJ-1
      IF(JHIIN.GT.LENG(2)) JHIIN=LENG(2)
      NDX(1)=1
      NDX(2)=JLOIN
C   
      NSLIC=JHIIN-JLOIN+1
      NC=NSLIC*LENG(1)
      CALL HISIN(LUD,LUH,ID,NDX,NC,INBUF,IERR)  !READ IN NEW DATA
      IF(IERR.EQ.0) GO TO 120
C   
      CALL HISERR(IERR)
      RETURN
C   
  120 IA=LENG(1)*(J-JLOIN)+IXA                  !SET UP INBUF INDICES
      IB=IA+IXB-IXA
      N=1
      NK=0
      IF(NHW.EQ.1) GO TO 160                    !IS IT 16- OR 32-BIT
C   
      DO 140 I=IA,IB                            !LOAD FULL-WD DATA
      IDAT(N)=IDAT(N)+INBUF(I)
      NK=NK+1
      IF(NK.LT.KRUNX) GO TO 140                 !TST CRUNCH CNTR
      N=N+1                                     !INC CHAN# CNTR
      NK=0                                      !ZERO CRUNCH CNTR
  140 CONTINUE
      GO TO 200
C   
  160 DO 180 I=IA,IB                            !LOAD HALF-WD DATA
      IDATT=INBUH(I)
      IF(ISIGNF.EQ.'USDA') IDATT=IAND(IDATT,Z'FFFF')  !UNSIGNED?
      IDAT(N)=IDAT(N)+IDATT
      NK=NK+1                                   !INC CRUNCH CNTR
      IF(NK.LT.KRUNX) GO TO 180                 !TST AGAINST MAX VAL
      N=N+1                                     !INC CHAN# CNTR
      NK=0                                      !ZERO CRUNCH CNTR
  180 CONTINUE
C   
  200 CONTINUE
      RETURN
      END
