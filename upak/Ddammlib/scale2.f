C$PROG SCALE2    - Computes Z-scale for 2-D displays
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SCALE2(IDW,LUH,LUD,ID,KPLO,NZVAL,IXA,IXB,
     &                   MINY,MAXY,KRUNX,KRUNY,AA,BB,IERR)
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/DIR/  KLOC(6),JHSP(4),LENG(4),ND,NHW,           !/DIR
     &             LENH,LENT,IOF,LDF,NHIS,LEND(4),           !/DIR
     &             LENS(4),MINC(4),MAXC(4),CONS(4),ITEX(20), !/DIR
     &             ITIT(10),LABX(3),LABY(3),MSER(10),KFILT   !/DIR 
C     ------------------------------------------------------------------
      COMMON/DML2/ IDATF(65536),IHEDF(32),MAXCH
C     ------------------------------------------------------------------
      COMMON/PL03/ IDAT(4096),MINCN(2,20),MAXCN(2,20),MINZ,MAXZ
C     ------------------------------------------------------------------
      COMMON/PL11/ NZADD
C     ------------------------------------------------------------------
      CHARACTER*4  KPLO,MODE
C
      INTEGER*4    INBUF(16384)
C
      INTEGER*2    INBUH(32768)
C
      INTEGER*4    NDX(4)
C   
      EQUIVALENCE (INBUH,IDATF),(INBUF,IDATF)
C   
      DATA NDX/4*0/
C
      SAVE
C
C     ==================================================================
C   
      IF(MAXZ.NE.0) GO TO 200                !TST FOR AUTOSCALE REQUIRED
C   
C     ------------------------------------------------------------------
C     IF YES, FIND MIN AND MAX VALUES (MINZ & MAXZ) BY SUMMING THINGS
C     JUST AS YOU WILL FOR THE ACTUAL PLOTTING
C     ------------------------------------------------------------------
C   
      MODE='GET '
      MINZSAV=0
      IF(MINZ.NE.0) MINZSAV=MINZ
      MINZ=100000000
      MAXZ=-100000000
C   
      KDOX=(IXB-IXA+KRUNX)/KRUNX                !#X-CHANS POSTCRUN
C
      JYB=MINY-1                                !INIT 2ND INDEX
  100 JYA=JYB+1                                 !LOOP ON 2ND PARM
      IF(JYA.GT.MAXY) GO TO 190                 !TST FOR DONE
      IF(MSGF.NE.'    ') RETURN                 !TST STOPIT FLAG
      JYB=JYA+KRUNY-1
      IF(JYB.GT.MAXY) JYB=MAXY
C   
      CALL SLICE2(IDW,MODE,LUH,LUD,ID,IXA,IXB,KRUNX,JYA,JYB,IERR)  
      IF(IERR.NE.0) RETURN
C   
      DO 120 I=1,KDOX                           !FIND IT'S MIN & MAX
      IF(IDAT(I).LT.MINZ) GO TO 110
      IF(IDAT(I).LE.MAXZ) GO TO 120
      MAXZ=IDAT(I)
      GO TO 120
C   
  110 MINZ=IDAT(I)
  120 CONTINUE
      GO TO 100
C
  190 IF(MINZSAV.NE.0) MINZ=MINZSAV
C   
C     ------------------------------------------------------------------
C     WE HAVE MINZ & MAXZ - COMPUTE SCALE COEFFS AA & BB
C     ------------------------------------------------------------------
C
  200 IF(KPLO.EQ.'LOG ') GO TO 220
C
      IF(MAXZ.LT.MINZ+2) MAXZ=MINZ+2   
      ZMIN=MINZ
      ZMAX=MAXZ
      XNUM=NZVAL
C   
      BB=XNUM/(ZMAX-ZMIN)                       !SET COEFFS FOR LIN
      AA=1.00-BB*ZMIN
      RETURN
C
  220 NZADD=0
      IF(MINZ.LT.0) NZADD=-MINZ
      IF(MAXZ.LT.MINZ+2) MAXZ=MINZ+2
      ZMIN=MINZ+NZADD
      IF(ZMIN.LT.1.0) ZMIN=1.0
      ZMAX=MAXZ+NZADD
      IF(ZMAX.LT.ZMIN+2.0) ZMAX=ZMIN+2.0
      XNUM=NZVAL
C   
      BB=XNUM/(ALOG(ZMAX)-ALOG(ZMIN))           !SET COEFFS FOR LOG
      AA=1.00-BB*ALOG(ZMIN)
      RETURN
      END
