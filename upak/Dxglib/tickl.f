C$PROG TICKL     - Specifies tick & label locations
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE TICKL(A,B,FPIX,JFIR,JINC,NJ,KFIR,KINC,NK)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
C
      INTEGER*4 JINCR(27),KINCR(27)
C
      REAL*4    A,B,FPIX
C
      INTEGER*4 JFIR,JINC,NJ,KFIR,KINC,NK
C
      INTEGER*4 NPIX,NLAB,TINC,NDX,JNOW,KNOW,NDIG,I
C
      DATA JINCR/1,2,5,10,20,50,100,200,500,1000,2000,5000,
     &          10000,20000,50000,100000,200000,500000,1000000,
     &          2000000,5000000,10000000,20000000,50000000,
     &          100000000,200000000,500000000/
C
      DATA KINCR/1,1,1,1,2,5,10,20,50,100,200,500,1000,2000,5000,
     &           10000,20000,50000,100000,200000,500000,1000000,
     &           2000000,5000000,10000000,20000000,50000000/
C
      SAVE
C
C     ------------------------------------------------------------------
C     TICKL SPECIFIES TICK AND LABEL LOCATIONS AS DEFINED BELOW
C     DEFINITION OF ARGUMENTS FOLLOW:
C
C     A    = MIN AXIS VALUE (FLOATING USER-UNITS)
C     B    = MAX AXIS VALUE (FLOATING USER-UNITS)
C     IA   = MIN AXIS VALUE (INTEGER  USER-UNITS)
C     IB   = MAX AXIS VALUE (INTEGER  USER-UNITS)
C
C     NPIX = NUMBER OF PIXELS AVAILABLE
C
C     JFIR = LOC OF FIRST LONG TICK (USER UNITS)
C     JINC = LONG TICK INCREMENT    (USER UNITS)
C     NJ   = NUMBER OF LONG TICKS
C
C     KFIR = LOC OF FIRST SHORT TICK (USER UNITS)
C     KINC = SHORT TICK INCREMENT    (USER UNITS)
C     NK   = NUMBER OF SHORT TICKS
C     ------------------------------------------------------------------
C
C
      IA=A+0.9
      IB=B+0.9
      NPIX=FPIX+0.5
C
CX    IF(NPIX.NE.0) return
CX    IF(NPIX.EQ.0) return
C
      NLAB=10
C
   10 TINC=(IB-IA)/NLAB
      IF(NLAB*TINC.LT.IB-IA) TINC=TINC+1
C
      DO 15 I=1,27
      IF(JINCR(I).GE.TINC) GO TO 20
   15 CONTINUE
      I=27
C
   20 NDX=I
      JINC=JINCR(NDX)
      JFIR=JINC*(IA/JINC)
      IF(JFIR.LT.IA) JFIR=JFIR+JINC
      JNOW=JFIR
      NDIG=NDIGIT(JNOW)
      NJ=1
      DO 30 I=1,50
      JNOW=JNOW+JINC
      IF(JNOW.GE.IB) GO TO 40
      NDIG=NDIG+NDIGIT(JNOW)
      NJ=NJ+1
   30 CONTINUE
C
   40 KINC=KINCR(NDX)
      KFIR=JFIR
      DO 50 I=1,10
      KTST=KFIR-KINC
      IF(KTST.LT.IA) GO TO 60
      KFIR=KTST
   50 CONTINUE
C
   60 KNOW=KFIR
      NK=1
      DO 70 I=1,100
      KNOW=KNOW+KINC
      IF(KNOW.GT.IB) GO TO 80
      NK=NK+1
   70 CONTINUE
C
   80 IF(16*NDIG.GT.NPIX)  THEN
                           NLAB=NLAB-1
                           IF(NLAB.LE.0) RETURN
                           GO TO 10
                           ENDIF
C
c 100 WRITE(6,200)IA,IB,NPIX
c     WRITE(6,210)JFIR,JINC,NJ
c     WRITE(6,220)KFIR,KINC,NK
c     WRITE(6,230)
C
c 200 FORMAT(1H ,'IA,  IB,  NPIX =',3I10)
c 210 FORMAT(1H ,'JFIR,JINC,  NJ =',3I10)
c 220 FORMAT(1H ,'KFIR,KINC,  NK =',3I10)
c 230 FORMAT(1H )
    
C
      RETURN
      END
