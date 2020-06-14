C$PROG SLOUT     - Loads slice from ABUF into BBUF and stores
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SLOUT(MODE,IDO,JY,NCH,F,IERR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/DML2/ IDATF(65536),IHEDF(32),MAXCH
C     ------------------------------------------------------------------
      COMMON/TDX2/ LUDI,LUHI,LUDO,LUHO
C     ------------------------------------------------------------------
      COMMON/TDX7/ KMDS,FACI,FACO
      CHARACTER*4  KMDS
C     ------------------------------------------------------------------
      COMMON/TDX4/ ISEED
C     ------------------------------------------------------------------
      CHARACTER*4  MODE
C
      INTEGER*4    IDAT(8192),JDAT(8192)
C
      INTEGER*4    KDAT(8192)
C   
      DIMENSION    ABUF(8192),BBUF(8192),CBUF(8192)
C   
      REAL*8       DSUM
C   
      EQUIVALENCE (IDAT,IDATF(49153)),(JDAT,IDATF(57345))
C
      EQUIVALENCE (ABUF(1),IDAT(1)),
     &            (BBUF(1),JDAT(1)),
     &            (CBUF(1),KDAT(1))
C   
      DATA LASJY/1000000/
      DATA DSUM/0.0D0/
C
      SAVE
C   
C     ------------------------------------------------------------------
C     LOADS DATA FROM ABUF INTO BBUF & STORES
C     ------------------------------------------------------------------
C     MODE = 'INIT' SAYS ZERO BBUF & RESET LASJY
C     MODE = 'STOR' SAYS ADD THE FRACTION "F" OF ABUF TO BBUF
C     MODE = 'FLUS' SAYS FLUSH BBUF & SLICO
C     NCH  = NUMBER OF CHANNELS TO STORE
C     ------------------------------------------------------------------
C   
      IF(MODE.EQ.'STOR') GO TO 100
      IF(MODE.EQ.'FLUS') GO TO 200
C   
C     ------------------------------------------------------------------
C     INIT SLICO AND MAYBE SLICIB
C     ------------------------------------------------------------------
C   
      DO 10 I=1,NCH
      BBUF(I)=0.0
   10 CONTINUE
      LASJY=1000000
      DSUM=0.0
      IF(KMDS.EQ.'SHIF') RETURN
C   
      CALL SLICIB(LUDO,LUHO,IDO,0,KDAT,IERR)
C   
      RETURN
C   
C     ------------------------------------------------------------------
C     DO STORE  OR  ADD & STORE
C     ------------------------------------------------------------------
C   
  100 IF(LASJY.EQ.1000000) GO TO 105
      IF(JY.LT.LASJY) THEN
                      WRITE(6,101)LASJY,JY
                      ENDIF
  101 FORMAT(1H ,'LASJY,JY=',2I5)
C   
  105 IF(JY.LE.LASJY) GO TO 150
C   
      IF(KMDS.NE.'SHAD') GO TO 120
C   
      CALL SLICIB(LUDO,LUHO,IDO,LASJY,KDAT,IERR)
      IF(IERR.NE.0) RETURN
C   
      DO 110 I=1,NCH
      BBUF(I)=FACI*BBUF(I)+FACO*FLOAT(KDAT(I))
      X=BBUF(I)
      IF(X.GT.0.0) X=X+RAN(ISEED)
      IF(X.LT.0.0) X=X-RAN(ISEED)
      JDAT(I)=X
  110 CONTINUE
      GO TO 135
C   
  120 DO 130 I=1,NCH
      X=FACI*BBUF(I)
      IF(X.GT.0.0) X=X+RAN(ISEED)
      IF(X.LT.0.0) X=X-RAN(ISEED)
      JDAT(I)=X
  130 CONTINUE
C   
  135 CALL SLICO(LUDO,LUHO,IDO,LASJY,JDAT,IERR)
      IF(IERR.NE.0) RETURN
C   
      DO 140 I=1,NCH
      BBUF(I)=0.0
  140 CONTINUE
C   
  150 DO 160 I=1,NCH
      BBUF(I)=BBUF(I)+F*ABUF(I)
      DSUM=DSUM+F*ABUF(I)
  160 CONTINUE
      LASJY=JY
      RETURN
C   
C     ------------------------------------------------------------------
C     FLUSH OUT WHATEVER IS LEFT
C     ------------------------------------------------------------------
C   
  200 IF(KMDS.NE.'SHAD') GO TO 220
C   
      CALL SLICIB(LUDO,LUHO,IDO,LASJY,KDAT,IERR)
      IF(IERR.NE.0) RETURN
C   
      DO 210 I=1,NCH
      BBUF(I)=FACI*BBUF(I)+FACO*FLOAT(KDAT(I))
      X=BBUF(I)
      IF(X.GT.0.0) X=X+RAN(ISEED)
      IF(X.LT.0.0) X=X-RAN(ISEED)
      JDAT(I)=X
  210 CONTINUE
      GO TO 240
C   
  220 DO 230 I=1,NCH
      X=FACI*BBUF(I)
      IF(X.GT.0.0) X=X+RAN(ISEED)
      IF(X.LT.0.0) X=X-RAN(ISEED)
      JDAT(I)=X
  230 CONTINUE
C   
  240 CALL SLICO(LUDO,LUHO,IDO,LASJY,JDAT,IERR)
C   
      CALL SLICO(LUDO,LUHO,IDO,-1,JDAT,IERR)
C   
      WRITE(CMSSG,250)DSUM
  250 FORMAT('TOTAL FLOATING SUM =',1PD16.8)
      CALL MESSLOG(6,7)
C   
      RETURN
      END
