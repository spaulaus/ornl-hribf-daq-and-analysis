C$PROG SPKIN1    - Reads data from spk-file - for 1-D display routines
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SPKIN1(MODE,KSOR,ID,CNO,ILO,IHI,NCHD,IERR)
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
      COMMON/DML4/ LABLX(3),LABLY(3),LABTIT(10)
      CHARACTER*40 CLABTIT
      CHARACTER*12 CLABLX,  CLABLY
      EQUIVALENCE (CLABLX,LABLX),(CLABLY,LABLY),(CLABTIT,LABTIT)
C     ------------------------------------------------------------------
      COMMON/DML6/ ISIGNF
      CHARACTER*4  ISIGNF
C     ------------------------------------------------------------------
      COMMON/PL17/ LENSPK
C     ------------------------------------------------------------------
      CHARACTER*4  MODE,KSOR,KINDF
C
      INTEGER*4    NDX(4)
C
      INTEGER*2    ITMH(2)
C   
      EQUIVALENCE (ITMF,ITMH(1))
C   
      SAVE
C   
C     ------------------------------------------------------------------
C     ROUTINE TO READ IN DATA FROM EITHER SPK- OR HIS-FILES
C     ------------------------------------------------------------------
C     MODE = 'TEST' SAYS GET ATTRIBUTES ONLY, NO DATA READ
C     MODE = 'READ' SAYS READ DATA
C     ID   = ID-NUMBER TO BE RETRIEVED
C     ILO  = FIRST ELEMENT TO BE READ (FIRST CHANNEL# +1)
C     IHI  = LAST  ELEMENT TO BE READ (LAST  CHANNEL# +1)
C     NCHD = NUMBER OF CHANNELS OF DATA
C     NHWD = NUMBER OF HALF-WDS PER CHANNEL
C     ------------------------------------------------------------------
C
      CLABLX=' '
      CLABLY=' '
      CLABTIT=' '
C   
      LENSPK=0
      NDX(1)=ILO
      JHI=IHI
      IF(JHI.GT.NCHD) JHI=NCHD
      NCH=JHI-ILO+1
      IF(MODE.EQ.'TEST') THEN
                         NDX(1)=1
                         NCH=0
                         ENDIF
C   
      IF(KSOR.EQ.'M   ') GO TO 100
C   
      CALL LUGET(KSOR,LUH,LUD,KINDF,IERR)
      IF(IERR.NE.0) GO TO 520
      LUS=LUH
C   
      IF(KINDF.EQ.'SPK ') GO TO 200
      IF(KINDF.EQ.'HIS ') GO TO 300
      GO TO 520
C   
C     ------------------------------------------------------------------
C     READ DATA FROM MILDO MEMORY BUFFERS - B1 OR B2
C     ------------------------------------------------------------------
C   
  100 CALL MEMIN(ID,IHEDF,IDATF,ILO,NCH,IERR)
C   
      IF(IERR.NE.0) GO TO 500
C
      LABLX(1)=IHEDF(2)
      LABLX(2)=IHEDF(3)
      LABLX(3)=IHEDF(4)
      DO 110 I=1,10
      LABTIT(I)=IHEDF(I+22)
  110 CONTINUE
C   
      NCHD=IHEDF(12)
      NHWD=2
      LENSPK=IHEDF(14)
      IF(MODE.EQ.'TEST') RETURN
      GO TO 405
C   
C     ------------------------------------------------------------------
C     READ DATA FROM SPK-FILES
C     ------------------------------------------------------------------
C   
  200 CALL SPKIO(1,LUS,ID,IHEDF,64,IDATF,NDX,NCH,IERR)!READ FROM .SPK
C   
      CALL SPKERR(IERR)                               !REPORT ERROR
      IF(IERR.NE.0) GO TO 500                         !TST FOR ERROR
      NCHD=IHEDF(12)                                  !SET # CHANS
      LENSPK=IHEDF(14)
      LABLX(1)=IHEDF(2)
      LABLX(2)=IHEDF(3)
      LABLX(3)=IHEDF(4)
      DO 210 I=1,10
      LABTIT(I)=IHEDF(I+22)
  210 CONTINUE
      NHWD=2
      IF(MODE.EQ.'TEST') RETURN
      GO TO 405
C   
C     ------------------------------------------------------------------
C     READ DATA FROM HIS-FILES
C     ------------------------------------------------------------------
C   
  300 IF(MODE.EQ.'READ') GO TO 310                    !TST FOR READ
C   
      CALL HISIN(LUD,LUH,ID,NDX,NCH,IDATF,IERR)       !GET ATTRIBUTES
C   
      CALL HISERR(IERR)
      IF(IERR.NE.0) GO TO 500
      IF(ND.NE.1)   GO TO 510                         !TST FOR 1-D
      NCHD=LENG(1)                                    !SET # CHANS
      NHWD=NHW
      RETURN
C   
  310 CALL HISIN(LUD,LUH,ID,NDX,0,IDATF,IERR)         !GET ATTRIBUTES
C
      CALL HISERR(IERR)
      IF(IERR.NE.0) GO TO 500
C
      LABLX(1)=LABX(1)
      LABLX(2)=LABX(2)
      LABLX(3)=LABX(3)
      DO 315 I=1,10
      LABTIT(I)=ITIT(I)
  315 CONTINUE
C
      LENSPK=LENS(1)                                  !SCALED LENGTH
C
      IF(NHW.EQ.1) GO TO 320                          !TST FULL/HALF
C   
      CALL HISIN(LUD,LUH,ID,NDX,NCH,IDATF,IERR)       !READ DATA
C   
      CALL HISERR(IERR)
      IF(IERR.NE.0) GO TO 500
      GO TO 405
C   
  320 IF=NCH+1                                        !READ 1/2 WORD
C                                                     !DATA INTO UPPER
      CALL HISIN(LUD,LUH,ID,NDX,NCH,IDATF(IF),IERR)   !PART OF BUFFER
C   
      CALL HISERR(IERR)
      IF(IERR.NE.0) GO TO 500                         !TST FOR ERROR
C   
      N=0                                             !AND LOAD INTO
      M=NCH                                           !LOWER PART
  330 N=N+1
      M=M+1
      IF(N.GT.NCH) GO TO 400
      ITMF=IDATF(M)
      IDATF(N)=ITMH(1)
      N=N+1
      IF(N.GT.NCH) GO TO 400
      IDATF(N)=ITMH(2)
      GO TO 330
C
  400 IF(ISIGNF.EQ.'SIDA') GO TO 405
C
      DO 402 I=1,NCH
      IDATF(I)=IAND(IDATF(I),Z'FFFF')
  402 CONTINUE
C   
  405 IF(CNO.EQ.1.0) GO TO 420
C   
      DO 410 I=1,NCH
      DAT=CNO*FLOAT(IDATF(I))+0.5
      IDATF(I)=DAT
  410 CONTINUE
C   
  420 IF(JHI.GE.IHI) RETURN
      IA=NCH+1
      IB=IA+IHI-JHI-1
      DO 430 I=IA,IB
      IDATF(I)=0
  430 CONTINUE
      RETURN
C   
C     ------------------------------------------------------------------
C     RETURN ERROR MESSAGES
C     ------------------------------------------------------------------
C   
  500 WRITE(CMSSG,505)ID,KSOR
  505 FORMAT('ERROR READING ID# ',I8,'  FROM ',A3,'-FILE')
      GO TO 600
C
  510 WRITE(CMSSG,515)ID,KSOR
  515 FORMAT('REQUESTED ID# ',I8,'  FROM ',A3,'-FILE NOT 1-D')
      GO TO 600
C
  520 WRITE(CMSSG,525)
  525 FORMAT('REQUESTED FILE NOT OPEN')
C   
  600 IERR=1
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
