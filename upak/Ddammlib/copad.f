C$PROG COPAD     - Copies or adds one his-ID to another
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE COPAD
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
C   
      COMMON/TDX2/ LUDI,LUHI,LUDO,LUHO
C   
      COMMON/TDX4/ ISEED
C     ------------------------------------------------------------------
      COMMON/TDX10/NXID
      INTEGER*4    NXID
C     ------------------------------------------------------------------
      CHARACTER*4  KMI,KMX
C
      INTEGER*4 IDAT(8192),JDAT(8192)
C
      INTEGER*4 HEDF(32),MSER(10),NDX(4)
C   
      INTEGER*2 HEDH(64),MINC(4),MAXC(4),ND,NHW
C
      EQUIVALENCE (KMI,IWD(1)),(KMX,LWD(1,3))
C   
      EQUIVALENCE (IDAT,IDATF(49153)),(JDAT,IDATF(57345))
C
      EQUIVALENCE (HEDH(1),HEDF(1)),
     &            (ND     ,HEDH(1)),
     &            (NHW    ,HEDH(2)),
     &            (MINC(1),HEDH(15)),
     &            (MAXC(1),HEDH(19))
C   
      DATA NDX/1,1,0,0/
C
      INTEGER*4    HEDSAV(32)
C
      SAVE
C   
C     ------------------------------------------------------------------
C     ROUTINE TO COPY OR ADD ONE HIS-ID TO ANOTHER
C     ------------------------------------------------------------------
C
      IERR=0
C   
      CALL IVALU(LWD(1,2),IDI,IERR)
      IF(IERR.NE.0) GO TO 300
      IF(IDI.LE.0)  GO TO 300
C
      IF(KMX.EQ.'NXID') THEN
      IF(NXID.LE.0) GO TO 330
      IDO=NXID
      GO TO 5
      ENDIF
C
      CALL IVALU(LWD(1,3),IDO,IERR)
      IF(IERR.NE.0) GO TO 300
      IF(IDO.LE.0)  GO TO 300
C   
    5 FACI=1.0
      FACO=1.0
      IF(NF.GE.4) CALL MILV(LWD(1,4),IV,FACI,KIND,IERR)
      IF(IERR.NE.0) GO TO 300
      IF(NF.GE.5) CALL MILV(LWD(1,5),IV,FACO,KIND,IERR)
      IF(IERR.NE.0) GO TO 300
C   
C   
      CALL HISIO('INIT',LUDI,LUHI,IDI,NDX,0,HEDF,IDAT,IERR,MSER)
      CALL HISIOER(IERR,MSER)
      IF(IERR.NE.0) RETURN
      CALL HISIO('READ',LUDI,LUHI,IDI,NDX,0,HEDF,IDAT,IERR,MSER)
      CALL HISIOER(IERR,MSER)
      IF(IERR.NE.0) RETURN
C
      DO 10 I=1,32
      HEDSAV(I)=HEDF(I)
   10 CONTINUE
C
      NDI=ND
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
C
      IF(IERR.NE.0) RETURN
C
      CALL HISIO('READ',LUDO,LUHO,IDO,NDX,0,HEDF,IDAT,IERR,MSER)
      CALL HISIOER(IERR,MSER)
C
      IF(IERR.EQ.0) GO TO 20
C
      IF(IERR.NE.1) RETURN
C
      CALL EXPANMAN(LUDO,LUHO,IDO,HEDSAV,IERR)
C
      IF(IERR.NE.0) RETURN
C
      CALL HISIO('INIT',LUDO,LUHO,IDO,NDX,0,HEDF,IDAT,IERR,MSER)
      CALL HISIOER(IERR,MSER)
C
      IF(IERR.NE.0) RETURN
C
      CALL HISIO('READ',LUDO,LUHO,IDO,NDX,0,HEDF,IDAT,IERR,MSER)
      CALL HISIOER(IERR,MSER)
C
      IF(IERR.NE.0) RETURN
C
C
   20 NDO=ND
      NHWO=NHW
      NCXO=MAXC(1)-MINC(1)+1
      NCYO=MAXC(2)-MINC(2)+1
C   
      MINXO=MINC(1)+1
      MAXXO=MAXC(1)+1
      MINYO=MINC(2)+1
      MAXYO=MAXC(2)+1
C   
      IF(MINXI.NE.MINXO) GO TO 320
      IF(MAXXI.NE.MAXXO) GO TO 320
      IF(MINYI.NE.MINYO) GO TO 320
      IF(MAXYI.NE.MAXYO) GO TO 320
C   
      IF(KMI.EQ.'HADD') GO TO 200
      IF(KMI.EQ.'HDIV') GO TO 200
C   
C     ------------------------------------------------------------------
C     COPY FACI*IDI FROM INPUT-FILE TO OUTPUT-FILE
C     ------------------------------------------------------------------
C   
      CALL SLICIA(LUDI,LUHI,IDI,0,IDAT,IERR)
      IF(IERR.NE.0) RETURN
      CALL  SLICO(LUDO,LUHO,IDO,0,IDAT,IERR)
      IF(IERR.NE.0) RETURN
C   
      DO 100 JY=MINYI,MAXYI
      CALL SLICIA(LUDI,LUHI,IDI,JY,IDAT,IERR)
      IF(IERR.NE.0) RETURN
C   
      IF(FACI.EQ.1.0) GO TO 60
C   
      DO 50 I=1,NCXI
      X=FACI*FLOAT(IDAT(I))
      IF(X.GT.0.0) X=X+RAN(ISEED)
      IF(X.LT.0.0) X=X-RAN(ISEED)
      IDAT(I)=X
   50 CONTINUE
C   
   60 CALL  SLICO(LUDO,LUHO,IDO,JY,IDAT,IERR)
      IF(IERR.NE.0) RETURN
  100 CONTINUE
      CALL  SLICO(LUDO,LUHO,IDO,-1,IDAT,IERR)
C
      IF(KMX.EQ.'NXID'.AND.NXID.GT.0) NXID=NXID+1
C
      RETURN
C   
C     ------------------------------------------------------------------
C     ADD FACI*IDI TO FACO*IDO AND STORE ON OUTPUT-FILE
C     ------------------------------------------------------------------
C   
  200 CALL SLICIA(LUDI,LUHI,IDI,0,IDAT,IERR)
      IF(IERR.NE.0) RETURN
      CALL SLICIB(LUDO,LUHO,IDO,0,IDAT,IERR)
      IF(IERR.NE.0) RETURN
      CALL  SLICO(LUDO,LUHO,IDO,0,IDAT,IERR)
      IF(IERR.NE.0) RETURN
C   
      DO 250 JY=MINYI,MAXYI
      CALL SLICIA(LUDI,LUHI,IDI,JY,IDAT,IERR)
      IF(IERR.NE.0) RETURN
      CALL SLICIB(LUDO,LUHO,IDO,JY,JDAT,IERR)
      IF(IERR.NE.0) RETURN
C   
      IF(KMI.EQ.'HDIV')              GO TO 220
C   
      IF(FACI.EQ.1.0.AND.FACO.EQ.1.0) GO TO 210
C   
      DO 205 I=1,NCXI
      X=FACI*FLOAT(IDAT(I))+FACO*FLOAT(JDAT(I))
      IF(X.GT.0.0) X=X+RAN(ISEED)
      IF(X.LT.0.0) X=X-RAN(ISEED)
      JDAT(I)=X
  205 CONTINUE
      GO TO 240
C   
  210 DO 215 I=1,NCXI
      JDAT(I)=IDAT(I)+JDAT(I)
  215 CONTINUE
      GO TO 240
C   
  220 DO 225 I=1,NCXI
C   
      IF(JDAT(I).EQ.0) GO TO 225
      X=FACI*FLOAT(IDAT(I))/FLOAT(JDAT(I))
      IF(X.GT.0.0) X=X+RAN(ISEED)
      IF(X.LT.0.0) X=X-RAN(ISEED)
      JDAT(I)=X
  225 CONTINUE
C   
  240 CALL  SLICO(LUDO,LUHO,IDO,JY,JDAT,IERR)
      IF(IERR.NE.0) RETURN
  250 CONTINUE
C   
      CALL  SLICO(LUDO,LUHO,IDO,-1,JDAT,IERR)
C
      IF(KMX.EQ.'NXID'.AND.NXID.GT.0) NXID=NXID+1
C
      RETURN
C   
C     ------------------------------------------------------------------
C     ERROR returns
C     ------------------------------------------------------------------
C
  300 WRITE(CMSSG,305)
  305 FORMAT('SYNTAX ERROR OR ILLEGAL VALUE - CMD IGNORED')
      GO TO 1000
C   
  320 WRITE(CMSSG,325)
  325 FORMAT('INPUT & OUTPUT HISTOGRAMS NOT DIMENSIONALLY IDENTICAL')
      GO TO 1000
C
  330 WRITE(CMSSG,335)
  335 FORMAT('HCOP IDI NXID request rejected - NXID undefined')
      GO TO 1000
C
 1000 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
