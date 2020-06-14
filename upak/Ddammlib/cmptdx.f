C$PROG CMPTDX    - Command processor for 2-D manipulation 
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE CMPTDX(IDONE,IERR)
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
      COMMON/DML1/ NAMFIL(20,20),KFIL(20),LUC(20),LIN,LCM,LCI
      INTEGER*4    NAMFIL,                LUC,    LIN,LCM,LCI
      CHARACTER*4                KFIL
C     ------------------------------------------------------------------
      COMMON/MD01/ KODFIL
C   
      COMMON/TDX2/ LUDI,LUHI,LUDO,LUHO
C   
      COMMON/TDX3/ XGS(4),YGS(4),IGSX,IGSY
C   
      COMMON/TDX5/ NAMFI(20),NAMFO(20)
C
      COMMON/TDX9/ NEGSET
C
      CHARACTER*4  IDONE,KMD,KMX,IGSX,IGSY,KINF,CNEGSET,CIWD
C
      EQUIVALENCE (KMD,LWD(1,1)),(KMX,LWD(1,2)),(CNEGSET,NEGSET)
C
      EQUIVALENCE (CIWD,IWD(1))
C   
      character*4 cigsx, cigsy
      equivalence (cigsx, igsx), (cigsy, igsy)
      DATA ISEED,cIGSX,cIGSY/-1,'NO  ','NO  '/
C   
      DATA CNEGSET/'OFF '/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
      IDONE='NO  '
C
      LUHO=LUC(10)
      LUDO=LUC(11)
C
      DO 10 I=1,20
      NAMFO(I)=NAMFIL(I,10)
   10 CONTINUE
C   
      IF(KMD.EQ.'GSXO') GO TO 140
      IF(KMD.EQ.'GSYO') GO TO 145
      IF(KMD.EQ.'GSX ') GO TO 150
      IF(KMD.EQ.'GSY ') GO TO 170
      IF(KMD.EQ.'SNEG') GO TO 190
C   
      IF(KMD.EQ.'HCOP') GO TO 200
      IF(KMD.EQ.'HADD') GO TO 200
      IF(KMD.EQ.'HDIV') GO TO 200
      IF(KMD.EQ.'SHIF') GO TO 200
      IF(KMD.EQ.'SHAD') GO TO 200
C
      IF(KMD.EQ.'HSET') GO TO 300
      IF(KMD.EQ.'HRAN') GO TO 310
      IF(KMD.EQ.'HZOT') GO TO 320
C
      IF(KMD.EQ.'HSTA') GO TO 360
C   
      RETURN
C   
C     ------------------------------------------------------------------
C     PROCESS GAIN-SHIFT SPECIFICATIONS
C     ------------------------------------------------------------------
C   
  140 IGSX='NO  '
      GO TO 500
  145 IGSY='NO  '
      GO TO 500
C   
  150 IGSX='NO  '
      IF(NTER.NE.0) GO TO 400
      IF(NF.NE.5)   GO TO 400
C   
      DO 160 I=1,4
      CALL MILV(LWD(1,I+1),IV,XGS(I),KIND,IERR)
      IF(IERR.NE.0) GO TO 400
  160 CONTINUE
      IGSX='YES '
      GO TO 500
C   
  170 IGSY='NO  '
      IF(NTER.NE.0) GO TO 400
      IF(NF.NE.5)   GO TO 400
C   
      DO 180 I=1,4
      CALL MILV(LWD(1,I+1),IV,YGS(I),KIND,IERR)
      IF(IERR.NE.0) GO TO 400
  180 CONTINUE
      IGSY='YES '
      GO TO 500
C
  190 IF(KMX.EQ.'OFF ') THEN
                        CNEGSET='OFF '
                        GO TO 500
                        ENDIF
      CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 400
      NEGSET=IV
      GO TO 500
C   
C     ------------------------------------------------------------------
C     PROCESS COPY, ADD, SHIF, SHAD, ETC
C     ------------------------------------------------------------------
C
  200 CALL LOCCOD(LWD(1,2),NF)
      CALL LUGET(KODFIL,LUHI,LUDI,KINF,IERR)
      IF(KINF.NE.'HIS ')     GO TO 410
      IF(KFIL(10).NE.'HIS ') GO TO 420
C
      DO 210 I=1,20
      NAMFI(I)=NAMFIL(I,LUDI-1)
  210 CONTINUE
C
      IF(KMD.EQ.'HCOP') GO TO 220
      IF(KMD.EQ.'HADD') GO TO 220
      IF(KMD.EQ.'HDIV') GO TO 220
      IF(KMD.EQ.'SHIF') GO TO 240
      IF(KMD.EQ.'SHAD') GO TO 240
C
  220 CALL COPAD                            !COPY OR ADD
      GO TO 500
C
  240 CALL IVALU(LWD(1,2),IDI,IERR)         !SHIF OR SHAD
      IF(IERR.NE.0) GO TO 400
      IF(IDI.LE.0)  GO TO 400
      CALL IVALU(LWD(1,3),IDO,IERR)
      IF(IERR.NE.0) GO TO 400
      IF(IDO.LE.0)  GO TO 400
C   
      IF(IDI.NE.IDO) GO TO 260
C   
      DO 245 I=1,20
      IF(NAMFI(I).NE.NAMFO(I)) GO TO 260
  245 CONTINUE
C   
      WRITE(CMSSG,250)
  250 FORMAT('ILLEGAL TO SHIFT IN-PLACE')
      CALL MESSLOG(6,7)
      GO TO 500
C   
  260 IF(CIWD.EQ.'SHAD') GO TO 270
      CALL SETUM(IDO,IERR)
      IF(IERR.NE.0) GO TO 500
C   
  270 CALL SHIFT
      GO TO 500
C
  300 IF(KFIL(10).NE.'HIS ') GO TO 420
C   
      CALL SETUM(0,IERR)                    !SET  IDO to IV
      GO TO 500
C
  310 IF(KFIL(10).NE.'HIS ') GO TO 420
C
      CALL SETUMRAN(0,IERR)                 !SET  IDO to randomized IV
      GO TO 500
C
  320 IF(KFIL(10).NE.'HIS ') GO TO 420
C   
      CALL IVALU(LWD(1,2),IDO,IERR)         !ZERO IDO
      IF(IERR.NE.0) GO TO 400
      CALL SETUM(IDO,IERR)
      GO TO 500
C   
C     ------------------------------------------------------------------
C     DISPLAY STATUS (GSX, GSY, & OPEN FILES)
C     ------------------------------------------------------------------
C   
  360 WRITE(6,362)XGS
  362 FORMAT(1H ,'GSX  =',4F10.2)
      WRITE(6,364)YGS
  364 FORMAT(1H ,'GSY  =',4F10.2)
C
      IF(LUHI.EQ.0.AND.LUHO.EQ.0) THEN
      WRITE(6,368)
  368 FORMAT(1H ,'NO FILES OPEN')
      GO TO 500
                                  ENDIF  
C
      IF(LUHI.EQ.LUHO)            THEN
      WRITE(6,370)(NAMFI(I),I=1,18)
  370 FORMAT(1H ,'INOU = ',18A4)
      GO TO 500
                                  ENDIF
C
      IF(LUHI.NE.0)               THEN
      WRITE(6,372)(NAMFI(I),I=1,18)
  372 FORMAT(1H ,'IN   = ',18A4)
                                  ENDIF
C
      IF(LUHO.NE.0)               THEN
      WRITE(6,374)(NAMFO(I),I=1,18)
  374 FORMAT(1H ,'OU   = ',18A4)
                                  ENDIF
C
      GO TO 500
C
C     ------------------------------------------------------------------
C     SEND ERROR MESSAGES AND/OR RETURN
C     ------------------------------------------------------------------
C   
  400 WRITE(CMSSG,405)
  405 FORMAT('SYNTAX ERROR OR ILLEGAL VALUE - CMD IGNORED')
      GO TO 490
C
  410 WRITE(CMSSG,415)
  415 FORMAT('INPUT HIS-FILE NOT OPEN - CMD IGNORED')
      GO TO 490
C
  420 WRITE(CMSSG,425)
  425 FORMAT('OUTPUT HIS-FILE NOT OPEN - CMD IGNORED')
      GO TO 490
C
  490 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
C
  500 IDONE='YES '
      RETURN
      END
