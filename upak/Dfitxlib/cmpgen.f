C$PROG CMPGEN    - Command processor (general) for program FITX
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
C
      SUBROUTINE CMPGEN(IDONE,IERR)
C
      IMPLICIT NONE 
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
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/FT01/ IHEPF
      INTEGER*4    IHEPF
C     ------------------------------------------------------------------
      COMMON/FT02/ LIN,LCM,LCI
      INTEGER*4    LIN,LCM,LCI
C     ------------------------------------------------------------------
      COMMON/FT08/ NAMF(20)
      INTEGER*4    NAMF
C     ------------------------------------------------------------------
      COMMON/FT10/ KINDUU,KINDU,USUN,ALUN,UMUL
      INTEGER*4    KINDUU,KINDU
      REAL*8                    USUN,ALUN,UMUL
C
      DATA         KINDUU,KINDU  /2,3/
      DATA         USUN,ALUN,UMUL/5.0,5.0,1.0/
C     ------------------------------------------------------------------
      COMMON/FT13/ XMIN,XMAX,YMIN,YMAX
      REAL*4       XMIN,XMAX,YMIN,YMAX
C     ------------------------------------------------------------------
      COMMON/FT14/ IDW,KDSP
      INTEGER*4    IDW
      CHARACTER*4      KDSP
      DATA         IDW,KDSP/1,'LIN '/
C     ------------------------------------------------------------------
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
      INTEGER*8    DPY,WDID
      INTEGER*4                 XN,    YN,    NUWIN,    WN
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN,ISOPEN
C     ------------------------------------------------------------------
      COMMON/XLHH/ ITITL(20),IBANNER
      INTEGER*4    ITITL,    IBANNER
C     ------------------------------------------------------------------
      REAL*4       XXMIN,XXMAX,YYMIN,YYMAX
C
      INTEGER*4    IHELP(20,200),NAMCMD(20)
C
      INTEGER*4    LU,IV,KIND,ISTAT,LDA,IERR
C
      REAL*4       XV
C
      DATA         LU/1/
C
      DATA         LDA/1/
C
      CHARACTER*4  KMD,IDONE
C
      EQUIVALENCE (KMD,LWD(1,1))
C
      SAVE
C
C     ------------------------------------------------------------------
C     KINDUU= 1 SAYS ALL UNCERT = ALUN (%)
C     KINDUU= 2 SAYS UNSPECIFIED UNCERT = USUN (%)
C     KINDU = 1 SAYS COMPUTE UNCERT AS COUNTING STATISTICS
C     KINDU = 2 SAYS UNCERT GIVEN BY "UIN" IS ABSOLUTE
C     KINDU = 3 SAYS UNCERT GIVEN BY "UIN" IS IN %
C     ------------------------------------------------------------------
C
      IDONE='NO  '
C
      IERR=0
C
      IF(KMD.EQ.'H   ') GO TO 100
C
      IF(KMD.EQ.'CMD ') GO TO 110
      IF(KMD.EQ.'IN  ') GO TO 120
      IF(KMD.EQ.'DSYM') GO TO 130
C
      IF(KMD.EQ.'STAT') GO TO 170
C
      IF(KMD.EQ.'UCOM') GO TO 180
C
      IF(KMD.EQ.'XMM ') GO TO 200
      IF(KMD.EQ.'YMM ') GO TO 210
C
      IF(KMD.EQ.'CSUN') GO TO 250       !USE COUNTING STAT UNCERT
      IF(KMD.EQ.'ABUN') GO TO 260       !GIVEN UNCERT ARE ABSOLUTE
      IF(KMD.EQ.'PCUN') GO TO 270       !GIVEN UNCERT ARE IN %
      IF(KMD.EQ.'ALUN') GO TO 280       !VALUE FOR ALL UNCERT IN %
      IF(KMD.EQ.'USUN') GO TO 290       !UNSPECIFIED UNCERT IN %
      IF(KMD.EQ.'MULU') GO TO 300       !MULTIPLIER FOR ALL UNCERT
C
      RETURN
C
  100 CALL HELPMANU(IWD,13,IHELP,200,20,IHEPF)
      GO TO 2000
C
C     ------------------------------------------------------------------
C     OPEN NEW CMD-FILE & NEW DATA FILE
C     ------------------------------------------------------------------
C
  110 CALL CMDOPEN(IWDRAW,NAMCMD,LCI,LIN,LCM,IERR)
      GO TO 2000
C
  120 CALL NUDAF(IWDRAW,LDA,LIN,IERR)
      IF(IERR.NE.0) GO TO 2000
      CALL GETDATX(-1,IERR)
      GO TO 2000
C
  130 CALL SYMLOG('LSYM')
      GO TO 2000
C
C
  170 CALL STATMAN
      GO TO 2000
C
  180 CALL USEND(IWD,IERR)
      IF(IERR.NE.0) GO TO 1070
      GO TO 2000
C
  200 IF(NF.LT.3) THEN
      XMIN=0.0
      XMAX=0.0
      GO TO 2000
      ENDIF
C
      CALL MILV(LWD(1,2),IV,XXMIN,KIND,IERR)
      IF(IERR.NE.0)      GO TO 1050
      CALL MILV(LWD(1,3),IV,XXMAX,KIND,IERR)
      IF(IERR.NE.0)      GO TO 1050
      IF(XXMIN.GE.XXMAX) GO TO 1050
      XMIN=XXMIN
      XMAX=XXMAX
      GO TO 2000
C
  210 IF(NF.LT.3) THEN
      YMIN=0.0
      YMAX=0.0
      GO TO 2000
      ENDIF
C
      CALL MILV(LWD(1,2),IV,YYMIN,KIND,IERR)
      IF(IERR.NE.0)      GO TO 1060
      CALL MILV(LWD(1,3),IV,YYMAX,KIND,IERR)
      IF(IERR.NE.0)      GO TO 1060
      IF(YYMIN.GE.YYMAX) GO TO 1060
      YMIN=YYMIN
      YMAX=YYMAX
      GO TO 2000
C
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C
  250 KINDU=1
      KINDUU=2
      GO TO 2000
  260 KINDU=2
      GO TO 2000
  270 KINDU=3
      GO TO 2000
C
C     ------------------------------------------------------------------
C     PICK UP "ALUN" 
C     ------------------------------------------------------------------
C
  280 CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1000
      ALUN=XV
      KINDUU=1
      GO TO 2000
C
C     ------------------------------------------------------------------
C     PICK UP "USUN" 
C     ------------------------------------------------------------------
C
  290 CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1000
      USUN=XV
      KINDUU=2
      GO TO 2000
C
C     ------------------------------------------------------------------
C     PICK UP "UMUL" - UNCERT MULTIPLIER 
C     ------------------------------------------------------------------
C
  300 CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1000
      UMUL=XV
      GO TO 2000
C
C     ------------------------------------------------------------------
C     Report error messages
C     ------------------------------------------------------------------
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('Syntax error or illegal value - command ignored')
      GO TO 1900
C
 1010 WRITE(CMSSG,1015)
 1015 FORMAT('Window# requested exceeds number defined - cmd ignored')
      GO TO 1900
C
C
 1050 WRITE(CMSSG,1055)
 1055 FORMAT('Illegal display X-range - command ignored')
      GO TO 1900
C
 1060 WRITE(CMSSG,1065)
 1065 FORMAT('Illegal display Y-range - command ignored')
      GO TO 1900
C
 1070 WRITE(CMSSG,1075)
 1075 FORMAT('Illegal UCOM command - ignored')
      GO TO 1900
C
 1900 CALL MESSLOG(LOGUT,LOGUP)
C
C
 2000 IDONE='YES '
C
      RETURN
C
      END
