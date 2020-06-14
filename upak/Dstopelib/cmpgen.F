C$PROG CMPGEN    - Command processor (general) for program FITU
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
      COMMON/ST04/ XMIN,XMAX,YMIN,YMAX
      REAL*4       XMIN,XMAX,YMIN,YMAX
C
      DATA         XMIN,XMAX,YMIN,YMAX/4*0.0/
C     ------------------------------------------------------------------
      COMMON/ST05/ IDW,KDSP
      INTEGER*4    IDW
      CHARACTER*4      KDSP
      DATA         IDW,KDSP/1,'LIN '/
C     ------------------------------------------------------------------
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
#if   (PTRSIZE == 64)
      INTEGER*4    DPY,WDID                                 !Alpha STAR8
#else
      INTEGER*4    DPY,WDID                                 !Alpha STAR8
#endif
      INTEGER*4                 XN,    YN,              WN
      CHARACTER*4                             NUWIN
C     ------------------------------------------------------------------
      COMMON/XLCC/ WINDAT(10,20),WINFLG(6,20),NUMWIN,ISOPEN
      REAL*4       WINDAT
      INTEGER*4                  WINFLG,      NUMWIN
      CHARACTER*4                WINFLC(6,20),       ISOPEN
      EQUIVALENCE (WINFLC,WINFLG)
C     ------------------------------------------------------------------
      COMMON/XLHH/ ITITL(20),IBANNER
      INTEGER*4    ITITL
      CHARACTER*4            IBANNER
C     ------------------------------------------------------------------
C
      CHARACTER*4  IDONE,KMD
C
      REAL*4       XXMIN,XXMAX,YYMIN,YYMAX
C
      INTEGER*4    LU,IV,KIND,ISTAT,LDA,IERR
C
      REAL*4       XV
C
      DATA         LU/1/
C
      DATA         LDA/1/
C
      EQUIVALENCE (KMD,LWD(1,1))
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IDONE='NO  '
C
      IERR=0
C
      IF(KMD.EQ.'DSYM') GO TO 130
C
      IF(KMD.EQ.'FIG ') GO TO 140
      IF(KMD.EQ.'CLR ') GO TO 150
      IF(KMD.EQ.'WIN ') GO TO 160
C
      IF(KMD.EQ.'REVV') GO TO 190
C
      IF(KMD.EQ.'XMM ') GO TO 200
      IF(KMD.EQ.'YMM ') GO TO 210
C
      IF(KMD.EQ.'LIN ') GO TO 220
      IF(KMD.EQ.'LOG ') GO TO 220
C
      RETURN
C
  130 CALL SYMLOG('LSYM')
      GO TO 2000
C
  140 CALL NEWFIG(LU,IWD,IERR)
      KMD='FIGG'
      CALL XX_WINMAN(KMD,0)
      CALL LABLMAN('INIT',0,0,0,0)
      GO TO 2000
C
  150 IV=1
      CALL XX_WINMAN(KMD,IV)
      CALL LABLRU(IDONE,IERR)
      GO TO 2000
C
  160 CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0)    GO TO 1000
      IF(IV.LT.1) THEN
      IDW=1
      GO TO 2000
      ENDIF
      IF(IV.GT.NUMWIN) GO TO 1010
      IDW=IV
      GO TO 2000
C
  190 CALL REVV
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
  220 KDSP=KMD
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
C
 1900 CALL MESSLOG(LOGUT,LOGUP)
C
C
 2000 IDONE='YES '
C
      RETURN
C
      END
