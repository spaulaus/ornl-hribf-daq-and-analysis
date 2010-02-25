C$PROG CMPSCAD   - Command processor for program SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/22/2005
C     ******************************************************************
C
C     ------------------------------------------------------------------
C     SCALER DISPLAY PROGRAM - FOR CAMAC SCALERS
C     CAN USE TVI912C OR ANSI TERMINALS (DEPENDING ON START STRING)
C     ------------------------------------------------------------------
C
      SUBROUTINE CMPSCAD(IDONE,IERR)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/HEP/  IHEPF
      INTEGER*4    IHEPF
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/SD03/ LALOC(2,512),DALOC(2,512),DATLOC(2),HOMLOC(2)
      INTEGER*4    LALOC,       DALOC,       DATLOC,   HOMLOC
C     ------------------------------------------------------------------
      COMMON/SD06/ NERR
      INTEGER*4    NERR
C     ------------------------------------------------------------------
      COMMON/SD07/ KMD,SEC,LSEC
      CHARACTER*4  KMD
      INTEGER*4        SEC,LSEC
C     ------------------------------------------------------------------
      COMMON/SD08/ KTERM
      INTEGER*4    KTERM
C     ------------------------------------------------------------------
      COMMON/SD10/ MODEGO
      CHARACTER*4  MODEGO
C     ------------------------------------------------------------------
      COMMON/SD11/ MAXR, MAXC, MAXD, MAXT, MAXS
      INTEGER*4    MAXR, MAXC, MAXD, MAXT, MAXS
C     ------------------------------------------------------------------
      COMMON/SD13/ ISET,LULG
      CHARACTER*4  ISET
      INTEGER*4         LULG
C     ------------------------------------------------------------------
      COMMON/SD14/ MXBEEP,MXGOOD,BELL,BELLV,NBEEP
      INTEGER*4    MXBEEP,MXGOOD,BELL,BELLV,NBEEP
C     ------------------------------------------------------------------
      COMMON/SD15/ LUAL,THUSH,TNOW,ALFLG
      INTEGER*4    LUAL,THUSH,TNOW
      CHARACTER*4                  ALFLG
C     ------------------------------------------------------------------
      COMMON/SD16/ PRNAM
      CHARACTER*76 PRNAM
      DATA         PRNAM/' '/
C     ------------------------------------------------------------------
      COMMON/III/  LIN,LCM,LCI,LOU
      INTEGER*4    LIN,LCM,LCI,LOU
C     ------------------------------------------------------------------
      INTEGER*4    IHELP(20,300),LHEP
C
      CHARACTER*20 MDATIM
C
      CHARACTER*4  CLWD(2,40),IDONE
      EQUIVALENCE (CLWD,LWD)
C
      INTEGER*4    NAMCMD(20),IERR,BLANK
      character*4  CBLANK
      equivalence (CBLANK,BLANK)
c
      character*4  CKTERM
      equivalence  (CKTERM, KTERM)
C
      DATA         SEC,LSEC/5,0/
C
      DATA         CKTERM,CBLANK/'ANSI','    '/
C
      DATA         LHEP/16/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C   
C
      WRITE(6,777)KMY
  777 FORMAT(A)
C
      IERR=0
C
      KMD=CLWD(1,1)
C
      IF(KMD.EQ.'LON ') GO TO 100
      IF(KMD.EQ.'LOF ') GO TO 100
      IF(KMD.EQ.'CMD ') GO TO 110
      IF(KMD.EQ.'    ') GO TO 2000
      IF(KMD.EQ.'H   ') GO TO 125
      IF(KMD.EQ.'HELP') GO TO 125
      IF(KMD.EQ.'END ') GO TO 500
      IF(KMD.EQ.'NORS') GO TO 130
      IF(KMD.EQ.'NORT') GO TO 130
      IF(KMD.EQ.'TAB ') GO TO 140
      IF(KMD.EQ.'LOG ') GO TO 150
      IF(KMD.EQ.'SNAP') GO TO 155
      IF(KMD.EQ.'ZERO') GO TO 160
      IF(KMD.EQ.'SEC ') GO TO 170
      IF(KMD.EQ.'LSEC') GO TO 170
C
      IF(KMD.EQ.'HUSH') GO TO 180
C
      IF(KMD.EQ.'RLIM') GO TO 200
      IF(KMD.EQ.'RLMM') GO TO 205
      IF(KMD.EQ.'GLIM') GO TO 205
C
      IF(KMD.EQ.'BPON') GO TO 210
      IF(KMD.EQ.'BPOF') GO TO 210
C
      IF(KMD.EQ.'NAMP') GO TO 220
C
      RETURN
C
  100 LISFLG=KMD
      GO TO 2000
C
  110 CALL CMDOPEN(IWDRAW,NAMCMD,LCI,LIN,LCM,IERR)  !OPEN COMMAND FILE
      GO TO 2000
C
  125 CALL HELPMANU(IWD,LHEP,IHELP,300,20,IHEPF)
      GO TO 2000
C 
  130 CALL NORMAN
      GO TO 2000
C
  140 CALL TABO                         !LIST ARRAYS FOR DIAGNOSTICS
      GO TO 2000
C
  150 CALL READUM                       !READ SCALERS
      CALL LOGUM(LOGUT,LOGUP)           !LOG ON SCAD.LOG
      GO TO 2000
C
  155 CALL SNAP                         !WRITE SCALERS TO scadymdhms.snap
      GO TO 2000
C
  160 CALL ZOTUM                        !ZERO ALL SCALERS
      GO TO 2000
C
  170 CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
      IF(NTER.NE.0) GO TO 1000
      IF(KMD.EQ.'LSEC') GO TO 172
      CALL LIMIV(LWD(1,2),1,20,IV,IERR)
      IF(IERR.NE.0) GO TO 1010
      SEC=IV
      GO TO 2000
C
  172 CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1010
      IF(IV.EQ.0)   GO TO 174
      IF(IV.GE.300.AND.IV.LE.3600) GO TO 174
      GO TO 1020
  174 LSEC=IV
      GO TO 2000
C
  180 CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
      IF(NTER.NE.0) GO TO 1000
      CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1000
      CALL SECSENS70(TNOW)
      THUSH=TNOW+60*IV
      GO TO 2000
C
  200 CALL LIMSET
      GO TO 2000
C
  205 CALL GLIMSET
      GO TO 2000
C
  210 CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1000
      IF(IV.LT.0.OR.IV.GT.1000) GO TO 1060
C
      IF(KMD.EQ.'BPON') MXGOOD=IV
      IF(KMD.EQ.'BPOF') MXBEEP=IV
      GO TO 2000
C
  220 IF(IWDRAW(2).EQ.BLANK) THEN
      PRNAM=' '
      GO TO 2000
      ENDIF
      CALL FINAME(IWDRAW,5,80,PRNAM,IERR)
      IF(IERR.NE.0) GO TO 1000
      GO TO 2000
C
  500 CALL SCLR
      CALL EXIT
C
 1000 WRITE(6,1005)
 1005 FORMAT(1H ,'Syntax error or illegal value - Ignored')
      GO TO 1500
C
 1010 WRITE(6,1015)
 1015 FORMAT(1H ,'Illegal display interval - Legal range = 1,20')
      GO TO 1500
C
 1020 WRITE(6,1025)
 1025 FORMAT(1H ,'Illegal Log-interval - Legals = 0 or 300 to 3600')
      GO TO 1500
C
 1060 WRITE(6,1065)IV
 1065 FORMAT(1H ,'Illegal value =',I8,' - legal range = 0 to 1000')
      GO TO 1500
C
 1500 IERR=1
C
 2000 IDONE='YES '
      RETURN
      END
