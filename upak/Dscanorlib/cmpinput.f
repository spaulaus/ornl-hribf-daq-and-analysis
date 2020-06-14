C$PROG CMPINPUT  - Does "input control" (open, close, read, etc)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 08/05/99
C     ******************************************************************
C
      SUBROUTINE CMPINPUT(IDONE,RETN)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/SC03/ LUC(10)
      INTEGER*4    LUC
C     ------------------------------------------------------------------
      COMMON/SC14/ NBRED,NBTOP,ICNF
      INTEGER*4    NBRED,NBTOP
      CHARACTER*4              ICNF
C     ------------------------------------------------------------------
      COMMON/SC16/ INDIR(8192),INTYP,INRECI,LUINF
      INTEGER*4    INDIR,            INRECI,LUINF
      CHARACTER*4              INTYP
C     ------------------------------------------------------------------
      CHARACTER*4  KMD,IDONE
C
      EQUIVALENCE (KMD,LWD(1,1))
C     ------------------------------------------------------------------
      INTEGER*4    IDUM,DUMI,DUMJ,DUMK,KERR
C
      INTEGER*4    RETN,IV,IERR
C
      INTEGER*4    LUT
      EQUIVALENCE (LUT,LUC(1))
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      RETN=0
      IDONE='NO  '
C
      IF(KMD.EQ.'CLOS') GO TO 100
      IF(KMD.EQ.'CLOT') GO TO 100
      IF(KMD.EQ.'CLOF') GO TO 100
      IF(KMD.EQ.'CLOI') GO TO 100
      IF(KMD.EQ.'CLUN') GO TO 110
C
      IF(KMD.EQ.'FILE') GO TO 165
      IF(KMD.EQ.'LDF ') GO TO 165
      IF(KMD.EQ.'UDF ') GO TO 170
C
      IF(NTER.NE.0) GO TO 1010
C
      IF(KMD.EQ.'REW ') GO TO 140
      IF(KMD.EQ.'BR  ') GO TO 140
      IF(KMD.EQ.'FR  ') GO TO 140
      IF(KMD.EQ.'FF  ') GO TO 140
      IF(KMD.EQ.'BF  ') GO TO 140
      IF(KMD.EQ.'FIND') GO TO 150
C
      IF(KMD.EQ.'STX ') GO TO 160
C
      IF(KMD.EQ.'TAPE') GO TO 190
C
      IF(KMD.EQ.'SHM ') GO TO 200
      IF(KMD.EQ.'IPC ') GO TO 200
      IF(KMD.EQ.'ACQ ') GO TO 200
C
      RETURN
C
C     ------------------------------------------------------------------
C     Close any input file/device which may be open
C     ------------------------------------------------------------------
C
  100 CALL CLOSEALL
      GO TO 1000
C
C     ------------------------------------------------------------------
C     Close & unload tapes
C     ------------------------------------------------------------------
C
  110 IF(INTYP.NE.'TAPE') GO TO 1020
      CALL TAPOPEN(IERR)
      ICNF='NO  '
      INTYP='    '
      IF(IERR.EQ.0) GO TO 1000
      GO TO 550
C
C   
C     ------------------------------------------------------------------
C     DO TAPE/FILE CONTROL OPERATIONS
C     ------------------------------------------------------------------
C
  140 IF(INTYP.EQ.'LDF ') THEN          !Do it for a LDF-file
      CALL LDFHAN(IERR)
      ICNF='NO  '
      IF(IERR.EQ.0) GO TO 1000
      GO TO 550
      ENDIF
C
      IF(INTYP.EQ.'UDF ') THEN          !Do it for a UDF-file
      CALL UDFHAN(IERR)
      ICNF='NO  '
      IF(IERR.EQ.0) GO TO 1000
      GO TO 550
      ENDIF
C
      CALL IVALU(LWD(1,2),IV,IERR)      !DO TAPE CONTROL FUNCTIONS
      IF(IERR.NE.0) GO TO 1010
      CALL TAPHAN(KMD,IV,IERR)
      ICNF='NO  '
      IF(IERR.EQ.0) GO TO 1000
      GO TO 550
C
  150 IF(INTYP.EQ.'LDF ') THEN          !Find header on disk file
      CALL LDFHAN(IERR)
      ICNF='NO  '
      IF(IERR.EQ.0) GO TO 1000
      GO TO 550
      ENDIF
C
      CALL IVALU(LWD(1,2),IV,IERR)      !FIND HEADER # ON TAPE
      IF(IERR.NE.0) GO TO 1010
      IF(IV.LE.0)   GO TO 1010
      CALL HEDLOC(IV,IERR)
      ICNF='NO  '
      IF(IERR.EQ.0) GO TO 1000
      GO TO 550
C
  160 CALL EXABSTAT(1,LUT,DUMI,DUMJ,DUMK,IDUM,KERR)
      GO TO 550
C   
C     ------------------------------------------------------------------
C     Open TAPE units, LDF & UDF files and SHM segments
C     ------------------------------------------------------------------
C   
  165 CALL CLOSEALL
      CALL LDFOPEN(IERR)
      ICNF='NO  '
      IF(IERR.EQ.0) GO TO 1000
      GO TO 550
C
  170 CALL CLOSEALL
      CALL UDFOPEN(IERR)
      ICNF='NO  '
      IF(IERR.EQ.0) GO TO 1000
      GO TO 550
C
  190 CALL TAPOPEN(IERR)
      ICNF='NO  '
      IF(IERR.EQ.0) THEN
      INTYP='TAPE'
      GO TO 1000
      ENDIF
      GO TO 550
C
  200 CALL CLOSEALL
      CALL IPCOPEN(KMD,IERR)
      ICNF='NO  '
      IF(IERR.EQ.0) GO TO 1000
      GO TO 550
C
C     ------------------------------------------------------------------
C     Normal & error returns
C     ------------------------------------------------------------------
C
  550 RETN=50
      GO TO 1000
C
 1000 IDONE='YES '
      RETURN
C   
C     ------------------------------------------------------------------
C     LIST ERROR MESSAGES
C     ------------------------------------------------------------------
C
C
 1010 WRITE(CMSSG,1015)
      CALL MESSLOG(LOGUT,LOGUP)
 1015 FORMAT('SYNTAX ERROR OR ILLEGAL COMMAND - IGNORED')
      IDONE='YES '
      RETN=50
      RETURN
C
 1020 WRITE(CMSSG,1025)
 1025 FORMAT('No input tape is currently assigned - cmd ignored')
      CALL MESSLOG(LOGUT,LOGUP)
      IDONE='YES '
      RETN=50
      RETURN
      END
