C$PROG CMPTAPE   - Executes tape & file control cmds - for SCANOR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 02/03/99
C     ******************************************************************
C
      SUBROUTINE CMPTAPE(IDONE,RETN)
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
      CHARACTER*4  KOM,IDONE
C
      EQUIVALENCE (KOM,LWD(1,1))
C     ------------------------------------------------------------------
      INTEGER*4    IDUM,DUMI,DUMJ,DUMK,KERR
C
      INTEGER*4    RETN,IV,IERR
C
      INTEGER*4    LUT
      EQUIVALENCE (LUT,LUC(1))
C
      SAVE
C
C     ------------------------------------------------------------------
C
      RETN=0
      IDONE='NO  '
C
      IF(KOM.EQ.'FILE') GO TO 165
C
      IF(NTER.NE.0) GO TO 1010
C
      IF(KOM.EQ.'REW ') GO TO 140
      IF(KOM.EQ.'BR  ') GO TO 140
      IF(KOM.EQ.'FR  ') GO TO 140
      IF(KOM.EQ.'FF  ') GO TO 140
      IF(KOM.EQ.'BF  ') GO TO 140
      IF(KOM.EQ.'FIND') GO TO 150
C
      IF(KOM.EQ.'STX ') GO TO 160
C
      IF(KOM.EQ.'TAPE') GO TO 170
      IF(KOM.EQ.'CLOT') GO TO 170
      IF(KOM.EQ.'CLUN') GO TO 170
C
      IF(KOM.EQ.'SHM ') GO TO 200
      IF(KOM.EQ.'IPC ') GO TO 200
      IF(KOM.EQ.'ACQ ') GO TO 200
      IF(KOM.EQ.'CLOI') GO TO 200
C
      RETURN
C
C   
C     ------------------------------------------------------------------
C     DO TAPE/FILE CONTROL OPERATIONS
C     ------------------------------------------------------------------
C
  140 IF(INTYP.EQ.'LDF ') THEN          !Do it for a disk file
      CALL LDFHAN(IERR)
      ICNF='NO  '
      IF(IERR.EQ.0) GO TO 1000
      GO TO 550
      ENDIF
C
      CALL IVALU(LWD(1,2),IV,IERR)      !DO TAPE CONTROL FUNCTIONS
      IF(IERR.NE.0) GO TO 1010
      CALL TAPHAN(KOM,IV,IERR)
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
C     CLOSE AND OPEN MAG TAPE UNITS and LDF files
C     ------------------------------------------------------------------
C   
  165 CALL LDFOPEN(IERR)
      ICNF='NO  '
      IF(IERR.EQ.0) GO TO 1000
      GO TO 550
C
  170 CALL TAPOPEN(IERR)
      ICNF='NO  '
      INTYP='TAPE'
      IF(IERR.EQ.0) GO TO 1000
      GO TO 550
C
C     ------------------------------------------------------------------
C     Open & close SHM for input
C     ------------------------------------------------------------------
C
  200 CALL IPCOPEN(KOM,IERR)
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
      END
