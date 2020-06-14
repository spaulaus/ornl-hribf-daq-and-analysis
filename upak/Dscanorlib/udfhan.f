C$PROG UDFHAN    - Manages UDF-file position pointers for SCANOR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 11/22/2004
C     ******************************************************************
C
      SUBROUTINE UDFHAN(IERR)
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
      COMMON/SC15/ NCEOF,LAUTO
      INTEGER*4    NCEOF
      CHARACTER*4        LAUTO
C     ------------------------------------------------------------------
      COMMON/SC16/ INDIR(8192),INTYP,INRECI,LUINF
      INTEGER*4    INDIR,            INRECI,LUINF
      CHARACTER*4              INTYP
C     ------------------------------------------------------------------
      COMMON/SC29/ UDFNAM(20),UDFRECL,UDFNPAR
      INTEGER*4    UDFNAM,    UDFRECL,UDFNPAR
C     ------------------------------------------------------------------
      INTEGER*4    IERR,KIND,NUM,NV,N
C
      REAL*4       XV
C
      CHARACTER*4  KMD
      EQUIVALENCE (KMD,LWD(1,1))
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C     LUINF    =  Logical unit# for UDF-file
C
C     INTYP    = '    ', TAPE, FILE = input type
C
C     INRECI   =  Current rec# pointer for input file if DIRECT access
C                 Otherwise just keeps track of position but not used
C     ------------------------------------------------------------------
C
      IERR=0
C
      CALL MILV(LWD(1,2),NV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 1000
      NUM=NV
      IF(NUM.LE.0) NUM=1
C
      IF(INTYP.NE.'UDF ') GO TO 1000
C
      IF(KMD.EQ.'REW ') GO TO 100
      IF(KMD.EQ.'BR  ') GO TO 150
      IF(KMD.EQ.'FR  ') GO TO 200
C
      GO TO 1000
C
C     ------------------------------------------------------------------
C     Process REW  -  Rewind input file
C     ------------------------------------------------------------------
C
  100 REWIND LUINF               !For SEQUENTIAL access only
      NCEOF=0                    !Reset contiguous EOF counter
      INRECI=1                   !Next REC# to be read for DIRECT access
      UDFNPAR=0                  !Reset event accumulator cntr
      RETURN
C
C     ------------------------------------------------------------------
C     Process BR   -  Backup record/s on input file
C     ------------------------------------------------------------------
C
  150 DO 160 N=1,NUM
      BACKSPACE LUINF            !For SEQUENTIAL access only
      INRECI=INRECI-1
      IF(INRECI.LT.1) THEN
      INRECI=1
      UDFNPAR=0
      RETURN
      ENDIF
  160 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     Process FR   -  Forward record/s on input file
C     ------------------------------------------------------------------
C
  200 DO 210 N=1,NUM
      READ(LUINF,205)KIND         !For SEQUENTIAL access only
  205 FORMAT(A4)
      INRECI=INRECI+1
      UDFNPAR=0
  210 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     Send error messages
C     ------------------------------------------------------------------
C
 1000 IERR=1
      RETURN
      END
