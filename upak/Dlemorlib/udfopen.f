C$PROG UDFOPEN   - Opens Input UDF-Files for LEMOR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 03/31/2005
C     ******************************************************************
C
      SUBROUTINE UDFOPEN
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
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
      INTEGER*4    LUINF,LUOUF
      CHARACTER*4              INFOP,OUFOP
C     ------------------------------------------------------------------
      COMMON/LM23/ INDIR(8192),OUDIR(8192),INTYP,OUTYP,INRECI,OURECI
      INTEGER*4    INDIR,      OUDIR,                  INRECI,OURECI
      CHARACTER*4                          INTYP,OUTYP
C     ------------------------------------------------------------------
      COMMON/LM33/ UDFNAM(20),UDFRECL,UDFNPAR,UDFRECI
      INTEGER*4    UDFNAM,    UDFRECL,UDFNPAR,UDFRECI
C     ------------------------------------------------------------------
      COMMON/LM34/ NCEOF,LAUTO
      INTEGER*4    NCEOF
      CHARACTER*4        LAUTO
C     ------------------------------------------------------------------
      CHARACTER*80 CNAMF
      EQUIVALENCE (CNAMF,UDFNAM)
      INTEGER*4    RECLVALU,IOS,IERR
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C     Process: UDF filename <RECL>     ;i.e. get UDFNAM & UDFRECL
C     ------------------------------------------------------------------
C
      CALL UDFNAME(IWDRAW,UDFNAM,UDFRECL,IERR)
C
      IF(IERR.NE.0) GO TO 1000
C
C     ------------------------------------------------------------------
C     Open User-Defined-File for input
C     ------------------------------------------------------------------
C
      CLOSE(UNIT=LUINF)               !Close input file if open
      INTYP='    '                    !Reset input type
      NCEOF=0                         !Reset contiguous EOF counter
C
      IF(UDFRECL.EQ.0) THEN           !Test for RECL specified
      OPEN(UNIT      = LUINF,         !Otherwise,
     &     FILE      = CNAMF,         !Open UDF for sequential access
     &     STATUS    = 'OLD',
     &     IOSTAT    = IOS)
      GO TO 100                       !Go test status
      ENDIF
C
      OPEN(UNIT      = LUINF,         !Otherwise,
     &     FILE      = CNAMF,         !Open UDF for DIRECT access
     &     ACCESS    = 'DIRECT',
     &     RECL      = RECLVALU(UDFRECL),!with RECORD LENGTH = UDFRECL
     &     STATUS    = 'OLD',
     &     IOSTAT    = IOS)
C
  100 IF(IOS.NE.0) THEN               !Test OPEN status for good
      CALL IOFERR(IOS)
      GO TO 1000
      ENDIF
C
      INTYP='UDF '                    !Set input type to UDF
C
      UDFRECI=1                       !Next rec# to be read
C
      UDFNPAR=0
C
      RETURN
C
C     ------------------------------------------------------------------
C     Send error messages
C     ------------------------------------------------------------------
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('UDFOPEN ERROR')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
