C$PROG MAKFNAM   - Makes full-path filename from abreviation
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 08/20/2005 - for gnu
C     ******************************************************************
C
C
      SUBROUTINE MAKFNAM(IWD,FINAM,IEXT,IERR)
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
      INTEGER*4   IWD(20),NAMFD(40),FINAM(40),NEX,STAT
C
      INTEGER*4   IDOT,BLANK
      character*4 cblank
      equivalence (cblank, blank)

      DATA        IDOT,cBLANK/Z'2E','    '/
C
      INTEGER*4     CURWD(40)
C
      BYTE          NAMBYT(160)
C
      CHARACTER*160 CNAMFD,CURWDC
C
      EQUIVALENCE  (CNAMFD,NAMFD),(CURWDC,CURWD)
C
      EQUIVALENCE  (NAMBYT,CNAMFD)
C
      INTEGER*4     STRAPPEND,STRLEN,BYTERR
C
      INTEGER*4     IERR,LLO,LHI,IA,IB,LDOT,ILOST,IEXT,I
C
      INTEGER*4     NXBL,NXNB,LSNB,LEXT
C
C     ------------------------------------------------------------------
      SAVE
C
C     ------------------------------------------------------------------
C     ON ENTRY  - IWD   contains - cmd /partialpath/filename.ext
C
C     ON RETURN - FINAM contains - /fullpath/filename.ext
C                 IEXT  contains - .ext
C     ------------------------------------------------------------------
C
      CALL GETCWD(CURWDC,STAT)          !Get current working directory
      IF(STAT.NE.0) GO TO 150           !Test for error
C
      CNAMFD=' '                        !Blank filename
      IEXT=BLANK                        !BLANK FIL/DEV EXT WORD
      LDOT=0                            !LOC OF DOT (1ST EXT BYTE)
      IERR=0                            !ERROR FLAG
C
      LLO=NXBL(IWD,1,80)                !LOC OF 1ST BLANK AFTER CMD
      IF(LLO.LE.0) GO TO 200            !ERROR IF NOT FOUND
      LLO=NXNB(IWD,LLO,80)              !LOC OF 1ST FIL/DEV BYTE
      IF(LLO.LE.0) GO TO 200            !ERROR IF NOT FOUND
      LHI=LSNB(IWD,LLO,80)              !LOC OF LAST NON-BLANK
      IF(LHI.LE.0) GO TO 200            !ERROR IF NOT FOUND
C
      LDOT=LEXT(IWD,LLO,LHI)            !LOC OF DOT (1ST EXT BYTE)
C
   30 IB=LHI                            !LAST BYTE IN IWD FOR FILNAM
      IA=LLO
      CALL LODUP(IWD,IA,IB,NAMFD,1)     !LOAD FILNAM
      IF(LDOT.GT.0) GO TO 40            !TST FOR EXTENSION SPECIFIED
      GO TO 200
C
   40 CALL LODUP(IWD,LDOT,LDOT+3,IEXT,1)!Save extension in IEXT
C
      IF(CNAMFD(1:1).EQ.'/') GO TO 100
C
      IF(BYTERR(NAMBYT(1)).NE.0) GO TO 220
C
      ILOST=STRAPPEND(CURWDC,'/')
      IF(ILOST.NE.0) GO TO 210
C
      ILOST=STRAPPEND(CURWDC,CNAMFD(1:STRLEN(CNAMFD)))
      IF(ILOST.NE.0) GO TO 210
C
      DO 50 I=1,20
      FINAM(I)=CURWD(I)
   50 CONTINUE
      RETURN
C
  100 DO 110 I=1,20
      FINAM(I)=NAMFD(I)
  110 CONTINUE
      RETURN
C
C     ------------------------------------------------------------------
C     Error return
C     ------------------------------------------------------------------
C
  150 WRITE(CMSSG,155)
  155 FORMAT('Unable to get current working directory')
      GO TO 1000
C
  200 WRITE(CMSSG,205)
  205 FORMAT('Syntax error or illegal file specification')
      GO TO 1000
C
  210 WRITE(CMSSG,215)
  215 FORMAT('Specified filename more than 160 characters - aborted')
      GO TO 1000
C
  220 WRITE(CMSSG,225)
  225 FORMAT('Unsupported or illegal filename syntax - aborted')
      GO TO 1000
C
 1000 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
