C$PROG NUINP     - Opens a new input file (command file)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 02/17/99
C     ******************************************************************
C
      SUBROUTINE NUINP(LCMD,IERR)
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
      COMMON/SC01/ NAMCMD(20)
      INTEGER*4    NAMCMD
C     ------------------------------------------------------------------
      INTEGER*4    LCMD,IERR
C
      INTEGER*4    NAMFIL(20),DISK(2)
C
      CHARACTER*80 CNAMFIL
C
      EQUIVALENCE (CNAMFIL,NAMFIL)
C
      INTEGER*4    IEXT,JEXT,LDOT,INEW,STAT,I
C
      character*4 cjext, cdisk(2)
      equivalence (cjext,jext), (cdisk,disk)
      DATA cJEXT,cDISK/'.cmd',2*'    '/
C     ------------------------------------------------------------------
      SAVE
C
C     ------------------------------------------------------------------
C     PROCESS - CMD FILNAME  !OPEN NEW COMMAND INPUT FILE
C     ------------------------------------------------------------------
C
      IERR=0
      CLOSE(UNIT=LCMD,IOSTAT=STAT)
C
      CALL BILNAM(IWDRAW,DISK,JEXT,NAMFIL,IEXT,LDOT,INEW,IERR)
C
      IF(IERR.NE.0) GO TO 200
C
      DO 10 I=1,20
      NAMCMD(I)=NAMFIL(I)
   10 CONTINUE
C
      CALL ISBYTE(0,NAMFIL,LDOT+3)
C
      OPEN(UNIT    = LCMD,
     &     FILE    = CNAMFIL,
     &     STATUS  = 'OLD',
     &     ACCESS  = 'SEQUENTIAL',
     &     FORM    = 'FORMATTED',
     &     IOSTAT  = STAT)
C
      IF(STAT.NE.0) THEN
                    CALL IOFERR(STAT)
                    GO TO 100
                    ENDIF
C
      RETURN
C
C     ------------------------------------------------------------------
C     Error returns
C     ------------------------------------------------------------------
C
  100 WRITE(CMSSG,110)
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(6,'(A)')CNAMFIL(1:LDOT+3)
  110 FORMAT('ERROR TRYING TO OPEN NEW CMD-FILE')
      IERR=1
      RETURN
C
  200 WRITE(CMSSG,210)
      WRITE(6,'(I6, A)')LDOT,CNAMFIL(1:LDOT+3)
      CALL MESSLOG(LOGUT,LOGUP)
  210 FORMAT('SYNTAX ERROR IN COMMAND FILE SPECIFICATION')
      IERR=1
      RETURN
      END
