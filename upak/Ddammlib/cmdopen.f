C$PROG CMDOPEN   - Opens command files for damm
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE CMDOPEN(IWD,NAMCMD,LCI,LIN,LCM,IERR)
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
      INTEGER*4 IWD(20),NAMCMD(20),NAMFIL(20),DISK(2)
      integer*4 jext
      character*4 cjext, cdisk(2)
      equivalence (cjext,jext), (cdisk,disk)
C
      CHARACTER*80 CNAMFIL
C
      EQUIVALENCE (CNAMFIL,NAMFIL)
C
      DATA cJEXT,cDISK/'.cmd',2*'    '/
C
      SAVE
C
C     ------------------------------------------------------------------
C     ROUTINE TO OPEN COMMAND FILES (DEFAULT EXT - .CMD)
C     ------------------------------------------------------------------
C     IWD     - CONTAINS CMD FILENAME<.EXT> - INPUT
C     LCI     = LOGICAL UNIT FOR VDT INPUT  - INPUT
C     LIN     = LOGICAL UNIT FOR INPUT      - INPUT
C     LCM     = LOGICAL UNIT TO BE ASSIGNED - INPUT
C     NAMCMD  - CONTAINS FULL FILENAME      - RETURNED
C     IERR    = ERROR FLAG                  - RETURNED
C
C     IF NO ERROR, SETS LIN=LCM ON RETURN
C     ------------------------------------------------------------------
C
      IERR=0
C
      CLOSE(UNIT=LCM)
C
      CALL BILNAM(IWD,DISK,JEXT,NAMFIL,IEXT,LDOT,INEW,IERR)
C
      IF(IERR.NE.0) GO TO 200
C
      DO 10 I=1,20
      NAMCMD(I)=NAMFIL(I)
   10 CONTINUE
C
      OPEN(UNIT    = LCM,
     &     FILE    = CNAMFIL,
     &     STATUS  = 'OLD',
     &     IOSTAT  = STAT)
C
      IF(STAT.NE.0) THEN
                    CALL IOFERR(STAT)
                    GO TO 100
                    ENDIF
C
      LIN=LCM
C
      REWIND LIN
C
      RETURN
C
  100 WRITE(CMSSG,110)
  110 FORMAT('ERROR TRYING TO OPEN NEW CMD-FILE')
      GO TO 300
C
  200 WRITE(CMSSG,210)
  210 FORMAT('SYNTAX ERROR IN COMMAND FILE SPECIFICATION')
C
  300 CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,310)
  310 FORMAT('CONTROL RETURNED TO VDT')
C
      CALL MESSLOG(LOGUT,LOGUP)
      LIN=LCI
      IERR=1
      RETURN
      END
