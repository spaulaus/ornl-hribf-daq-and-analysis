C$PROG BEAMLINE  - Beamline entry routine
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED  4/31/2004
C     ******************************************************************
C
      SUBROUTINE BEAMLINE(IERR)
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
      COMMON/EP01/ BLNAME,EPOPEN
      CHARACTER*5  BLNAME,EPOPEN
C
      DATA         BLNAME,EPOPEN/'????','NO'/
C     ------------------------------------------------------------------
      INTEGER*4   IERR,NTRY
C
      CHARACTER*4 BLIST(5),BNAM
C
      DATA        BLIST/'rms','drs','enge','bl21','bl23'/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IERR=0
C
      CALL DINGER(3)
  100 WRITE(6,105)
  105 FORMAT(' You must enter the beamline you are working on')
      WRITE(6,110)BLIST
  110 FORMAT(' beamlines are - ',5(A,'  '))
C
      NTRY=0
C
  150 WRITE(6,155)
  155 FORMAT(' enter beamline name->',$)
C
      READ(5,160)BNAM
  160 FORMAT(A)
C
      IF(BNAM.EQ.'rms')  GO TO 200
      IF(BNAM.EQ.'drs')  GO TO 200
      IF(BNAM.EQ.'enge') GO TO 200
      IF(BNAM.EQ.'bl21') GO TO 200
      IF(BNAM.EQ.'bl23') GO TO 200
C
      CALL DINGER(3)
      WRITE(6,170)
  170 FORMAT(' BAD BOY! - try again')
      NTRY=NTRY+1
      IF(NTRY.GE.5) GO TO 500
      GO TO 150
C
  200 WRITE(6,205)
  205 FORMAT(' GOOD BOY!')
      BLNAME=BNAM
      CALL ISBYTE(0,BLNAME,4)
      RETURN
C
  500 WRITE(6,505)
  505 FORMAT(' I give up! - bye bye')
      IERR=1
      STOP
      END

