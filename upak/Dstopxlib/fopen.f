C$PROG FOPEN     - Opens file for Eloss storage
C
C     ******************************************************************
C     BY T.C. Awes at HHIRF - LAST MODIFIED by W.T. Milner 04/25/2002
C     ******************************************************************
C
      SUBROUTINE FOPEN(CNAMFIL,LUN,IERR)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4    LUN,IERR,ISTAT
C
      CHARACTER*80 CNAMFIL
C     ------------------------------------------------------------------
C
      IERR=0
      CLOSE(UNIT=LUN)
C
      OPEN(UNIT     = LUN,
     &     FILE     = CNAMFIL,
     &     STATUS   = 'NEW',
     &     ACCESS   = 'SEQUENTIAL',
     &     RECL     = 80,
     &     IOSTAT   = ISTAT)
C
      IF(ISTAT.EQ.0) RETURN
C
      WRITE(6,10)ISTAT
   10 FORMAT(1H ,'ERROR OPENING ELOSS OUTPUT FILE - ISTAT =',I8)
      IERR=1
      RETURN
      END
