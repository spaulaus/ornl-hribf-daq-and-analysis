C$PROG STATMAN   - Status display routine for SCOP
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 11/10/99
C     ******************************************************************
C
      SUBROUTINE STATMAN
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
      COMMON/DD11/ RATFLG,   RATAVG(9),   RATPAR(9),   RATTYP(9),
     &             RATDSP,   NRATE
C
      INTEGER*4              RATAVG,      RATPAR,      NRATE
      CHARACTER*4  RATFLG,   RATDSP,      RATTYP
C     ------------------------------------------------------------------
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LAG(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LAG,      NSCA
C     ------------------------------------------------------------------
      COMMON/SCD1/ SNITNAM
      CHARACTER*80 SNITNAM
C     ------------------------------------------------------------------
      COMMON/SCD2/ MSPRED,DISPSEC
      INTEGER*4    MSPRED,DISPSEC
C     ------------------------------------------------------------------
      COMMON/SD17/ GLIMON(16),GLIMLO(16),GLIMHI(16)
      CHARACTER*4  GLIMON
      REAL*4                  GLIMLO,    GLIMHI
C     ------------------------------------------------------------------
      INTEGER*4    I,J,N
C
      CHARACTER*4  STATE
C     ------------------------------------------------------------------
C
      SAVE
C
C     ------------------------------------------------------------------
C     Display RATE related status
C     ------------------------------------------------------------------
C
      WRITE(CMSSG,50)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,55)SNITNAM
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,56)DISPSEC
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,50)
      CALL MESSLOG(LOGUT,LOGUP)
C
   50 FORMAT('===================================================',
     &       '=============')
C
   55 FORMAT('Snit-file name = ',A)
   56 FORMAT('Displays/sec   = ',I2)
C
      WRITE(CMSSG,60)
   60 FORMAT('NAME            C   N   A   F  NAGV  STATE')
      CALL MESSLOG(LOGUT,LOGUP)
C
      DO 100 N=1,NSCA
C
      STATE=' OFF'
C
      IF(RATTYP(N).EQ.'SCAL') STATE='  ON'
C
      WRITE(CMSSG,70)(LAG(I,N),I=1,3),CC(N),NN(N),AA(N),FF(N),
     &                RATAVG(N),STATE
   70 FORMAT(3A4,1X,4I4,I6,3X,A)
      CALL MESSLOG(LOGUT,LOGUP)
C
  100 CONTINUE
C
C     ------------------------------------------------------------------
C     Display limit related status
C     ------------------------------------------------------------------
C
      WRITE(CMSSG,50)
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,110)
  110 FORMAT('Limit Specifications       LOW        HIGH')
C
      CALL MESSLOG(LOGUT,LOGUP)
C
      DO 120 J=1,NSCA
      WRITE(CMSSG,115)(LAG(I,J),I=1,3),GLIMLO(J),GLIMHI(J),GLIMON(J)
  115 FORMAT(3A4,6X,2F12.1,2X,A4)
      CALL MESSLOG(LOGUT,LOGUP)
  120 CONTINUE
C
      WRITE(CMSSG,50)
      CALL MESSLOG(LOGUT,LOGUP)
C
      RETURN
C
      END
