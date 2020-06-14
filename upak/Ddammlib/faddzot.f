C$PROG FADDZOT   - Zeros fadd output his-file
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 08/20/2005
C     ******************************************************************
C
      SUBROUTINE FADDZOT(IERR)
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
      COMMON/HIS1/ NAMI,NAMO,LUHI,LUDI,LUHO,LUDO,INOP,OUOP
      CHARACTER*160 NAMI,NAMO
      INTEGER*4              LUHI,LUDI,LUHO,LUDO
      CHARACTER*4                                INOP,OUOP
C     ------------------------------------------------------------------
      COMMON/HIS2/ NRDI,NRHI,NRDO,NRHO
      INTEGER*4    NRDI,NRHI,NRDO,NRHO
C     ------------------------------------------------------------------
      COMMON/HIS4/ IBUFF(16384),OBUFF(16384)
      INTEGER*4    IBUFF,       OBUFF
C     ------------------------------------------------------------------
C
      INTEGER*4    IERR,N,I
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IERR=0
C
      CALL FADDCHECK(IERR)            !Check input/output compatibility??
      IF(IERR.NE.0) RETURN
C
      IF(OUOP.NE.'YES ') GO TO 1000 !Check for output open
C
      DO 10 I=1,16384               !Set buffer to zero
      IBUFF(I)=0
   10 CONTINUE
C
      DO 20 N=1,NRHO                !Loop on all output records
      IF(MSGF.NE.'    ') RETURN     !Test for Ctrl/C interrupt
      WRITE(LUHO,REC=N)IBUFF        !Write zero-filled buffer to output
   20 CONTINUE
C
      RETURN
C
C     ------------------------------------------------------------------
C     Return error messages
C     ------------------------------------------------------------------
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('Output file is not open - cmd not executed')
      GO TO 2000
C
 2000 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
