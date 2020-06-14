C$PROG FADDUM    - Adds input fadd his-file to output fadd his-file
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 08/20/2005
C     ******************************************************************
C
      SUBROUTINE FADDUM(IERR)
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
      COMMON/HIS3/ RECLIS(100),HPCLIS(100),NLIS
      INTEGER*4    RECLIS,     HPCLIS,     NLIS
C     ------------------------------------------------------------------
      COMMON/HIS4/ IBUFF(16384),OBUFF(16384)
      INTEGER*4    IBUFF,       OBUFF
C
      INTEGER*2    IBUFH(32768),OBUFH(32768)
      EQUIVALENCE (IBUFH,IBUFF),(OBUFH,OBUFF)
C     ------------------------------------------------------------------
C
      INTEGER*4    IERR,N,I,HWPCVAL
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IERR=0
C
      CALL FADDCHECK(IERR)            !Check input/output compatility
C
      IF(IERR.NE.0) RETURN            !Return if error
C
      DO 100 N=1,NRHI                 !Loop on all his-file records
C
      IF(MSGF.NE.'    ') RETURN       !Test for Ctrl/C interrupt
C
      READ(LUHI,REC=N,ERR=1000)IBUFF  !Read input  record
C
      READ(LUHO,REC=N,ERR=1010)OBUFF  !Read output record
C
      IF(HWPCVAL(N).EQ.1) THEN        !Test for 16-bit data
      DO 10 I=1,32768                 !Loop to add input data to output
      OBUFH(I)=OBUFH(I)+IBUFH(I)      !in 16-bit mode
   10 CONTINUE
      ENDIF
C
      IF(HWPCVAL(N).EQ.2) THEN        !Test for 32-bit data
      DO 20 I=1,16384                 !Loop to add input data to output
      OBUFF(I)=OBUFF(I)+IBUFF(I)      !in 32-bit mode
   20 CONTINUE
      ENDIF
C
      WRITE(LUHO,REC=N,ERR=1020)OBUFF !Write sum-buffer to output file
C
  100 CONTINUE
C
      RETURN
C
C     ------------------------------------------------------------------
C     Return error messages
C     ------------------------------------------------------------------
C
 1000 WRITE(CMSSG,1005)N
 1005 FORMAT('Error reading input file at REC# ',I8)
      GO TO 2000
C
 1010 WRITE(CMSSG,1015)N
 1015 FORMAT('Error reading output file at REC# ',I8)
      GO TO 2000
C
 1020 WRITE(CMSSG,1025)N
 1025 FORMAT('Error writing output file at REC# ',I8)
      GO TO 2000
C
 2000 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
