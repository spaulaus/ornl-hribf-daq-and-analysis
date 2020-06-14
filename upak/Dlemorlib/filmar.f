C$PROG FILMAR    - Writes NEOF file-marks & backs up NBAK file-marks
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE FILMAR(C,NEOF,NBAK,IERR)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
      INTEGER*4    LUINF,LUOUF
      CHARACTER*4              INFOP,OUFOP
C     ------------------------------------------------------------------
      INTEGER*4    C,NEOF,NBAK,IERR,I
C
      CHARACTER*4  STAT
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(OUFOP.EQ.'YES ') THEN
      CALL LDFILMAR(NEOF,NBAK,IERR)
      RETURN
      ENDIF
C
      IF(NEOF.LE.0) GO TO 20
C
      DO 10 I=1,NEOF
      CALL TAPHAN(C,'EOF ',1,STAT)
      IF(STAT.NE.'GOOD') GO TO 100
   10 CONTINUE
      IF(NBAK.LE.0) RETURN
C
   20 DO 30 I=1,NBAK
      CALL TAPHAN(C,'BF  ',1,STAT)
      IF(STAT.NE.'GOOD') GO TO 100
   30 CONTINUE
      RETURN
C
  100 IERR=1
      RETURN
      END
