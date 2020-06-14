C$PROG CALLER    - Caller routine for program SWAPO
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/25/02
C     ******************************************************************
C
      SUBROUTINE CALLER(IDONE,IERR)
C
      CHARACTER*4  IDONE
C
      IF(IDONE.NE.'YES ') CALL EQUATE(IDONE,IERR)
C   
      IF(IDONE.NE.'YES ') CALL MODFIN(IERR)
      IF(IERR.NE.0)       RETURN
C   
      IF(IDONE.NE.'YES ') CALL LWDMOD
C   
      IF(IDONE.NE.'YES ') CALL CMDPROC(IDONE,IERR)
C   
      RETURN
      END
