C$PROG MARKONS   - Turns ON 1-D SAM markers which are currently OFF
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/12/2003
C     ******************************************************************
C
      SUBROUTINE MARKONS(IDW)
C   
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SM08/ MARFS(4,20),JLOX,JHIX,JLOF,JHIF,SLOX,SHIX,SLOF,SHIF
C
      CHARACTER*4  MARFS
      INTEGER*4                JLOX,JHIX,JLOF,JHIF
      REAL*4                                       SLOX,SHIX,SLOF,SHIF
C     ------------------------------------------------------------------
C
      INTEGER*4    IDW
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(MARFS(1,IDW).EQ.'OFF ') THEN
      CALL MARKIT1(IDW,'OBLU',1,SLOX)
      MARFS(1,IDW)='ON  '
      ENDIF
C
      IF(MARFS(2,IDW).EQ.'OFF ') THEN
      CALL MARKIT1(IDW,'OBLU',1,SHIX)
      MARFS(2,IDW)='ON  '
      ENDIF
C
      IF(MARFS(3,IDW).EQ.'OFF ') THEN
      CALL MARKIT1(IDW,'OBLU',2,SLOF)
      MARFS(3,IDW)='ON  '
      ENDIF
C
      IF(MARFS(4,IDW).EQ.'OFF ') THEN
      CALL MARKIT1(IDW,'OBLU',2,SHIF)
      MARFS(4,IDW)='ON  '
      ENDIF
C
      RETURN
      END
