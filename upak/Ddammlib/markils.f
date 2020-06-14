C$PROG MARKILS  -  Kills markers for SAM 1-D displays
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/12/2003
C     ******************************************************************
C
      SUBROUTINE MARKILS(IDW)
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
      IF(MARFS(1,IDW).EQ.'ON  ') CALL MARKIT1(IDW,'OBLU',1,SLOX)
      IF(MARFS(2,IDW).EQ.'ON  ') CALL MARKIT1(IDW,'OBLU',1,SHIX)
      IF(MARFS(3,IDW).EQ.'ON  ') CALL MARKIT1(IDW,'OBLU',2,SLOF)
      IF(MARFS(4,IDW).EQ.'ON  ') CALL MARKIT1(IDW,'OBLU',2,SHIF)
C   
      JLOX=-1
      JHIX=-1
      JLOF=-1
      JHIF=-1
C
      SLOX=-1.0
      SHIX=-1.0
      SLOF=-1.0
      SHIF=-1.0
C
      MARFS(1,IDW)='    '
      MARFS(2,IDW)='    '
      MARFS(3,IDW)='    '
      MARFS(4,IDW)='    '
C
      RETURN
      END
