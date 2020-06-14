C$PROG DMAR      - Displays 1-D markers which are ON and Peaks for SAM
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/12/2003
C     ******************************************************************
C
      SUBROUTINE DMAR(IDW)
C   
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SM05/PAT(500,15),NPAT,MAXPAT,NUPAT,FWA,FWB,FWC,ASLO,ASHI
C
      REAL*4      PAT,                          FWA,FWB,FWC,ASLO,ASHI
      INTEGER*4               NPAT,MAXPAT,NUPAT
C     ------------------------------------------------------------------
      COMMON/SM08/ MARFS(4,20),JLOX,JHIX,JLOF,JHIF,SLOX,SHIX,SLOF,SHIF
C
      CHARACTER*4  MARFS
      INTEGER*4                JLOX,JHIX,JLOF,JHIF
      REAL*4                                       SLOX,SHIX,SLOF,SHIF
C     ------------------------------------------------------------------
C
      INTEGER*4    IDW,I
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(MARFS(1,IDW).EQ.'ON  ') CALL MARKIT1(IDW,'OBLU',1,SLOX)
C
      IF(MARFS(2,IDW).EQ.'ON  ') CALL MARKIT1(IDW,'OBLU',1,SHIX)
C
      IF(MARFS(3,IDW).EQ.'ON  ') CALL MARKIT1(IDW,'OBLU',2,SLOF)
C
      IF(MARFS(4,IDW).EQ.'ON  ') CALL MARKIT1(IDW,'OBLU',2,SHIF)
C
      DO 20 I=1,NPAT
      CALL PMAR(IDW,I)
   20 CONTINUE
C
      RETURN
      END
