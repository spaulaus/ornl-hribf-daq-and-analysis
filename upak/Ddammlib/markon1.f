C$PROG MARKON1   - Turns ON 1-D markers which are currently OFF
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/12/2003
C     ******************************************************************
C
      SUBROUTINE MARKON1(IDW)
C   
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/PL01/ ILOX(20),IHIX(20),ILOF(20),IHIF(20),      !/PL01
     &             FLOX(20),FHIX(20),FLOF(20),FHIF(20),      !/PL01
     &             GWID,MOPL,ECAL(3)                         !/PL01
C
      INTEGER*4    ILOX,    IHIX,    ILOF,    IHIF,  MOPL
      REAL*4       FLOX,    FHIX,    FLOF,    FHIF
      REAL*4       GWID,    ECAL
C     ------------------------------------------------------------------
      COMMON/PL13/ MARF(4,20),MARF2(2,20)
      CHARACTER*4  MARF,      MARF2
C     ------------------------------------------------------------------
C
      INTEGER*4    IDW
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(MARF(1,IDW).EQ.'OFF ') THEN
      CALL MARKIT1(IDW,'OBLU',1,FLOX(IDW))
      MARF(1,IDW)='ON  '
      ENDIF
C
      IF(MARF(2,IDW).EQ.'OFF ') THEN
      CALL MARKIT1(IDW,'OBLU',1,FHIX(IDW))
      MARF(2,IDW)='ON  '
      ENDIF
C
      IF(MARF(3,IDW).EQ.'OFF ') THEN
      CALL MARKIT1(IDW,'OBLU',2,FLOF(IDW))
      MARF(3,IDW)='ON  '
      ENDIF
C
      IF(MARF(4,IDW).EQ.'OFF ') THEN
      CALL MARKIT1(IDW,'OBLU',2,FHIF(IDW))
      MARF(4,IDW)='ON  '
      ENDIF
C
      RETURN
      END
